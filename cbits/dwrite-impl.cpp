#define VC_EXTRALEAN
#define INITGUID
#include <windows.h>
#include <wincodec.h>
#include <d2d1.h>
#include <dwrite.h>
#include <cassert>
#include <cstdlib>
#include <memory>

#include "btcb.h"

DEFINE_GUID(GUID_WICPixelFormat8bppAlpha,
   0xe6cd0116, 0xeeba, 0x4161, 0xaa, 0x85, 0x27, 0xdd, 0x9f, 0xb3, 0xa8, 0x95);

struct COMApartment {
    COMApartment() : mSetup(false) {}
    void setup() {
        if (!mSetup) {
            HRESULT hr = CoInitializeEx(
                NULL, COINIT_MULTITHREADED | COINIT_DISABLE_OLE1DDE);
            assert (SUCCEEDED(hr));
            mSetup = true;
        }
    }
    ~COMApartment() {
        if (mSetup) {
            CoUninitialize();
        }
    }
private:
    bool mSetup;
};

static thread_local COMApartment gCom; 

DWORD COMKeepAliveThreadProc(LPVOID param)
{
    HANDLE sem = (HANDLE)param;
    gCom.setup();
    BOOL ok = ReleaseSemaphore(sem, 1, NULL);
    assert (ok);
    // Wait for the keep alive manager to release this thread
    WaitForSingleObject(sem, INFINITE);
    CloseHandle(sem);
    return 0;
}

// COM must remain initialised on at least one thread until all COM objects
// have been released
class COMKeepAliveManager {
public:
    COMKeepAliveManager()
        : mRefCount(0), mSem(NULL) { }
    void acquire() {
        if (1 == InterlockedIncrement(&mRefCount)) {
            mSem = CreateSemaphore(NULL, 0, 1, NULL);
            HANDLE thr = CreateThread(
                NULL, 0, COMKeepAliveThreadProc, (LPVOID)mSem, 0, NULL);
            assert (thr);
            // Wait for keep alive thread to initialise COM
            DWORD wait = WaitForSingleObject(mSem, INFINITE);
            assert (WAIT_OBJECT_0 == wait);
        }
    }
    void release() {
        HANDLE oldSem = mSem;
        if (0 == InterlockedDecrement(&mRefCount)) {
            BOOL ok = ReleaseSemaphore(oldSem, 1, NULL);
            assert (ok);
        }
    }
private:
    unsigned long mRefCount;
    HANDLE mSem;
};

static COMKeepAliveManager gComKeepAlive;

struct COMKeepAlive {
    COMKeepAlive() { gComKeepAlive.acquire(); }
    ~COMKeepAlive() { gComKeepAlive.release(); }
};

struct COMDeleter {
    template<typename T> void operator()(T* ptr) {
        ptr->Release();
    }
};

struct BTCB_StringImpl {
    int len;
    WCHAR str[];
};

struct BTCB_FontDescImpl {
    BTCB_FontDescImpl(
        BTCB_String** families,
        double size)
    : mRefCount(1)
    {
        HRESULT hr;

        IDWriteFactory* factory;
        hr = DWriteCreateFactory(
            DWRITE_FACTORY_TYPE_SHARED,
            __uuidof(IDWriteFactory),
            reinterpret_cast<IUnknown**>(&factory));
        assert (SUCCEEDED(hr));
        mFactory.reset(factory);

        IDWriteFontCollection* fontSet;
        hr = factory->GetSystemFontCollection(&fontSet, FALSE);
        assert (SUCCEEDED(hr));
        mFontSet.reset(fontSet);

        WCHAR* familyName = NULL;
        for (int i=0; families[i]; i++) {
            UINT32 fontIndex;
            BOOL fontExists;
            fontSet->FindFamilyName(families[i]->str, &fontIndex, &fontExists);
            if (!fontExists && families[i+1]) {
                // Try next family name if there are more
                continue;
            }
            familyName = families[i]->str;
            break;
        }

        IDWriteTextFormat* format;
        hr = factory->CreateTextFormat(
            familyName,
            fontSet,
            DWRITE_FONT_WEIGHT_NORMAL,
            DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_STRETCH_NORMAL,
            size,
            L"en-GB",
            &format);
        assert (SUCCEEDED(hr));
        mFormat.reset(format);

        // Create WIC factory
        IWICImagingFactory* wicFactory;
        hr = CoCreateInstance(
            CLSID_WICImagingFactory,
            NULL,
            CLSCTX_INPROC_SERVER,
            IID_IWICImagingFactory,
            (LPVOID*)&wicFactory);
        assert (SUCCEEDED(hr));
        mWicFactory.reset(wicFactory);

        // Create WIC bitmap
        IWICBitmap* wicBitmap;
        hr = wicFactory->CreateBitmap(
            (int)size, (int)size,
            GUID_WICPixelFormat8bppAlpha,
            WICBitmapCacheOnDemand,
            &wicBitmap);
        assert (SUCCEEDED(hr));
        mWicBitmap.reset(wicBitmap);

        // Create D2D factory
        ID2D1Factory* d2dFactory;
        hr = D2D1CreateFactory(
            D2D1_FACTORY_TYPE_MULTI_THREADED,
            &d2dFactory);
        assert (SUCCEEDED(hr));
        mD2dFactory.reset(d2dFactory);

        // Create D2D render target
        ID2D1RenderTarget* d2dTarget;
        D2D1_RENDER_TARGET_PROPERTIES targetProps =
            D2D1::RenderTargetProperties();
        targetProps.pixelFormat =
            D2D1::PixelFormat(DXGI_FORMAT_A8_UNORM, D2D1_ALPHA_MODE_STRAIGHT);
        hr = d2dFactory->CreateWicBitmapRenderTarget(
            wicBitmap,
            targetProps,
            &d2dTarget);
        assert (SUCCEEDED(hr));
        mD2dTarget.reset(d2dTarget);

        // Create D2D white brush
        ID2D1SolidColorBrush* d2dBrush;
        hr = d2dTarget->CreateSolidColorBrush({1,1,1,1}, &d2dBrush);
        assert (SUCCEEDED(hr));
        mD2dBrush.reset(d2dBrush);
    }

    void AddRef() {
        InterlockedIncrement(&mRefCount);
    }

    void Release() {
        if (!InterlockedDecrement(&mRefCount)) {
            delete this;
        }
    }

    unsigned long mRefCount;
    COMKeepAlive comKeepAlive;
    std::unique_ptr<IDWriteFactory, COMDeleter> mFactory;
    std::unique_ptr<IDWriteFontCollection, COMDeleter> mFontSet;
    std::unique_ptr<IDWriteTextFormat, COMDeleter> mFormat;
    std::unique_ptr<IWICImagingFactory, COMDeleter> mWicFactory;
    std::unique_ptr<IWICBitmap, COMDeleter> mWicBitmap;
    std::unique_ptr<ID2D1Factory, COMDeleter> mD2dFactory;
    std::unique_ptr<ID2D1RenderTarget, COMDeleter> mD2dTarget;
    std::unique_ptr<ID2D1Brush, COMDeleter> mD2dBrush;
};

BTCB_FontDesc* btcb_create_font_desc(
    BTCB_String** families,
    double size)
{
    return new BTCB_FontDesc(families, size);
}

void btcb_free_font_desc(BTCB_FontDesc* fd)
{
    gCom.setup();
    fd->Release();
}

struct BTCB_GlyphFontImpl {
    std::unique_ptr<BTCB_FontDesc, COMDeleter> mContext;
    std::unique_ptr<IDWriteFontFace, COMDeleter> mFont;
    DWRITE_FONT_METRICS mMetrics;
    double mSize;

    BTCB_GlyphFontImpl(
        BTCB_FontDesc* context, IDWriteFontFace* font, double size)
        : mContext(context)
        , mFont(font)
        , mSize(size)
    {
        context->AddRef();
        font->AddRef();
        font->GetMetrics(&mMetrics);
    }

    ~BTCB_GlyphFontImpl()
    {
    }
};

struct BaxterGlyphRun {
    BaxterGlyphRun* nextRun;
    BTCB_GlyphFont* font;
    int glyphCount;
    BTCB_Glyph glyphs[];

    inline BTCB_GlyphRun* toExt() {
        return reinterpret_cast<BTCB_GlyphRun*>(&glyphs);
    }

    static inline BaxterGlyphRun* fromExt(BTCB_GlyphRun* runExt) {
        if (runExt) {
            return reinterpret_cast<BaxterGlyphRun*>(
                reinterpret_cast<char*>(runExt) -
                offsetof(BaxterGlyphRun, glyphs));
        }
        return NULL;
    }
};

class BaxterTextRenderer : public IDWriteTextRenderer
{
public:
    BaxterTextRenderer(BTCB_FontDesc* context, BaxterGlyphRun** nextSlot)
        : mRefCount(1)
        , mContext(context)
        , mNextSlot(nextSlot)
    {
    }

    ~BaxterTextRenderer()
    {
    }

    IFACEMETHOD(IsPixelSnappingDisabled)(
        void* clientDrawingContext,
        BOOL* isDisabled)
    {
        *isDisabled = TRUE;
        return S_OK;
    }

    IFACEMETHOD(GetCurrentTransform)(
        void* clientDrawingContext,
        DWRITE_MATRIX* transform)
    {
        transform->m11 = 1;
        transform->m12 = 0;
        transform->m21 = 0;
        transform->m22 = 1;
        transform->dx = 0;
        transform->dy = 0;
        return S_OK;
    }

    IFACEMETHOD(GetPixelsPerDip)(
        void* clientDrawingContext,
        FLOAT* pixelsPerDip)
    {
        *pixelsPerDip = 1;
        return S_OK;
    }

    IFACEMETHOD(DrawGlyphRun)(
        void* clientDrawingContext,
        FLOAT baselineOriginX,
        FLOAT baselineOriginY,
        DWRITE_MEASURING_MODE measuringMode,
        DWRITE_GLYPH_RUN const* glyphRun,
        DWRITE_GLYPH_RUN_DESCRIPTION const* glyphRunDescription,
        IUnknown* clientDrawingEffect)
    {
        BaxterGlyphRun* run = reinterpret_cast<BaxterGlyphRun*>(calloc(1,
            offsetof(BaxterGlyphRun, glyphs) +
            sizeof(BTCB_Glyph)*glyphRun->glyphCount));
        *mNextSlot = run;
        mNextSlot = &run->nextRun;

        run->font = new BTCB_GlyphFont(
            mContext, glyphRun->fontFace, glyphRun->fontEmSize);
        run->glyphCount = glyphRun->glyphCount;

        float xPos = baselineOriginX;
        float yPos = baselineOriginY;
        for (int i=0; i<glyphRun->glyphCount; i++) {
            BTCB_Glyph* glyphOut = &run->glyphs[i];
            float x = xPos + glyphRun->glyphOffsets[i].advanceOffset;
            float y = yPos - glyphRun->glyphOffsets[i].ascenderOffset;
            glyphOut->glyph = glyphRun->glyphIndices[i];
            glyphOut->x = x;
            glyphOut->y = y;
            xPos += glyphRun->glyphAdvances[i];
        }
        return S_OK;
    }

    IFACEMETHOD(DrawUnderline)(
        void* clientDrawingContext,
        FLOAT baselineOriginX,
        FLOAT baselineOriginY,
        DWRITE_UNDERLINE const* underline,
        IUnknown* clientDrawingEffect)
    {
        return S_OK;
    }

    IFACEMETHOD(DrawStrikethrough)(
        void* clientDrawingContext,
        FLOAT baselineOriginX,
        FLOAT baselineOriginY,
        DWRITE_STRIKETHROUGH const* strikethrough,
        IUnknown* clientDrawingEffect)
    {
        return S_OK;
    }

    IFACEMETHOD(DrawInlineObject)(
        void* clientDrawingContext,
        FLOAT originX,
        FLOAT originY,
        IDWriteInlineObject* inlineObject,
        BOOL isSideways,
        BOOL isRightToLeft,
        IUnknown* clientDrawingEffect)
    {
        return S_OK;
    }

    IFACEMETHOD_(unsigned long, AddRef)()
    {
        return InterlockedIncrement(&mRefCount);
    }

    IFACEMETHOD_(unsigned long, Release)()
    {
        unsigned long count = InterlockedDecrement(&mRefCount);
        if (count == 0) {
            delete this;
        }
        return count;
    }

    IFACEMETHOD(QueryInterface)(
        IID const& riid,
        void** ppvObject)
    {
        if (__uuidof(IDWriteTextRenderer) == riid)
        {
            *ppvObject = this;
        }
        else if (__uuidof(IUnknown) == riid)
        {
            *ppvObject = this;
        }
        else {
            *ppvObject = NULL;
            return E_FAIL;
        }
        AddRef();
        return S_OK;
    }

private:
    unsigned long mRefCount;
    BTCB_FontDesc* mContext;
    BaxterGlyphRun** mNextSlot;
};

BTCB_GlyphRun* btcb_layout_text(
    BTCB_String* text,
    BTCB_FontDesc* fd)
{
    IDWriteTextLayout* layout;
    HRESULT hr = fd->mFactory->CreateTextLayout(
        text->str,
        text->len,
        fd->mFormat.get(),
        1000.0, 1000.0,
        &layout);
    assert (SUCCEEDED(hr));
    std::unique_ptr<IDWriteTextLayout, COMDeleter> layoutRef(layout);

    BaxterGlyphRun* firstRun = NULL;
    std::unique_ptr<BaxterTextRenderer, COMDeleter> rendererRef(
        new BaxterTextRenderer(fd, &firstRun));
    layoutRef->Draw(NULL, rendererRef.get(), 0, 0);

    return firstRun->toExt();
}

int btcb_get_run_length(
    BTCB_GlyphRun* runExt)
{
    return BaxterGlyphRun::fromExt(runExt)->glyphCount;
}

BTCB_GlyphRun* btcb_get_next_run(
    BTCB_GlyphRun* runExt)
{
    BaxterGlyphRun* run = BaxterGlyphRun::fromExt(runExt);
    return run->nextRun ? (BTCB_GlyphRun*)run->nextRun->glyphs : NULL;
}

BTCB_GlyphFont* btcb_get_run_font(
    BTCB_GlyphRun* runExt)
{
    BaxterGlyphRun* run = BaxterGlyphRun::fromExt(runExt);
    return run->font;
}

void btcb_get_glyph_metrics(
    BTCB_GlyphFont* font,
    int glyph,
    BTCB_GlyphMetrics* out)
{
    UINT16 glyphId = static_cast<UINT16>(glyph);
    DWRITE_GLYPH_METRICS metrics;
    HRESULT hr = font->mFont->GetDesignGlyphMetrics(
        &glyphId, 1, &metrics);
    assert (SUCCEEDED(hr));

    double scale = font->mSize / font->mMetrics.designUnitsPerEm;
    out->width = (metrics.advanceWidth -
        metrics.leftSideBearing - metrics.rightSideBearing) * scale;
    out->height = (metrics.advanceHeight -
        metrics.topSideBearing - metrics.bottomSideBearing) * scale;
}

void btcb_render_glyph(
    BTCB_GlyphFont* font,
    int glyph,
    unsigned char* buffer,
    int width,
    int height,
    int stride)
{
    UINT16 glyphIndex = static_cast<UINT16>(glyph);
    DWRITE_GLYPH_METRICS metrics;
    HRESULT hr = font->mFont->GetDesignGlyphMetrics(
        &glyphIndex, 1, &metrics);
    assert (SUCCEEDED(hr));

    D2D1_POINT_2F point = {0, height};
    DWRITE_GLYPH_RUN run;
    run.fontFace = font->mFont.get();
    run.fontEmSize = font->mSize;
    run.glyphCount = 1;
    run.glyphIndices = &glyphIndex;
    FLOAT glyphAdvance = 0.0;
    run.glyphAdvances = &glyphAdvance;
    double scale = font->mSize / font->mMetrics.designUnitsPerEm;
    DWRITE_GLYPH_OFFSET glyphOffset =
        {-(int)(scale*metrics.leftSideBearing), 0};
    run.glyphOffsets = &glyphOffset;
    run.isSideways = false;
    run.bidiLevel = 0;

    gCom.setup();
    font->mContext->mD2dTarget->BeginDraw();
    font->mContext->mD2dTarget->DrawGlyphRun(
        point, &run, font->mContext->mD2dBrush.get());
    hr = font->mContext->mD2dTarget->EndDraw();
    assert (SUCCEEDED(hr));

    WICRect rect = {0, 0, width, height};
    hr = font->mContext->mWicBitmap->CopyPixels(
        &rect, stride, stride*height, buffer);
    assert (SUCCEEDED(hr));
}

void btcb_free_glyph_font(
    BTCB_GlyphFont* font)
{
    delete font;
}

void btcb_free_run(
    BTCB_GlyphRun* runExt)
{
    BaxterGlyphRun* run = BaxterGlyphRun::fromExt(runExt);
    free(run);
}
