#define VC_EXTRALEAN
#include <windows.h>
#include <dwrite.h>
#include <cassert>
#include <cstdlib>
#include <memory>

#include "btcb.h"

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
    }

    std::unique_ptr<IDWriteFactory, COMDeleter> mFactory;
    std::unique_ptr<IDWriteFontCollection, COMDeleter> mFontSet;
    std::unique_ptr<IDWriteTextFormat, COMDeleter> mFormat;
};

BTCB_FontDesc* btcb_create_font_desc(
    BTCB_String** families,
    double size)
{
    return new BTCB_FontDesc(families, size);
}

void btcb_free_font_desc(BTCB_FontDesc* fd)
{
    delete fd;
}

struct BaxterGlyphRun {
    BaxterGlyphRun* nextRun;
    BTCB_Font* font;
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
    BaxterTextRenderer(BaxterGlyphRun** nextSlot)
        : mRefCount(1)
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
        new BaxterTextRenderer(&firstRun));
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

BTCB_Font* btcb_get_run_font(
    BTCB_GlyphRun* runExt)
{
    BaxterGlyphRun* run = BaxterGlyphRun::fromExt(runExt);
    return run->font;
}

void btcb_free_font(
    BTCB_Font* font)
{
}

void btcb_free_run(
    BTCB_GlyphRun* runExt)
{
    BaxterGlyphRun* run = BaxterGlyphRun::fromExt(runExt);
    free(run);
}
