#include <dwrite.h>
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
        mFactory.reset(factory);

        IDWriteFontCollection* fontSet;
        hr = factory->GetSystemFontCollection(&fontSet, FALSE);
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
        factory->CreateTextFormat(
            familyName,
            fontSet,
            DWRITE_FONT_WEIGHT_NORMAL,
            DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_STRETCH_NORMAL,
            size,
            L"en-GB",
            &format);
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

BTCB_GlyphRun* btcb_layout_text(
    BTCB_String* text,
    BTCB_FontDesc* fd)
{
    return NULL;
}

int btcb_get_run_length(
    BTCB_GlyphRun* run)
{
    return 0;
}

BTCB_GlyphRun* btcb_get_next_run(
    BTCB_GlyphRun* run)
{
    return NULL;
}

void btcb_free_run(
    BTCB_GlyphRun* run)
{
}
