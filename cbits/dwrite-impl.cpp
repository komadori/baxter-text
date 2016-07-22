#include <dwrite.h>

#include "btcb.h"

struct BTCB_FontDescImpl {
    IDWriteFactory* dwrite_factory;
    IDWriteTextFormat* dwrite_format;
};

BTCB_FontDesc* btcb_create_font_desc(
    char** familyPtrs,
    int* familyLens,
    double size)
{
    BTCB_FontDesc* fd = (BTCB_FontDesc*)calloc(1, sizeof(BTCB_FontDesc));

    HRESULT hr = DWriteCreateFactory(
        DWRITE_FACTORY_TYPE_SHARED,
        __uuidof(IDWriteFactory),
        reinterpret_cast<IUnknown**>(&fd->dwrite_factory));

    return fd;
}

void btcb_free_font_desc(BTCB_FontDesc* fd)
{
    fd->dwrite_factory->Release();
}
