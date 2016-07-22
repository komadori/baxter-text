#ifndef BAXTER_TEXT_CBITS_H
#define BAXTER_TEXT_CBITS_H

#ifdef __cplusplus
extern "C" {
#endif

struct BTCB_FontDescImpl;
typedef struct BTCB_FontDescImpl BTCB_FontDesc;

extern BTCB_FontDesc* btcb_create_font_desc(
    char** familyptrs,
    int* familylens,
    double size);

extern void btcb_free_font_desc(
    BTCB_FontDesc* fd);

#ifdef __cplusplus
}
#endif

#endif
