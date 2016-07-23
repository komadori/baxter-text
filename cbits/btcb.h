#ifndef BAXTER_TEXT_CBITS_H
#define BAXTER_TEXT_CBITS_H

#ifdef __cplusplus
extern "C" {
#endif

struct BTCB_StringImpl;
typedef struct BTCB_StringImpl BTCB_String;

struct BTCB_FontDescImpl;
typedef struct BTCB_FontDescImpl BTCB_FontDesc;

extern BTCB_FontDesc* btcb_create_font_desc(
    BTCB_String** families,
    double size);

extern void btcb_free_font_desc(
    BTCB_FontDesc* fd);

#ifdef __cplusplus
}
#endif

#endif
