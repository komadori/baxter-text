#ifndef BAXTER_TEXT_CBITS_H
#define BAXTER_TEXT_CBITS_H

#ifdef __cplusplus
extern "C" {
#endif

struct BTCB_StringImpl;
typedef struct BTCB_StringImpl BTCB_String;

struct BTCB_FontDescImpl;
typedef struct BTCB_FontDescImpl BTCB_FontDesc;

struct BTCB_GlyphFontImpl;
typedef struct BTCB_GlyphFontImpl BTCB_GlyphFont;

typedef struct {
    int glyph;
    double x;
    double y;
} BTCB_Glyph;

struct BTCB_GlyphRunImpl;
typedef struct BTCB_GlyphRunImpl BTCB_GlyphRun;

typedef struct {
    double width;
    double height;
} BTCB_GlyphMetrics;

extern BTCB_FontDesc* btcb_create_font_desc(
    BTCB_String** families,
    double size);

extern void btcb_free_font_desc(
    BTCB_FontDesc* fd);

extern BTCB_GlyphRun* btcb_layout_text(
    BTCB_String* text,
    BTCB_FontDesc* fd);

extern int btcb_get_run_length(
    BTCB_GlyphRun* run);

extern BTCB_GlyphRun* btcb_get_next_run(
    BTCB_GlyphRun* run);

extern BTCB_GlyphFont* btcb_get_run_font(
    BTCB_GlyphRun* run);

extern void btcb_get_glyph_metrics(
    BTCB_GlyphFont* font,
    int glyph,
    BTCB_GlyphMetrics* out);

extern void btcb_render_glyph(
    BTCB_GlyphFont* font,
    int glyph,
    unsigned char* buffer,
    int width,
    int height,
    int stride);

extern void btcb_free_glyph_font(
    BTCB_GlyphFont* font);

extern void btcb_free_run(
    BTCB_GlyphRun* run);

#ifdef __cplusplus
}
#endif

#endif
