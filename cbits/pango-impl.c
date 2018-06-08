#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <glib.h>
#include <glib-object.h>
#include <pango/pango.h>
#include <pango/pangoft2.h>
#include <ft2build.h>
#include FT_FREETYPE_H

#include "btcb.h"

struct BTCB_StringImpl {
    int len;
    char str[];
};

struct BTCB_FontDescImpl {
    gint ref_count;
    PangoFontMap* font_map;
    PangoContext* context;
    PangoFontDescription* font_desc;
};

BTCB_FontDesc* btcb_create_font_desc(
    BTCB_String** families,
    double size)
{
    BTCB_FontDesc* fd = calloc(1, sizeof(BTCB_FontDesc));

    fd->ref_count = 1;
    fd->font_map = pango_ft2_font_map_new();
    fd->context = pango_font_map_create_context(fd->font_map);

    fd->font_desc = pango_font_description_new();

    if (families[0]) {
        // Build a comma-delimited family string
        int familyDelimLen = 0;
        for (int i=0; families[i]; i++) {
            familyDelimLen += families[i]->len + 1;
        }
        char familyDelim[familyDelimLen];
        char* familyDelimPtr = familyDelim;
        for (int i=0; families[i]; i++) {
            int len = families[i]->len;
            memcpy(familyDelimPtr, families[i]->str, len);
            familyDelimPtr += len;
            *(familyDelimPtr++) = families[i+1] ? ',' : '\0';    
        }
        assert (familyDelimPtr - familyDelim == familyDelimLen);

        pango_font_description_set_family(fd->font_desc, familyDelim);
    }

    // Other properties
    pango_font_description_set_absolute_size(fd->font_desc, PANGO_SCALE * size);

    return fd;
}

void btcb_free_font_desc(BTCB_FontDesc* fd)
{
    if (g_atomic_int_dec_and_test(&fd->ref_count)) {
        pango_font_description_free(fd->font_desc);
        g_object_unref(fd->context);
        g_object_unref(fd->font_map);
        free(fd);
    }
}

struct BTCB_GlyphFontImpl {
    BTCB_FontDesc* context;
    PangoFont* font;
};

struct BaxterGlyphRunImpl {
    struct BaxterGlyphRunImpl* next_run;
    BTCB_GlyphFont* font;
    int glyph_count;
    BTCB_Glyph glyphs[];
};
typedef struct BaxterGlyphRunImpl BaxterGlyphRun;

#define FROM_BTCB_GLYPH_RUN(x) \
    ((BaxterGlyphRun*)((char*)x - offsetof(BaxterGlyphRun, glyphs)))

typedef struct {
    PangoRenderer parent_instance;

    BTCB_FontDesc* context;
    BaxterGlyphRun** next_slot;
} BaxterPangoRenderer;

typedef struct
{
    PangoRendererClass parent_class;
} BaxterPangoRendererClass;

G_DEFINE_TYPE (BaxterPangoRenderer, baxter_pango_renderer, PANGO_TYPE_RENDERER)

static void baxter_pango_renderer_init(BaxterPangoRenderer* rend)
{
}

static void baxter_pango_draw_glyphs(
    PangoRenderer* rend_base,
    PangoFont* font,
    PangoGlyphString* glyphs,
    int base_x,
    int base_y)
{
    BaxterPangoRenderer* rend = (BaxterPangoRenderer*)rend_base;

    BaxterGlyphRun* run = calloc(1,
        offsetof(BaxterGlyphRun, glyphs) +
        sizeof(BTCB_Glyph)*glyphs->num_glyphs);
    *(rend->next_slot) = run;
    rend->next_slot = &run->next_run;

    g_atomic_int_inc(&rend->context->ref_count);
    g_object_ref(font);
    run->font = calloc(1, sizeof(BTCB_GlyphFont));
    run->font->context = rend->context;
    run->font->font = font;

    run->glyph_count = glyphs->num_glyphs;

    int xpos = 0;
    for (int i=0; i<glyphs->num_glyphs; i++) {
        PangoGlyphInfo *glyph = &glyphs->glyphs[i];
        BTCB_Glyph* glyph_out = &run->glyphs[i];
        int x = base_x + xpos + glyph->geometry.x_offset;
        int y = base_y + glyph->geometry.y_offset;
        glyph_out->glyph = glyph->glyph;
        glyph_out->x = x / (double)PANGO_SCALE;
        glyph_out->y = y / (double)PANGO_SCALE;
        xpos += glyph->geometry.width;
    }
}

static void baxter_pango_renderer_class_init(BaxterPangoRendererClass* klass)
{
    PangoRendererClass* rend_class = PANGO_RENDERER_CLASS(klass);

    rend_class->draw_glyphs = &baxter_pango_draw_glyphs;
}

BTCB_GlyphRun* btcb_layout_text(
    BTCB_String* text,
    BTCB_FontDesc* fd)
{
    PangoLayout* layout = pango_layout_new(fd->context);
    pango_layout_set_text(layout, text->str, text->len);
    pango_layout_set_font_description(layout, fd->font_desc);

    BaxterGlyphRun* first_run = NULL;
    BaxterPangoRenderer* rend = g_object_new(
        baxter_pango_renderer_get_type(), NULL);
    rend->context = fd;
    rend->next_slot = &first_run;
    pango_renderer_draw_layout((PangoRenderer*)rend, layout, 0, 0);
    g_object_unref(rend);

    g_object_unref(layout);
    return (BTCB_GlyphRun*)first_run->glyphs;
}

int btcb_get_run_length(
    BTCB_GlyphRun* run_ext)
{
    BaxterGlyphRun* run = FROM_BTCB_GLYPH_RUN(run_ext);
    return run->glyph_count;
}

BTCB_GlyphRun* btcb_get_next_run(
    BTCB_GlyphRun* run_ext)
{
    BaxterGlyphRun* run = FROM_BTCB_GLYPH_RUN(run_ext);
    return run->next_run ? (BTCB_GlyphRun*)run->next_run->glyphs : NULL;
}

BTCB_GlyphFont* btcb_get_run_font(
    BTCB_GlyphRun* run_ext)
{
    BaxterGlyphRun* run = FROM_BTCB_GLYPH_RUN(run_ext);
    return run->font;
}

void btcb_get_glyph_metrics(
    BTCB_GlyphFont* font,
    int glyph,
    BTCB_GlyphMetrics* out)
{
    FT_Face face = pango_fc_font_lock_face((PangoFcFont*)font->font);
    FT_Load_Glyph(face, glyph, FT_LOAD_NO_SCALE);
    double scale = face->size->metrics.x_ppem / (double)face->units_per_EM;
    out->width = face->glyph->metrics.width * scale;
    out->height = face->glyph->metrics.height * scale;
    pango_fc_font_unlock_face((PangoFcFont*)font->font);
}

extern void btcb_render_glyph(
    BTCB_GlyphFont* font,
    int glyph,
    unsigned char* buffer,
    int width,
    int height,
    int stride)
{
    FT_Face face = pango_fc_font_lock_face((PangoFcFont*)font->font);
    FT_Load_Glyph(face, glyph, FT_LOAD_RENDER);
    FT_Bitmap* bmap = &face->glyph->bitmap;
    int min_width = width < bmap->width ? width : bmap->width;
    int min_height = height < bmap->rows ? height : bmap->rows;
    int src_skip = bmap->pitch - min_width;
    int dst_skip = stride - min_width;
    unsigned char* src_p = bmap->buffer;
    unsigned char* dst_p = buffer;
    for (int x=0; x<min_width; x++) {
        for (int y=0; y<min_height; y++) {
            *dst_p++ = *src_p++;
        }
        src_p += src_skip;
        dst_p += dst_skip;
    }
    pango_fc_font_unlock_face((PangoFcFont*)font->font);
}

void btcb_free_glyph_font(
    BTCB_GlyphFont* font)
{
    g_object_unref(font->font);
    btcb_free_font_desc(font->context);
    free(font);
}

void btcb_free_run(
    BTCB_GlyphRun* run_ext)
{
    if (run_ext) {
        BaxterGlyphRun* run = FROM_BTCB_GLYPH_RUN(run_ext);
        free(run);
    }
}
