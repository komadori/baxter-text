#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <glib-object.h>
#include <pango/pango.h>
#include <pango/pangoft2.h>

#include "btcb.h"

struct BTCB_StringImpl {
    int len;
    char str[];
};

struct BTCB_FontDescImpl {
    PangoFontMap* font_map;
    PangoContext* context;
    PangoFontDescription* font_desc;
};

BTCB_FontDesc* btcb_create_font_desc(
    BTCB_String** families,
    double size)
{
    BTCB_FontDesc* fd = calloc(1, sizeof(BTCB_FontDesc));

    fd->font_map = pango_ft2_font_map_new();
    fd->context = pango_font_map_create_context(fd->font_map);

    fd->font_desc = pango_font_description_new();

    if (!families[0]) {
        // Build a comma-delimited family string
        int familyDelimLen = 0;
        for (int i=0; families[i]; i++) {
            familyDelimLen += families[i]->len;
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
    pango_font_description_free(fd->font_desc);
    g_object_unref(fd->context);
    g_object_unref(fd->font_map);
    free(fd);
}

typedef struct {
    PangoRenderer parent_instance;
} BaxterPangoRenderer;

typedef struct
{
    PangoRendererClass parent_class;
} BaxterPangoRendererClass;

G_DEFINE_TYPE (BaxterPangoRenderer, baxter_pango_renderer, PANGO_TYPE_RENDERER)

static void baxter_pango_renderer_init(BaxterPangoRenderer* rend)
{
}

static void baxter_pango_draw_glyph(
    PangoRenderer* rend,
    PangoFont* font,
    PangoGlyph glyph,
    double x,
    double y)
{
    printf("(%f, %f) = %d\n", x, y, glyph);
}

static void baxter_pango_renderer_class_init(BaxterPangoRendererClass* klass)
{
    PangoRendererClass* rend_class = PANGO_RENDERER_CLASS(klass);

    rend_class->draw_glyph = &baxter_pango_draw_glyph;
}

void btcb_layout_text(
    BTCB_String* text,
    BTCB_FontDesc* fd)
{
    PangoLayout* layout = pango_layout_new(fd->context);
    pango_layout_set_text(layout, text->str, text->len);
    pango_layout_set_font_description(layout, fd->font_desc);

    BaxterPangoRenderer* rend = g_object_new(
        baxter_pango_renderer_get_type(), NULL);
    pango_renderer_draw_layout((PangoRenderer*)rend, layout, 0, 0);
    g_object_unref(rend);

    g_object_unref(layout);
}
