#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pango/pango.h>
#include <pango/pangoft2.h>

#include "btcb.h"

struct BTCB_StringImpl {
    int len;
    char str[];
};

struct BTCB_FontDescImpl {
    PangoFontDescription* pango_fd;
};

BTCB_FontDesc* btcb_create_font_desc(
    BTCB_String** families,
    double size)
{
    BTCB_FontDesc* fd = calloc(1, sizeof(BTCB_FontDesc));
    fd->pango_fd = pango_font_description_new();

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

        pango_font_description_set_family(fd->pango_fd, familyDelim);
    }

    // Other properties
    pango_font_description_set_absolute_size(fd->pango_fd, PANGO_SCALE * size);

    return fd;
}

void btcb_free_font_desc(BTCB_FontDesc* fd)
{
    pango_font_description_free(fd->pango_fd);
    free(fd);
}
