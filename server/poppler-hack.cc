// Copyright (C) 2013, 2014  Andreas Politz

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <config.h>
#include <PDFDocEncoding.h>
#include <Annot.h>
#include <glib.h>
#include <glib-object.h>

extern "C"
{
  
GType poppler_annot_get_type (void) G_GNUC_CONST;
GType poppler_annot_markup_get_type (void) G_GNUC_CONST;

#define POPPLER_TYPE_ANNOT (poppler_annot_get_type ())
#define POPPLER_ANNOT(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), POPPLER_TYPE_ANNOT, PopplerAnnot))
#define POPPLER_IS_ANNOT_MARKUP(obj) \
  (G_TYPE_CHECK_INSTANCE_TYPE ((obj), POPPLER_TYPE_ANNOT_MARKUP))
#define POPPLER_TYPE_ANNOT_MARKUP (poppler_annot_markup_get_type ())

  struct PopplerAnnot
  {
    GObject  parent_instance;
    Annot   *annot;
  };

  struct PopplerAnnotMarkup
  {
    GObject  parent_instance;
  };

  struct PopplerRectangle
  {
    double x1;
    double y1;
    double x2;
    double y2;
  };

  char *_xpoppler_goo_string_to_utf8(GooString *s)
  {
    char *result;

    if (! s)
      return NULL;
    
    if (s->hasUnicodeMarker()) {
      result = g_convert (s->getCString () + 2,
                          s->getLength () - 2,
                          "UTF-8", "UTF-16BE", NULL, NULL, NULL);
    } else {
      int len;
      gunichar *ucs4_temp;
      int i;
    
      len = s->getLength ();
      ucs4_temp = g_new (gunichar, len + 1);
      for (i = 0; i < len; ++i) {
        ucs4_temp[i] = pdfDocEncoding[(unsigned char)s->getChar(i)];
      }
      ucs4_temp[i] = 0;

      result = g_ucs4_to_utf8 (ucs4_temp, -1, NULL, NULL, NULL);

      g_free (ucs4_temp);
    }

    return result;
  }
#ifdef HAVE_POPPLER_ANNOT_WRITE
  // Set the rectangle of an annotation.  It was first added in v0.26.
  void xpoppler_annot_set_rectangle (PopplerAnnot *a, PopplerRectangle *rectangle)
  {
    GooString *state = a->annot->getAppearState ();
    char *ustate = _xpoppler_goo_string_to_utf8 (state);
    
    a->annot->setRect (rectangle->x1, rectangle->y1,
                       rectangle->x2, rectangle->y2);
    a->annot->setAppearanceState (ustate);
    g_free (ustate);
  }
#endif  
  // This function is in the library, but the enforced date parsing is
  // incomplete (at least in some versions), because it ignores the
  // timezone.
  gchar *xpoppler_annot_markup_get_created (PopplerAnnotMarkup *poppler_annot)
  {
    AnnotMarkup *annot;
    GooString *text;

    g_return_val_if_fail (POPPLER_IS_ANNOT_MARKUP (poppler_annot), NULL);

    annot = static_cast<AnnotMarkup *>(POPPLER_ANNOT (poppler_annot)->annot);
    text = annot->getDate ();

    return text ? _xpoppler_goo_string_to_utf8 (text) : NULL;
  }
}
