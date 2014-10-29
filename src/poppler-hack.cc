#include <poppler/Annot.h>
#include <poppler/PDFDocEncoding.h>
extern "C"
{
#include <glib-object.h>

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

  char *_poppler_goo_string_to_utf8(GooString *s)
  {
    char *result;

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
#ifndef HAVE_POPPLER_ANNOT_SET_RECT
  // Set the rectangle of an annotation.  Not available in poppler-glib .
  void poppler_annot_set_rectangle (PopplerAnnot *annot, PopplerRectangle *rectangle)
  {
    annot->annot->setRect (rectangle->x1, rectangle->y1,
                           rectangle->x2, rectangle->y2);
  }
#endif  // HAVE_POPPLER_ANNOT_SET_RECT
  // This function is in the library, but the enforced date parsing is
  // incomplete, because it ignores the timezone.
  gchar *poppler_annot_markup_get_created (PopplerAnnotMarkup *poppler_annot)
  {
    AnnotMarkup *annot;
    GooString *text;
    time_t timet;

    g_return_val_if_fail (POPPLER_IS_ANNOT_MARKUP (poppler_annot), NULL);

    annot = static_cast<AnnotMarkup *>(POPPLER_ANNOT (poppler_annot)->annot);
    text = annot->getDate ();

    return text ? _poppler_goo_string_to_utf8 (text) : NULL;
  }
}
