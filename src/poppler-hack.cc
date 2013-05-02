#include <poppler/Annot.h>
extern "C"
{
#include <glib-object.h>

  struct PopplerAnnot
  {
    GObject  parent_instance;
    Annot   *annot;
  };

  struct PopplerRectangle
  {
    double x1;
    double y1;
    double x2;
    double y2;
  };


  void poppler_annot_set_rectangle (PopplerAnnot *annot, PopplerRectangle rectangle)
  {
    //   if (annot && annot->annot)
      annot->annot->setRect (rectangle.x1, rectangle.y1,
                             rectangle.x2, rectangle.y2);
  }
}
