// template.C
//
// The following is a workaround for a gcc bug which means that the
// template code must be visible at the point of the instantiation.

#include "ftable.h"
#include "crop_impl.h"
#include "ftable.t"

static void dummy ()
{
  dFTable<CropFun> d1;
  dummy ();
}

