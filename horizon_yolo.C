// horizon_yolo.C

#include "horizon_yolo.h"
#include "syntax.h"
#include "alist.h"
#include <vector.h>

#define exception BUG_exception
#include <math.h>
#undef exception

double 
HorizonYolo::Theta (double h) const
{
  if (h < -1.0)
    return 0.124 + 274.2 / (739.0 + pow (log (-h), 4));
  else
    return 0.495;
}

double 
HorizonYolo::K (double h) const
{
  if (h < -1.0)
    return 3600 * 1.53e-3 / (124.6 + pow (-h, 1.77));
  else
    return 3600 * 1.23e-5;
}

double
HorizonYolo::Cw1 (double h) const
{
  return Theta (h) - Cw2 (h) * h;
}

double 
HorizonYolo::Cw2 (double h) const
{
  if (h < -1.0)
    return - (  (-4 * 274.2 * pow (log (-h), 3))
	      / (-h * pow (739.0 + pow (log (-h), 4), 2)));
  else
    return 0.0;
}

double 
HorizonYolo::h (double /* Theta */) const
{
  THROW (Unimplemented ("Calculate h from Theta"));
  return -1;
}

HorizonYolo::HorizonYolo (const AttributeList& al)
  : Horizon (al)
{ }

HorizonYolo::~HorizonYolo ()
{ }

// Add the HorizonYolo syntax to the syntax table.

Horizon&
HorizonYolo::make (AttributeList& al)
{
  return *new HorizonYolo (al);
}

static struct HorizonYoloSyntax
{
  HorizonYoloSyntax ();
} horizonYolo_syntax;

HorizonYoloSyntax::HorizonYoloSyntax ()
{ 
  Syntax* syntax = new Syntax ();
  AttributeList* alist = new AttributeList ();
  Horizon::add_type ("yolo", *alist, *syntax, &HorizonYolo::make);
}
