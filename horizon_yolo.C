// horizon_yolo.C

#include "horizon.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "csmp.h"

class HorizonYolo : public Horizon
{
  int M_intervals;

public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;

  // Create and Destroy.
private:
  friend class HorizonYoloSyntax;
  static Horizon& make (AttributeList& al);
  HorizonYolo (const AttributeList&);
public:
  virtual ~HorizonYolo ();
};

double 
HorizonYolo::Theta (const double h) const
{
  if (h < -1.0)
    return min (0.495, 0.124 + 274.2 / (739.0 + pow (log (-h), 4)));
  else
    return 0.495;
}

double 
HorizonYolo::K (const double h) const
{
  if (h < -1.0)
    return 3600 * 1.53e-3 / (124.6 + pow (-h, 1.77));
  else
    return 3600 * 1.23e-5;
}

double 
HorizonYolo::Cw2 (const double h) const
{
  if (h < -1.0)
    return - (  (-4 * 274.2 * pow (log (-h), 3))
	      / (-h * pow (739.0 + pow (log (-h), 4), 2)));
  else
    return 0.0;
}

double 
HorizonYolo::h (const double Theta) const
{
  if (Theta < 0.495)
    return -exp(sqrt(sqrt(274.2 / (Theta - 0.124) - 739.)));
  else
    return -1;
}

double 
HorizonYolo::M (double h) const
{
  // Use.
  static CSMP csmp;
  static bool initialized = false;
  if (!initialized)
    {
      K_to_M (csmp, M_intervals);
      initialized = true;
    }
  return csmp (h);
}

HorizonYolo::HorizonYolo (const AttributeList& al)
  : M_intervals (al.integer ("M_intervals"))
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
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("M_intervals", Syntax::Integer, Syntax::Const);
  alist.add ("M_intervals", 500);
  Horizon::add_type ("yolo", alist, syntax, &HorizonYolo::make);
}
