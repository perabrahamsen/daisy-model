// hydraulic_yolo.C

#include "hydraulic.h"
#include "csmp.h"

class HydraulicYolo : public Hydraulic
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
  friend class HydraulicYoloSyntax;
  static Hydraulic& make (const AttributeList& al);
  HydraulicYolo (const AttributeList&);
public:
  virtual ~HydraulicYolo ();
};

double 
HydraulicYolo::Theta (const double h) const
{
  if (h < -1.0)
    return min (0.495, 0.124 + 274.2 / (739.0 + pow (log (-h), 4)));
  else
    return 0.495;
}

double 
HydraulicYolo::K (const double h) const
{
  if (h < -1.0)
    return 3600 * 1.53e-3 / (124.6 + pow (-h, 1.77));
  else
    return 3600 * 1.23e-5;
}

double 
HydraulicYolo::Cw2 (const double h) const
{
  if (h < -1.0)
    return - (  (-4 * 274.2 * pow (log (-h), 3))
	      / (-h * pow (739.0 + pow (log (-h), 4), 2)));
  else
    return 0.0;
}

double 
HydraulicYolo::h (const double Theta) const
{
  if (Theta < 0.495)
    return -exp(sqrt(sqrt(274.2 / (Theta - 0.124) - 739.)));
  else
    return -1;
}

double 
HydraulicYolo::M (double h) const
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

HydraulicYolo::HydraulicYolo (const AttributeList& al)
  : Hydraulic (al),
    M_intervals (al.integer ("M_intervals"))
{ 
  // This is not to be changed.
  assert (Theta_sat > 0.4949 && Theta_sat < 0.4951);
}

HydraulicYolo::~HydraulicYolo ()
{ }

// Add the HydraulicYolo syntax to the syntax table.

Hydraulic&
HydraulicYolo::make (const AttributeList& al)
{
  return *new HydraulicYolo (al);
}

static struct HydraulicYoloSyntax
{
  HydraulicYoloSyntax ();
} hydraulicYolo_syntax;

HydraulicYoloSyntax::HydraulicYoloSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Hydraulic::load_syntax (syntax, alist);
  alist.add ("Theta_sat", 0.495);
  syntax.add ("M_intervals", Syntax::Integer, Syntax::Const);
  alist.add ("M_intervals", 500);
  Librarian<Hydraulic>::add_type ("yolo", alist, syntax, &HydraulicYolo::make);
}
