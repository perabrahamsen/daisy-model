// horizon_M_C.C
//
// Campbell retention curve model with Mualem theory.

#include "horizon.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class HorizonM_C : public Horizon
{
  // Content.
  const double Theta_sat;
  const double h_b;
  const double b;
  const double K_sat;

  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;
private:
  double Sr (double h) const;
  
  // Create and Destroy.
private:
  friend class HorizonM_CSyntax;
  static Horizon& make (AttributeList& al);
  HorizonM_C (const AttributeList&);
public:
  ~HorizonM_C ();
};

double 
HorizonM_C::Theta (const double h) const
{
  return Sr (h) * Theta_sat;
}

double 
HorizonM_C::K (const double h) const
{
  return K_sat * pow (Sr (h), (2 + 2.5/b) * b);
}

double 
HorizonM_C::Cw2 (const double h) const
{
  if (h < h_b)
    return -(Theta_sat*(pow(h_b / h, 1 / b - 1)*h_b) / (pow(h, 2)*b));
  else
    return 0.0;
}

double 
HorizonM_C::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow(Theta / Theta_sat, b);
  else
    return h_b;
}

double 
HorizonM_C::M (double h) const
{
  if (h < h_b)
    return K_sat * (-h_b / (1 + 2.5/b)) * pow (h_b / h, 1 + 2.5/b);
  else
    return K_sat * h;
}

double 
HorizonM_C::Sr (double h) const
{
  if (h < h_b)
    return pow (h_b / h, 1 / b);
  else
    return 1;
}

HorizonM_C::HorizonM_C (const AttributeList& al)
  : Horizon (al),
    Theta_sat (al.number ("Theta_sat")),
    h_b (al.number ("h_b")),
    b (al.number ("b")),
    K_sat (al.number ("K_sat"))
{ }

HorizonM_C::~HorizonM_C ()
{ }

// Add the HorizonM_C syntax to the syntax table.

Horizon&
HorizonM_C::make (AttributeList& al)
{
  return *new HorizonM_C (al);
}

static struct HorizonM_CSyntax
{
  HorizonM_CSyntax ();
} horizonM_C_syntax;

HorizonM_CSyntax::HorizonM_CSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Horizon::load_syntax (syntax, alist);
  syntax.add ("Theta_sat", Syntax::Number, Syntax::Const);
  syntax.add ("h_b", Syntax::Number, Syntax::Const);
  syntax.add ("b", Syntax::Number, Syntax::Const);
  syntax.add ("K_sat", Syntax::Number, Syntax::Const);

  Horizon::add_type ("M_C", alist, syntax, &HorizonM_C::make);
}
