// hydraulic_mod_C.C
//
// Modified Campbell retention curve model with Burdine theory.

#include "hydraulic.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

class Hydraulic_mod_C : public Hydraulic
{
  // Content.
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
  friend class Hydraulic_mod_CSyntax;
  static Hydraulic& make (AttributeList& al);
  Hydraulic_mod_C (const AttributeList&);
public:
  ~Hydraulic_mod_C ();
};

double 
Hydraulic_mod_C::Theta (const double h) const
{
  return Sr (h) * Theta_sat;
}

double 
Hydraulic_mod_C::K (const double h) const
{
  return K_sat * pow (Sr (h), (2 + 3/b) * b);
}

double 
Hydraulic_mod_C::Cw2 (const double h) const
{
  if (h < 0)
    return - (Theta_sat
	      * (b * (pow (1.0 / (1 + pow ((1.0/h_b) * h, 5)), b - 1)
		      * (5 * (pow ((1.0/h_b) * h, 5 - 1) * (1.0/h_b)))))
	      / pow (1 + pow((1.0/h_b) * h, 5), 2));
  else
    return 0.0;
}

double 
Hydraulic_mod_C::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b * pow (pow (Theta / Theta_sat, -5.0 / b) - 1.0, 1.0/5.0);
  else
    return 0.0;
}

double 
Hydraulic_mod_C::M (double h) const
{
  if (h < h_b)
    return K_sat * (-h_b / (1 + 3/b)) * pow (h_b / h, 1 + 3/b);
  else
    return K_sat * h;
}

double 
Hydraulic_mod_C::Sr (double h) const
{
  if (h < h_b)
    return pow (1.0 / (1.0 + pow (h / h_b, 5.0)), 6.0/5.0);
  else
    return 1;
}

Hydraulic_mod_C::Hydraulic_mod_C (const AttributeList& al)
  : Hydraulic (al),
    h_b (al.number ("h_b")),
    b (al.number ("b")),
    K_sat (al.number ("K_sat"))
{ }

Hydraulic_mod_C::~Hydraulic_mod_C ()
{ }

// Add the Hydraulic_mod_C syntax to the syntax table.

Hydraulic&
Hydraulic_mod_C::make (AttributeList& al)
{
  return *new Hydraulic_mod_C (al);
}

static struct Hydraulic_mod_CSyntax
{
  Hydraulic_mod_CSyntax ();
} hydraulic_mod_C_syntax;

Hydraulic_mod_CSyntax::Hydraulic_mod_CSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  Hydraulic::load_syntax (syntax, alist);
  syntax.add ("h_b", Syntax::Number, Syntax::Const);
  syntax.add ("b", Syntax::Number, Syntax::Const);
  syntax.add ("K_sat", Syntax::Number, Syntax::Const);

  Hydraulic::add_type ("mod_C", alist, syntax, &Hydraulic_mod_C::make);
}
