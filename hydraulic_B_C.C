// hydraulic_B_C.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

//
// Campbell retention curve model with Burdine theory.

#include "hydraulic.h"

class HydraulicB_C : public Hydraulic
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
public:
  HydraulicB_C (const AttributeList&);
  ~HydraulicB_C ();
};

double 
HydraulicB_C::Theta (const double h) const
{
  return Sr (h) * Theta_sat;
}

double 
HydraulicB_C::K (const double h) const
{
  return K_sat * pow (Sr (h), (2 + 3/b) * b);
}

double 
HydraulicB_C::Cw2 (const double h) const
{
  if (h < h_b)
    return -(Theta_sat*(pow(h_b / h, 1 / b - 1)*h_b) / (pow(h, 2)*b));
  else
    return 0.0;
}

double 
HydraulicB_C::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow(Theta / Theta_sat, b);
  else
    return h_b;
}

double 
HydraulicB_C::M (double h) const
{
  if (h <= h_b)
    return K_sat * (-h_b / (1 + 3/b)) * pow (h_b / h, 1 + 3/b);
  else
    return M (h_b) + K_sat * (h - h_b);
}

double 
HydraulicB_C::Sr (double h) const
{
  if (h < h_b)
    return pow (h_b / h, 1 / b);
  else
    return 1;
}

HydraulicB_C::HydraulicB_C (const AttributeList& al)
  : Hydraulic (al),
    h_b (al.number ("h_b")),
    b (al.number ("b")),
    K_sat (al.number ("K_sat"))
{ }

HydraulicB_C::~HydraulicB_C ()
{ }

// Add the HydraulicB_C syntax to the syntax table.

static struct HydraulicB_CSyntax
{
  static Hydraulic& make (const AttributeList& al)
    {
      return *new HydraulicB_C (al);
    }

  static bool check_alist (const AttributeList& al, Treelog& err)
    { 
      bool ok = true;

      non_positive (al.number ("h_b"), "h_b", ok, err);
      const double b = al.number ("b");
      if (b <= 0.0 || b > 1.0)
	{
	  Treelog::Open nest (err, "b");
	  err.entry ("Value should be between 0 and 1 (but not 0)");
	  ok = false;
	}
      non_negative (al.number ("K_sat"), "K_sat", ok, err);

      return ok;
    }

  HydraulicB_CSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add_check (check_alist);
      alist.add ("description", 
		 "Campbell retention curve model with Burdine theory.");
      Hydraulic::load_syntax (syntax, alist);
      syntax.add ("h_b", "cm", Syntax::Const,
		  "Bubbling pressure.");
      syntax.add ("b", Syntax::None (), Syntax::Const,
		  "Campbell parameter.");
      syntax.add ("K_sat", "cm/h", Syntax::Const,
		  "Water conductivity of saturated soil.");

      Librarian<Hydraulic>::add_type ("B_C", alist, syntax, &make);
    }
} hydraulicB_C_syntax;

