// hydraulic_M_C.C -- Campbell retention curve model with Mualem theory.
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
// Campbell retention curve model with Mualem theory.

#include "hydraulic.h"
#include "check.h"

class HydraulicM_C : public Hydraulic
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
  HydraulicM_C (const AttributeList&);
  ~HydraulicM_C ();
};

double 
HydraulicM_C::Theta (const double h) const
{
  return Sr (h) * Theta_sat;
}

double 
HydraulicM_C::K (const double h) const
{
  return K_sat * pow (Sr (h), (2 + 2.5/b) * b);
}

double 
HydraulicM_C::Cw2 (const double h) const
{
  if (h < h_b)
    return -(Theta_sat*(pow(h_b / h, 1 / b - 1)*h_b) / (pow(h, 2)*b));
  else
    return 0.0;
}

double 
HydraulicM_C::h (const double Theta) const
{
  if (Theta < Theta_sat)
    return h_b / pow(Theta / Theta_sat, b);
  else
    return h_b;
}

double 
HydraulicM_C::M (double h) const
{
  if (h <= h_b)
    return K_sat * (-h_b / (1 + 2.5/b)) * pow (h_b / h, 1 + 2.5/b);
  else
    return M (h_b) + K_sat * (h - h_b);
}

double 
HydraulicM_C::Sr (double h) const
{
  if (h < h_b)
    return pow (h_b / h, 1 / b);
  else
    return 1;
}

HydraulicM_C::HydraulicM_C (const AttributeList& al)
  : Hydraulic (al),
    h_b (al.number ("h_b")),
    b (al.number ("b")),
    K_sat (al.number ("K_sat"))
{ }

HydraulicM_C::~HydraulicM_C ()
{ }

// Add the HydraulicM_C syntax to the syntax table.

static struct HydraulicM_CSyntax
{
  static Hydraulic& make (const AttributeList& al)
  {
    return *new HydraulicM_C (al);
  }

  HydraulicM_CSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    syntax.add_check (Hydraulic::zero_Theta_res);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Campbell retention curve model with Mualem theory.\n\
Note:  Theta_res must be zero for this model.");
    Hydraulic::load_syntax (syntax, alist);
    syntax.add ("h_b", "cm", Check::negative (), Syntax::Const,
		"Bubbling pressure.");
    syntax.add ("b", Syntax::None (), Check::positive (), Syntax::Const,
		"Campbell parameter.");
    syntax.add ("K_sat", "cm/h", Check::positive (), Syntax::Const,
		"Water conductivity of saturated soil.");

    Librarian<Hydraulic>::add_type ("M_C", alist, syntax, &make);
  }
} hydraulicM_C_syntax;

