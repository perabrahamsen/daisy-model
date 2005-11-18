// hydraulic_yolo.C
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


#include "hydraulic.h"
#include "plf.h"
#include "mathlib.h"

using namespace std;

class HydraulicYolo : public Hydraulic
{
  int M_intervals;
  mutable PLF M_;

public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;

  // Create and Destroy.
private:
  friend class HydraulicYoloSyntax;
  static Hydraulic& make (Block& al);
  HydraulicYolo (Block&);
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
    return 1.0e-8;
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
  if (M_.size () == 0)
    K_to_M (M_, M_intervals);

  return M_ (h);
}

HydraulicYolo::HydraulicYolo (Block& al)
  : Hydraulic (al),
    M_intervals (al.integer ("M_intervals")),
    M_ ()
{ }

HydraulicYolo::~HydraulicYolo ()
{ }

// Add the HydraulicYolo syntax to the syntax table.

Hydraulic&
HydraulicYolo::make (Block& al)
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
  alist.add ("description", "Yolo soil.  Haverkamp et.al., 1977.");
  syntax.add ("M_intervals", Syntax::Integer, Syntax::Const,
	      "Number of intervals for numeric integration of K.");
  alist.add ("M_intervals", 500);
  Librarian<Hydraulic>::add_type ("yolo", alist, syntax, &HydraulicYolo::make);
}
