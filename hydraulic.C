// hydraulic.C
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
#include "log.h"
#include "tmpstream.h"
#include "check_range.h"

void 
Hydraulic::set_porosity (double Theta)
{ 
  daisy_assert (Theta > Theta_res);
  Theta_sat = Theta; 
}

void 
Hydraulic::output (Log& log) const
{
  log.output ("Theta_sat", Theta_sat); 
}

void
Hydraulic::K_to_M (PLF& plf, const int intervals) const
{
  static const double h0 = -20000.0;
  const double Ksat = K (0.0);
  const double max_change = pow (Ksat / K (h0), 1.0 / intervals);
  double step = (0 - h0) / 4.0;

  double h = h0;
  double sum = 0.0;
  while (h < 0)
    {
      plf.add (h, sum);
      step *= 2;
      while (K (h + step) / K (h) > max_change)
	step /= 2;
      sum += step * (K (h) + K (h + step)) / 2;
      h += step;
    }
  plf.add (h, sum);
}

static bool
check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  const double Theta_res = al.number ("Theta_res");
  const double Theta_sat = al.number ("Theta_sat");

  if (Theta_res >= Theta_sat)
    {
      err.entry ("Theta_sat should be above Theta_res");
      ok = false;
    }
  return ok;
}  


void
Hydraulic::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  static RangeEI K_sat_range (0.0, 0.9);
  syntax.add ("Theta_sat", "cm^3 H2O/cm^3", K_sat_range, Syntax::State,
	      "Saturation point.");
  syntax.add ("Theta_res", "cm^3 H2O/cm^3", Check::fraction (), Syntax::Const,
	      "Soil residual water.");
  alist.add ("Theta_res", 0.0);
}

void
Hydraulic::initialize (double /* clay */, double /* silt */, double /* sand */)
{ }

Hydraulic::Hydraulic (const AttributeList& al)
  : name (al.name ("type")),
    Theta_sat (al.number ("Theta_sat")),
    Theta_res (al.number ("Theta_res"))
{ }

Hydraulic::~Hydraulic ()
{ }

EMPTY_TEMPLATE
Librarian<Hydraulic>::Content* Librarian<Hydraulic>::content = NULL;

const char *const Hydraulic::description = "\
This component is responsible for specifying the soils hydraulic\n\
properties.";
