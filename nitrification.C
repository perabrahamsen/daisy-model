// nitrification.C
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


#include "nitrification.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "soil_NH4.h"
#include "soil_NO3.h"
#include "log.h"
#include "mathlib.h"

EMPTY_TEMPLATE
Librarian<Nitrification>::Content* Librarian<Nitrification>::content = NULL;

const char *const Nitrification::description = "\
The nitrification process, transforming ammonium into nitrate and\n\
nitrous oxide.";

double 
Nitrification::f_h (double h)
{
  if (h >= 0.0)
    return 0.6;

  const double pF = h2pF (h);

  if (pF <= 0.0)
    return 0.0;
  if (pF <= 1.5)
    return pF / 1.5;
  if (pF <= 2.5)
    return 1.0;
  if (pF <= 5.0)
    return 1.0 - (pF - 2.5) / (5.0 - 2.5);
  
  return 0.0;
}
      
double 
Nitrification::f_T (double T)
{
#if 1
  if (T < 2.0)
    return 0.0;
  if (T < 6.0)
    return 0.15 * (T - 2.0);
  if (T < 20.0)
    return 0.10 * T;
#else
  if (T < 1.0)
    return 0.0;
  if (T < 5.0)
    return 0.125 * (T - 1.0);
  if (T < 20.0)
    return 0.10 * T;
#endif
  if (T < 37.0)
    return exp (0.47 - 0.027 * T + 0.00193 * T * T);
  if (T < 60.0)
    {
      // J.A. van Veen and M.J.Frissel.
      const double max_val = exp (0.47 - 0.027 * T + 0.00193 * T * T);
      return max_val * (1.0 - (T - 37.0) / (60.0 - 37.0));
    }
  return 0.0;
}

void
Nitrification::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_fraction ("N2O_fraction", Syntax::Const, 
                       "Fraction of ammonium lost as N2O.");
  alist.add ("N2O_fraction", 0.02);
}

Nitrification::Nitrification (const AttributeList& al)
  : name (al.identifier ("type")),
    N2O_fraction (al.number ("N2O_fraction"))
{ }

Nitrification::~Nitrification ()
{ }

