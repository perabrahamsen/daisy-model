// abiotic.C -- Standard abiotic factors.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#define BUILD_DLL

#include "abiotic.h"
#include "mathlib.h"

double
Abiotic::f_T0 (const double T)
{
  // Used by chemical decomposition.
  if (T < 0.0)
    return 0.0;
  if (T < 20.0)
    return 0.1 * T;
  if (T < 37.0)
    return exp (0.47 - 0.027 * T + 0.00193 * T *T);

  if (T < 60.0)
    {
      // J.A. van Veen and M.J.Frissel.
      const double T_max = 37.0;
      const double max_val = exp (0.47 - 0.027 * T_max + 0.00193 * sqr (T_max));
      return max_val * (1.0 - (T - 37.0) / (60.0 - 37.0));
    }
  return 0.0;
}

double 
Abiotic::f_T2 (const double T)
{
  if (T < 2.0)
    return 0.0;
  if (T < 6.0)
    return 0.15 * (T - 2.0);
  if (T < 20.0)
    return 0.10 * T;
  if (T < 37.0)
    return exp (0.47 - 0.027 * T + 0.00193 * T * T);
  if (T < 60.0)
    {
      // J.A. van Veen and M.J.Frissel.
      const double T_max = 37.0;
      const double max_val = exp (0.47 - 0.027 * T_max + 0.00193 * sqr (T_max));
      return max_val * (1.0 - (T - 37.0) / (60.0 - 37.0));
    }
  return 0.0;
}

// abiotic.C ends here.
