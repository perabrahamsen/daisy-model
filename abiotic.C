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
#include "assertion.h"
#include "block_model.h"
#include "plf.h"
#include "check.h"

#include <sstream>

double
Abiotic::f_h (double h)
{
  // Used by mineralization and chemical decomposition.
  if (h >= 0.0)
    return 0.6;

  const double pF = h2pF (h);

  if (pF <= 0.0)
    return 0.6;
  if (pF <= 1.5)
    return 0.6 + (1.0 - 0.6) * pF / 1.5;
  if (pF <= 2.5)
    return 1.0;
  if (pF <= 6.5)
    return 1.0 - (pF - 2.5) / (6.5 - 2.5);

  return 0;
}

double
Abiotic::f_T0 (const double T)
{
  // Used by mineralization and chemical decomposition.
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
  // Used by nitrification and denitrification.
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

double
Abiotic::find_T_scale (const BlockModel& al)
{
  const double T_ref = al.number ("T_ref");
  const PLF& decompose_heat_factor = al.plf ("decompose_heat_factor");
  const double ref_value = (decompose_heat_factor.size () < 1)
    ? Abiotic::f_T0 (T_ref)
    : decompose_heat_factor (T_ref);

  daisy_assert (ref_value > 0.0);
  return 1.0 / ref_value;
}
    
double
Abiotic::find_SMB_scale (const BlockModel& al)
{
  const double SMB_ref = al.number ("SMB_ref", -1.0);
  if (SMB_ref < 0.0)
    // No scaling.
    return 1.0;

  const double decompose_SMB_KM = al.number ("decompose_SMB_KM");
  const double ref_value = (decompose_SMB_KM > 0.0)
    ? SMB_ref / (decompose_SMB_KM + SMB_ref)
    : 1.0;

  daisy_assert (ref_value > 0.0);
  return 1.0 / ref_value;
}

static bool
check_alist (const Metalib& metalib, const Frame& al, Treelog& msg)
{
  bool ok = true;

  // T_ref
  const double T_ref = al.number ("T_ref");
  const PLF& decompose_heat_factor = al.plf ("decompose_heat_factor");
  const double ref_value = (decompose_heat_factor.size () < 1)
    ? Abiotic::f_T0 (T_ref)
    : decompose_heat_factor (T_ref);

  if (!(ref_value > 0.0))
    {
      std::ostringstream tmp;
      tmp << "heat_factor at " << T_ref << " dg C (T_ref) is " << ref_value
	  << ", should be > 0";
      msg.error (tmp.str ());
      ok = false;
    }

  // SMB_ref
  if (al.check ("SMB_ref"))
    {
      const double SMB_ref = al.number ("SMB_ref");
      const double decompose_SMB_KM = al.number ("decompose_SMB_KM");
      const double ref_value = (decompose_SMB_KM > 0.0)
	? SMB_ref / (decompose_SMB_KM + SMB_ref)
	: 1.0;

      if (!(ref_value > 0.0))
	{
	  std::ostringstream tmp;
	  tmp << "SMB_factor at " << SMB_ref << " g C/cm^3 is " << ref_value
	      << ", should be > 0";
	  msg.error (tmp.str ());
	  ok = false;
	}
    }
  return ok;
}
    
void
Abiotic::load_frame (Frame& frame) 
{ 
  frame.add_check (check_alist);
  frame.declare ("decompose_heat_factor", "dg C", Attribute::None (),
		 Attribute::Const, "Heat factor on decomposition.");
  frame.set ("decompose_heat_factor", PLF::empty ());
  frame.declare ("T_ref", "dg C", Attribute::Const, "\
Reference temperature for decomposition.\n\
The heat factor on decomposition will be scaled so it is 1 at\n\
this temperature..");
  frame.set ("T_ref", 10.0);
  frame.declare ("decompose_water_factor", "cm", Attribute::None (),
		 Attribute::Const,
		 "Water potential factor on decomposition.");
  frame.set ("decompose_water_factor", PLF::empty ());
  frame.declare_integer ("decompose_SMB_pool", Attribute::Const, "\
SMB pool for Michaelis-Menten kinetics.\n\
Use 0 for SMB1, 1 for SMB2, or -1 for all SMB pools.");
  frame.set ("decompose_SMB_pool", 1);
  frame.declare ("decompose_SMB_KM", "g C/cm^3", Check::non_negative (),
		 Attribute::Const, "\
Michaelis-Menten kinetics parameter.\n\
Decompose rate is modified by C / (KM + C), where C is the carbon content\n\
in the pool specified by 'decompose_SMB_pool'.");
  frame.set ("decompose_SMB_KM", 0.0);
  frame.declare ("SMB_ref", "g C/cm^3", Attribute::OptionalConst, "\
Reference SMB carbon for docomposition of mulch.\n\
The SMB factor for decomposition will be scaled so it is 1 at\n\
this amount of SMB carbon. By default, it will not be scaled.");
}

// abiotic.C ends here.
