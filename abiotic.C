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
#include "function.h"
#include "units.h"
#include "librarian.h"

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

// The 'T_scale' base model.

struct FunctionTScale : public Function
{
  mutable double scale;		// []
  const double ref;		// [dg C]
  
  // Simulation.
  virtual double factor (const double T) const = 0;
  double value (const double T) const
  {
    if (scale < 0.0)
      {
	const double ref_factor = factor (ref);
	if (!std::isnormal (ref_factor))
	  throw "Bad reference temperature";
	scale = 1.0 / ref_factor;
	daisy_assert (std::isnormal (scale));
      }
      return factor (T) * scale;
  }

  // Create.
  FunctionTScale (const BlockModel& al)
    : Function (al),
      scale (-42.42e42),
      ref (al.number ("ref"))
  { }
};

static struct FunctionTScaleSyntax : public DeclareBase
{
  FunctionTScaleSyntax ()
    : DeclareBase (Function::component, "T_scale", 
		   "Scale to reference temperature.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare ("ref", "dg C", Attribute::Const, "\
Temperature at which the function is one.");
    frame.set ("domain", Units::dgC ());
    frame.set ("range", Attribute::None ());
  }
} FunctionTScale_syntax;

// The 'T_min' model.

struct FunctionTMin : public FunctionTScale
{
  // Simulation.
  double factor (const double T) const
  { return Abiotic::f_T0 (T); }

  // Create.
  FunctionTMin (const BlockModel& al)
    : FunctionTScale (al)
  { }
};

static struct FunctionTMinSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new FunctionTMin (al); }
  FunctionTMinSyntax ()
    : DeclareModel (Function::component, "T_min", "T_scale",
		    "Default temperature function used for mineralization.\n\
Equation (6-13) in A10, with a linear decrease beginning at 37 dg C, down\n\
to 0 at 60 dg C, according to J.A. van Veen and M.J.Frissel.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("ref", 10.0);
    frame.set_strings ("cite", "daisy-def");
  }
} FunctionTMin_syntax;

// The 'T_min_15' parameterization.

static struct FunctionTMin15Syntax : public DeclareParam
{
  FunctionTMin15Syntax ()
    : DeclareParam (Function::component, "T_min_15", "T_min",
		    "T_min normalized to 15 dg C.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("ref", 15.0);
  }
} FunctionTMin15_syntax;

// The 'T_nit' model.

struct FunctionTNit : public FunctionTScale
{
  // Simulation.
  double factor (const double T) const
  { return Abiotic::f_T2 (T); }

  // Create.
  FunctionTNit (const BlockModel& al)
    : FunctionTScale (al)
  { }
};

static struct FunctionTNitSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new FunctionTNit (al); }
  FunctionTNitSyntax ()
    : DeclareModel (Function::component, "T_nit", "T_scale",
		    "Default temperature function used for nitrification.\n\
Equation (7-3) in A10, with a linear decrease beginning at 37 dg C, down\n\
to 0 at 60 dg C, according to J.A. van Veen and M.J.Frissel.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("ref", 10.0);
    frame.set_strings ("cite", "daisy-def");
  }
} FunctionTNit_syntax;

// abiotic.C ends here.
