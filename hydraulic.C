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

#define BUILD_DLL

#include "hydraulic.h"
#include "library.h"
#include "plf.h"
#include "log.h"
#include "check_range.h"
#include "block_model.h"
#include "block_submodel.h"
#include "submodeler.h"
#include "treelog.h"
#include "mathlib.h"
#include "program.h"
#include "vcheck.h"
#include "librarian.h"
#include "frame.h"
#include <memory>
#include <sstream>

// The 'hydraulic' component.

const char *const Hydraulic::component = "hydraulic";

symbol
Hydraulic::library_id () const
{
  static const symbol id (component);
  return id;
}

double
Hydraulic::r2h (const double r)
{ return -1500.0 /* [um cm] */ / r; }

struct Hydraulic::K_at_h
{
  // Parameters.
  const double h;
  const double K;

  // Create and Destroy.
  static void load_syntax (Frame& frame)
  {
    frame.declare ("h", "cm", Check::non_positive (), Attribute::Const, 
		"Soil water pressure.");
    frame.declare ("K", "cm/h", Check::positive (), Attribute::Const, 
		"Water conductivity.");
    frame.order ("h", "K");
  }
  K_at_h (const BlockSubmodel& al)
    : h (al.number ("h")),
      K (al.number ("K"))
  { }
};

void 
Hydraulic::set_porosity (double Theta)
{ 
  daisy_assert (Theta > Theta_res);
  Theta_sat = Theta; 
}

void
Hydraulic::tillage (double)
{ }

void 
Hydraulic::tick (const double, const double)
{ }

void 
Hydraulic::output (Log& log) const
{
  output_variable (Theta_sat, log); 
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
	{
	  if (step < 1e-15)
	    {
	      std::ostringstream tmp;
	      tmp << "Hydraulic conductivity changes too fast in " 
		     << objid << "\n";
	      tmp << "h = " << h << ", step = " << step 
		     << " and h + step = " << (h + step) << "\n";
	      tmp << "K (h) = " << K (h) << ", K (h + step) = "
		     << K (h + step) << " and K (0) = " << Ksat << "\n";
	      tmp << "Change = " << K (h + step) / K (h) 
		     << " > Max = " << max_change;
	      Assertion::debug (tmp.str ());
	      break;
	    }
	  step /= 2;
	}
      sum += step * (K (h) + K (h + step)) / 2;
      h += step;
    }
  plf.add (h, sum);
}

static bool
check_Theta_res (const Metalib&, const Frame& al, Treelog& err)
{
  bool ok = true;

  daisy_assert (al.check ("Theta_res") && al.check ("Theta_sat"));
  const double Theta_res = al.number ("Theta_res");
  const double Theta_sat = al.number ("Theta_sat");

  if (Theta_res >= Theta_sat)
    {
      err.error ("Theta_sat should be above Theta_res");
      ok = false;
    }
  return ok;
}  

void
Hydraulic::load_Theta_sat (Frame& frame)
{ frame.declare_fraction ("Theta_sat",  Attribute::State, "Saturation point."); }

void
Hydraulic::load_Theta_res (Frame& frame)
{ 
  load_Theta_sat (frame);
  frame.add_check (check_Theta_res);
  frame.declare_fraction ("Theta_res", Attribute::Const, "Soil residual water.");
  frame.set ("Theta_res", 0.0);
}

static bool
check_K_sat_optional (const Metalib&, const Frame& al, Treelog& err)
{
  bool ok = true;

  if (al.check ("K_sat") && al.check ("K_at_h"))
    {
      err.error ("You cannot specify both 'K_sat' and 'K_at_h'");
      ok = false;
    }
  return ok;
}  
void
Hydraulic::load_K_sat_optional (Frame& frame)
{
  frame.add_check (check_K_sat_optional);
  frame.declare ("K_sat", "cm/h", Check::positive (), Attribute::OptionalConst,
	      "Water conductivity of saturated soil.");
  frame.declare_submodule ("K_at_h", Attribute::OptionalConst, "\
Water conductivity at specified pressure.", K_at_h::load_syntax);
}

static bool
check_K_sat (const Metalib&, const Frame& al, Treelog& err)
{
  bool ok = true;

  if (!al.check ("K_sat") && !al.check ("K_at_h"))
    {
      err.error ("You must specify either 'K_sat' or 'K_at_h'");
      ok = false;
    }
  return ok;
}

void
Hydraulic::load_K_sat (Frame& frame)
{
  frame.add_check (check_K_sat);
  load_K_sat_optional (frame);
}

void
Hydraulic::initialize (const Texture&, 
                       double /* rho_b */, bool /* top_soil */, 
                       double /* CEC */, double /* center_z */, Treelog&)
{
  if (K_init)
    {
      daisy_assert (K_sat < 0.0);
      K_sat = 1.0;
      const double K_one = K (K_init->h);
      daisy_assert (K_one > 0.0);
      K_sat = K_init->K / K_one;
      daisy_assert (approximate (K (K_init->h), K_init->K));
    }
}

bool 
Hydraulic::check (Treelog& msg) const
{ 
  bool ok = true;
  if (K_sat < 0.0)
    {
      msg.error ("Not initialized");
      ok = false;
    }
  return ok;
}

Hydraulic::Hydraulic (const BlockModel& al)
  : ModelDerived (al.type_name ()),
    K_init (al.check ("K_at_h")
	    ? submodel<K_at_h> (al, "K_at_h")
	    : NULL),
    Theta_sat (al.number ("Theta_sat", -42.42e42)),
    Theta_res (al.number ("Theta_res", 0.0)),
    K_sat (al.number ("K_sat", -42.42e42))
{ }

Hydraulic::~Hydraulic ()
{ }

static struct HydraulicInit : public DeclareComponent 
{
  HydraulicInit ()
    : DeclareComponent (Hydraulic::component, "\
This component is responsible for specifying the soils hydraulic\n\
properties.")
  { }
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame);
  }
} Hydraulic_init;


// The 'hydraulic' program model.

#include "horizon.h"

struct ProgramHydraulic_table : public Program
{
  const std::auto_ptr<Horizon> horizon;
  const int intervals;
  const bool top_soil;

  bool run (Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << "pressure\tpressure\tTheta\tK";
    tmp << "\n";
    tmp << "pF\tcm\t%\tcm/h";
    tmp << "\n";
    for (int i = 0; i <= intervals; i++)
      {
        const double pF = (5.0 * i) / (intervals + 0.0);
        const double h = pF2h (pF);
        const double Theta = horizon->hydraulic->Theta (h);
        const double K = horizon->hydraulic->K (h);
        tmp << pF << "\t" << h << "\t" << Theta * 100 << "\t" << K;
        tmp << "\n";
      }
    msg.message (tmp.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Block& al)
  { 
    Treelog& msg = al.msg ();
    const double center_z = top_soil ? -10.0 : -50.0;
    horizon->initialize (top_soil, 2, center_z, msg);
  };
  bool check (Treelog&)
  { return true; }
  ProgramHydraulic_table (const BlockModel& al)
    : Program (al),
      horizon (Librarian::build_item<Horizon> (al, "horizon")),
      intervals (al.integer ("intervals")),
      top_soil (al.flag ("top_soil"))
  { }
  ~ProgramHydraulic_table ()
  { }
};

static struct ProgramHydraulic_tableSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramHydraulic_table (al); }
  ProgramHydraulic_tableSyntax ()
    : DeclareModel (Program::component, "hydraulic", "\
Generate a table of the rentention curve and hydraulic conductivity.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("horizon", Horizon::component, 
                          Attribute::Const, Attribute::Singleton, "\
The hydraulic model to show in the table.");
    frame.declare_integer ("intervals", Attribute::Const, "\
Number of intervals in the table.");
    frame.set ("intervals", 50);
    frame.declare_boolean ("top_soil", Attribute::Const, "\
Set to true for the plowing layer.");
    frame.order ("horizon");
  }
} ProgramHydraulic_table_syntax;

// hydraulic.C ends here.
