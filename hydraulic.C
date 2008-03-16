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
#include "block.h"
#include "plf.h"
#include "log.h"
#include "check_range.h"
#include "syntax.h"
#include "block.h"
#include "treelog.h"
#include "mathlib.h"
#include "program.h"
#include "vcheck.h"
#include "librarian.h"
#include <memory>
#include <sstream>

const char *const Hydraulic::component = "hydraulic";

symbol
Hydraulic::library_id () const
{
  static const symbol id (component);
  return id;
}

struct Hydraulic::K_at_h
{
  // Parameters.
  const double h;
  const double K;

  // Create and Destroy.
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add ("h", "cm", Check::non_positive (), Syntax::Const, 
		"Soil water pressure.");
    syntax.add ("K", "cm/h", Check::positive (), Syntax::Const, 
		"Water conductivity.");
    syntax.order ("h", "K");
  }
  K_at_h (const AttributeList& al)
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
		     << name << "\n";
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
check_Theta_res (const AttributeList& al, Treelog& err)
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
Hydraulic::load_Theta_sat (Syntax& syntax, AttributeList&)
{ syntax.add_fraction ("Theta_sat",  Syntax::State, "Saturation point."); }

void
Hydraulic::load_Theta_res (Syntax& syntax, AttributeList& alist)
{ 
  load_Theta_sat (syntax, alist);
  syntax.add_check (check_Theta_res);
  syntax.add_fraction ("Theta_res", Syntax::Const, "Soil residual water.");
  alist.add ("Theta_res", 0.0);
}

static bool
check_K_sat_optional (const AttributeList& al, Treelog& err)
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
Hydraulic::load_K_sat_optional (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_K_sat_optional);
  syntax.add ("K_sat", "cm/h", Check::positive (), Syntax::OptionalConst,
	      "Water conductivity of saturated soil.");
  syntax.add_submodule ("K_at_h", alist, Syntax::OptionalConst, "\
Water conductivity at specified pressure.", K_at_h::load_syntax);
}

static bool
check_K_sat (const AttributeList& al, Treelog& err)
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
Hydraulic::load_K_sat (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_K_sat);
  load_K_sat_optional (syntax, alist);
}

void
Hydraulic::initialize (const Texture&, 
                       double /* rho_b */, bool /* top_soil */, Treelog&)
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

Hydraulic::Hydraulic (Block& al)
  : ModelNamed (al.identifier ("type")),
    K_init (al.check ("K_at_h")
	    ? new K_at_h (al.alist ("K_at_h"))
	    : NULL),
    Theta_sat (al.number ("Theta_sat", -42.42e42)),
    Theta_res (al.number ("Theta_res", 0.0)),
    K_sat (al.number ("K_sat", -42.42e42))
{ }

Hydraulic::~Hydraulic ()
{ }

struct ProgramHydraulic_table : public Program
{
  const std::auto_ptr<Hydraulic> hydraulic;
  const int intervals;

  bool run (Treelog& msg)
  {
    std::ostringstream tmp;
    tmp << "pressure\tpressure\tTheta\tK\n";
    tmp << "pF\tcm\t%\tcm/h\n";
    for (int i = 0; i <= intervals; i++)
      {
        const double pF = (5.0 * i) / (intervals + 0.0);
        const double h = pF2h (pF);
        const double Theta = hydraulic->Theta (h) * 100;
        const double K = hydraulic->K (h);
        tmp << pF << "\t" << h << "\t" << Theta << "\t" << K << "\n";
      }
    msg.message (tmp.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { };
  bool check (Treelog& msg)
  { return hydraulic->check (msg); }
  ProgramHydraulic_table (Block& al)
    : Program (al),
      hydraulic (Librarian::build_item<Hydraulic> (al, "hydraulic")),
      intervals (al.integer ("intervals"))
  { }
  ~ProgramHydraulic_table ()
  { }
};

static struct ProgramHydraulic_tableSyntax
{
  static Model& make (Block& al)
  { return *new ProgramHydraulic_table (al); }
  ProgramHydraulic_tableSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Generate a table of the rentention curve and hydraulic conductivity.");
    syntax.add_object ("hydraulic", Hydraulic::component, 
                       Syntax::Const, Syntax::Singleton, "\
The hydraulic model to show in the table.");
    syntax.add ("intervals", Syntax::Integer, Syntax::Const, "\
Number of intervals in the table.");
    alist.add ("intervals", 50);
    syntax.order ("hydraulic");
    Librarian::add_type (Program::component, "hydraulic", alist, syntax, &make);
  }
} ProgramHydraulic_table_syntax;

static Librarian Hydraulic_init (Hydraulic::component, "\
This component is responsible for specifying the soils hydraulic\n\
properties.");
