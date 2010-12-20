// bioincorporation.C --- Biological incorporation of organic matter in soil. 
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

#include "bioincorporation.h"
#include "frame_submodel.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "am.h"
#include "librarian.h"
#include "plf.h"
#include "time.h"
#include "aom.h"
#include "check.h"
#include "vcheck.h"
#include "mathlib.h"
#include <algorithm>
#include <sstream>

static struct AOMSlowBioincorporationSyntax : public DeclareParam
{
  AOMSlowBioincorporationSyntax ()
    : DeclareParam (AOM::component, "AOM-SLOW-BIOINCORPORATION", "AOM-SLOW", "\
Lower C/N ration for bioincorporated matter.")
  { }
  void load_frame (Frame& frame) const
  {
    std::vector<double> CN;
    CN.push_back (60.0);
    frame.set ("C_per_N", CN);
  }
} AOMSlowBioincorporation_syntax;

struct Bioincorporation::Implementation
{ 
  // Parameters.
  const double R_max;
  const double k_half;
  const PLF C_per_N_factor;
  const PLF T_factor;
  const double respiration;
  const PLF distribution;
  std::vector<double> density;
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& aom_alists; // Stem AM parameters.
  
  // Content.
  AM* aom;

  // Log.
  double C_removed;
  double N_removed;
  std::vector<double> C_added;
  std::vector<double> N_added;  
  double speed;

  // Utitlites.
  static bool am_compare (const AM* a, const AM* b);

  // Simulation.
  void tick (const Geometry&, std::vector <AM*>& am, double T, double& CO2, 
             double dt);
  void output (Log&) const;

  // Utitlites.
  void add (const Geometry& geo, std::vector<double>& input, 
	    double amount) const;

  // Create and destroy.
  void initialize (const Geometry&, const Soil&);
  AM* create_am (const Metalib&, const Geometry& geo, Treelog&);
  void set_am (AM*);
  Implementation (const FrameSubmodel& al);
};

bool 
Bioincorporation::Implementation::am_compare (const AM* a, const AM* b)
{
  const double a_top_C = a->top_C ();
  daisy_assert (a_top_C >= 0);
  if (iszero (a_top_C))
    return false;

  const double b_top_C = b->top_C ();
  daisy_assert (b_top_C >= 0);
  if (iszero (b_top_C))
    return true;
      
  const double a_top_N = a->top_N ();
  daisy_assert (a_top_N > 0.0);
  const double b_top_N = b->top_N ();
  daisy_assert (b_top_N > 0.0);
  return a_top_C / a_top_N < b_top_C / b_top_N;
}

static const double DM_to_C = 0.420; // C fraction of DM.
static const double C_to_DM = 1.0 / DM_to_C;
static const double m2_per_cm2 = 0.0001;
static const double cm2_per_m2 = 1.0 / m2_per_cm2;
static const double surface_to_soil = DM_to_C * m2_per_cm2;
static const double soil_to_surface = 1.0 / surface_to_soil;

void
Bioincorporation::Implementation::tick (const Geometry& geo, 
					std::vector <AM*>& am, double T, 
					double& CO2,
                                        const double dt)
{
  // No bioincorporation.
  if (iszero (R_max))
    return;

  // Clear old log variables.
  C_removed = 0.0;
  N_removed = 0.0;
  fill (C_added.begin (), C_added.end (), 0.0);
  fill (N_added.begin (), N_added.end (), 0.0);

  // Check available bioincorporation.
  const double R_total = R_max * T_factor (T) * surface_to_soil;// [g C/cm^2/h]
  if (R_total < 1.0e-10)
    return;
  const double k_total = k_half * surface_to_soil;// [g C/cm^2]

  // Eat from each AM, lowest C/N first.
  const unsigned int am_size = am.size ();
  sort (am.begin (), am.end (), am_compare);
  double available = R_total * dt;	// [g C/cm^2]
  double last_C_per_N = 0.0;

  for (size_t i = 0; i < am_size; i++)
  {
    const double top_C = am[i]->top_C ();

    // Not a worthwhile AOM..
    if (top_C < 1e-30)
      continue;
    
    // Find how much to take from this AOM.
    const double top_N = am[i]->top_N ();
    daisy_assert (top_N > 0.0);
    const double C_per_N = top_C / top_N;
    if (C_per_N < last_C_per_N && !approximate (C_per_N, last_C_per_N))
      {
        std::ostringstream tmp;
        tmp << "C/N = " << top_C << "/" << top_N << " = " << C_per_N << "\n"
            << "last C/N = ";
        if (i > 0)
          tmp << am[i-1]->top_C () << "/" << am[i-1]->top_N () << " = ";
        tmp << last_C_per_N;
        daisy_warning (tmp.str ());
      }
    speed = R_total * C_per_N_factor (C_per_N) * top_C / (top_C + k_total);

    // Don't take more than the bioincorporation can handle.
    if (speed * dt > available)
      speed = available / dt;
    
    if (speed * dt > top_C)
      {
	// Take all.
	am[i]->multiply_top (0.0);
	daisy_assert (iszero (am[i]->top_C ()));
	daisy_assert (iszero (am[i]->top_N ()));
	C_removed += top_C;
	N_removed += top_N;
      }
    else
      {
	// Take some.
	const double fraction = speed * dt / top_C;
	C_removed += speed * dt;
	N_removed += top_N * fraction;
	am[i]->multiply_top (1.0 - fraction);
	daisy_assert (approximate (am[i]->top_C (), top_C - speed * dt));
	daisy_assert (approximate (am[i]->top_N (), top_N * (1.0 - fraction)));
      }

    // No more available bioincorporation.
    available -= speed * dt;
    if (available < 1.0e-10)
      break;

    // Next pool.
    last_C_per_N = C_per_N;
  }

  const double C_to_add = C_removed * (1.0 - respiration);
  const double N_to_add = N_removed;

  // Add bioincorporation to soil.
  daisy_assert (aom);
  aom->add_surface (geo, C_to_add, N_to_add, density);
  geo.add_surface (C_added, density, C_to_add / dt);
  geo.add_surface (N_added, density, N_to_add / dt);  

  // Update CO2.
  CO2 += (C_removed - C_to_add) / dt;

  // Update log variables.
  C_removed *= cm2_per_m2 / dt;
  N_removed *= cm2_per_m2 / dt;
}
  
void 
Bioincorporation::Implementation::output (Log& log) const
{ 
  output_value (respiration * C_removed, "CO2", log);
  output_value (C_removed * C_to_DM, "DM", log);
  output_variable (C_removed, log);
  output_variable (N_removed, log);
  output_variable (C_added, log);
  output_variable (N_added, log);
  output_variable (speed, log);
}

void 
Bioincorporation::Implementation::add (const Geometry& geo,
				       std::vector<double>& input,
				       const double amount) const
{ geo.add_surface (input, density, amount /* * (1.0 - respiration) */); }

void 
Bioincorporation::Implementation::initialize (const Geometry& geo,
                                              const Soil& soil)
{ 
  // Calculate distribution density for all cells.
  for (size_t i = 0; i < geo.cell_size (); i++)
    density.push_back (distribution (geo.cell_z (i)));

  C_added.insert (C_added.end (), soil.size (), 0.0);
  N_added.insert (N_added.end (), soil.size (), 0.0);
}

AM*
Bioincorporation::Implementation::create_am (const Metalib& metalib, 
                                             const Geometry& geo,
                                             Treelog& msg)
{ 
  static const symbol bio_symbol ("bio");
  static const symbol incorporation_symbol ("incorporation");
  aom = &AM::create (metalib, geo, Time (1, 1, 1, 1), aom_alists,
		     bio_symbol, incorporation_symbol, AM::Locked, msg); 
  return aom;
}

void 
Bioincorporation::Implementation::set_am (AM* am)
{ aom = am; }

Bioincorporation::Implementation::Implementation (const FrameSubmodel& al)
  : R_max (al.number ("R_max")),
    k_half (al.number ("k_half")),
    C_per_N_factor (al.plf ("C_per_N_factor")),
    T_factor (al.plf ("T_factor")),
    respiration (al.number ("respiration")),
    distribution (al.plf ("distribution")), 
    aom_alists (al.model_sequence ("AOM")),
    C_removed (0.0),
    N_removed (0.0),
    speed (-42.42e42)
{ }

void 
Bioincorporation::tick (const Geometry& geo, std::vector <AM*>& am, double T,
			double& CO2, const double dt)
{
  impl->tick (geo, am, T, CO2, dt);
}

void 
Bioincorporation::output (Log& log) const
{
  impl->output (log);
}

void 
Bioincorporation::add (const Geometry& geo, std::vector<double>& input,
		       const double amount) const
{ impl->add (geo, input, amount); }

void 
Bioincorporation::initialize (const Geometry& geo,
                              const Soil& soil)
{ impl->initialize (geo, soil); }

AM*
Bioincorporation::create_am (const Metalib& metalib, const Geometry& geo,
                             Treelog& msg)
{ return impl->create_am (metalib, geo, msg); }

void 
Bioincorporation::set_am (AM* am)
{ impl->set_am (am); }

void
Bioincorporation::load_syntax (Frame& frame)
{ 
  // Incorporation speed.
  frame.declare ("R_max", "g DM/m^2/h", Check::non_negative (), Attribute::Const, 
	      "Maximal speed of incorporation.");
  frame.set ("R_max", 0.5);
  frame.declare ("k_half", "g DM/m^2", Check::positive (), Attribute::Const,
	      "Halflife constant.");
  frame.set ("k_half", 1.0);
  frame.declare ("speed", "g DM/m^2/h", Attribute::LogOnly, 
	      "Fraction of litter incorporated this hour.\n\
The formula is speed = (R_max * litter) / (k_half + litter).");
  frame.declare ("C_per_N_factor", "(g C/cm^2)/(g N/cm^2)", Attribute::None (), 
	      Check::non_negative (), Attribute::Const, 
	      "Limiting factor for high C/N ratio.");
  PLF C_per_N_factor;
  C_per_N_factor.add (50.0, 1.0);
  C_per_N_factor.add (100.0, 0.1);
  C_per_N_factor.add (120.0, 0.01);
  
  frame.set ("C_per_N_factor", C_per_N_factor);
  frame.declare ("T_factor", "dg C", Attribute::None (), Check::non_negative (), 
	      Attribute::Const, "Limiting factor for low temperature.");
  PLF T_factor;
  T_factor.add (4.0, 0.0);
  T_factor.add (6.0, 1.0);
  frame.set ("T_factor", T_factor);

  // Incorporation amounts.
  frame.declare_fraction ("respiration", Attribute::Const,
		       "Fraction of C lost in respiration.");
  frame.set ("respiration", 0.5);
  frame.declare ("DM", "g DM/m^2/h", Attribute::LogOnly, 
	      "DM removed from surface.");
  frame.declare ("C_removed", "g C/m^2/h", Attribute::LogOnly,
              "C removed from surface.");
  frame.declare ("N_removed", "g N/m^2/h", Attribute::LogOnly, 
              "N removed from surface.");
  frame.declare ("CO2", "g C/m^2/h", Attribute::LogOnly, "C respirated.");
  frame.declare ("C_added", "g C/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
              "C added to soil.");
  frame.declare ("N_added", "g N/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
              "N added to soil.");

  // Incorporation location.
  frame.declare ("distribution", "cm", Attribute::None (), Check::non_negative (),
	      Attribute::Const, "\
Distribution of incorporated matter in the soil.\
\n(X, Y), where X is the depth (negative numbers), and Y is the relative\n\
weight in that depth.  To get the fraction in a specific interval [a:b], we\n\
integrate the plf over that interval, and divide by the integration over\n\
the whole profile.");
  PLF distribution;
  distribution.add (-80.0, 0.0);
  distribution.add (-18.0, 100.0);
  distribution.add (0.0, 100.0);
  frame.set ("distribution", distribution);

  frame.declare_object ("AOM", AOM::component, Attribute::Const, Attribute::Variable, "\
Incorporated AM parameters.");
  frame.set_check ("AOM", AM::check_om_pools ());
  frame.set_strings ("AOM", "AOM-SLOW-BIOINCORPORATION", "AOM-FAST");
}
  
Bioincorporation::Bioincorporation (const FrameSubmodel& al)
  : impl (new Implementation (al))
{ }

Bioincorporation::~Bioincorporation ()
{ }

static DeclareSubmodel
bioincorporation_submodel (Bioincorporation::load_syntax, "Bioincorporation", "\
Biological incorporation of organic matter in soil.");

// bioincorporation.C ends here.
