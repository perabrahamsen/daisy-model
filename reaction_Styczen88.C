// reaction_Styczen88.C -- Colloids generation based in rainfall momentum.
// 
// Copyright 2009 Per Abrahamsen and KU
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "reaction.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block.h"
#include "librarian.h"
#include "chemistry.h"
#include "chemical.h"
#include "log.h"
#include "geometry.h"
#include "soil.h"
#include "treelog.h"
#include "frame.h"
#include "plf.h"
#include "ponddamp.h"
#include <memory>

struct ReactionStyczen88 : public Reaction
{
  // Parameters.
  const symbol colloid_name;
  const double Ae;              // Soil resitance factor [h^2/g C/cm^2 S]
  const double MA;              // Mulch factor (protective coverage) []
  const double droplet_diameter; // Size of vegetation droplets. [mm]
  const std::auto_ptr<Ponddamp> ponddamp;

  // Log variable.
  double KH;                    // Ponding factor []
  double DH;                    // Squaret droplet momentum [g^2 C/cm/h]
  double CM;                    // Vegetation factor []
  double MR;                    // Squared direct rainfall momentum [g^2 C/h^3]
  double D;                     // Depletion [g C/cm^2 S/h]

  // Simulation.
  static double find_DH_2_below (const double h_veg, 
                                 const double droplet_diameter);
  static double find_DH_2_13 (const double h_veg, 
                              const double droplet_diameter);
  static double find_DH_13_above (const double /* h_veg */, 
                                  const double droplet_diameter);
  static double find_DH (const double h_veg, const double droplet_diameter);
  void colloid_generation (const double P /* [mm/h] */,
                           const double LD /* [mm/h] */,
                           const double f_cov /* [] */,
                           const double h_veg /* [m] */,
                           const double h_pond /* [mm] */);
  void  tick_top (const double total_rain, const double direct_rain,
                  const double canopy_drip /* [mm/h] */, 
                  const double cover, const double h_veg, 
                  const double h_pond,
                  Chemistry& chemistry, const double dt, Treelog&);
                           
  void output (Log& log) const;

  // Create and Destroy.
  void initialize (const Units&, const Geometry& geo,
                   const Soil& soil, const SoilWater&, const SoilHeat&, 
                   Treelog&);
  bool check (const Units&, const Geometry& geo,
              const Soil&, const SoilWater&, const SoilHeat&,
	      const Chemistry& chemistry, Treelog& msg) const;
  ReactionStyczen88 (Block& al);
};

double
ReactionStyczen88::find_DH_2_below (const double h_veg, 
                                    const double droplet_diameter)
{
  static const struct B : public PLF
  { 
    B ()
    {
      add (4.5, 0.7954);
      add (5.0, 1.1058);
      add (5.5, 1.4916);
      add (6.0, 1.9601);
    };
  } b;

  return b (droplet_diameter) * h_veg;
}

double
ReactionStyczen88::find_DH_2_13 (const double h_veg, 
                                 const double droplet_diameter)
{
  static const struct A : public PLF
  { 
    A ()
    {
      add (4.5, -0.5);
      add (5.0, -0.5);
      add (5.5, -0.5);
      add (6.0, -0.5);
    };
  } a_;

  static const struct B : public PLF
  { 
    B ()
    {
      add (4.5, 1.2031);
      add (5.0, 1.5930);
      add (5.5, 2.0692);
      add (6.0, 2.5496);
    };
  } b_;

  static const struct C : public PLF
  { 
    C ()
    {
      add (4.5, -0.12416);
      add (5.0, -0.15954);
      add (5.5, -0.20184);
      add (6.0, -0.23976);
    };
  } c_;

  static const struct D : public PLF
  { 
    D ()
    {
      add (4.5, 4.33e-3);
      add (5.0, 5.44e-3);
      add (5.5, 6.70e-3);
      add (6.0, 7.68e-3);
    };
  } d_;

  const double a = a_ (droplet_diameter);
  const double b = b_ (droplet_diameter);
  const double c = c_ (droplet_diameter);
  const double d = d_ (droplet_diameter);
  const double h = h_veg;
    
  return a + b * h + c * h * h + d * h * h * h;
}

double
ReactionStyczen88::find_DH_13_above (const double /* h_veg */, 
                                     const double droplet_diameter)
{
  static const struct A : public PLF
  { 
    A ()
    {
      add (4.5, 3.8647);
      add (5.0, 5.4080);
      add (5.5, 7.2934);
      add (6.0, 9.5310);
    }
  } a;

  return a (droplet_diameter);
}

double
ReactionStyczen88::find_DH (const double h_veg, const double droplet_diameter)
{
  if (h_veg < 2.0)
    return find_DH_2_below (h_veg, droplet_diameter);
  if (h_veg < 13.0)
    return find_DH_2_13 (h_veg, droplet_diameter);
  
  return find_DH_13_above (h_veg, droplet_diameter);
}

void
ReactionStyczen88::colloid_generation (const double P /* [mm/h] */,
                                       const double LD /* [mm/h] */,
                                       const double f_cov /* [] */,
                                       const double h_veg /* [m] */,
                                       const double h_pond /* [mm] */)
{
  // Direct rainfall momentum
  if (P <= 75)
    MR = 2.04e-8 * std::pow (P, 1.63);
  else
    MR = 4.83e-8 * std::pow (P, 1.43);

  // Droplet momentum.
  DH = find_DH (h_veg, droplet_diameter);

  // Vegetation modifier.
  CM = ((1.0 - f_cov) * MR + LD * DH) / MR;

  // Ponding factor.
  KH = ponddamp->value (h_pond, P);

  // Detachment of colloids at the surface. [g cm^-2 h^-1]
  D = Ae * (1.0 - MA) * KH * CM * MR; 
}

void 
ReactionStyczen88::tick_top (const double total_rain, const double direct_rain,
                             double canopy_drip /* [mm/h] */, 
                             const double cover, const double h_veg, 
                             const double h_pond,
                             Chemistry& chemistry, const double dt, Treelog&)
{
  Chemical& colloid = chemistry.find (colloid_name);
  
  const double P = total_rain; // [mm/h]
  const double LD = canopy_drip; // [mm/h]
  const double f_cov = cover;                 // []

  colloid_generation (P, LD, f_cov, h_veg, h_pond);

  colloid.add_to_surface_transform_source (D);
}

void 
ReactionStyczen88::output (Log& log) const 
{
  output_variable (KH, log); 
  output_variable (DH, log); 
  output_variable (CM, log); 
  output_variable (MR, log); 
  output_variable (D, log); 
}


void 
ReactionStyczen88::initialize (const Units&, const Geometry&, 
                               const Soil&, const SoilWater&, const SoilHeat&, 
                               Treelog&)
{  }

bool 
ReactionStyczen88::check (const Units&, const Geometry& geo,
                          const Soil&, const SoilWater&, const SoilHeat&,
                          const Chemistry& chemistry, Treelog& msg) const
{ 
  bool ok = true;
  if (!chemistry.know (colloid_name))
    {
      msg.error ("'" + colloid_name + "' not traced");
      ok = false;
    }
  return ok;
}

ReactionStyczen88::ReactionStyczen88 (Block& al)
  : Reaction (al),
    colloid_name (al.name ("colloid")),
    Ae (al.number ("Ae")),
    MA (al.number ("MA")),
    droplet_diameter (al.number ("droplet_diameter")),
    ponddamp (Librarian::build_item<Ponddamp> (al, "ponddamp")),
    KH (-42.42e42),
    DH (-42.42e42),
    CM (-42.42e42),
    MR (-42.42e42),
    D (-42.42e42)
{ }

static struct ReactionStyczen88Syntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ReactionStyczen88 (al); }
  ReactionStyczen88Syntax ()
    : DeclareModel (Reaction::component, "colgen_Styczen88", "\
Colloid generation using rainfall momentum.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add_strings ("cite", "styczen88");
    frame.add ("colloid", Value::String, Value::Const, "Colloid to generate.");

    frame.add ("Ae", "h^2/g/cm^2", Check::positive (), Value::Const, 
               "Soil resistance factor.");
    frame.add ("MA", Value::Fraction (), Value::Const, 
               "Protective cover (mulch factor).");
    frame.add ("droplet_diameter", "mm", Check::positive (), Value::Const, 
               "Size of droplets from vegetation.");
    frame.add_object ("ponddamp", Ponddamp::component,
                      Value::Const, Value::Singleton,
                      "Model for calculating 'KH'.");
    frame.add ("KH", Value::Fraction (), Value::LogOnly, 
               "Ponding factor.");
    frame.add ("DH", "g^2/cm/h", Value::LogOnly, 
               "Squared vegetation droplet momentum.");
    frame.add ("CM", Value::Fraction (), Value::LogOnly, 
               "Vegetation factor.");
    frame.add ("MR", "g^2/h^3", Value::LogOnly, 
               "Squared direct rainfall momentum.");
    frame.add ("D", "g/cm^2/h", Value::LogOnly, 
               "Depletion of detachable particles from top soil.");
  }
} ReactionStyczen88syntax;

// reaction_Styczen88.C ends here.
