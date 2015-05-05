// litter.C -- Litter lay below vegetation.
// 
// Copyright 2010 KU
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

#include "litter.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "check.h"
#include "log.h"

// The 'litter' component.

const char *const Litter::component = "litter";

symbol 
Litter::library_id () const
{
  static const symbol id (component);
  return id;
}

void
Litter::output (Log& log) const
{
  output_value (cover (), "cover", log);
}

Litter::Litter  (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

Litter::~Litter ()
{ }

static struct LitterInit : public DeclareComponent 
{
  LitterInit ()
    : DeclareComponent (Litter::component, "\
Litter, surface residuals, or mulch below canopy.")
  { }
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame); 
    frame.declare_fraction ("cover", Attribute::LogOnly, "\
Fraction of surface area covered by litter.");
  }
} Litter_init;

// The 'none' model.

struct LitterNone : public Litter
{
  // Simulation.
  void update (const double)
  { }
  double cover () const
  { return 0.0; }
  double vapor_flux_factor () const
  { return 1.0; }
  double water_capacity () const
  { return 0.0; }
  double albedo () const
  { return -1.0; }

  // Create and Destroy.
  void initialize (const double)
  { }
  LitterNone (const BlockModel& al)
    : Litter (al)
  { }
  ~LitterNone ()
  { }
};

static struct LitterNoneSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LitterNone (al); }
  LitterNoneSyntax ()
    : DeclareModel (Litter::component, "none", "\
The effect of surface residuals is ignored by the model.")
  { }
  void load_frame (Frame&) const
  { }
} LitterNone_syntax;

// The 'permanent' model.

struct LitterPermanent : public Litter
{
  // Parameters.
  const double vapor_flux_factor_;
  const double interception_capacity;
  const double albedo_;

  // Simulation.
  void update (const double)
  { }
  double cover () const
  { return 1.0; }
  double vapor_flux_factor () const
  { return vapor_flux_factor_; }
  double water_capacity () const
  { return interception_capacity; }
  double albedo () const
  { return albedo_; }

  // Create and Destroy.
  void initialize (const double)
  { }
  LitterPermanent (const BlockModel& al)
    : Litter (al),
      vapor_flux_factor_ (al.number ("vapor_flux_factor")),
      interception_capacity (al.number ("interception_capacity")),
      albedo_ (al.number ("albedo", -1.0))
  { }
  ~LitterPermanent ()
  { }
};

static struct LitterPermanentSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LitterPermanent (al); }
  LitterPermanentSyntax ()
    : DeclareModel (Litter::component, "permanent", "\
A permanent litter layer cover the ground, as for example in a forest.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_fraction ("vapor_flux_factor", Attribute::Const, "\
Reduction factor for potential evaporation below litter.");
    frame.set ("vapor_flux_factor", 1.0);
    frame.declare ("interception_capacity", "mm", Attribute::Const,
                   "Storage capacity of litter.");
    frame.declare ("albedo", Attribute::None (), Check::positive (),
                   Attribute::OptionalConst, "Reflection factor.\n\
By default, the surface albedo will be used.");
  }
} LitterPermanent_syntax;

// The 'residue' model.

struct LitterResidue : public Litter
{
  // Parameters.
  const double water_capacity_; // Max water in litter DM [L/kg]
  const double vapor_flux_factor_;        // Ep-reduction []
  const double specific_AI;            // Spec. litter area [m^2/kg DM]
  const double extinction_coefficent;  // Beers law for cover []
  const double albedo_;
  
  // Variables.
  double DM;                    // Surface residuals [kg DM/m^2]

  // Simulation.
  void update (const double top_DM /* [kg DM/m^2] */)
  { DM = top_DM; }
  double cover () const
  { 
    const double MAI = DM * specific_AI; 
    return 1.0 - exp (- MAI * extinction_coefficent);
  }
  double vapor_flux_factor () const
  { return vapor_flux_factor_; }
  double water_capacity () const
  { return DM * water_capacity_; }
  double albedo () const
  { return albedo_; }

  // Create and Destroy.
  void initialize (const double top_DM /* [kg DM/m^2] */)
  { DM = top_DM; }
  LitterResidue (const BlockModel& al)
    : Litter (al),
      water_capacity_ (al.number ("water_capacity")),
      vapor_flux_factor_ (al.number ("vapor_flux_factor")),
      specific_AI (al.number ("specific_AI")),
      extinction_coefficent (al.number ("extinction_coefficent")),
      albedo_ (al.number ("albedo", -1.0)),
      DM (-42.42e42)
  { }
  ~LitterResidue ()
  { }
};

static struct LitterResidueSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LitterResidue (al); }
  LitterResidueSyntax ()
    : DeclareModel (Litter::component, "residue", "\
A dynamic litter layer based on applied fertilizer and crop residuals.\n\
\n\
A 'mulch area index' is calculated from the surface organic\n\
matericals, and from that a mulch cover is calculated based on Beer's law\n\
similarily to how the crop cover is calculated from the leaf area\n\
index.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "scopel2004");

    frame.declare ("water_capacity", "L/kg", Attribute::Const, "\
Water holding capacity of surface residulas.");
    frame.declare_fraction ("vapor_flux_factor", Attribute::Const, "\
Reduction factor for potential evaporation below litter.\n\
Only area covered by residue is affected.");
    frame.set ("vapor_flux_factor", 0.0);
    frame.declare ("specific_AI", "m^2/kg DM", Attribute::Const, "\
Area covered per litter mass.");
    frame.declare ("extinction_coefficent", 
                   Attribute::None (), Attribute::Const, "\
Beer's law extinction coefficient for litter.");
    frame.declare ("albedo", Attribute::None (), Check::positive (),
                   Attribute::OptionalConst, "Reflection factor.\n\
By default, the surface albedo will be used.");
  }
} LitterResidue_syntax;

// The 'Millet' parameterization.

static struct LitterMilletsyntax : public DeclareParam
{ 
  LitterMilletsyntax ()
    : DeclareParam (Litter::component, "Millet", "residue", "\
Millet crop residues in Planaltina.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "macena2003");

    frame.set ("water_capacity", 3.2);
    frame.set ("specific_AI", 39.);
    frame.set ("extinction_coefficent", 0.45);
  }
} LitterMillet_syntax;

// The 'Maize' parameterization.

static struct LitterMaizesyntax : public DeclareParam
{ 
  LitterMaizesyntax ()
    : DeclareParam (Litter::component, "Maize", "residue", "\
Maize crop residues in La Tinaja.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "scopel1998");

    frame.set ("water_capacity", 3.8);
    frame.set ("specific_AI", 37.);
    frame.set ("extinction_coefficent", 0.80);
  }
} LitterMaize_syntax;

// litter.C ends here.
