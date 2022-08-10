// litter.C -- Litter layer below vegetation.
// 
// Copyright 2003 KU
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

double
Litter::intercept () const
{ return 1.0; }

bool
Litter::diffuse () const 
{ return false; }

double
Litter::water_protected () const    // Water not evapable [mm]
{ return 0.0; }

double
Litter::potential_exfiltration () const // Water exchange with soil [mm/h]
{ return 0.0; }

double
Litter::decompose_factor () const
{ return 1.0; }

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
  void tick (const Bioclimate&, const Geometry& geo, const Soil& soil,
	     const SoilWater& soil_water, const SoilHeat& soil_heat,
	     OrganicMatter& organic, Chemistry& chemistry,
	     const double dt,
	     Treelog& msg)
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
  void tick (const Bioclimate&, const Geometry& geo, const Soil& soil,
	     const SoilWater& soil_water, const SoilHeat& soil_heat,	    
	     OrganicMatter& organic, Chemistry& chemistry,
	     const double dt,
	     Treelog& msg)
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


// litter.C ends here.
