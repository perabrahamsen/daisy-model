// reaction_adsorption.C -- Equilibrium between sorbed and solute.
// 
// Copyright 2004, 2007 Per Abrahamsen and KVL.
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

#include "reaction.h"
#include "block_model.h"
#include "number.h"
#include "adsorption.h"
#include "chemistry.h"
#include "chemical.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "scope_soil.h"
#include "units.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "mathlib.h"
#include "treelog.h"
#include "frame.h"
#include <memory>

struct ReactionAdsorption : public Reaction
{
  const Units& units;
  
  // Parameters.
  const symbol name_solute;
  const symbol name_sorbed;
  const std::unique_ptr<Adsorption> equilibrium;
  const std::unique_ptr<Number> adsorption_rate;
  const std::unique_ptr<Number> desorption_rate;
  
  // Output.
  std::vector<double> adsorption_source;
  void output (Log& log) const
  { output_variable (adsorption_source, log); }

  // Simulation.
  void tick_soil (const Geometry& geo, const Soil& soil,
                  const SoilWater& soil_water, const SoilHeat& soil_heat, 
		  OrganicMatter&, Chemistry& chemistry,
                  const double dt, Treelog& msg)
  { 
    const size_t cell_size = geo.cell_size ();
    Chemical& solute = chemistry.find (name_solute);
    Chemical& sorbed = chemistry.find (name_sorbed);
    
    ScopeSoil scope (geo, soil, soil_water, soil_heat);
    for (size_t c = 0; c < cell_size; c++)
      { 
	scope.set_cell (c);
	const double Theta_old = soil_water.Theta_old (c);
	const double Theta_new = soil_water.Theta (c);
	const double has_solute = solute.C_primary (c) * Theta_old;
	const double has_sorbed = sorbed.M_primary (c);
	const double has_M = has_solute + has_sorbed;
	const double want_C = equilibrium->M_to_C1 (soil, Theta_new, c, has_M);
	const double want_solute = want_C * Theta_new;
	const double want_sorbed = has_M - want_solute;

	daisy_assert (approximate (has_solute + has_sorbed, 
				   want_solute + want_sorbed));
	
	double convert = 0.0;

	if (has_solute > want_solute)
	  {
	    if (!adsorption_rate->tick_value (units, convert,
					      Units::per_h (), scope, msg))
	      msg.error ("Could not evaluate 'adsorption_rate'");
	    
	    if (convert * dt >= 1.0)
	      convert = (has_solute - want_solute) / dt;
	    else
	      convert *= (has_solute - want_solute);
	  }
	else
	  {
	    if (!desorption_rate->tick_value (units, convert,
					      Units::per_h (), scope, msg))
	      msg.error ("Could not evaluate 'desorption_rate'");
	    
	    if (convert * dt >= 1.0)
	      convert = (has_sorbed - want_sorbed) / dt;
	    else
	      convert *= (has_sorbed - want_sorbed);
	    
	    convert *= -1.0;
	  }
      
      adsorption_source[c] = convert;
	
    }
    solute.add_to_transform_sink (adsorption_source);
    sorbed.add_to_transform_source (adsorption_source);
  }

  // Create.
  bool check (const Geometry& geo, 
              const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const OrganicMatter&, const Chemistry& chemistry,
	      Treelog& msg) const
  { 
    bool ok = true;
    if (!chemistry.know (name_solute))
      {
        msg.error ("'" + name_solute.name () + "' not traced");
        ok = false;
      }
    if (!chemistry.know (name_sorbed))
      {
        msg.error ("'" + name_sorbed.name () + "' not traced");
        ok = false;
      }
    ScopeSoil scope (geo, soil, soil_water, soil_heat);
    if (!adsorption_rate->check_dim (units, scope, Units::per_h (), msg))
      ok = false;
    if (!desorption_rate->check_dim (units, scope, Units::per_h (), msg))
      ok = false;

    return ok;
  }
  void initialize (const Geometry& geo, 
                   const Soil& soil, const SoilWater& soil_water, 
                   const SoilHeat& soil_heat,
		   const OrganicMatter&, const Surface&, Treelog& msg)
  { 
    adsorption_source.insert (adsorption_source.begin (), soil.size (), 0.0);
    daisy_assert (adsorption_source.size () == soil.size ());
    ScopeSoil scope (geo, soil, soil_water, soil_heat);
    adsorption_rate->initialize (units, scope, msg); 
    desorption_rate->initialize (units, scope, msg); 
  }
  explicit ReactionAdsorption (const BlockModel& al)
    : Reaction (al),
      units (al.units ()),
      name_solute (al.name ("solute")),
      name_sorbed (al.name ("sorbed")),
      equilibrium (Librarian::build_item<Adsorption> (al, "equilibrium")),
      adsorption_rate (Librarian::build_item<Number> (al, "adsorption_rate")),
      desorption_rate (al.check ("desorption_rate")
	    ? Librarian::build_item<Number> (al, "desorption_rate")
	    : Librarian::build_item<Number> (al, "adsorption_rate"))
  { }
};

static struct ReactionAdsorptionSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionAdsorption (al); }

  ReactionAdsorptionSyntax ()
    : DeclareModel (Reaction::component, "adsorption", 
	       "Maintain equilibrium between solute and sorbed from.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("solute", Attribute::Const,
		"Name of solute form of chemical.");
    frame.declare_string ("sorbed", Attribute::Const,
		"Name of sorbed form of chemical.");
    frame.declare_object ("equilibrium", Adsorption::component, "\
Function for calculating equilibrium between solute and sorbed form.");
    frame.declare_object ("adsorption_rate", Number::component,
                       Attribute::Const, Attribute::Singleton, 
                       "Tranformation rate from solute to sorbed form.");
    frame.declare_object ("desorption_rate", Number::component,
                       Attribute::OptionalConst, Attribute::Singleton,
                       "Tranformation rate from sorbed to solute form.\n\
By default, this is identical to 'adsorption_rate'.");
    frame.declare ("adsorption_source", "g/cm^3/h", 
		Attribute::LogOnly, Attribute::SoilCells, "\
Converted from solute to sorbed form this timestep (may be negative).");

  }
} ReactionAdsorption_syntax;

// reaction_adsorption.C ends here.
