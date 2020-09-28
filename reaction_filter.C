// reaction_filter.C -- Filtration of soil colloids.
// 
// Copyright 2008 Birgitte Gjetterman, Per Abrahamsen and KVL.
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
#include "transform.h"
#include "chemistry.h"
#include "chemical.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "check.h"
#include "mathlib.h"
#include "treelog.h"
#include "frame.h"
#include <memory>

struct ReactionFilter : public Reaction
{
  // Parameters.
  const symbol immobile;
  const symbol mobile;
  const double fc_primary;
  const double fc_secondary;
   
  // Output.
  std::vector<double> F_primary; //Filtration sink in matrix 
  std::vector<double> F_secondary; //Filtration sink in cracks 
  void output (Log& log) const
  {
    output_variable (F_primary, log); 
    output_variable (F_secondary, log); 
  }

  // Simulation.
  void tick_soil (const Geometry& geo, const Soil& soil,
                  const SoilWater& soil_water, const SoilHeat&, 
                  OrganicMatter&, Chemistry& chemistry,
		  const double dt, Treelog& msg)
  { 
    const size_t cell_size = soil.size ();
    Chemical& mob = chemistry.find (mobile);  
    daisy_assert (F_primary.size() == soil.size());
    daisy_assert (F_secondary.size() == soil.size());

    for (size_t i = 0; i < cell_size; i++)
      {
        // Extract soil and water.
        const double C_primary = mob.C_primary (i);//[g cm^-3 water]
        const double C_secondary = mob.C_secondary (i);//[g cm^-3 water]
        const double Theta_primary 
          = soil_water.Theta_primary (i); //[cm^3 cm^-3]
        const double Theta_secondary
          = soil_water.Theta_secondary (i); //[cm^3 cm^-3]
#if 0
        const double M_primary = C_primary * Theta_primary;//[g cm^-3 soil]
        const double M_secondary
          = C_secondary * Theta_secondary;//[g cm^-3 soil]
#else
        const double M_primary = 0.5 * mob.M_primary (i);//[g cm^-3 soil]
        const double M_secondary = 0.5 * mob.M_secondary (i);//[g cm^-3 soil]
#endif
        // Extract pore water velocity.
        const double v_primary
          = soil_water.velocity_cell_primary (geo, i); // [cm/h]
        daisy_assert (std::isfinite (v_primary));
        daisy_assert (v_primary >= 0.0);
        const double v_secondary
          = soil_water.velocity_cell_secondary (geo, i); // [cm/h]
        daisy_assert (std::isfinite (v_secondary));
        daisy_assert (v_secondary >= 0.0);

        // Calculate filter.
        F_primary[i] = std::min (fc_primary *  Theta_primary 
                                 * v_primary * C_primary,
                                 M_primary / dt);   //[g cm^-3 soil h^-1]
        daisy_assert (std::isfinite (F_primary[i]));
        daisy_assert (F_primary[i] >= 0.0);
        F_secondary[i] = std::min(fc_secondary *  Theta_secondary 
                                  * v_secondary * C_secondary,
                                  M_secondary / dt);   //[g cm^-3 soil h^-1]
        daisy_assert (std::isfinite (F_secondary[i]));
        daisy_assert (F_secondary[i] >= 0.0);
      }
    mob.add_to_transform_sink (F_primary);
    mob.add_to_transform_sink_secondary (F_secondary);
    if (chemistry.know (immobile))
      {
        Chemical& immob = chemistry.find (immobile); 
        immob.add_to_transform_source (F_primary);
        immob.add_to_transform_source_secondary (F_secondary);
      }
  }

  // Create.
  bool check (const Geometry&, 
              const Soil& soil, const SoilWater& soil_water, const SoilHeat&,
              const OrganicMatter&, const Chemistry& chemistry,
	      Treelog& msg) const
  { 
    bool ok = true;
    if (!chemistry.know (immobile) && immobile != Attribute::None ())
      {
        msg.error ("'" + immobile.name () + "' not traced");
        ok = false;
      }
    if (!chemistry.know (mobile))
      {
        msg.error ("'" + mobile.name () + "' not traced");
        ok = false;
      }
    return ok;
  }
  void initialize (const Geometry&, const Soil& soil, 
                   const SoilWater&, const SoilHeat&, const OrganicMatter&,
		   const Surface&, Treelog&)
  { 
    F_primary.insert (F_primary.begin (), soil.size (), 0.0);
    daisy_assert (F_primary.size () == soil.size ());

    F_secondary.insert (F_secondary.begin (), soil.size (), 0.0);
    daisy_assert (F_secondary.size () == soil.size ());
  }
  explicit ReactionFilter (const BlockModel& al)
    : Reaction (al),
      immobile (al.name ("immobile", Attribute::None ())),
      mobile (al.name ("mobile")),
      fc_primary (al.number ("fc_primary")),
      fc_secondary (al.number ("fc_secondary"))
  { }
};

static struct ReactionFilterSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ReactionFilter (al); }
  ReactionFilterSyntax ()
    : DeclareModel (Reaction::component, "filter_velocity",
                    "Filtration of soil colloids.")
  { }
  void load_frame (Frame& frame) const
  {


    frame.declare_string ("immobile", Attribute::OptionalConst,
                   "Immobile colloids in the soil.\n\
By default, filtered colloids are not tracked.");
    frame.declare_string ("mobile", Attribute::Const,
                   "Mobile colloids dissolved in soil water.");
    frame.declare ("F_primary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Filtration in the primary domain (intra-aggregate pores).");
    frame.declare ("F_secondary", "g/cm^3/h", Attribute::LogOnly, Attribute::SoilCells,
                   "Filtration in secondary domain (inter-aggregate pores).");

    frame.declare ("fc_primary", "cm^-1", Check::positive (), Attribute::Const,
                   "Filter coefficient in the primary domain");
    // frame.set ("fc_primary", 1.0);
   
    frame.declare ("fc_secondary", "cm^-1", Check::positive (), Attribute::Const,
                   "Filter coefficient in secondary domain");
    // frame.set ("fc_secondary", 0.5);

  }
  
} ReactionFilter_syntax;


