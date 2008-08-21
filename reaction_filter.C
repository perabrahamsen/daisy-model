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
#include "block.h"
#include "transform.h"
#include "chemistry.h"
#include "chemical.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "check.h"
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
  void tick (const Geometry& geo, const Soil& soil, const SoilWater& soil_water, 
	     const SoilHeat&, const OrganicMatter&,
             Chemistry& chemistry, const double dt, Treelog& msg)
  { 
    const size_t cell_size = soil.size ();
    Chemical& mob = chemistry.find (mobile);  
    Chemical& immob = chemistry.find (immobile); 
    daisy_assert (F_primary.size() == soil.size());
    daisy_assert (F_secondary.size() == soil.size());

    for (size_t i = 0; i < cell_size; i++)
      {
	const double C_primary = mob.C_primary (i);//[g cm^-3 water]
	const double C_secondary = mob.C_secondary (i);//[g cm^-3 water]
        const double Theta_primary = soil_water.Theta_primary (i); //[cm^3 cm^-3]
        const double Theta_secondary = soil_water.Theta_secondary (i); //[cm^3 cm^-3]
	const double M_primary = C_primary * Theta_primary;//[g cm^-3 soil]
	const double M_secondary = C_secondary * Theta_secondary;//[g cm^-3 soil]

        //pore water velocity:
        const double v_primary = soil_water.velocity_cell_primary (geo, i); // [cm/h]
        const double v_secondary = soil_water.velocity_cell_secondary (geo, i); // [cm/h]

        F_primary[i] = std::min(fc_primary *  Theta_primary * v_primary * C_primary,
                                M_primary / dt);   //[g cm^-3 soil h^-1]
        F_secondary[i] = std::min(fc_secondary *  Theta_secondary * v_secondary 
                                  * C_secondary,
                                  M_secondary / dt);   //[g cm^-3 soil h^-1]
      }
    mob.add_to_sink_primary (F_primary);
    mob.add_to_sink_secondary (F_secondary);
    immob.add_to_source_primary (F_primary);
    immob.add_to_source_secondary (F_secondary);
  }

  // Create.
  bool check (const Soil& soil, const SoilWater& soil_water,
	      const SoilHeat&,
	      const Chemistry& chemistry, Treelog& msg) const
  { 
    bool ok = true;
    if (!chemistry.know (immobile))
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
  void initialize (const Soil& soil, Treelog& msg)
  { 
    F_primary.insert (F_primary.begin (), soil.size (), 0.0);
    daisy_assert (F_primary.size () == soil.size ());

    F_secondary.insert (F_secondary.begin (), soil.size (), 0.0);
    daisy_assert (F_secondary.size () == soil.size ());
  }
  explicit ReactionFilter (Block& al)
    : Reaction (al),
      immobile (al.identifier ("immobile")),
      mobile (al.identifier ("mobile")),
      fc_primary (al.number ("fc_primary")),
      fc_secondary (al.number ("fc_seondary"))
  { }
};

static struct ReactionFilterSyntax
{
  static Model& make (Block& al)
  { return *new ReactionFilter (al); }
  ReactionFilterSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description",
               "Filtration of soil colloids.");

    syntax.add ("immobile", Syntax::String, Syntax::Const,
		"Immobile colloids in the soil.");
    syntax.add ("mobile", Syntax::String, Syntax::Const,
		"Mobile colloids dissolved in soil water.");
    syntax.add ("F_primary", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		"Filtration in the primary domain (intra-aggregate pores).");
    syntax.add ("F_secondary", "g/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		"Filtration in secondary domain (inter-aggregate pores).");

    syntax.add ("fc_primary", "m^-1", Check::positive (), Syntax::Const,
                "Filter coefficient in the primary domain");
    alist.add ("fc_primary", 100.0);
   
    syntax.add ("fc_secondary", "m^-1", Check::positive (), Syntax::Const,
                "Filter coefficient in secondary domain");
    alist.add ("fc_secondary", 50.0);

    Librarian::add_type (Reaction::component, "filter", alist, syntax, &make);
  }
  
} ReactionFilter_syntax;


