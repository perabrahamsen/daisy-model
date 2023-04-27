// harvest.C
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

#include "harvest.h"
#include "frame.h"
#include "log.h"
#include "librarian.h"

double
Harvest::total_N () const
{ return stem_N + dead_N + leaf_N + sorg_N; }

double 
Harvest::total_C () const
{ return stem_C + dead_C + leaf_C + sorg_C; }

double 
Harvest::total_DM () const
{ return stem_DM + dead_DM + leaf_DM + sorg_DM; }


void 
Harvest::output (Log& log) const
{
  output_variable (column, log);
  if (sow_time != Time::null ())
    output_submodule (sow_time, "sow_time", log);
  if (emerge_time != Time::null ())
    output_submodule (emerge_time, "emerge_time", log);
  if (flowering_time != Time::null ())
    output_submodule (flowering_time, "flowering_time", log);
  if (ripe_time != Time::null ())
    output_submodule (ripe_time, "ripe_time", log);
  if (harvest_time != Time::null ())
    output_submodule (harvest_time, "harvest_time", log);
  output_variable (crop, log);
  output_variable (stem_DM, log);
  output_variable (stem_N, log);
  output_variable (stem_C, log);
  output_variable (dead_DM, log);
  output_variable (dead_N, log);
  output_variable (dead_C, log);
  output_variable (leaf_DM, log);
  output_variable (leaf_N, log);
  output_variable (leaf_C, log);
  output_variable (sorg_DM, log);
  output_variable (sorg_N, log);
  output_variable (sorg_C, log);
  output_variable (water_stress_days, log);
  output_variable (nitrogen_stress_days, log);
  output_variable (water_productivity, log);
  output_variable (harvest_index, log);
  if (sorg_DM > 0.0)
    output_value (sorg_N / sorg_DM, "sorg_N_per_DM", log);
}

void 
Harvest::load_syntax (Frame& frame)
{
  frame.declare_string ("column", Attribute::State,
	      "Name of column where the yield were harvested.");
  frame.declare_submodule ("sow_time", Attribute::OptionalState, "\
Time of sowing or planting of crop.",
                           Time::load_syntax);
  frame.declare_submodule ("emerge_time", Attribute::OptionalState,
                           "Time the crop emerge on the field.",
                           Time::load_syntax);
  frame.declare_submodule ("flowering_time", Attribute::OptionalState,
                           "Time of flowering.", Time::load_syntax);
  frame.declare_submodule ("ripe_time", Attribute::OptionalState,
                           "Time the crop became ripe.", Time::load_syntax);
  frame.declare_submodule ("harvest_time", Attribute::OptionalState,
                           "Time of the harvest operation.", Time::load_syntax);
  frame.declare_string ("crop", Attribute::State,
	      "Name of crop that was harvested.");
  frame.declare ("stem_DM", "g/m^2", Attribute::State,
	      "Total stem dry matter in harvest.");
  frame.declare ("stem_N", "g/m^2", Attribute::State,
	      "Total stem nitrogen in harvest.");
  frame.declare ("stem_C", "g/m^2", Attribute::State,
	      "Total stem carbon in harvest.");
  frame.declare ("dead_DM", "g/m^2", Attribute::State,
	      "Total dead leaf dry matter in harvest.");
  frame.declare ("dead_N", "g/m^2", Attribute::State,
	      "Total dead leaf nitrogen in harvest.");
  frame.declare ("dead_C", "g/m^2", Attribute::State,
	      "Total dead leaf carbon in harvest.");
  frame.declare ("leaf_DM", "g/m^2", Attribute::State,
	      "Total leaf dry matter in harvest.");
  frame.declare ("leaf_N", "g/m^2", Attribute::State,
	      "Total leaf nitrogen in harvest.");
  frame.declare ("leaf_C", "g/m^2", Attribute::State,
	      "Total leaf carbon in harvest.");
  frame.declare ("sorg_DM", "g/m^2", Attribute::State,
	      "Total storage organ dry matter in harvest.");
  frame.declare ("sorg_N", "g/m^2", Attribute::State,
	      "Total storage organ nitrogen in harvest.");
  frame.declare ("sorg_C", "g/m^2", Attribute::State,
	      "Total storage organ carbon in harvest.");
  frame.declare ("water_stress_days", "d", Attribute::State, 
              "Production days lost due to water stress.");
  frame.declare ("nitrogen_stress_days", "d", Attribute::State, 
              "Production days lost due to water stress.");
  frame.declare ("water_productivity", "kg DM/m^3 H2O", Attribute::State, 
              "Storage organ harvested per evapotranspiration.");
  frame.declare_fraction ("harvest_index", Attribute::State, 
                          "Economic yield as fraction of total shoot.");
  frame.declare_fraction ("sorg_N_per_DM", Attribute::LogOnly, "\
Nitrogen fraction is storage dy matter.");
}

Harvest::Harvest (const Block& alist)
  : column (alist.name ("column")),
    sow_time (alist.check ("sow_time")
	      ? Time (alist.submodel ("sow_time"))
	      : Time::null ()),
    emerge_time (alist.check ("emerge_time")
		 ? Time (alist.submodel ("emerge_time"))
		 : Time::null ()),
    flowering_time (alist.check ("flowering_time")
		    ? Time (alist.submodel ("flowering_time"))
		    : Time::null ()),
    ripe_time (alist.check ("ripe_time")
	       ? Time (alist.submodel ("ripe_time"))
	       : Time::null ()),
    harvest_time (alist.check ("harvest_time")
		  ? Time (alist.submodel ("harvest_time"))
		  : Time::null ()),
    crop (alist.name ("crop")),
    stem_DM (alist.number ("stem_DM")),
    stem_N (alist.number ("stem_N")),
    stem_C (alist.number ("stem_C")),
    dead_DM (alist.number ("dead_DM")),
    dead_N (alist.number ("dead_N")),
    dead_C (alist.number ("dead_C")),
    leaf_DM (alist.number ("leaf_DM")),
    leaf_N (alist.number ("leaf_N")),
    leaf_C (alist.number ("leaf_C")),
    sorg_DM (alist.number ("sorg_DM")),
    sorg_N (alist.number ("sorg_N")),
    sorg_C (alist.number ("sorg_C")),
    water_stress_days (alist.number ("water_stress_days")),
    nitrogen_stress_days (alist.number ("nitrogen_stress_days")),
    water_productivity (alist.number ("water_productivity")),
    harvest_index (alist.number ("harvest_index"))
{ }
  

Harvest::Harvest (const symbol col, 
                  const Time& sow_t,
                  const Time& emerge_t,
                  const Time& flowering_t,
                  const Time& ripe_t,
                  const Time& harvest_t,
                  const symbol crp, 
		  double sDM, double sN, double sC, 
		  double dDM, double dN, double dC,
		  double lDM, double lN, double lC, 
		  double oDM, double oN, double oC, 
                  double wsd, double nsd, double wp_et, double hi)
  : column (col),
    sow_time (sow_t),
    emerge_time (emerge_t),
    flowering_time (flowering_t),
    ripe_time (ripe_t),
    harvest_time (harvest_t),
    crop (crp),
    stem_DM (sDM),
    stem_N (sN),
    stem_C (sC),
    dead_DM (dDM),
    dead_N (dN),
    dead_C (dC),
    leaf_DM (lDM),
    leaf_N (lN),
    leaf_C (lC),
    sorg_DM (oDM),
    sorg_N (oN),
    sorg_C (oC),
    water_stress_days (wsd),
    nitrogen_stress_days (nsd),
    water_productivity (wp_et),
    harvest_index (hi)
{ }

static DeclareSubmodel 
harvest_submodel (Harvest::load_syntax, "Harvest", "\
Log of all harvests during the simulation.");

// harvest.C ends here.
