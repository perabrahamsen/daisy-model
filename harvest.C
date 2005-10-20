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


#include "harvest.h"
#include "syntax.h"
#include "log.h"
#include "submodel.h"

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
  output_submodule (time, "time", log);
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
  output_submodule (chemicals, "chemicals", log);
}

void 
Harvest::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Harvest");
  alist.add ("description", "Log of all harvests during the simulation.");
  syntax.add ("column", Syntax::String, Syntax::State,
	      "Name of column where the yield were harvested.");
  syntax.add_submodule ("time", alist, Syntax::State,
			"Time of the harvest operation.", Time::load_syntax);
  syntax.add ("crop", Syntax::String, Syntax::State,
	      "Name of crop that was harvested.");
  syntax.add ("stem_DM", "g/m^2", Syntax::State,
	      "Total stem dry matter in harvest.");
  syntax.add ("stem_N", "g/m^2", Syntax::State,
	      "Total stem nitrogen in harvest.");
  syntax.add ("stem_C", "g/m^2", Syntax::State,
	      "Total stem carbon in harvest.");
  syntax.add ("dead_DM", "g/m^2", Syntax::State,
	      "Total dead leaf dry matter in harvest.");
  syntax.add ("dead_N", "g/m^2", Syntax::State,
	      "Total dead leaf nitrogen in harvest.");
  syntax.add ("dead_C", "g/m^2", Syntax::State,
	      "Total dead leaf carbon in harvest.");
  syntax.add ("leaf_DM", "g/m^2", Syntax::State,
	      "Total leaf dry matter in harvest.");
  syntax.add ("leaf_N", "g/m^2", Syntax::State,
	      "Total leaf nitrogen in harvest.");
  syntax.add ("leaf_C", "g/m^2", Syntax::State,
	      "Total leaf carbon in harvest.");
  syntax.add ("sorg_DM", "g/m^2", Syntax::State,
	      "Total storage organ dry matter in harvest.");
  syntax.add ("sorg_N", "g/m^2", Syntax::State,
	      "Total storage organ nitrogen in harvest.");
  syntax.add ("sorg_C", "g/m^2", Syntax::State,
	      "Total storage organ carbon in harvest.");
  syntax.add ("water_stress_days", "d", Syntax::State, 
              "Production days lost due to water stress.");
  syntax.add ("nitrogen_stress_days", "d", Syntax::State, 
              "Production days lost due to water stress.");
  Chemicals::add_syntax  ("chemicals", syntax, alist, Syntax::State,
			  "Chemicals in harvest.");
}

Harvest::Harvest (const Block& alist)
  : column (alist.identifier ("column")),
    time (alist.alist ("time")),
    crop (alist.identifier ("crop")),
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
    chemicals (alist.alist_sequence ("chemicals"))
{ }
  

Harvest::Harvest (const symbol col, Time t, const symbol crp, 
		  double sDM, double sN, double sC, 
		  double dDM, double dN, double dC,
		  double lDM, double lN, double lC, 
		  double oDM, double oN, double oC, 
                  double wsd, double nsd, const Chemicals& chem)
  : column (col),
    time (t),
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
    chemicals (chem)
{ }

static Submodel::Register 
harvest_submodel ("Harvest", Harvest::load_syntax);
