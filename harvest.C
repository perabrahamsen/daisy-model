// harvest.C

#include "harvest.h"
#include "syntax.h"
#include "log.h"
#include "submodel.h"

void 
Harvest::output (Log& log, Filter& filter) const
{
  log.output ("column", filter, column, true);
  log.output ("time", filter, time, true);
  log.output ("crop", filter, crop, true);
  log.output ("stem_DM", filter, stem_DM, true);
  log.output ("stem_N", filter, stem_N, true);
  log.output ("leaf_DM", filter, leaf_DM, true);
  log.output ("leaf_N", filter, leaf_N, true);
  log.output ("sorg_DM", filter, sorg_DM, true);
  log.output ("sorg_N", filter, sorg_N, true);
  output_submodule_log_only (chemicals, "chemicals", log, filter);
}

void 
Harvest::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Harvest");
  alist.add ("description", "Log of all harvests during the simulation.");
  syntax.add ("column", Syntax::String, Syntax::State,
	      "Name of column where the yield were harvested.");
  syntax.add ("time", Syntax::Date, Syntax::State,
	      "Time of the harvest operation.");
  syntax.add ("crop", Syntax::String, Syntax::State,
	      "Name of crop that was harvested.");
  syntax.add ("stem_DM", "g/m^2", Syntax::State,
	      "Total stem dry matter in harvest.");
  syntax.add ("stem_N", "g/m^2", Syntax::State,
	      "Total stem nitrogen in harvest.");
  syntax.add ("leaf_DM", "g/m^2", Syntax::State,
	      "Total leaf dry matter in harvest.");
  syntax.add ("leaf_N", "g/m^2", Syntax::State,
	      "Total leaf nitrogen in harvest.");
  syntax.add ("sorg_DM", "g/m^2", Syntax::State,
	      "Total storage organ dry matter in harvest.");
  syntax.add ("sorg_N", "g/m^2", Syntax::State,
	      "Total storage organ nitrogen in harvest.");
  Chemicals::add_syntax  ("chemicals", syntax, alist, Syntax::State,
			  "Chemicals in harvest.");
}

Harvest::Harvest (const AttributeList& alist)
  : column (alist.name ("column")),
    time (alist.time ("time")),
    crop (alist.name("crop")),
    stem_DM (alist.number ("stem_DM")),
    stem_N (alist.number ("stem_N")),
    leaf_DM (alist.number ("leaf_DM")),
    leaf_N (alist.number ("leaf_N")),
    sorg_DM (alist.number ("sorg_DM")),
    sorg_N (alist.number ("sorg_N")),
    chemicals (alist.alist_sequence ("chemicals"))
{ }
  

Harvest::Harvest (string col, Time t, string crp, 
		  double sC, double sN, double lC, double lN, 
		  double oC, double oN, const Chemicals& chem)
  : column (col),
    time (t),
    crop (crp),
    stem_DM (sC),
    stem_N (sN),
    leaf_DM (lC),
    leaf_N (lN),
    sorg_DM (oC),
    sorg_N (oN),
    chemicals (chem)
{ }

static Submodel::Register 
harvest_submodel ("Harvest", Harvest::load_syntax);
