// harvest.C

#include "harvest.h"
#include "syntax.h"
#include "log.h"
#include "submodel.h"

void 
Harvest::output (Log& log) const
{
  log.output ("column", column);
  log.output ("time", time);
  log.output ("crop", crop);
  log.output ("stem_DM", stem_DM);
  log.output ("stem_N", stem_N);
  log.output ("stem_C", stem_C);
  log.output ("dead_DM", dead_DM);
  log.output ("dead_N", dead_N);
  log.output ("dead_C", dead_C);
  log.output ("leaf_DM", leaf_DM);
  log.output ("leaf_N", leaf_N);
  log.output ("leaf_C", leaf_C);
  log.output ("sorg_DM", sorg_DM);
  log.output ("sorg_N", sorg_N);
  log.output ("sorg_C", sorg_C);
  output_submodule (chemicals, "chemicals", log);
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
  Chemicals::add_syntax  ("chemicals", syntax, alist, Syntax::State,
			  "Chemicals in harvest.");
}

Harvest::Harvest (const AttributeList& alist)
  : column (alist.name ("column")),
    time (alist.time ("time")),
    crop (alist.name("crop")),
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
    chemicals (alist.alist_sequence ("chemicals"))
{ }
  

Harvest::Harvest (string col, Time t, string crp, 
		  double sDM, double sN, double sC, 
		  double dDM, double dN, double dC,
		  double lDM, double lN, double lC, 
		  double oDM, double oN, double oC, const Chemicals& chem)
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
    chemicals (chem)
{ }

static Submodel::Register 
harvest_submodel ("Harvest", Harvest::load_syntax);
