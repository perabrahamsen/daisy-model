// harvest.C

#include "harvest.h"
#include "syntax.h"
#include "log.h"

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
  syntax.add ("column", Syntax::String, Syntax::LogOnly,
	      "Name of column where the harvest were harvested.");
  syntax.add ("time", Syntax::Date, Syntax::LogOnly,
	      "Time of the harvest operation.");
  syntax.add ("crop", Syntax::String, Syntax::LogOnly,
	      "Name of crop that was harvested.");
  syntax.add ("stem_DM", "g/m²", Syntax::LogOnly,
	      "Total stem dry matter in harvest.");
  syntax.add ("stem_N", "g/m²", Syntax::LogOnly,
	      "Total stem nitrogen in harvest.");
  syntax.add ("leaf_DM", "g/m²", Syntax::LogOnly,
	      "Total leaf dry matter in harvest.");
  syntax.add ("leaf_N", "g/m²", Syntax::LogOnly,
	      "Total leaf nitrogen in harvest.");
  syntax.add ("sorg_DM", "g/m²", Syntax::LogOnly,
	      "Total storage organ dry matter in harvest.");
  syntax.add ("sorg_N", "g/m²", Syntax::LogOnly,
	      "Total storage organ nitrogen in harvest.");
  Chemicals::add_syntax  ("chemicals",
			  syntax, alist, Syntax::LogOnly,
			  "Chemicals in harvest.");
}

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
