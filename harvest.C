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
}

void 
Harvest::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("column", Syntax::String, Syntax::LogOnly);
  syntax.add ("time", Syntax::Date, Syntax::LogOnly);
  syntax.add ("crop", Syntax::String, Syntax::LogOnly);
  syntax.add ("stem_DM", Syntax::Number, Syntax::LogOnly);
  syntax.add ("stem_N", Syntax::Number, Syntax::LogOnly);
  syntax.add ("leaf_DM", Syntax::Number, Syntax::LogOnly);
  syntax.add ("leaf_N", Syntax::Number, Syntax::LogOnly);
  syntax.add ("sorg_DM", Syntax::Number, Syntax::LogOnly);
  syntax.add ("sorg_N", Syntax::Number, Syntax::LogOnly);
}

Harvest::Harvest (string col, Time t, string crp, 
		  double sC, double sN, double lC, double lN, 
		  double oC, double oN)
  : column (col),
    time (t),
    crop (crp),
    stem_DM (sC),
    stem_N (sN),
    leaf_DM (lC),
    leaf_N (lN),
    sorg_DM (oC),
    sorg_N (oN)
{ }
