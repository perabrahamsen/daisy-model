// development.h -- Default crop development submodel.
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

#include "development.h"
#include "production.h"
#include "vernalization.h"
#include "plf.h"
#include "log.h"
#include "message.h"
#include "submodel.h"

void
Development::light_hour ()
{ partial_day_length += 1.0; }

void
Development::tick_daily (const string& name, 
			 const double Ta, const double WLeaf, 
			 Production& production, Vernalization& vernalization,
			 const double cut_stress)
{
  // Update final day length.
  day_length = partial_day_length;
  partial_day_length = 0.0;

  // Update DS.
  if (fmod (DS, 2.0) < 1.0)
    {
      // Only increase DS if assimilate production covers leaf respiration.
      if (production.IncWLeaf +  production.DeadWLeaf
	  >  -WLeaf /1000.0) // It lost 0.1% of its leafs to resp.
	DS += (DSRate1 * TempEff1 (Ta) * PhotEff1 (day_length + 1.0))
	  * (1.0 - cut_stress);
      vernalization (Ta, DS);

      if (DS >= 1.0)
	COUT << " [" << name << " is flowering]\n";
    }
  else
    {
      DS += DSRate2 * TempEff2 (Ta) * (1.0 - cut_stress);
      if (DS > DSRepeat)
       {
         DS -= DSSetBack;
       }
      if (DS > DSMature)
       {
	 COUT << " [" << name << " is ripe]\n";
	 DS = DSMature;
	 production.none ();
       }
    }

  assert (DS <= defined_until_ds);
}

void
Development::emergence ()
{
  DS += soil_temperature / EmrTSum * EmrSMF (soil_h);
  if (DS > 0)
    DS = DS_Emr;
}

void 
Development::output (Log& log) const
{
  log.output ("DS", DS);
  log.output ("partial_day_length", partial_day_length);
  log.output ("day_length", day_length);
  log.output ("partial_soil_temperature", partial_soil_temperature);
  log.output ("soil_temperature", soil_temperature);
  log.output ("soil_h", soil_h);
}

void 
Development::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Development");
  alist.add ("description", "\
Crop development and phenology in the default crop model.");

  // Parameters.
  syntax.add ("EmrTSum", "dg C d", Syntax::Const,
	      "Soil temperature sum at emergence.");
  alist.add ("EmrTSum", 100.0);
  syntax.add ("EmrSMF", "cm", "d", Syntax::Const,
	      "Soil moisture (h-function) effect on emergense.");
  PLF SMF;
  SMF.add (-1000.0, 0.00);
  SMF.add (-150.0, 1.00);
  SMF.add (-50.00, 1.00);
  SMF.add (-30.00, 0.00);
  alist.add("EmrSMF",SMF);
  syntax.add ("DS_Emr", Syntax::None (), Syntax::Const,
	      "Development stage at emergence.");
  alist.add ("DS_Emr", 0.01);
  syntax.add ("DSRate1", Syntax::None (), Syntax::Const,
	      "Development rate in the vegetative stage.");
  syntax.add ("DSRate2", Syntax::None (), Syntax::Const,
	      "Development rate in the reproductive stage.");
  syntax.add ("TempEff1", "dg C", Syntax::None (), Syntax::Const,
	      "Temperature effect, vegetative stage.");
  syntax.add ("TempEff2", "dg C", Syntax::None (), Syntax::Const,
	      "Temperature effect, reproductive stage.");
  syntax.add ("PhotEff1", "h", Syntax::None (), Syntax::Const,
	      "Photoperiode effect, vegetative stage.");
  syntax.add ("DSMature", Syntax::None (), Syntax::Const,
	      "Development stage at maturation.");
  alist.add ("DSMature", 2.0);
  syntax.add ("DSRepeat", Syntax::None (), Syntax::Const,
	      "Development stage when DS set back is activated.");
  alist.add ("DSRepeat", 4.0);
  syntax.add ("DSSetBack", Syntax::None (), Syntax::Const,
	      "Development stage set babk at DSRepeat.");
  alist.add ("DSSetBack", 1.7);
  syntax.add ("defined_until_ds", Syntax::None (), Syntax::Const,
	      "\
This parameterization is only valid until the specified development state.");
  alist.add ("defined_until_ds", 2.0);

  // Variables.
  syntax.add ("DS", Syntax::None (), Syntax::State,
	      "Development Stage.");
  alist.add ("DS", -1.0);
  syntax.add ("partial_day_length", "h", Syntax::State,
	      "Number of light hours this day, so far.");
  alist.add ("partial_day_length", 0.0);
  syntax.add ("day_length", "h", Syntax::State,
	      "Number of light hours yesterday.");
  alist.add ("day_length", 0.0);
  syntax.add ("partial_soil_temperature", "dg C h", Syntax::State,
	      "Soil temperature hours this day, so far.");
  alist.add ("partial_soil_temperature", 0.0);
  syntax.add ("soil_temperature", "dg C", Syntax::State,
	      "Average soil temperature yesterday.");
  alist.add ("soil_temperature", 0.0);
  syntax.add ("soil_h", "cm", Syntax::State,
	      "Soil pressure potential.");
  alist.add ("soil_h", -100.0);

}

Development::Development (const AttributeList& al)
  // Parameters.
  : EmrTSum (al.number ("EmrTSum")),
    EmrSMF (al.plf ("EmrSMF")),
    DS_Emr (al.number ("DS_Emr")),
    DSRate1 (al.number ("DSRate1")),
    DSRate2 (al.number ("DSRate2")),
    TempEff1 (al.plf ("TempEff1")),
    TempEff2 (al.plf ("TempEff2")),
    PhotEff1 (al.plf ("PhotEff1")),
    DSMature (al.number ("DSMature")),
    DSRepeat (al.number ("DSRepeat")),
    DSSetBack (al.number ("DSSetBack")),
    defined_until_ds (al.number ("defined_until_ds")),
    // State.
    DS (al.number ("DS")),
    partial_day_length (al.number ("partial_day_length")),
    day_length (al.number ("day_length")),
    partial_soil_temperature (al.number ("partial_soil_temperature")),
    soil_temperature (al.number ("soil_temperature")),
    soil_h (al.number ("soil_h"))
{ }

Development::~Development ()
{ }

static Submodel::Register 
development_submodel ("Development", Development::load_syntax);
