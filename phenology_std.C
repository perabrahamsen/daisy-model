// phenology_std.C -- Default crop phenology model.
// 
// Copyright 2003 Per Abrahamsen and Søren Hansen
// Copyright 2003 KVL.
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


#include "phenology.h"
#include "production.h"
#include "vernalization.h"
#include "plf.h"
#include <math.h>

class PhenologyStandard : public Phenology
{
  // Parameters.
private:
  const double EmrTSum;		// Soil temp sum at emergence
  const PLF& EmrSMF;		// Soil moisture effect on emergence
  const double DS_Emr;		// Development stage (DS) emergence
  const double DSRate1;		// Development rate [C-1 or d-1],
				// the vegetative stage
  const double DSRate2;		// Development rate [C-1 or d-1],
				// the reproductive stage
  const PLF& TempEff1;	        // Temperature effect, vegetative stage
  const PLF& TempEff2;	        // Temperature effect, reproductive stage
  const PLF& PhotEff1; // Ptotoperiode effect, vegetative stage defi. limit

  const double DSMature;	// DS at maturation
  const double DSRepeat;        // DS where DS is set back (perennial crops)
  const double DSSetBack; 	// DS set back at DSRepeat
  const double defined_until_ds; // Model invalid after this DS.


  // Simulation.
private:
  void tick_daily (double Ta, double WLeaf, 
		   Production&, Vernalization&, double cut_stress, Treelog&);
  void emergence ();

  // Create.
public:
  PhenologyStandard (const AttributeList&);
};

void
PhenologyStandard::tick_daily (const double Ta, const double WLeaf, 
			       Production& production, 
			       Vernalization& vernalization,
			       const double cut_stress, Treelog& out)
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
	out.message ("==> flowering");
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
	 out.message ("==> ripe");
	 DS = DSMature;
	 production.none ();
       }
    }

  daisy_assert (DS <= defined_until_ds);
}

void
PhenologyStandard::emergence ()
{
  DS += soil_temperature / EmrTSum * EmrSMF (soil_h);
  if (DS > 0)
    DS = DS_Emr;
}

PhenologyStandard::PhenologyStandard (const AttributeList& al)
  : Phenology (al),
    EmrTSum (al.number ("EmrTSum")),
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
    defined_until_ds (al.number ("defined_until_ds"))
{ }

static struct PhenologyStandardSyntax
{
  static Phenology&
  make (const AttributeList& al)
  { return *new PhenologyStandard (al); }

  PhenologyStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Phenology::load_syntax (syntax, alist);
    alist.add ("description", 
	       "Default crop phenology model.");

    // Parameters.
    syntax.add ("EmrTSum", "dg C d", Syntax::Const,
		"Soil temperature sum at emergence.");
    syntax.add ("EmrSMF", "cm", "d", Syntax::Const,
		"Soil moisture (h-function) effect on emergense.");
    PLF SMF;
    SMF.add (-1000.0, 1.00);
    SMF.add (-150.0, 1.00);
    SMF.add (-50.00, 1.00);
    SMF.add (-30.00, 1.00);
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
    syntax.add ("defined_until_ds", Syntax::None (), Syntax::Const, "\
This parameterization is only valid until the specified development state.");
    alist.add ("defined_until_ds", 2.0);

    Librarian<Phenology>::add_type ("default", alist, syntax, &make);
  }
} PhenologyStandard_syntax;
