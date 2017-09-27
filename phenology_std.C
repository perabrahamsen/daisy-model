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

#define BUILD_DLL

#include "phenology.h"
#include "block_model.h"
#include "production.h"
#include "vernalization.h"
#include "plf.h"
#include "mathlib.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"

class PhenologyStandard : public Phenology
{
  // Parameters.
private:
  const double EmrTSum;         // Soil temp sum at emergence
  const PLF& EmrSMF;            // Soil moisture effect on emergence
  const double DS_Emr;          // Development stage (DS) emergence
  const double DSRate1;         // Development rate [DS/d],
                                // the vegetative stage
  const double DSRate2;         // Development rate [DS/d],
                                // the reproductive stage
  const PLF& TempEff1;          // Temperature effect, vegetative stage
  const PLF& TempEff2;          // Temperature effect, reproductive stage
  const PLF& PhotEff1; // Photoperiode effect, vegetative stage defi. limit

  const double DSMature;        // DS at maturation
  const double DSRepeat;        // DS where DS is set back (perennial crops)
  const double DSSetBack;       // DS set back at DSRepeat
  const double defined_until_ds; // Model invalid after this DS.
  const symbol DS1_name;         // Name of DS 1->2 transition.

  // Simulation.
private:
  void tick_daily (double Ta, bool leaf_growth, 
                   Production&, Vernalization&, double cut_stress, Treelog&);
  void emergence (double h, double T, double dt);
  bool mature () const
  { return DS >= DSMature; }

  // Create.
public:
  PhenologyStandard (const BlockModel&);
};

void
PhenologyStandard::tick_daily (const double Ta, const bool leaf_growth, 
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
      if (leaf_growth)
        DS += (DSRate1 * TempEff1 (Ta) * PhotEff1 (day_length + 1.0))
          * (1.0 - cut_stress);
      vernalization (Ta, DS);

      if (DS >= 1.0)
        out.message (DS1_name.name ());
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
          out.message ("Ripe");
          DS = DSMature;
        }
    }
  daisy_assert (DS <= defined_until_ds);
}

void
PhenologyStandard::emergence (const double h, const double T, const double dt)
{
  DS += (dt / 24.0) * T / EmrTSum * EmrSMF (h);
  if (DS > 0)
    DS = DS_Emr;
}

PhenologyStandard::PhenologyStandard (const BlockModel& al)
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
    defined_until_ds (al.number ("defined_until_ds")),
    DS1_name (al.name ("DS1_name"))
{ }

static struct PhenologyStandardSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PhenologyStandard (al); }

  PhenologyStandardSyntax ()
    : DeclareModel (Phenology::component, "default", 
                    "Default crop phenology model.")
  { }
  void load_frame (Frame& frame) const
  {
    // Parameters.
    frame.declare ("EmrTSum", "dg C d", Attribute::Const,
                   "Soil temperature sum at emergence.");
    frame.declare ("EmrSMF", "cm", Attribute::None (), Attribute::Const,
                   "Soil moisture (h-function) effect on emergence.");
    PLF SMF;
    SMF.add (-1000.0, 1.00);
    SMF.add (-150.0, 1.00);
    SMF.add (-50.00, 1.00);
    SMF.add (-30.00, 1.00);
    frame.set ("EmrSMF",SMF);
    frame.declare ("DS_Emr", "DS", Attribute::Const,
                   "Development stage at emergence.");
    frame.set ("DS_Emr", 0.01);
    frame.declare ("DSRate1", "DS/d", Attribute::Const,
                   "Development rate in the vegetative stage.");
    frame.declare ("DSRate2", "DS/d", Attribute::Const,
                   "Development rate in the reproductive stage.");
    frame.declare ("TempEff1", "dg C", Attribute::None (), Attribute::Const,
                   "Temperature effect, vegetative stage.");
    frame.declare ("TempEff2", "dg C", Attribute::None (), Attribute::Const,
                   "Temperature effect, reproductive stage.");
    frame.declare ("PhotEff1", "h", Attribute::None (), Attribute::Const,
                   "Photoperiode effect, vegetative stage.");
    frame.declare ("DSMature", "DS", Attribute::Const,
                   "Development stage at maturation.");
    frame.set ("DSMature", 2.0);
    frame.declare ("DSRepeat", "DS", Attribute::Const,
                   "Development stage when DS set back is activated.");
    frame.set ("DSRepeat", 4.0);
    frame.declare ("DSSetBack", "DS", Attribute::Const,
                   "Development stage set back at DSRepeat.");
    frame.set ("DSSetBack", 1.7);
    frame.declare ("defined_until_ds", "DS", Attribute::Const, "\
This parameterization is only valid until the specified development state.");
    frame.set ("defined_until_ds", 2.0);
    frame.declare_string ("DS1_name", Attribute::Const, "\
Name of development stage 1 to 2 transition.");
    frame.set ("DS1_name", "Flowering");
  }
} PhenologyStandard_syntax;
