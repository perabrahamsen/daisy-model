// production.C -- Default crop production submodel.
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

#include "production.h"
#include "organic_matter.h"
#include "am.h"
#include "geometry.h"
#include "log.h"
#include "time.h"
#include "submodel.h"

double
Production::RSR () const
{
  const double shoot = WStem + WSOrg + WLeaf;
  const double root = WRoot;
  if (shoot < 20.0 || root < 20.0)
    return 0.33333;
  return root/shoot;
}

double
Production::DM () const
{
  return (WSOrg + WStem + WLeaf + WDead) * 10; // [g/m^2 -> kg/ha]
}

double
Production::total_N () const
{
  // kg/ha -> g/m^2
  const double conv = 1000.0 / (100.0 * 100.0);
  return NCrop / conv;
}

void 
Production::output (Log& log) const
{
  log.output ("CH2OPool", CH2OPool);
  log.output ("WLeaf", WLeaf);
  log.output ("WStem", WStem);
  log.output ("WRoot", WRoot);
  log.output ("WSOrg", WSOrg);
  log.output ("WDead", WDead);
  log.output ("CCrop", CCrop);
  log.output ("CLeaf", CLeaf);
  log.output ("CStem", CStem);
  log.output ("CRoot", CRoot);
  log.output ("CSOrg", CSOrg);
  log.output ("CDead", CDead);
  log.output ("NLeaf", NLeaf);
  log.output ("NStem", NStem);
  log.output ("NRoot", NRoot);
  log.output ("NSOrg", NSOrg);
  log.output ("NDead", NDead);
  log.output ("NCrop", NCrop);
  log.output ("C_AM", C_AM);
  log.output ("N_AM", N_AM);
}

void 
Production::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Production");
  alist.add ("description", "\
Crop production in the default crop model.");

  // Parameters.
  syntax.add ("CH2OReleaseRate", Syntax::None (), Syntax::Const,
	      "CH2O Release Rate constant.");
  alist.add ("CH2OReleaseRate", 0.04);
  syntax.add ("E_Root", Syntax::None (), Syntax::Const,
	      "Conversion efficiency, root.");
  alist.add ("E_Root", 0.69);
  syntax.add ("E_Leaf", Syntax::None (), Syntax::Const,
	      "Conversion efficiency, leaf.");
  alist.add ("E_Leaf", 0.68);
  syntax.add ("E_Stem", Syntax::None (), Syntax::Const,
	      "Conversion efficiency, stem.");
  alist.add ("E_Stem", 0.66);
  syntax.add ("E_SOrg", Syntax::None (), Syntax::Const,
	      "Conversion efficiency, storage organ.");
  syntax.add ("r_Root", Syntax::None (), Syntax::Const,
	      "Maintenance respiration coefficient, root.");
  alist.add ("r_Root", 0.015);
  syntax.add ("r_Leaf", Syntax::None (), Syntax::Const,
	      "Maintenance respiration coefficient, leaf.");
  syntax.add ("r_Stem", Syntax::None (), Syntax::Const,
	      "Maintenance respiration coefficient, stem.");
  syntax.add ("r_SOrg", Syntax::None (), Syntax::Const,
	      "Maintenance respiration coefficient, storage organ.");
  syntax.add ("ShldResC", Syntax::None (), Syntax::Const,
	      "Capacity of shielded reserves (fraction of stem DM).");
  alist.add ("ShldResC", 0.0);
  syntax.add ("ReMobilDS", Syntax::None (), Syntax::Const,
	      "Remobilization, Initial DS.");
  alist.add ("ReMobilDS", 1.20);
  syntax.add ("ReMobilRt", "d^-1", Syntax::Const,
	      "Remobilization, release rate.");
  alist.add ("ReMobilRt", 0.1);
  syntax.add ("ExfoliationFac", Syntax::None (), Syntax::Const,
	      "Exfoliation factor, 0-1.");
  alist.add ("ExfoliationFac", 1.0);
  syntax.add ("GrowthRateRedFac", Syntax::None (), Syntax::Const,
	      "Growth rate reduction factor, 0-1.");
  alist.add ("GrowthRateRedFac", 0.0);
  syntax.add ("LfDR", "DS", " d^-1", Syntax::Const,
	      "Death rate of Leafs.");
  syntax.add ("RtDR", "DS", " d^-1", Syntax::Const,
	      "Death rate of Roots.");
  syntax.add ("Large_RtDR", "d^-1", Syntax::Const,
	      "Extra death rate for large root/shoot.");
  alist.add ("Large_RtDR", 0.05);
  syntax.add ("IntDSRelRtRes", Syntax::None (), Syntax::Const,
	      "Initial DS for the release of root reserves.");
  alist.add ("IntDSRelRtRes", 0.80);
  syntax.add ("EndDSRelRtRes", Syntax::None (), Syntax::Const,
	      "End DS for the release of root reserves.");
  alist.add ("EndDSRelRtRes", 0.80);
  syntax.add ("RelRateRtRes", "d^-1", Syntax::Const,
	      "Release rate of root reserves.");
  alist.add ("RelRateRtRes", 0.05);
  syntax.add ("LfRtRelRtRes", Syntax::None (), Syntax::Const,
	      "Max Leaf:Root for the release of root res.");
  alist.add ("LfRtRelRtRes", 0.80);

  // Variables.
  syntax.add ("CH2OPool", "g DM/m^2", Syntax::State, "CH2O Pool.");
  alist.add ("CH2OPool", 0.001);
  syntax.add ("WLeaf", "g DM/m^2", Syntax::State, "Leaf dry matter weight.");
  alist.add ("WLeaf", 0.001);
  syntax.add ("WStem", "g DM/m^2", Syntax::State, "Stem dry matter weight.");
  alist.add ("WStem", 0.000);
  syntax.add ("WRoot", "g DM/m^2", Syntax::State, "Root dry matter weight.");
  alist.add ("WRoot", 0.001);
  syntax.add ("WSOrg", "g DM/m^2", Syntax::State,
	      "Storage organ dry matter weight.");
  alist.add ("WSOrg", 0.000);
  syntax.add ("WDead", "g DM/m^2", Syntax::State,
	      "Dead leaves dry matter weight.");
  alist.add ("WDead", 0.000);
  syntax.add ("CCrop", "g C/m^2", Syntax::LogOnly, "Crop C weight.");
  syntax.add ("CLeaf", "g C/m^2", Syntax::LogOnly, "Leaf C weight.");
  syntax.add ("CStem", "g C/m^2", Syntax::LogOnly, "Stem C weight.");
  syntax.add ("CRoot", "g C/m^2", Syntax::LogOnly, "Root C weight.");
  syntax.add ("CSOrg", "g C/m^2", Syntax::LogOnly, "Storage organ C weight.");
  syntax.add ("CDead", "g C/m^2", Syntax::LogOnly, "Dead leaves C weight.");
  syntax.add ("NLeaf", "g N/m^2", Syntax::State,
	      "Nitrogen stored in the leaves.");
  alist.add ("NLeaf", 0.000);
  syntax.add ("NStem", "g N/m^2", Syntax::State,
	      "Nitrogen stored in the stem.");
  alist.add ("NStem", 0.000);
  syntax.add ("NRoot", "g N/m^2", Syntax::State,
	      "Nitrogen stored in the roots.");
  alist.add ("NRoot", 0.000);
  syntax.add ("NSOrg", "g N/m^2", Syntax::State,
	      "Nitrogen stored in the storage organ.");
  alist.add ("NSOrg", 0.000);
  syntax.add ("NDead", "g N/m^2", Syntax::State,
	      "Nitrogen stored in dead leaves.");
  alist.add ("NDead", 0.000);
  syntax.add ("NCrop", "g N/m^2", Syntax::OptionalState,
	      "Total crop nitrogen content.\n\
For initialization, set this to the amount of N in the seed.");
  syntax.add ("C_AM", "g C/m^2", Syntax::State,
	      "Added C in plant material.");
  alist.add ("C_AM", 0.000);
  syntax.add ("N_AM", "g N/m^2", Syntax::State,
	      "Added N in plant material.");
  alist.add ("N_AM", 0.000);
}

void
Production::initialize (const double SeedN)
{
  if (NCrop <= 0.0)
    NCrop = SeedN;
}

void
Production::initialize (const string& name, 
			const vector<AttributeList*>& root,
			const vector<AttributeList*>& dead,
			const Geometry& geometry,
			OrganicMatter& organic_matter)
{
  // Hotstart, find pool in organic matter.
  AM_root = organic_matter.find_am (name, "root");
  AM_leaf = organic_matter.find_am (name, "dead");

  // If not found, we is planting emerged crops.  Create pools.
  if (!AM_root)
    {
      AM_root = &AM::create (geometry, Time (1, 1, 1, 1), root,
			     name, "root", AM::Locked);
      organic_matter.add (*AM_root);
    }
	  
  if (!AM_leaf)
    {
      AM_leaf = &AM::create (geometry, Time (1, 1, 1, 1), dead,
			     name, "dead", AM::Locked);
      organic_matter.add (*AM_leaf);
    }
}

Production::Production (const AttributeList& al)
  // Parameters.
  : CH2OReleaseRate (al.number ("CH2OReleaseRate")),
    E_Root (al.number ("E_Root")),
    E_Leaf (al.number ("E_Leaf")),
    E_Stem (al.number ("E_Stem")),
    E_SOrg (al.number ("E_SOrg")),
    r_Root (al.number ("r_Root")),
    r_Leaf (al.number ("r_Leaf")),
    r_Stem (al.number ("r_Stem")),
    r_SOrg (al.number ("r_SOrg")),
    ShldResC (al.number ("ShldResC")),
    ReMobilDS (al.number ("ReMobilDS")),
    ReMobilRt (al.number ("ReMobilRt")),
    ExfoliationFac (al.number ("ExfoliationFac")),
    GrowthRateRedFac (al.number ("GrowthRateRedFac")),
    LfDR (al.plf ("LfDR")),
    RtDR (al.plf ("RtDR")),
    Large_RtDR (al.number ("Large_RtDR")),
    IntDSRelRtRes (al.number ("IntDSRelRtRes")),
    EndDSRelRtRes (al.number ("EndDSRelRtRes")),
    RelRateRtRes (al.number ("RelRateRtRes")),
    LfRtRelRtRes (al.number ("LfRtRelRtRes")),
    // State.
    CH2OPool (al.number ("CH2OPool")),
    WLeaf (al.number ("WLeaf")),
    WStem (al.number ("WStem")),
    WRoot (al.number ("WRoot")),
    WSOrg (al.number ("WSOrg")),
    WDead (al.number ("WDead")),
    CCrop (0.0),
    CLeaf (0.0),
    CStem (0.0),
    CRoot (0.0),
    CSOrg (0.0),
    CDead (0.0),
    NCrop (al.check ("NCrop") ? al.number ("NCrop") : -42.42e42),
    NLeaf (al.number ("NLeaf")),
    NStem (al.number ("NStem")),
    NRoot (al.number ("NRoot")),
    NSOrg (al.number ("NSOrg")),
    NDead (al.number ("NDead")),
    C_AM  (al.number ("C_AM")),
    N_AM  (al.number ("N_AM")),
    AM_root (NULL),
    AM_leaf (NULL)
{ }

Production::~Production ()
{ }

static Submodel::Register 
production_submodel ("Production", Production::load_syntax);
