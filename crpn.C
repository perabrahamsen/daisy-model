// crpn.C -- Default crop nitrogen parameters.
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

#include "crpn.h"
#include "production.h"
#include "root_system.h"
#include "syntax.h"
#include "alist.h"
#include "treelog.h"
#include "log.h"
#include "plf.h"
#include "mathlib.h"
#include "submodel.h"
#include "check.h"
void
CrpN::cut (const double DS)
{
  if (DS > DS_start_fixate)
    DS_start_fixate = DS_cut_fixate;
}

void
CrpN::content (const double DS, Production& production)
{
  PtNCnt = PtLeafCnc (DS) * production.WLeaf
    + PtStemCnc (DS) * production.WStem
    + PtSOrgCnc (DS) * production.WSOrg
    + PtRootCnc (DS) * production.WRoot;

  CrNCnt = CrLeafCnc (DS) * production.WLeaf
    + CrStemCnc (DS) * production.WStem
    + CrSOrgCnc (DS) * production.WSOrg
    + CrRootCnc (DS) * production.WRoot;

  NfNCnt = NfLeafCnc (DS) * production.WLeaf
    + NfStemCnc (DS) * production.WStem
    + NfSOrgCnc (DS) * production.WSOrg
    + NfRootCnc (DS) * production.WRoot;

  if (production.NCrop >= CrNCnt)
    {
      const double x = (production.NCrop - CrNCnt)
        / (PtNCnt - CrNCnt);
      production.NLeaf = ((PtLeafCnc (DS) - CrLeafCnc (DS)) * x
        + CrLeafCnc (DS)) * production.WLeaf;
      production.NStem = ((PtStemCnc (DS) - CrStemCnc (DS)) * x
        + CrStemCnc (DS)) * production.WStem;
      production.NSOrg = ((PtSOrgCnc (DS) - CrSOrgCnc (DS)) * x
        + CrSOrgCnc (DS)) * production.WSOrg;
    }
  else
    {
      const double x = (production.NCrop - CrNCnt) / (NfNCnt - CrNCnt);
      production.NLeaf = ((NfLeafCnc (DS) - CrLeafCnc (DS)) * x
        + CrLeafCnc (DS)) * production.WLeaf;
      production.NStem = ((NfStemCnc (DS) - CrStemCnc (DS)) * x
        + CrStemCnc (DS)) * production.WStem;
      production.NSOrg = ((NfSOrgCnc (DS) - CrSOrgCnc (DS)) * x
        + CrSOrgCnc (DS)) * production.WSOrg;
    }
  production.NRoot = production.NCrop 
    - production.NLeaf - production.NStem - production.NSOrg;
  daisy_assert (production.NLeaf >= 0.0);
  daisy_assert (production.NStem >= 0.0);
  daisy_assert (production.NSOrg >= 0.0);
  daisy_assert (production.NRoot >= 0.0);
}

void
CrpN::update (const int Hour, double& NCrop, const double DS,
	      const bool enable_N_stress,
	      const Soil& soil, const SoilWater& soil_water,
	      SoilNH4& soil_NH4, SoilNO3& soil_NO3,
	      RootSystem& root_system)
{
  double PotNUpt = (PtNCnt - NCrop) / ((Hour == 0) ? 1.0 : (25.0 - Hour));

  const double NUpt = root_system.nitrogen_uptake (soil, soil_water, 
						   soil_NH4, soil_NO3,
						   PotNUpt);
  NCrop += NUpt;
  PotNUpt -= NUpt;

  if (PotNUpt > 0 && DS > DS_start_fixate)
    {
      Fixated = fixate_factor * PotNUpt;
      AccFixated += Fixated;
      NCrop += Fixated;
      // PotNUpt -= auxiliary.Fixated;
    }
  else
    Fixated = 0.0;

  // Updating the nitrogen stress
  root_system.nitrogen_stress
    = 1.0 - bound (0.0, ((NCrop - NfNCnt) / (CrNCnt - NfNCnt)), 1.0);
  
  // Ensure we have enough N for all the crop parts.
  if (!enable_N_stress)
    NCrop = max (NCrop, CrNCnt);
}

void
CrpN::output (Log& log) const
{
  log.output ("PtNCnt", PtNCnt);
  log.output ("CrNCnt", CrNCnt);
  log.output ("NfNCnt", NfNCnt);
  log.output ("Fixated", Fixated);
  log.output ("AccFixated", AccFixated);
  log.output ("DS_start_fixate", DS_start_fixate);
}

bool 
CrpN::check_alist (const AttributeList& al, Treelog& err)
{
  static bool warned = false;
  if (al.check ("SeedN") && !warned)
    {
      err.entry ("OBSOLETE: Use 'Prod NCrop' instead of 'CrpN SeedN'");
      warned = true;
    }
  return true;
}

void 
CrpN::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "CrpN");
  alist.add ("description", "\
Default crop nitrogen parameters.");

  // Obsolete.
  syntax.add ("SeedN", "g N/m^2", Syntax::OptionalConst,
	      "N-content in seed.\n\
Obsolete: Use 'Prod NCrop' instead.");

  // Content.
  syntax.add ("PtLeafCnc", "DS", " g N/g DM", Check::non_negative (),
	      Syntax::Const,
	      "Upper limit for N-concentration in leaves.");
  syntax.add ("CrLeafCnc", "DS", " g N/g DM", Check::non_negative (),
	      Syntax::Const,
	      "Critical limit for N-concentration in leaves.");
  syntax.add ("NfLeafCnc", "DS", " g N/g DM", Check::non_negative (),
	      Syntax::Const, "\
Non-functional limit for N-concentration in leaves.");
  syntax.add ("PtStemCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const,
	      "Upper limit for N-concentration in stem.");
  syntax.add ("CrStemCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const,
	      "Critical limit for N-concentration in stem.");
  syntax.add ("NfStemCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const, "\
Non-functional limit for N-concentration in stem.");
  syntax.add ("PtSOrgCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const, "\
Upper limit for N-concentration in storage organ.");
  syntax.add ("CrSOrgCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const, "\
Critical limit for N-concentration in storage organ.");
  syntax.add ("NfSOrgCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const, "\
Non-functional limit for N-concentration in storage organ.");
  syntax.add ("PtRootCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const,
	      "Upper limit for N-concentration in roots.");
  syntax.add ("CrRootCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const,
	      "Critical limit for N-concentration in roots.");
  syntax.add ("NfRootCnc", "DS", " g N/g DM", Check::non_negative (),
              Syntax::Const, "\
Non-functional lim for N-concentration in roots.");
  syntax.add ("TLLeafEff", "DS", Syntax::Fraction (), Check::fraction (),
	      Syntax::Const,
	      "Translocation effiency, Leaf.");
  PLF TLLeafEff;
  TLLeafEff.add (0.00, 0.90);
  TLLeafEff.add (2.00, 0.90);
  alist.add ("TLLeafEff", TLLeafEff);
  syntax.add ("TLRootEff", "DS", Syntax::Fraction (), Check::fraction (),
	      Syntax::Const,
	      "Translocation effiency, Root.");
  PLF TLRootEff;
  TLRootEff.add (0.00, 0.10);
  TLRootEff.add (2.00, 0.10);
  alist.add ("TLRootEff", TLRootEff);
  syntax.add ("PtNCnt", "g/m^2", Syntax::LogOnly,
	      "Potential nitrogen content in crop.");
  syntax.add ("CrNCnt", "g/m^2", Syntax::LogOnly,
	      "Critical nitrogen content in crop.");
  syntax.add ("NfNCnt", "g/m^2", Syntax::LogOnly,
	      "Non-functional nitrogen content in crop.");

  // Fixation.
  syntax.add ("DS_fixate", Syntax::None (), Syntax::Const,
	      "DS at which to start fixation of atmospheric N.");
  alist.add ("DS_fixate", 42000.0);
  syntax.add ("DS_cut_fixate", Syntax::None (), Syntax::Const,
	      "Restore fixation this DS after cut.");
  alist.add ("DS_cut_fixate", 0.0);
  syntax.add ("fixate_factor", Syntax::None (), Syntax::Const,
	      "Fraction of needed N fixated by day.");
  alist.add ("fixate_factor", 0.8);
  syntax.add ("Fixated", "g N/m^2/h", Syntax::LogOnly,
	      "N fixation from air.");
  syntax.add ("AccFixated", "g N/m^2", Syntax::LogOnly, 
	      "Accumuated N fixation from air.");
  alist.add ("AccFixated", 0.0);
  syntax.add ("DS_start_fixate", Syntax::None (), Syntax::OptionalState,
	      "Development stage at which to restart fixation after a cut.");
}

CrpN::CrpN (const AttributeList& al)
  : SeedN (al.check ("SeedN") ? al.number ("SeedN") : -42.42e42),
    PtLeafCnc (al.plf ("PtLeafCnc")),
    CrLeafCnc (al.plf ("CrLeafCnc")),
    NfLeafCnc (al.plf ("NfLeafCnc")),
    PtStemCnc (al.plf ("PtStemCnc")),
    CrStemCnc (al.plf ("CrStemCnc")),
    NfStemCnc (al.plf ("NfStemCnc")),
    PtRootCnc (al.plf ("PtRootCnc")),
    CrRootCnc (al.plf ("CrRootCnc")),
    NfRootCnc (al.plf ("NfRootCnc")),
    PtSOrgCnc (al.plf ("PtSOrgCnc")),
    CrSOrgCnc (al.plf ("CrSOrgCnc")),
    NfSOrgCnc (al.plf ("NfSOrgCnc")),
    TLLeafEff (al.plf ("TLLeafEff")),
    TLRootEff (al.plf ("TLRootEff")),
    PtNCnt (0.0),
    CrNCnt (0.0),
    NfNCnt (0.0),
    DS_fixate (al.number ("DS_fixate")),
    DS_cut_fixate (al.number ("DS_cut_fixate")),
    fixate_factor (al.number ("fixate_factor")),
    Fixated (0.0),
    AccFixated (al.number ("AccFixated")),
    DS_start_fixate (al.check ("DS_start_fixate")
		     ? al.number ("DS_start_fixate")
		     : al.number ("DS_fixate"))
{ }

CrpN::~CrpN ()
{ }

static Submodel::Register 
crpn_submodel ("CrpN", CrpN::load_syntax);
