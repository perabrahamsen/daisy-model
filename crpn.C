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

#define BUILD_DLL

#include "crpn.h"
#include "production.h"
#include "root_system.h"
#include "block_submodel.h"
#include "treelog.h"
#include "log.h"
#include "plf.h"
#include "mathlib.h"
#include "librarian.h"
#include "check.h"
#include <sstream>

void
CrpN::cut (const double DS)
{
  if (DS > DS_start_fixate)
    DS_start_fixate = DS_cut_fixate;
}

void
CrpN::content (const double DS, Production& production, Treelog& msg)
{
  daisy_assert (production.WRoot >= 0.0);
  daisy_assert (production.WLeaf >= 0.0);
  daisy_assert (production.WStem >= 0.0);
  daisy_assert (production.WSOrg >= 0.0);
  daisy_assert (production.NCrop >= 0.0);

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
      daisy_assert (!isequal (PtNCnt, CrNCnt));
      const double x = (production.NCrop - CrNCnt)
        / (PtNCnt - CrNCnt);
      daisy_assert (x >= 0.0);
      if (x > 1.0)
        { 
          if (state == N_uninitialized)
            state = init_above;
          else if (state != init_above)
            state = above_PT;
        }
      else
        {
          if (state != PT_to_CR && state != above_PT)
            msg.message ("Luxury nitrogen uptake initiated");
          state = PT_to_CR;
        }
        
      production.NRoot = ((PtRootCnc (DS) - CrRootCnc (DS)) * x
        + CrRootCnc (DS)) * production.WRoot;
      production.NLeaf = ((PtLeafCnc (DS) - CrLeafCnc (DS)) * x
        + CrLeafCnc (DS)) * production.WLeaf;
      production.NStem = ((PtStemCnc (DS) - CrStemCnc (DS)) * x
        + CrStemCnc (DS)) * production.WStem;
      production.NSOrg = ((PtSOrgCnc (DS) - CrSOrgCnc (DS)) * x
        + CrSOrgCnc (DS)) * production.WSOrg;
    }
  else if (production.NCrop >= NfNCnt)
    {
      daisy_assert (!isequal (CrNCnt, NfNCnt));
      const double x = (production.NCrop - NfNCnt) / (CrNCnt - NfNCnt);
      daisy_assert (x >= 0.0);
      daisy_assert (x <= 1.0);
      if (state != CR_to_NF)
        msg.message ("Crop affected by nitrogen stress");
      state = CR_to_NF;

      production.NRoot = ((CrRootCnc (DS) - NfRootCnc (DS)) * x
        + NfRootCnc (DS)) * production.WRoot;
      production.NLeaf = ((CrLeafCnc (DS) - NfLeafCnc (DS)) * x
        + NfLeafCnc (DS)) * production.WLeaf;
      production.NStem = ((CrStemCnc (DS) - NfStemCnc (DS)) * x
        + NfStemCnc (DS)) * production.WStem;
      production.NSOrg = ((CrSOrgCnc (DS) - NfSOrgCnc (DS)) * x
        + NfSOrgCnc (DS)) * production.WSOrg;
    }
  else
    {
      if (state != below_NF)
        msg.warning ("Nitrogen content of crop below minimum");
      state = below_NF;
      daisy_assert (NfNCnt > 0.0);
      const double x = production.NCrop / NfNCnt;
      daisy_assert (x >= 0.0);
      daisy_assert (x <= 1.0);
      production.NRoot = NfRootCnc (DS) * x * production.WRoot;
      production.NLeaf = NfLeafCnc (DS) * x * production.WLeaf;
      production.NStem = NfStemCnc (DS) * x * production.WStem;
      production.NSOrg = NfSOrgCnc (DS) * x * production.WSOrg;
    }

  daisy_assert (production.NRoot >= 0.0);
  daisy_assert (production.NLeaf >= 0.0);
  daisy_assert (production.NStem >= 0.0);
  daisy_assert (production.NSOrg >= 0.0);
  daisy_assert (approximate (production.NCrop, 
                             production.NRoot + production.NLeaf 
                             + production.NStem + production.NSOrg));
  
  // Update NNI.
  const double ActNShoot = production.NLeaf 
    + production.NStem + production.NSOrg; 
  const double CrNShoot = CrLeafCnc (DS) * production.WLeaf
    + CrStemCnc (DS) * production.WStem
    + CrSOrgCnc (DS) * production.WSOrg;
  NNI = (CrNShoot > 0.0) 
    ? ActNShoot / CrNShoot
    : -1.0;
}

void
CrpN::clear ()
{
  Fixated = 0.0;
}

void
CrpN::update (double& NCrop, const double DS,
              const Geometry& geo,
	      const Soil& soil, const SoilWater& soil_water,
	      Chemistry& chemistry,
              const double day_fraction,
	      RootSystem& root_system,
              const double dt)
{
  const double one_hour = 1.0;  // [h]
  double PotNUpt = (PtNCnt - NCrop);

  const double NUpt = (NCrop < CrNCnt || DS < 1.0)
    ? root_system.nitrogen_uptake (geo, soil, soil_water, 
                                   chemistry,
                                   NH4_root_min, NO3_root_min,
                                   PotNUpt / one_hour) * dt
    : root_system.nitrogen_uptake (geo, soil, soil_water, 
                                   chemistry,
                                   NH4_root_min_luxury, NO3_root_min_luxury,
                                   PotNUpt / one_hour) * dt;
  NCrop += NUpt;
  PotNUpt -= NUpt;

  if (PotNUpt > 0 && DS > DS_start_fixate)
    {
      Fixated = fixate_factor * PotNUpt;
      AccFixated += Fixated * dt;
      NCrop += Fixated * dt;
      // PotNUpt -= auxiliary.Fixated;
    }
  else
    Fixated = 0.0;

  // Updating the nitrogen stress
  nitrogen_stress 
    = 1.0 - bound (0.0, ((NCrop - NfNCnt) / (CrNCnt - NfNCnt)), 1.0);
  nitrogen_stress_days += nitrogen_stress * day_fraction;
}

void
CrpN::output (Log& log) const
{
  output_variable (PtNCnt, log);
  output_variable (CrNCnt, log);
  output_variable (NfNCnt, log);
  output_variable (nitrogen_stress, log);
  output_variable (nitrogen_stress_days, log);
  output_variable (NNI, log);
  output_variable (Fixated, log);
  output_variable (AccFixated, log);
  output_variable (DS_start_fixate, log);
}

static void check_n_less (const symbol a, const symbol b, const Frame& al,
			  Treelog& err, bool& ok)
{
  if (!(al.plf (a) <= al.plf (b)))
    {
      err.error ("'" + a + "' should never be larger than '" + b + "'");
      ok = false;
    }
}

bool 
CrpN::check_alist (const Metalib&, const Frame& al, Treelog& err)
{
  bool ok = true;
  check_n_less ("CrLeafCnc", "PtLeafCnc", al, err, ok);
  check_n_less ("NfLeafCnc", "CrLeafCnc", al, err, ok);
  check_n_less ("CrStemCnc", "PtStemCnc", al, err, ok);
  check_n_less ("NfStemCnc", "CrStemCnc", al, err, ok);
  check_n_less ("CrSOrgCnc", "PtSOrgCnc", al, err, ok);
  check_n_less ("NfSOrgCnc", "CrSOrgCnc", al, err, ok);
  check_n_less ("CrRootCnc", "PtRootCnc", al, err, ok);
  check_n_less ("NfRootCnc", "CrRootCnc", al, err, ok);
  
  return ok;
}

void 
CrpN::load_syntax (Frame& frame)
{
  frame.add_check (check_alist);

  // Content.
  frame.declare ("PtLeafCnc", "DS", "g N/g DM", Check::non_negative (),
	      Attribute::Const,
	      "Upper limit for N-concentration in leaves.");
  frame.declare ("CrLeafCnc", "DS", "g N/g DM", Check::non_negative (),
	      Attribute::Const,
	      "Critical limit for N-concentration in leaves.");
  frame.declare ("NfLeafCnc", "DS", "g N/g DM", Check::non_negative (),
	      Attribute::Const, "\
Non-functional limit for N-concentration in leaves.");
  frame.declare ("PtStemCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const,
	      "Upper limit for N-concentration in stem.");
  frame.declare ("CrStemCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const,
	      "Critical limit for N-concentration in stem.");
  frame.declare ("NfStemCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const, "\
Non-functional limit for N-concentration in stem.");
  frame.declare ("PtSOrgCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const, "\
Upper limit for N-concentration in storage organ.");
  frame.declare ("CrSOrgCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const, "\
Critical limit for N-concentration in storage organ.");
  frame.declare ("NfSOrgCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const, "\
Non-functional limit for N-concentration in storage organ.");
  frame.declare ("PtRootCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const,
	      "Upper limit for N-concentration in roots.");
  frame.declare ("CrRootCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const,
	      "Critical limit for N-concentration in roots.");
  frame.declare ("NfRootCnc", "DS", "g N/g DM", Check::non_negative (),
              Attribute::Const, "\
Non-functional lim for N-concentration in roots.");
  frame.declare ("TLLeafEff", "DS", Attribute::Fraction (), Check::fraction (),
	      Attribute::Const,
	      "Translocation effiency, Leaf.");
  PLF TLLeafEff;
  TLLeafEff.add (0.00, 0.90);
  TLLeafEff.add (2.00, 0.90);
  frame.set ("TLLeafEff", TLLeafEff);
  frame.declare ("TLRootEff", "DS", Attribute::Fraction (), Check::fraction (),
	      Attribute::Const,
	      "Translocation effiency, Root.");
  PLF TLRootEff;
  TLRootEff.add (0.00, 0.10);
  TLRootEff.add (2.00, 0.10);
  frame.set ("TLRootEff", TLRootEff);
  frame.declare ("PtNCnt", "g/m^2", Attribute::LogOnly,
	      "Potential nitrogen content in crop.");
  frame.declare ("CrNCnt", "g/m^2", Attribute::LogOnly,
	      "Critical nitrogen content in crop.");
  frame.declare ("NfNCnt", "g/m^2", Attribute::LogOnly,
	      "Non-functional nitrogen content in crop.");

  // Root uptake.
  frame.declare ("NO3_root_min", "g N/cm^3", Check::non_negative (), 
	      Attribute::Const, "\
Minimum nitrate concentration near roots for uptake.");
  frame.set ("NO3_root_min", 0.0);
  frame.declare ("NH4_root_min", "g N/cm^3", Check::non_negative (),
	      Attribute::Const, "\
Minimum ammonium concentration near roots for uptake.");
  frame.set ("NH4_root_min", 0.0);

  frame.declare ("NO3_root_min_luxury", "g N/cm^3", Check::non_negative (), 
	      Attribute::OptionalConst, "\
Minimum nitrate concentration near roots for luxury uptake.\n\
Unly used in reproductive phase. By default identical to 'NO3_root_min'.");
  frame.declare ("NH4_root_min_luxury", "g N/cm^3", Check::non_negative (),
	      Attribute::OptionalConst, "\
Minimum ammonium concentration near roots for luxury_uptake.\n\
Unly used in reproductive phase. By default identical to 'NH4_root_min'.");

  // Stress.
  frame.declare ("nitrogen_stress", Attribute::None (), Check::fraction (),
	      Attribute::LogOnly,
	       "Nitrogen stress factor.");
  frame.declare ("nitrogen_stress_days", "d", Check::non_negative (),
	      Attribute::State,
	       "Number of days production has halted due to nitrogen stress.\n\
This is the sum of nitrogen stress for each hour, multiplied with the\n\
action of the radition of that day that was received that hour.");
  frame.set ("nitrogen_stress_days", 0.0);
  frame.declare ("NNI", Attribute::None (), Check::non_negative (),
		 Attribute::LogOnly,
		 "Nitrogen nutrition index.\n\
Actual nitrogen content of shoot divided by critical nitrogen content.\n\
Will be less than one for a stressed plant.\n\
For now, the storage organ is counted as part of the shoot.");


  // Fixation.
  frame.declare ("DS_fixate", Attribute::None (), Attribute::Const,
	      "DS at which to start fixation of atmospheric N.");
  frame.set ("DS_fixate", 42000.0);
  frame.declare ("DS_cut_fixate", Attribute::None (), Attribute::Const,
	      "Restore fixation this DS after cut.");
  frame.set ("DS_cut_fixate", 0.0);
  frame.declare ("fixate_factor", "h^-1", Attribute::Const,
		 "Fraction rate relative to potential N uptake.");
  frame.set ("fixate_factor", 0.8);
  frame.declare ("Fixated", "g N/m^2/h", Attribute::LogOnly,
	      "N fixation from air.");
  frame.declare ("AccFixated", "g N/m^2", Attribute::LogOnly, 
	      "Accumuated N fixation from air.");
  frame.set ("AccFixated", 0.0);
  frame.declare ("DS_start_fixate", Attribute::None (), Attribute::OptionalState,
	      "Development stage at which to restart fixation after a cut.");
}

CrpN::CrpN (const BlockSubmodel& al)
  : PtLeafCnc (al.plf ("PtLeafCnc")),
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
    NO3_root_min (al.number ("NO3_root_min")),
    NH4_root_min (al.number ("NH4_root_min")),
    NO3_root_min_luxury (al.number ("NO3_root_min_luxury", NO3_root_min)),
    NH4_root_min_luxury (al.number ("NH4_root_min_luxury", NH4_root_min)),
    nitrogen_stress (0.0),
    nitrogen_stress_days (al.number ("nitrogen_stress_days")),
    NNI (-1.0),
    state (N_uninitialized),
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

static DeclareSubmodel 
crpn_submodel (CrpN::load_syntax, "CrpN", "\
Default crop nitrogen parameters.");

// crpn.C ends here.
