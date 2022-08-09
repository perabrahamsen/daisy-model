// partition.C -- Assimilate partioning for the default crop model.
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

#include "partition.h"
#include "plf.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "check.h"
#include "mathlib.h"
#include "treelog.h"
#include "log.h"
#include <sstream>

void
Partition::tick (double DS, double current_RSR, double current_WRoot,
		 double nitrogen_stress,
		 double NNI,
		 double& f_Leaf, double& f_Stem,
		 double& f_Root, double& f_SOrg)
{
  if (nitrogen_stress > nitrogen_stress_limit && DS > 1.0)
    {
      f_SOrg = 1.0;
      f_Leaf = f_Stem = f_Root = 0.0;
      return;
    }

  if (current_RSR > RSR (DS)
      || current_WRoot > max_WRoot (DS))
    f_Root = 0.0;
  else
    f_Root = Root (DS);
  f_Leaf = (1 - f_Root) * Leaf (DS);
  f_Stem = (1 - f_Root) * Stem (DS);
  f_SOrg = std::max (0.0, 1 - f_Root - f_Leaf - f_Stem);

  // NNI 
  if (NNI < NNI_crit)
    {
      const double f_Shoot = f_Leaf + f_Stem;
      cf = 1.0 + (NNI_crit - NNI) * NNI_inc;
      f_Stem *= cf;
      if (f_Stem < f_Shoot)
	f_Leaf = f_Shoot - f_Stem;
      else
	{
	  f_Stem = f_Shoot;
	  f_Leaf = 0.0;
	}
      daisy_approximate (f_Shoot, f_Leaf + f_Stem);
    }
  else
    cf = 1.0;
  
  // Numeric weirdness.
  daisy_assert (f_SOrg > -1e-5);
  if (f_SOrg < 1e-5)
    {
      f_Root += f_SOrg;
      f_SOrg = 0.0;
    }
  daisy_approximate (f_Root + f_Leaf + f_Stem + f_SOrg, 1.0);
  daisy_assert (0.0 <= f_Leaf && f_Leaf <= 1.0);
  daisy_assert (0.0 <= f_Stem && f_Stem <= 1.0);
  daisy_assert (0.0 <= f_Root && f_Root <= 1.0);
  daisy_assert (0.0 <= f_SOrg && f_SOrg <= 1.0);
}

static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
{
  bool ok = true;

  const PLF& Leaf = al.plf ("Leaf");
  const PLF& Stem = al.plf ("Stem");

  PLF Shoot = Stem;
  Shoot += Leaf;

  if (Shoot.max () > 1.0001)
    {
      std::ostringstream tmp;
      tmp << "Leaf and Stem fractions must be <= 1.0 combined.\n"
	     << "They are " << Shoot.max () << " at " << Shoot.max_at ();
      err.error (tmp.str ());
      ok = false;
    }
  return ok;
}

void
Partition::tick_none ()
{ cf = 1.0; }

void 
Partition::output (Log& log) const
{ output_variable (cf, log); }

void 
Partition::load_syntax (Frame& frame)
{
  frame.add_check (check_alist);
  frame.declare ("Root", "DS", Attribute::Fraction (), Check::fraction (),
	      Attribute::Const, "\
Fraction of assimilate for growth that goes to the roots, as a function of\n\
the crop development stage.  The remaining growth assimilate goes to the\n\
shoot.");
  frame.declare ("Leaf", "DS", Attribute::Fraction (), Check::fraction (),
	      Attribute::Const,
	      "Fraction of shoot assimilate that goes to the leafs.");
  frame.declare ("Stem", "DS", Attribute::Fraction (), Check::fraction (),
	      Attribute::Const,
	      "Fraction of shoot assimilate that goes to the stem.");
  frame.declare ("RSR", "DS", Attribute::None (), Check::positive (), Attribute::Const,
	      "Maximal root/shoot ratio as a function of development state.\n\
If the root/shoot ratio is above this, the roots will start dying.");
  frame.declare ("max_WRoot", "DS", "g DM/m^2", Check::non_negative (),
		 Attribute::Const,
		 "Maximal root DM as a function of development state.\n\
If the root DM is above this, no assimilate will be allocated to them.");
  PLF max_WRoot;
  max_WRoot.add (0.0, 1000000.0); // 1 [Mg/m^2]
  frame.set ("max_WRoot", max_WRoot);
  frame.declare ("nitrogen_stress_limit", Attribute::None (), Check::fraction (), 
              Attribute::Const,
	      "If nitrogen stress is above this number and DS is above 1,\n\
allocate all assimilate to the storage organ.");
  frame.set ("nitrogen_stress_limit", 1.0);
  frame.declare_number_cited ("NNI_crit", 
			      Attribute::None (), Check::non_negative (),
			      Attribute::Const, Attribute::Singleton, "\
When NNI is below this value, modify Stem/Leaf partitioning.",
		 "gyldengren2020effects");
  frame.set ("NNI_crit", 0.0);
  frame.declare_number_cited ("NNI_inc", Attribute::None (), Check::none (),
			      Attribute::Const, Attribute::Singleton, "\
Stem/Leaf partitioning modifier for low NNI.", "gyldengren2020effects");
  frame.set ("NNI_inc", 0.0);
  frame.declare_number_cited ("cf", Attribute::None (), Check::non_negative (),
			      Attribute::LogOnly, Attribute::Singleton, "\
Stem/Leaf partitioning modifier for low NNI when DS < 1.0.\n\
cf = 1 + (NNI_crit - NNI)* NNI_inc.\n\
Stem assimilate partitioning is increased with cf, which is taken from the\n\
amount allocated to leafs.", "gyldengren2020effects");
}

Partition::Partition (const FrameSubmodel& al)
  : Root (al.plf ("Root")),
    Leaf (al.plf ("Leaf")),
    Stem (al.plf ("Stem")),
    RSR (al.plf ("RSR")),
    max_WRoot (al.plf ("max_WRoot")),
    nitrogen_stress_limit (al.number ("nitrogen_stress_limit")),
    NNI_crit (al.number ("NNI_crit")),
    NNI_inc (al.number ("NNI_inc")),
    cf (1.0)
{ }

Partition::~Partition ()
{ }

static DeclareSubmodel 
partition_submodel (Partition::load_syntax, "Partition", "\
Assimilate partitioning in the default crop model.\n\
The 'Root' parameter determine what fraction of the assimilate for growth\n\
goes to roots at a given development stage.  The remaining assimilate goes\n\
to the shoot.  The 'Leaf' and 'Stem' parameters determine what fraction of\n\
the shoot assimilate goes to the leaf and stem respectively.  The remaining\n\
shoot assimilate will go to the storage organ.");

// partition.C ends here.
