// crpaux.C -- Auxiliary crop state.
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

#include "crpaux.h"
#include "log.h"
#include "submodel.h"

void
CrpAux::no_production ()
{
  IncWLeaf = 0.0;
  IncWStem = 0.0;
  IncWSOrg = 0.0;
  IncWRoot = 0.0;
  NetPhotosynthesis = 0.0;
  AccNetPhotosynthesis = 0.0;
  Respiration = 0.0;
  MaintRespiration = 0.0;
  GrowthRespiration = 0.0;
  RootRespiration = 0.0;
  DeadWLeaf = 0.0;
  DeadNLeaf = 0.0;
  DeadWRoot = 0.0;
  DeadNRoot = 0.0;
  C_Loss = 0.0;
}

void
CrpAux::output (Log& log) const
{
  log.output ("StemRes", StemRes);
  log.output ("PotTransp", PotTransp);
  log.output ("PotCanopyAss", PotCanopyAss);
  log.output ("CanopyAss", CanopyAss);
  log.output ("NetPhotosynthesis", NetPhotosynthesis);
  log.output ("AccNetPhotosynthesis", AccNetPhotosynthesis);
  log.output ("Respiration", Respiration);
  log.output ("MaintRespiration", MaintRespiration);
  log.output ("GrowthRespiration", GrowthRespiration);
  log.output ("RootRespiration", RootRespiration);
  log.output ("IncWLeaf", IncWLeaf);
  log.output ("IncWStem", IncWStem);
  log.output ("IncWSOrg", IncWSOrg);
  log.output ("IncWRoot", IncWRoot);
  log.output ("DeadWLeaf", DeadWLeaf);
  log.output ("DeadNLeaf", DeadNLeaf);
  log.output ("DeadWRoot", DeadWRoot);
  log.output ("DeadNRoot", DeadNRoot);
  log.output ("C_Loss", C_Loss);
}

void 
CrpAux::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "CrpAux");
  alist.add ("description", "\
Auxiliary data for the default crop model.");

  syntax.add ("StemRes", "g DM/m^2", Syntax::State,
	      "Shielded reserves in stems.");
  alist.add ("StemRes", 0.0);
  syntax.add ("PotTransp", "mm/h", Syntax::State,
	      "Potential transpiration.");
  alist.add ("PotTransp", 0.0);
  syntax.add ("PotCanopyAss", "g CH2O/m^2", Syntax::State,
	      "Potential canopy assimilation this day until now.");
  alist.add ("PotCanopyAss", 0.0);
  syntax.add ("CanopyAss", "g CH2O/m^2", Syntax::State,
	      "Canopy assimilation this day until now.");
  alist.add ("CanopyAss", 0.0);
  syntax.add ("NetPhotosynthesis", "g CO2/m^2/h", Syntax::LogOnly,
	      "Net Photosynthesis.");
  syntax.add ("AccNetPhotosynthesis", "g CO2/m^2", Syntax::LogOnly,
	      "Accumulated Net Photosynthesis.");
  syntax.add ("Respiration", "g CH2O/m^2/h", Syntax::LogOnly,
	      "Crop Respiration.");
  syntax.add ("MaintRespiration", "g CH2O/m^2/h", Syntax::LogOnly,
	      "Maintenance Respiration.");
  syntax.add ("GrowthRespiration", "g CH2O/m^2/h", Syntax::LogOnly,
	      "Growth Respiration.");
  syntax.add ("RootRespiration", "g CH2O/m^2/h", Syntax::LogOnly,
	      "Root Respiration.");
  syntax.add ("IncWLeaf", "g DM/m^2/d", Syntax::LogOnly,
	      "Leaf growth.");
  syntax.add ("IncWStem", "g DM/m^2/d", Syntax::LogOnly,
	      "Stem growth.");
  syntax.add ("IncWSOrg", "g DM/m^2/d", Syntax::LogOnly,
	      "Storage organ growth.");
  syntax.add ("IncWRoot", "g DM/m^2/d", Syntax::LogOnly,
	      "Root growth.");
  syntax.add ("DeadWLeaf", "g DM/m^2/d", Syntax::LogOnly,
	      "Leaf DM removed.");
  syntax.add ("DeadNLeaf", "g N/m2/d", Syntax::LogOnly,
	      "Leaf N removed.");
  syntax.add ("DeadWRoot", "g DM/m^2/d", Syntax::LogOnly,
	      "Root DM removed.");
  syntax.add ("DeadNRoot", "g N/m2/d", Syntax::LogOnly,
	      "Root N removed.");
  syntax.add ("C_Loss", "g C/m^2", Syntax::LogOnly,"C lost from the crop");

}

CrpAux::CrpAux (const AttributeList& al)
  : StemRes (al.number ("StemRes")),
    PotTransp (al.number ("PotTransp")),
    PotCanopyAss (al.number ("PotCanopyAss")),
    CanopyAss (al.number ("CanopyAss")),
    NetPhotosynthesis (0.0),
    AccNetPhotosynthesis (0.0),
    Respiration (0.0),
    MaintRespiration (0.0),
    GrowthRespiration (0.0),
    RootRespiration (0.0),
    IncWLeaf (0.0),
    IncWStem (0.0),
    IncWSOrg (0.0),
    IncWRoot (0.0),
    DeadWLeaf (0.0),
    DeadNLeaf (0.0),
    DeadWRoot (0.0),
    DeadNRoot (0.0),
    C_Loss (0.0)
{ }

CrpAux::~CrpAux ()
{ }

static Submodel::Register 
crpaux_submodel ("CrpAux", CrpAux::load_syntax);
