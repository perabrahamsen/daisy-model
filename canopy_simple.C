// canopy_simple.C -- Canopy development for simple crop model.
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


#include "canopy_simple.h"
#include "submodel.h"
#include "log.h"

double
CanopySimple::EpFactor (double DS) const
{ return EpFac * EpFacDS (DS); }

void
CanopySimple::output (Log& log) const
{
  log.output ("Height", Height);
  log.output ("CAI", CAI);
  log.output ("LAIvsH", LAIvsH);
}

void 
CanopySimple::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "CanopySimple");
  static const PLF empty_plf;

  // Parameters.
  syntax.add ("PARref", Syntax::None (), Syntax::Const,
	      "PAR reflectance.");
  alist.add ("PARref", 0.06);
  syntax.add ("PARext", Syntax::None (), Syntax::Const,
	      "PAR extinction coefficient.");
  alist.add ("PARext", 0.60);
  syntax.add ("EPext", Syntax::None (), Syntax::Const,
	      "EP extinction coefficient.");
  alist.add ("EPext", 0.5);
  syntax.add ("IntcpCap", "mm", Syntax::Const,
	      "Interception capacity.");
  alist.add ("IntcpCap", 0.5);
  syntax.add ("EpFac", Syntax::None (), Syntax::Const,
	      "Potential evapotranspiration factor.");
  alist.add ("EpFac", 1.0);
  syntax.add ("EpFacDS", "DS", Syntax::None (), Syntax::Const,
	      "DS dependent potential evapotranspiration factor.");
  PLF EpFacDS;
  EpFacDS.add (1.0, 1.0);
  alist.add ("EpFacDS", EpFacDS);
  syntax.add ("rs_max", "s/m", Syntax::Const,
	      "Maximum transpiration resistance.");
  alist.add ("rs_max", 1.0e5);
  syntax.add ("rs_min", "s/m", Syntax::Const,
	      "Minimum transpiration resistance.");
  alist.add ("rs_min", 30.0);

  // Variables.
  syntax.add ("Height", "cm", Syntax::State, "Crop height.");
  alist.add ("Height", 0.0);
  syntax.add ("CAI", "m^2/m^2", Syntax::State, "Crop Area Index.");
  alist.add ("CAI", 0.0);
  syntax.add ("LAIvsH", "cm", "m^2/m^2", Syntax::State,
	      "Accumulated Leaf Area Index at Height.");
  alist.add ("LAIvsH", empty_plf);
}

CanopySimple::CanopySimple (const AttributeList& vl)
  : PARref (vl.number ("PARref")),
    PARext (vl.number ("PARext")),
    EPext (vl.number ("EPext")),
    IntcpCap (vl.number ("IntcpCap")),
    EpFac (vl.number ("EpFac")),
    EpFacDS (vl.plf ("EpFacDS")),
    rs_max (vl.number ("rs_max")),
    rs_min (vl.number ("rs_min")),
    Height (vl.number ("Height")),
    CAI    (vl.number ("CAI")),
    LAIvsH (vl.plf ("LAIvsH"))
{ }

CanopySimple::~CanopySimple ()
{ }

static Submodel::Register 
soil_submodel ("CanopySimple", CanopySimple::load_syntax);
