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

#define BUILD_DLL

#include "canopy_simple.h"
#include "submodel.h"
#include "log.h"
#include "frame.h"

double
CanopySimple::EpFactor (double DS) const
{ return EpFac * EpFacDS (DS); }

void
CanopySimple::output (Log& log) const
{
  output_variable (Height, log);
  output_variable (CAI, log);
  output_variable (LAIvsH, log);
}

void 
CanopySimple::load_syntax (Frame& frame)
{
  frame.alist ().add ("submodel", "CanopySimple");
  frame.alist ().add ("description", "Simple canopy model.");

  // Parameters.
  frame.add ("PARref", Value::None (), Value::Const,
	      "PAR reflectance.");
  frame.add ("PARref", 0.06);
  frame.add ("PARext", Value::None (), Value::Const,
	      "PAR extinction coefficient.");
  frame.add ("PARext", 0.60);
  frame.add ("NIRref", Value::None (), Value::Const,
	      "NIR reflectance. NIRref = 0.51 (Ross, 1975)");
  frame.add ("NIRref", 0.51);
  frame.add ("NIRext", Value::None (), Value::Const,
	      "NIR extinction coefficient. NIRext = 0.18 (Jones, 1983)");
  frame.add ("NIRext", 0.18);
  frame.add ("EPext", Value::None (), Value::Const,
	      "EP extinction coefficient.");
  frame.add ("EPext", 0.5);
  frame.add ("IntcpCap", "mm", Value::Const,
	      "Interception capacity.");
  frame.add ("IntcpCap", 0.5);
  frame.add ("EpFac", Value::None (), Value::Const,
	      "Potential evapotranspiration factor.");
  frame.add ("EpFac", 1.15);
  frame.add ("EpFacDS", "DS", Value::None (), Value::Const,
	      "DS dependent potential evapotranspiration factor.");
  PLF EpFacDS;
  EpFacDS.add (1.0, 1.0);
  frame.add ("EpFacDS", EpFacDS);
  frame.add ("rs_max", "s/m", Value::Const,
	      "Maximum transpiration resistance.");
  frame.add ("rs_max", 1.0e5);
  frame.add ("rs_min", "s/m", Value::Const,
	      "Minimum transpiration resistance.");
  frame.add ("rs_min", 100.0);

  // Variables.
  frame.add ("Height", "cm", Value::State, "Crop height.");
  frame.add ("Height", 0.0);
  PLF leaf_width;
  leaf_width.add (0.0, 3.0);
  leaf_width.add (2.0, 3.0);
  frame.add ("leaf_width", "DS", "cm", Value::Const, "Leaf width.");
  frame.add ("leaf_width", leaf_width);
  frame.add ("CAI", "m^2/m^2", Value::LogOnly, "Crop Area Index.");
  frame.add ("LAIvsH", "cm", "m^2/m^2", Value::LogOnly,
	      "Accumulated Leaf Area Index at Height.");
}

CanopySimple::CanopySimple (const AttributeList& vl)
  : PARref (vl.number ("PARref")),
    PARext (vl.number ("PARext")),
    NIRref (vl.number ("NIRref")),
    NIRext (vl.number ("NIRext")),
    EPext (vl.number ("EPext")),
    IntcpCap (vl.number ("IntcpCap")),
    EpFac (vl.number ("EpFac")),
    EpFacDS (vl.plf ("EpFacDS")),
    rs_max (vl.number ("rs_max")),
    rs_min (vl.number ("rs_min")),
    leaf_width (vl.plf ("leaf_width")),
    Height (vl.number ("Height")),
    CAI    (0.0)
{ }

CanopySimple::~CanopySimple ()
{ }

static Submodel::Register 
canopy_simple_submodel ("CanopySimple", CanopySimple::load_syntax);

// canopy_simple.C ends here.
