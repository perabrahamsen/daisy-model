// photosynthesis.C -- Default leaf photosynthesis parameters.
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

#include "photosynthesis.h"
#include "plf.h"
#include "submodel.h"
#include "alist.h"
#include "syntax.h"

void 
Photosynthesis::load_syntax (Syntax& syntax, AttributeList& alist)
{
  PLF DS_null_eff;
  DS_null_eff.add (0.0, 1.00);
  DS_null_eff.add (2.0, 1.00);

  alist.add ("submodel", "Photosynthesis");
  alist.add ("description", "\
Default leaf photosynthesis parameters.");

  syntax.add ("Qeff", "(g CO2/m^2/h)/(W/m^2)", Syntax::Const,
	      "Quantum efficiency at low light.");
  syntax.add ("Fm", "g CO2/m^2/h", Syntax::Const,
	      "Maximum assimilation rate.");
  syntax.add ("TempEff", "dg C", Syntax::None (), Syntax::Const,
	      "Temperature effect, photosynthesis.");
  syntax.add ("DSEff", "-", Syntax::None (), Syntax::Const,
	      "Development Stage effect, photosynthesis.");
  alist.add ("DSEff",DS_null_eff);
}

Photosynthesis::Photosynthesis (const AttributeList& al)
  : Qeff (al.number ("Qeff")),
    Fm (al.number ("Fm")),
    TempEff (al.plf ("TempEff")),
    DSEff (al.plf ("DSEff"))
{ }

Photosynthesis::~Photosynthesis ()
{ }

static Submodel::Register 
photosynthesis_submodel ("Photosynthesis", Photosynthesis::load_syntax);
