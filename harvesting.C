// harvesting.C -- Harvest parameters for the default crop model.
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

#include "harvesting.h"
#include "am.h"
#include "om.h"
#include "submodel.h"

void 
Harvesting::output (Log&) const
{ }

void 
Harvesting::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_submodule ("Stem", alist, Syntax::Const, Syntax::Sequence, 
			"Stem AOM parameters.", OM::load_syntax);
  alist.add ("Stem", AM::default_AOM ());
  syntax.add_submodule ("Leaf", alist, Syntax::Const, Syntax::Sequence,
			"Leaf AM parameters.", OM::load_syntax);
  alist.add ("Leaf", AM::default_AOM ());
  syntax.add_submodule ("Dead", alist, Syntax::Const, Syntax::Sequence,
			"Dead leaves AM parameters.", OM::load_syntax);
  alist.add ("Dead", AM::default_AOM ());
  syntax.add_submodule ("SOrg", alist, Syntax::Const, Syntax::Sequence,
			"Storage organ AM parameters.", OM::load_syntax);
  alist.add ("SOrg", AM::default_AOM ());
  syntax.add_submodule ("Root", alist, Syntax::Const, Syntax::Sequence,
			"Root AM parameters.", OM::load_syntax);
  alist.add ("Root", AM::default_AOM ());
  syntax.add ("EconomicYield_W", Syntax::None (), Syntax::Const, "\
Valuable fraction of storage organ (DM), e.g. grain or tuber.");
  alist.add ("EconomicYield_W", 1.00);
  syntax.add ("EconomicYield_N", Syntax::None (), Syntax::OptionalConst,
               "Valuable fraction of storage organ (N).\n\
By default the value for DM is used.");
  syntax.add ("DSmax", Syntax::None (), Syntax::Const, "\
Maximal development stage for which the crop survives harvest.");
  alist.add ("DSmax", 0.80);
  syntax.add ("DSnew", Syntax::None (), Syntax::Const,
	       "New development stage after harvest.");
  alist.add ("DSnew", 0.20);
}

Harvesting::Harvesting (const AttributeList& al)
  : Stem (al.alist_sequence ("Stem")),
    Leaf (al.alist_sequence ("Leaf")),
    Dead (al.alist_sequence ("Dead")),
    SOrg (al.alist_sequence ("SOrg")),
    Root (al.alist_sequence ("Root")),
    EconomicYield_W (al.number ("EconomicYield_W")),
    EconomicYield_N (al.check ("EconomicYield_N")
                     ? al.number ("EconomicYield_N")
                     : al.number ("EconomicYield_W")),
    DSmax (al.number ("DSmax")),
    DSnew (al.number ("DSnew"))
{ }

Harvesting::~Harvesting ()
{ }

static Submodel::Register 
soil_submodel ("Harvesting", Harvesting::load_syntax);
