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

#include "partition.h"
#include "plf.h"
#include "submodel.h"
#include "syntax.h"
#include "alist.h"

void
Partition::operator () (double DS, double current_RSR,
			double& f_Leaf, double& f_Stem,
			double& f_Root, double& f_SOrg)
{
  if (current_RSR > RSR (DS))
    f_Root = 0.0;
  else
    f_Root = Root (DS);
  f_Leaf = (1 - f_Root) * Leaf (DS);
  f_Stem = (1 - f_Root) * Stem (DS);
  f_SOrg = max (0.0, 1 - f_Root - f_Leaf - f_Stem);
  if (f_SOrg < 1e-5)
    {
      f_Root += f_SOrg;
      f_SOrg = 0.0;
    }
}

void 
Partition::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Partition");
  alist.add ("description", "\
Assimilate partitioning in the default crop model.");
  syntax.add ("Root", "DS", Syntax::None (), Syntax::Const,
	      "Partitioning functions for root.");
  syntax.add ("Leaf", "DS", Syntax::None (), Syntax::Const,
	      "Partitioning functions for leaves.");
  syntax.add ("Stem", "DS", Syntax::None (), Syntax::Const,
	      "Partitioning functions for stem.");
  syntax.add ("RSR", "DS", Syntax::None (), Syntax::Const,
	      "Root/Shoot ratio as a function of development state.");
}

Partition::Partition (const AttributeList& al)
  : Root (al.plf ("Root")),
    Leaf (al.plf ("Leaf")),
    Stem (al.plf ("Stem")),
    RSR (al.plf ("RSR"))
{ }

Partition::~Partition ()
{ }

static Submodel::Register 
partition_submodel ("Partition", Partition::load_syntax);
