// vernalization.C -- Default crop vernalization submodel.
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

#include "vernalization.h"
#include "submodel.h"
#include "log.h"
#include "alist.h"
#include "syntax.h"

void
Vernalization::operator () (double Ta, double& DS)
{
  if (!required)
    return;

  if (TaSum < 0)
    {
      TaSum -= min (Ta - TaLim, 0.0);
      if (DS > DSLim)
	DS = DSLim;
    }
}

void
Vernalization::output (Log& log) const
{
  if (!required)
    return;

  log.output ("TaSum", TaSum);
}

const AttributeList& 
Vernalization::no_vernalization ()
{
  static AttributeList noVernal;
  
  if (!noVernal.check ("required"))
    {
      noVernal.add ("required", false);
      noVernal.add ("DSLim", -42.42e42);
      noVernal.add ("TaLim", -42.42e42);
      noVernal.add ("TaSum", -42.42e42);
    }
  return noVernal;
}

void 
Vernalization::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "Vernalization");
  alist.add ("description", "\
Default crop vernalization submodel.");
  syntax.add ("required", Syntax::Boolean, Syntax::OptionalConst,
	      "True, iff the crop requires vernalization.");
  syntax.add ("DSLim", Syntax::None (), Syntax::Const,
	      "Development stage at vernalization.");
  syntax.add ("TaLim", "dg C", Syntax::Const,
	      "Vernalization temperature threshold.");
  syntax.add ("TaSum", "dg C d", Syntax::State,
	      "Vernalization temperature-sum requirement.");
}

Vernalization::Vernalization (const AttributeList& al)
  : required (al.check ("required") ? al.flag ("required") : true),
    DSLim (al.number ("DSLim")),
    TaLim (al.number ("TaLim")),
    TaSum (al.number ("TaSum"))
{ }

Vernalization::~Vernalization ()
{ }

static Submodel::Register 
vernalization_submodel ("Vernalization", Vernalization::load_syntax);
