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

#define BUILD_DLL

#include "vernalization.h"
#include "librarian.h"
#include "log.h"
#include "frame_submodel.h"

void
Vernalization::operator () (double Ta, double& DS)
{
  if (!required)
    return;

  if (TaSum < 0)
    {
      TaSum -= std::min (Ta - TaLim, 0.0);
      if (DS > DSLim)
	DS = DSLim;
    }
}

void
Vernalization::output (Log& log) const
{
  if (!required)
    return;

  output_variable (TaSum, log);
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
Vernalization::load_syntax (Frame& frame)
{
  frame.add ("required", Value::Boolean, Value::OptionalConst,
	      "True, iff the crop requires vernalization.");
  frame.add ("DSLim", Value::None (), Value::Const,
	      "Development stage at vernalization.");
  frame.add ("TaLim", "dg C", Value::Const,
	      "Vernalization temperature threshold.");
  frame.add ("TaSum", "dg C d", Value::State,
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

static DeclareSubmodel 
vernalization_submodel (Vernalization::load_syntax, "Vernalization", "\
Default crop vernalization submodel.");

// vernalization.C ends here.
