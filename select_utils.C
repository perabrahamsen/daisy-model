// select_itils.C --- Utilities for log selections.
// 
// copyright 2006 Per Abrahamsen and KVL.
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

#include "select_utils.h"
#include "border.h"
#include "block.h"
#include "check.h"
#include "mathlib.h"

void 
SelectUtil::Interval::initialize (double default_from, double default_to)
{
  if (default_from <= 0.0 && from > 0.0)
    from = default_from;
  if (default_to <= 0.0 && to > 0.0)
    to = default_to;

  if (from > 0.0)
    from = 0.0;
}

#if 0  
bool
SelectUtil::Interval::check_border (const Border& border, 
                                  const double default_from, 
                                  const double default_to,
                                  Treelog& msg) const
{ 
  bool ok = true;
  if (from < 0.0 
      && !approximate (from, default_from)
      && !border.check_border (from, msg))
    ok = false;
  if (to < 0.0 
      && !approximate (to, default_to)
      && !border.check_border (to, msg))
    ok = false;
  return ok; 
}
#endif

void 
SelectUtil::Interval::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("from", "cm", Check::non_positive (), Syntax::OptionalConst,
              "Specify top of interval (a negative number).\n\
By default, measure from the top of the soil profile.");
  syntax.add ("to", "cm", Check::negative (), Syntax::OptionalConst,
              "Specify bottom of interval (a negative number).\n\
By default, measure to the bottom of the soil profile.");
}

SelectUtil::Interval::Interval (Block& al)
  : from (al.number ("from", 1.0)),
    to (al.number ("to", 1.0))
{ }

SelectUtil::Interval::~Interval ()
{ }

// select_utils.C ends here.
