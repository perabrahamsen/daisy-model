// select_flux_top.C --- Select a state variable.
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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


#include "select_flux.h"
#include "border.h"
#include "mathlib.h"
#include <map>

struct SelectFluxTop : public SelectFlux
{
  // Create and Destroy.
  void initialize (const std::map<symbol, symbol>& conv, 
		   double default_from, double default_to,
		   const std::string& timestep)
  {
    Select::initialize (conv, default_from, default_to, timestep);

    // Overwrite default height.
    if (height > 0.0)
      if (default_from <= 0.0 && height > 0.0)
        height = default_from;
      else
        height = 0.0;
  }
  bool check_border (const Border& border, 
                     const double default_from, const double,
                     Treelog& msg) const
  { 
    bool ok = true;
    if (height < 0.0 
        && !approximate (height, default_from)
        && !border.check_border (height, msg))
      ok = false;
    return ok; 
  }
  SelectFluxTop (Block& al)
    : SelectFlux (al, al.number ("from", 1.0))
  { }
};

static struct SelectFluxTopSyntax
{
  static Select& make (Block& al)
  { return *new SelectFluxTop (al); }

  SelectFluxTopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SelectValue::load_syntax (syntax, alist);

    alist.add ("description", "Extract flux at top of specified interval.");

    syntax.add ("from", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure from.\n\
By default, measure from the top.");

    Librarian<Select>::add_type ("flux_top", alist, syntax, &make);
  } 
} Select_syntax;
