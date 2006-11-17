// select_flux_bottom.C --- Select a state variable.
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

struct SelectFluxBottom : public SelectFlux
{
  // Create and Destroy.
  bool initialize (double default_from, double default_to, 
		   const std::string& timestep, Treelog& msg)
  {
    bool ok = true;
    if (!Select::initialize (default_from, default_to, timestep, msg))
      ok = false;

    // Overwrite default height.
    if (default_to <= 0.0 && height > 0.0)
      height = default_to;

    return ok;
  }
  bool check_border (const Border& border, 
                     const Volume& default_volume,
                     Treelog& msg) const
  { 
    bool ok = true;
    if (height < 0.0 
        && !approximate (height, default_to)
        && !border.check_border (height, msg))
      ok = false;
    return ok; 
  }
  SelectFluxBottom (Block& al)
    : SelectFlux (al, al.number ("to", 1.0))
  { }
};

static struct SelectFluxBottomSyntax
{
  static Select& make (Block& al)
  { return *new SelectFluxBottom (al); }

  SelectFluxBottomSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SelectValue::load_syntax (syntax, alist);

    alist.add ("description", 
               "Extract flux at bottom of specified interval.");

    syntax.add ("to", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.");

    Librarian<Select>::add_type ("flux_bottom", alist, syntax, &make);
  }
} Select_syntax;
