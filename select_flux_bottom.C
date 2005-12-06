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


#include "select_value.h"
#include "soil.h"
#include "border.h"
#include "mathlib.h"
#include <sstream>
#include "treelog.h"

using namespace std;

struct SelectFluxBottom : public SelectValue
{
  // Content.
  double height;
  const Soil* last;
  int index;

  // Output routines.
  void output_array (const vector<double>& array, 
		     const Soil* soil, Treelog& msg)
  { 
    if (soil != last)
      {
        last = soil;
        if (height > 0.0) 
          index = soil->size ();
        else
          {
            index = soil->interval_border (height);
            if ((index == 0 && height < -1e-8)
                || !approximate (height, soil->zplus (index-1)))
              {
                std::ostringstream tmp;
                tmp << "Log column " << name 
                       << ": No interval near to = " << height 
                       << " [cm]; closest match is " 
                       << ((index == 0) ? 0 : soil->zplus (index-1))
                       << " [cm]";
                msg.warning (tmp.str ());
              }
          }
        daisy_assert (array.size () > index);
      }
    add_result (array[index]);
  }

  // Create and Destroy.
  void initialize (const map<symbol, symbol>& conv, 
		   double default_from, double default_to, 
		   const string& timestep )
  {
    Select::initialize (conv, default_from, default_to, timestep);

    // Overwrite default height.
    if (default_to <= 0.0 && height > 0.0)
      height = default_to;
  }
  bool check_border (const Border& border, 
                     const double, const double default_to,
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
    : SelectValue (al),
      height (al.number ("to", 1.0)),
      last (NULL),
      index (-1)
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
               "Extract flux at bottom of specified interval.\n\
By default, log the first member of the sequence.");

    syntax.add ("to", "cm", Syntax::OptionalConst,
		"Specify height (negative) to measure interval.\n\
By default, measure to the bottom.");

    Librarian<Select>::add_type ("flux_bottom", alist, syntax, &make);
  }
} Select_syntax;
