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


#include "select_value.h"
#include "soil.h"
#include "mathlib.h"
#include <sstream>
#include "treelog.h"

using namespace std;

struct SelectFluxTop : public SelectValue
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
        index = soil->interval_border (height);

        if ((index == 0)
            ? height < -1e-8
            : !approximate (height, soil->zplus (index-1)))
          {
            std::ostringstream tmp;
            tmp << "Log column " << name 
                   << ": No interval near from = " << height 
                   << " [cm]; closest match is " 
                   << ((index == 0) ? 0 : soil->zplus (index-1))
                   << " [cm]";
            msg.warning (tmp.str ());
          }
        daisy_assert (array.size () > index);
      }

    add_result (array[index]);
  }

  // Create and Destroy.
  void initialize (const map<symbol, symbol>& conv, 
		   double default_from, double default_to,
		   const string& timestep)
  {
    Select::initialize (conv, default_from, default_to, timestep);

    // Overwrite default height.
    if (default_from < 0.0)
      height = default_from;
  }
  SelectFluxTop (const Block& al)
    : SelectValue (al),
      height (0.0),
      last (NULL),
      index (-1)
  { }
};

static struct SelectFluxTopSyntax
{
  static Select& make (const Block& al)
  { return *new SelectFluxTop (al); }

  SelectFluxTopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SelectValue::load_syntax (syntax, alist);

    alist.add ("description", "Extract flux at top of specified interval.\n\
By default, log the first member of the sequence.");

    Librarian<Select>::add_type ("flux_top", alist, syntax, &make);
  }
} Select_syntax;
