// geometry.C
// 
// Copyright 2006 Per Abrahamsen and KVL.
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


#include "geometry.h"
#include "alist.h"
#include "check.h"
#include "vcheck.h"
#include <sstream>

static struct CheckLayers : public VCheck
{
  void check (const Syntax& syntax, const AttributeList& alist, 
	      const std::string& key) const throw (std::string)
  {
    daisy_assert (alist.check (key));
    daisy_assert (syntax.lookup (key) == Syntax::AList);
    daisy_assert (!syntax.is_log (key));
    daisy_assert (syntax.size (key) == Syntax::Sequence);

    const std::vector<AttributeList*>& layers = alist.alist_sequence (key);

    double last = 0.0;
    for (unsigned int i = 0; i < layers.size (); i++)
      {
	if (!layers[i]->check ("end"))
	  continue;

	const double next = layers[i]->number ("end");
	if (next < last)
	  last = next;
	else
	  {
	    std::ostringstream tmp;
	    tmp << "Layer ending at " << next 
		   << " should be below " << last;
	    throw std::string (tmp.str ());
	  }
      }
  }
} check_layers;

void 
Geometry::add_layer (Syntax& syntax, Syntax::category cat, 
                     const std::string& name,
		     const std::string& dimension,
                     const std::string& description)
{
  Syntax& layer = *new Syntax ();
  layer.add ("end", "cm", Check::negative (), Syntax::Const, 
	     "End point of this layer (a negative number).");
  if (dimension == Syntax::Fraction ())
    layer.add_fraction ("value", Syntax::Const, description);
  else
    layer.add ("value", dimension, Syntax::Const, description);
  layer.order ("end", "value");

  const std::string iname = "initial_" + name;
  syntax.add (iname, layer,
	      Syntax::OptionalConst, Syntax::Sequence, 
	      "Initial value of the '" + name + "' parameter.\n\
The initial value is given as a sequence of (END VALUE) pairs, starting\n\
from the top and going down.  The parameter will be initialized to\n\
VALUE from the END of the previous layer, to the END of the current layer.");
  syntax.add_check (iname, check_layers);
  if (dimension == Syntax::Fraction ())
    syntax.add_fraction (name, cat, Syntax::Sequence, 
                         description);
  else
    syntax.add (name, dimension, cat, Syntax::Sequence, 
                description);
}

Geometry::Geometry (Block&)
{ }

Geometry::~Geometry ()
{ }
