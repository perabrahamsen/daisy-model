// action_harvest.C
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


#include "action.h"
#include "daisy.h"
#include "field.h"

struct ActionHarvest : public Action
{
  const string name;
  const double stub;
  const double stem;
  const double leaf;
  const double sorg;

  void doIt (Daisy& daisy, Treelog& out)
  {
    if (daisy.field.crop_ds (name) < 0.0)
      {
	out.warning (string ("Attempting to harvest ") + name 
		     + " which has not emerged on the field");
	return;
      }
    daisy.field.harvest (daisy.time, name, stub, stem, leaf, sorg,
			 daisy.harvest, out);
    if (daisy.field.crop_ds (name) < 0.0)
      out.message (string(" [Harvesting ") + name + "]");
    else
      out.message (string(" [Cutting ") + name + "]");
  }

  ActionHarvest (const AttributeList& al)
    : Action (al),
      name (al.name ("name")), 
      stub (al.number ("stub")),
      stem (al.number ("stem")),
      leaf (al.number ("leaf")),
      sorg (al.number ("sorg"))
    { }
};

static struct ActionHarvestSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionHarvest (al); }
  ActionHarvestSyntax ();
} ActionHarvest_syntax;

ActionHarvestSyntax::ActionHarvestSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", "Harvest a crop.");
  syntax.add ("name", Syntax::String, Syntax::Const, 
	      "Name of the crop to harvest.\n\
If you specify 'all', all crops will be harvested.\n\
If there are no crop on the field with the specified name,\n\
nothing will happen.");
  alist.add ("name", "all");
  syntax.add ("stub", "cm", Syntax::Const, "\
Leave stem and leafs below this size on the field.");
  alist.add ("stub", 0.0);
  syntax.add_fraction ("stem", Syntax::Const, "\
Fraction of stem (above stub) to harvest.");
  alist.add ("stem", 1.0);
  syntax.add_fraction ("leaf", Syntax::Const, "\
Fraction of leafs (above stub) to harvest.");
  alist.add ("leaf", 1.0);
  syntax.add_fraction ("sorg", Syntax::Const, "\
Fraction of storage organ to harvest.");
  alist.add ("sorg", 1.0);
  syntax.order ("name");
  Librarian<Action>::add_type ("harvest", alist, syntax, &make);
}
