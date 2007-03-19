// action_harvest.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "action.h"
#include "daisy.h"
#include "field.h"
#include "harvest.h"
#include <sstream>

struct ActionEmerge : public Action
{
  const symbol crop;

  void doIt (Daisy& daisy, Treelog& out)
  {
    static const symbol all_symbol ("all");
    if (crop != all_symbol)
      {
        if (daisy.field->crop_ds (crop) < -1.0)
          {
            out.warning ("Attempted forced emerge of " 
                         + crop + " which is not on the field");
            return;
          }
        if (daisy.field->crop_ds (crop) >= 0.0)
          {
            out.warning ("Forced emerge of " + crop
                         + " which is already emerged");
            return;
          }
      }
    out.message ("Forcing emergence of " + crop);
    daisy.field->emerge (crop, out);
  }

  ActionEmerge (Block& al)
    : Action (al),
      crop (al.identifier ("crop"))
  { }
};

static struct ActionEmergeSyntax
{
  static Action& make (Block& al)
  { return *new ActionEmerge (al); }
  ActionEmergeSyntax ();
} ActionEmerge_syntax;

ActionEmergeSyntax::ActionEmergeSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", "Force a crop to emerge.");
  syntax.add ("crop", Syntax::String, Syntax::Const, 
	      "Name of the crop to emerge.\n\
If you specify 'all', all crops will emerge.\n\
If there are no crop on the field with the specified name,\n\
nothing will happen.");
  alist.add ("crop", "all");
  syntax.order ("crop");
  Librarian<Action>::add_type ("emerge", alist, syntax, &make);
}

struct ActionHarvest : public Action
{
  const symbol crop;
  const double stub;
  const double stem;
  const double leaf;
  const double sorg;
  const bool combine;

  void doIt (Daisy& daisy, Treelog& msg)
  {
    static const symbol all_symbol ("all");
    if (crop != all_symbol && daisy.field->crop_ds (crop) < 0.0)
      {
	msg.warning ("Attempting to harvest " + crop 
		     + " which has not emerged on the field");
	return;
      }
    double old_DM = 0.0;
    for (size_t i = 0; i < daisy.harvest.size (); i++)
      old_DM += daisy.harvest[i]->total_DM ();
    daisy.field->harvest (daisy.time, daisy.dt, 
                         crop, stub, stem, leaf, sorg, combine,
			 daisy.harvest, msg);
    double new_DM = 0.0;
    for (size_t i = 0; i < daisy.harvest.size (); i++)
      new_DM += daisy.harvest[i]->total_DM ();
    std::ostringstream tmp;
    if (daisy.field->crop_ds (crop) < 0.0)
      tmp << "Harvesting ";
    else
      tmp << "Cutting ";
    tmp << crop << ", removing " << (new_DM - old_DM) * 0.01 << " Mg DM/ha";
    msg.message (tmp.str ());
  }

  ActionHarvest (Block& al)
    : Action (al),
      crop (al.identifier ("crop")), 
      stub (al.number ("stub")),
      stem (al.number ("stem")),
      leaf (al.number ("leaf")),
      sorg (al.number ("sorg")),
      combine (al.flag ("combine"))
  { }
};

static struct ActionHarvestSyntax
{
  static Action& make (Block& al)
    { return *new ActionHarvest (al); }
  ActionHarvestSyntax ();
} ActionHarvest_syntax;

ActionHarvestSyntax::ActionHarvestSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", "Harvest a crop.");
  syntax.add ("crop", Syntax::String, Syntax::Const, 
	      "Name of the crop to harvest.\n\
If you specify 'all', all crops will be harvested.\n\
If there are no crop on the field with the specified name,\n\
nothing will happen.");
  alist.add ("crop", "all");
  syntax.add ("stub", "cm", Syntax::Const, "\
Leave stem and leafs below this height on the field.");
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
  syntax.add ("combine", Syntax::Boolean, Syntax::Const, "\
Set this to 'true' in order to combine all crop parts into stem\n\
in the harvest log files.\n\
This is mostly useful for silage.");
  alist.add ("combine", false);
  syntax.order ("crop");
  Librarian<Action>::add_type ("harvest", alist, syntax, &make);
}
