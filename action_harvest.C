// action_harvest.C

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

  void doIt (Daisy& daisy)
    {
      COUT << " [Harvesting " << name << "]\n";
      vector<const Harvest*> entry
	= daisy.field.harvest (daisy.time, name, stub, stem, leaf, sorg);
      daisy.harvest.insert (daisy.harvest.end (),
			    entry.begin (), entry.end ());
    }

  ActionHarvest (const AttributeList& al)
    : Action (al.name ("type")),
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
If you specify `all', all crops will be harvested.\n\
If there are no crop on the field with the specified name,\n\
nothing will happen.");
  alist.add ("name", "all");
  syntax.add ("stub", "cm", Syntax::Const, "\
Leave stem and leafs below this size on the field.");
  alist.add ("stub", 0.0);
  syntax.add ("stem", Syntax::None (), Syntax::Const, "\
Fraction of stem (above stub) to harvest.");
  alist.add ("stem", 1.0);
  syntax.add ("leaf", Syntax::None (), Syntax::Const, "\
Fraction of leafs (above stub) to harvest.");
  alist.add ("leaf", 1.0);
  syntax.add ("sorg", Syntax::None (), Syntax::Const, "\
Fraction of storage organ to harvest.");
  alist.add ("sorg", 1.0);
  syntax.order ("name");
  Librarian<Action>::add_type ("harvest", alist, syntax, &make);
}
