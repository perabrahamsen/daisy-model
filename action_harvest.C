// action_harvest.C

#include "action.h"
#include "daisy.h"
#include "column.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <iostream.h>
#include "library.h"

class ActionHarvest : public Action
{
private:
  const string name;
  const double stub;
  const double stem;
  const double leaf;
  const double sorg;
  const double dead;

public:
  void doIt (Daisy&);

  // Create and Destroy.
private:
  friend class ActionHarvestSyntax;
  static Action& make (const AttributeList&, const Action *const p);
  ActionHarvest (const AttributeList&, const Action *const p);
public:
  ~ActionHarvest ();
};

void 
ActionHarvest::doIt (Daisy& daisy)
{
  cout << " [Harvesting " << name << "at";
  ColumnList& cl = daisy.columns;
  for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
    { 
      if (!match (**i))
	continue;
      vector<const Harvest*> entry
	= (*i)->harvest (daisy.time, name, stub, stem, leaf, sorg, dead);
      daisy.harvest.insert (daisy.harvest.end (),
			    entry.begin (), entry.end ());
      cout << " " << (*i)->name;

    }
    cout << "]";
}

ActionHarvest::ActionHarvest (const AttributeList& al, const Action *const p)
  : Action (p),
    name (al.name ("name")), 
    stub (al.number ("stub")),
    stem (al.number ("stem")),
    leaf (al.number ("leaf")),
    sorg (al.number ("sorg")),
    dead (al.number ("dead"))
{ }

ActionHarvest::~ActionHarvest ()
{ }

// Add the ActionHarvest syntax to the syntax table.
Action&
ActionHarvest::make (const AttributeList& al, const Action *const p)
{
  return *new ActionHarvest (al, p);
}

static struct ActionHarvestSyntax
{
  ActionHarvestSyntax ();
} ActionHarvest_syntax;

ActionHarvestSyntax::ActionHarvestSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("name", Syntax::String, Syntax::Const);
  alist.add ("name", "all");
  syntax.add ("stub", Syntax::Number, Syntax::Const);
  alist.add ("stub", 0.0);
  syntax.add ("stem", Syntax::Number, Syntax::Const);
  alist.add ("stem", 1.0);
  syntax.add ("leaf", Syntax::Number, Syntax::Const);
  alist.add ("leaf", 1.0);
  syntax.add ("sorg", Syntax::Number, Syntax::Const);
  alist.add ("sorg", 1.0);
  syntax.add ("dead", Syntax::Number, Syntax::Const);
  alist.add ("dead", 1.0);
  syntax.order ("name");
  Action::add_type ("harvest", alist, syntax, &ActionHarvest::make);
}
