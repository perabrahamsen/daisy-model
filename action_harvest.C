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
  vector <AM*> harvest;

public:
  void doIt (Daisy&);

  // Create and Destroy.
private:
  friend class ActionHarvestSyntax;
  static Action& make (const AttributeList&);
  ActionHarvest (const AttributeList&);
public:
  ~ActionHarvest ();
};

void 
ActionHarvest::doIt (Daisy& daisy)
{
  ColumnList& cl = daisy.columns;
  for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
    { 
      vector<AM*> entry
	= (*i)->harvest (daisy.time, name, stub, stem, leaf, sorg, dead);
      harvest.insert (harvest.end (), entry.begin (), entry.end ());
    }
}

ActionHarvest::ActionHarvest (const AttributeList& al)
  : name (al.name ("name")), 
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
ActionHarvest::make (const AttributeList& al)
{
  return *new ActionHarvest (al);
}

static struct ActionHarvestSyntax
{
  ActionHarvestSyntax ();
} ActionHarvest_syntax;

ActionHarvestSyntax::ActionHarvestSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("name", Syntax::Function, Syntax::Const);
  alist.add ("name", "all");
  syntax.add ("stub", Syntax::Number, Syntax::Const);
  syntax.add ("stem", Syntax::Number, Syntax::Const);
  syntax.add ("leaf", Syntax::Number, Syntax::Const);
  syntax.add ("sorg", Syntax::Number, Syntax::Const);
  syntax.add ("dead", Syntax::Number, Syntax::Const);
  syntax.add ("harvest", Syntax::Number, Syntax::LogOnly);
  syntax.order ("name");
  Action::add_type ("harvest", alist, syntax, &ActionHarvest::make);
}
