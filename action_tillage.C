// action_tillage.C

#include "action.h"
#include "daisy.h"
#include "weather.h"
#include "column.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <iostream.h>

class ActionMix : public Action
{
  // Content.
  const double depth;
  const double penetration;

  // Simulation.
  void doIt (Daisy& daisy) const
  {
    ColumnList& cl = daisy.columns;
    for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
      {
	(*i)->mix (0.0, depth, penetration);
      }
  }

  bool check (Daisy&) const
  {
    bool ok = true;
    is_fraction (penetration, "penetration", ok);
    non_negative (depth, "depth", ok);
    if (!ok)
      cerr << "in mix action\n";
    return ok;
  }


  // Create and Destroy.
  friend class ActionMixSyntax;
  static Action& make (const AttributeList& al)
  { return *new ActionMix (al); }
  ActionMix (const AttributeList& al)
    : depth (al.number ("depth")),
      penetration (al.number ("penetration"))
  { }
public:
  ~ActionMix ()
  { }
};

static struct ActionMixSyntax
{
  ActionMixSyntax ();
} ActionMix_syntax;

ActionMixSyntax::ActionMixSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("depth", Syntax::Number, Syntax::Const);
  syntax.order ("depth");
  syntax.add ("penetration", Syntax::Number, Syntax::Const);
  alist.add ("penetration", 0.0);
  Action::add_type ("mix", alist, syntax, &ActionMix::make);
}

class ActionSwap : public Action
{
  // Content.
  const double middle;
  const double depth;

  // Simulation.
  void doIt (Daisy& daisy) const
  {
    ColumnList& cl = daisy.columns;
    for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
      {
	(*i)->swap (0.0, middle, depth);
      }
  }

  bool check (Daisy&) const
  {
    bool ok = true;
    non_negative (middle, "middle", ok);
    non_negative (depth, "depth", ok);
    if (middle <= depth)
      {
	cerr << "swap middle should be above the depth\n";
	ok = false;
      }
    if (!ok)
      cerr << "in swap action\n";
    return ok;
  }

  // Create and Destroy.
  friend class ActionSwapSyntax;
  static Action& make (const AttributeList& al)
  { return *new ActionSwap (al); }
  ActionSwap (const AttributeList& al)
    : middle (al.number ("middle")),
      depth (al.number ("depth"))
  { }
public:
  ~ActionSwap ()
  { }
};

static struct ActionSwapSyntax
{
  ActionSwapSyntax ();
} ActionSwap_syntax;

ActionSwapSyntax::ActionSwapSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("middle", Syntax::Number, Syntax::Const);
  syntax.add ("depth", Syntax::Number, Syntax::Const);
  Action::add_type ("swap", alist, syntax, &ActionSwap::make);
}
