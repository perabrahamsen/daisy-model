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
  void doIt (Daisy& daisy)
  {
    cout << " [Tillage]";
    ColumnList& cl = daisy.columns;
    for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
      {
	if (match (**i))
	  (*i)->mix (daisy.time, 0.0, depth, penetration);
      }
  }

  // Create and Destroy.
  friend class ActionMixSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  { return *new ActionMix (al, p); }
  ActionMix (const AttributeList& al, const Action *const p)
    : Action (p),
      depth (al.number ("depth")),
      penetration (al.number ("penetration"))
  { }
public:
  ~ActionMix ()
  { }
};

static struct ActionMixSyntax
{
  static bool check (const AttributeList& al)
  {
    const double penetration (al.number ("penetration"));
    const double depth (al.number ("depth"));
    bool ok = true;
    is_fraction (penetration, "penetration", ok);
    non_positive (depth, "depth", ok);
    if (!ok)
      cerr << "in mix action\n";
    return ok;
  }
  ActionMixSyntax ()
  { 
    Syntax& syntax = *new Syntax (check);
    AttributeList& alist = *new AttributeList ();
    syntax.add ("depth", Syntax::Number, Syntax::Const);
    syntax.order ("depth");
    syntax.add ("penetration", Syntax::Number, Syntax::Const);
    alist.add ("penetration", 0.0);
    Action::add_type ("mix", alist, syntax, &ActionMix::make);
  }
} ActionMix_syntax;

class ActionSwap : public Action
{
  // Content.
  const double middle;
  const double depth;

  // Simulation.
  void doIt (Daisy& daisy)
  {
    cout << " [Plowing]";

    ColumnList& cl = daisy.columns;
    for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
      {
	if (match (**i))
	  (*i)->swap (daisy.time, 0.0, middle, depth);
      }
  }

  // Create and Destroy.
  friend class ActionSwapSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  { return *new ActionSwap (al, p); }
  ActionSwap (const AttributeList& al, const Action *const p)
    : Action (p),
      middle (al.number ("middle")),
      depth (al.number ("depth"))
  { }
public:
  ~ActionSwap ()
  { }
};

static struct ActionSwapSyntax
{
  static bool check (const AttributeList& al)
  {
    const double middle (al.number ("middle"));
    const double depth (al.number ("depth"));
    bool ok = true;
    non_positive (middle, "middle", ok);
    non_positive (depth, "depth", ok);
    if (middle <= depth)
      {
	cerr << "swap middle should be above the depth\n";
	ok = false;
      }
    if (!ok)
      cerr << "in swap action\n";
    return ok;
  }
  ActionSwapSyntax ()
  {
    Syntax& syntax = *new Syntax (check);
    AttributeList& alist = *new AttributeList ();
    syntax.add ("middle", Syntax::Number, Syntax::Const);
    syntax.add ("depth", Syntax::Number, Syntax::Const);
    Action::add_type ("swap", alist, syntax, &ActionSwap::make);
  }
} ActionSwap_syntax;

