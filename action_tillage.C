// action_tillage.C

#include "action.h"
#include "daisy.h"
#include "frame.h"
#include "column.h"

struct ActionMix : public Action
{
  // Content.
  const double depth;
  const double penetration;

  // Simulation.
  void doIt (const Frame& frame, Daisy& daisy)
    {
      COUT << " [Tillage]\n";
      ColumnList& cl = daisy.columns;
      for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
	{
	  if (frame.match_column (**i))
	    (*i)->mix (daisy.time, 0.0, depth, penetration);
	}
    }

  ActionMix (const AttributeList& al)
    : Action (al.name ("type")),
      depth (al.number ("depth")),
      penetration (al.number ("penetration"))
    { }
};

static struct ActionMixSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionMix (al); }

  static bool check (const AttributeList& al)
    {
      const double penetration (al.number ("penetration"));
      const double depth (al.number ("depth"));
      bool ok = true;
      is_fraction (penetration, "penetration", ok);
      non_positive (depth, "depth", ok);
      if (!ok)
	CERR << "in mix action\n";
      return ok;
    }
  ActionMixSyntax ()
    { 
      Syntax& syntax = *new Syntax (&check);
      AttributeList& alist = *new AttributeList ();
      syntax.add ("depth", Syntax::Number, Syntax::Const);
      syntax.order ("depth");
      syntax.add ("penetration", Syntax::Number, Syntax::Const);
      alist.add ("penetration", 0.0);
      Librarian<Action>::add_type ("mix", alist, syntax, &make);
    }
} ActionMix_syntax;

struct ActionSwap : public Action
{
  // Content.
  const double middle;
  const double depth;

  // Simulation.
  void doIt (const Frame& frame, Daisy& daisy)
  {
    COUT << " [Plowing]\n";

    ColumnList& cl = daisy.columns;
    for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
      {
	if (frame.match_column (**i))
	  (*i)->swap (daisy.time, 0.0, middle, depth);
      }
  }

  ActionSwap (const AttributeList& al)
    : Action (al.name ("type")),
      middle (al.number ("middle")),
      depth (al.number ("depth"))
    { }
};

static struct ActionSwapSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionSwap (al); }

  static bool check (const AttributeList& al)
    {
      const double middle (al.number ("middle"));
      const double depth (al.number ("depth"));
      bool ok = true;
      non_positive (middle, "middle", ok);
      non_positive (depth, "depth", ok);
      if (middle <= depth)
	{
	  CERR << "swap middle should be above the depth\n";
	  ok = false;
	}
      if (!ok)
	CERR << "in swap action\n";
      return ok;
    }

  ActionSwapSyntax ()
    {
      Syntax& syntax = *new Syntax (&check);
      AttributeList& alist = *new AttributeList ();
      syntax.add ("middle", Syntax::Number, Syntax::Const);
      syntax.add ("depth", Syntax::Number, Syntax::Const);
      Librarian<Action>::add_type ("swap", alist, syntax, &make);
    }
} ActionSwap_syntax;

