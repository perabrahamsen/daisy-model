// action_tillage.C

#include "action.h"
#include "daisy.h"
#include "field.h"

struct ActionMix : public Action
{
  // Content.
  const double depth;
  const double penetration;

  // Simulation.
  void doIt (Daisy& daisy)
    {
      COUT << " [Tillage]\n";
      daisy.field.mix (daisy.time, 0.0, depth, penetration);
    }

  ActionMix (const AttributeList& al)
    : Action (al),
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
      const double depth (al.number ("depth"));
      bool ok = true;
      non_positive (depth, "depth", ok);
      if (!ok)
	CERR << "in mix action\n";
      return ok;
    }
  ActionMixSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      syntax.add_check (check);
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Mix soil content down to the specified depth.\n\
The effect is that nitrogen, water, temperature and such are averaged in\n\
the interval.");
      syntax.add ("depth", "cm", Syntax::Const,
		  "How far down to mix the soil (a negative number).");
      syntax.order ("depth");
      syntax.add ("penetration", Syntax::Fraction (), Syntax::Const, "\
Fraction of organic matter on surface that are incorporated in the soil \
by this operation.");
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
  void doIt (Daisy& daisy)
  {
    COUT << " [Plowing]\n";
    daisy.field.swap (daisy.time, 0.0, middle, depth);
  }

  ActionSwap (const AttributeList& al)
    : Action (al),
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
      Syntax& syntax = *new Syntax ();
      syntax.add_check (check);
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Swap two soil layers.  The top layer start at the surface and goes down to\n\
`middle', and the second layer starts with `middle' and goes down to\n\
 `depth'.  After the operation, the content (such as heat, water, and\n\
organic matter) will be averaged in each layer, and the bottom layer will\n\
be placed on top of what used to be the top layer.");
      syntax.add ("middle", "cm", Syntax::Const, "\
The end of the first layer and the start of the second layer to swap\n\
\(a negative number).");
      syntax.add ("depth", "cm", Syntax::Const, "\
The end of the second layer to swap (a negative number).");
      Librarian<Action>::add_type ("swap", alist, syntax, &make);
    }
} ActionSwap_syntax;


struct ActionSetPorosity : public Action
{
  // Content.
  const double porosity;
  const double depth;

  // Simulation.
  void doIt (Daisy& daisy)
  {
    daisy.field.set_porosity (depth, porosity);
  }

  ActionSetPorosity (const AttributeList& al)
    : Action (al),
      porosity (al.number ("porosity")),
      depth (al.number ("depth"))
    { }
};

static struct ActionSetPorositySyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionSetPorosity (al); }

  static bool check (const AttributeList& al)
    {
      const double porosity (al.number ("porosity"));
      const double depth (al.number ("depth"));
      bool ok = true;
      non_positive (depth, "depth", ok);
      if (porosity <= 0 || porosity >= 1.0)
	{
	  CERR << "porosity should be larger than 0 and less than 1\n";
	  ok = false;
	}
      if (!ok)
	CERR << "in swap action\n";
      return ok;
    }

  ActionSetPorositySyntax ()
    {
      Syntax& syntax = *new Syntax ();
      syntax.add_check (check);
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Set the porosity of the horizon at the specified depth.\n\
To get useful results, you need to use a hydraulic model that supports this.");
      syntax.add ("porosity", Syntax::Fraction (), Syntax::Const, "\
Non-solid fraction of soil.");
      syntax.add ("depth", "cm", Syntax::Const, "\
A point in the horizon to modify (a negative number).");
      alist.add ("depth", 0.0);
      Librarian<Action>::add_type ("set_porosity", alist, syntax, &make);
    }
} ActionSetPorosity_syntax;
