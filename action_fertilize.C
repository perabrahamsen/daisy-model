// action_fertilize.C

#include "action.h"
#include "daisy.h"
#include "field.h"
#include "am.h"
#include "im.h"

struct ActionFertilize : public Action
{
  const AttributeList& am;
  const double from;
  const double to;

  void doIt (Daisy& daisy)
    {
      COUT << " [Fertilizing " << am.name ("type") << "]\n";

      // Add inorganic matter.
      if (to < from)
	daisy.field.fertilize (IM (am), from, to);
      else 
	daisy.field.fertilize (IM (am));
      
	  // Add organic matter, if any.
      if (am.name ("syntax") != "mineral")
	{
	  AttributeList am_creation (am);
	  am_creation.add ("creation", daisy.time);
	  if (to < from)
	    daisy.field.fertilize (am, from, to);
	  else
	    daisy.field.fertilize (am);
	}
    }

  bool check (const Daisy& daisy) const
    {
      bool ok = true;
      if (am.name ("syntax") != "mineral" && !daisy.field.check_am (am))
	ok = false;
      return ok;
    }

  ActionFertilize (const AttributeList& al)
    : Action (al),
      am (al.alist ("am")), 
      from (al.number ("from")),
      to (al.number ("to"))
    { }
};

static struct ActionFertilizeSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionFertilize (al); }

  static bool check (const AttributeList& al)
    { 
      bool ok = true;
      const double from = al.number ("from");
      const double to = al.number ("to");

      if (from > 0.0 || to > 0.0)
	{
	  CERR << "You can only fertilize on or below the ground.\n";
	  ok = false;
	}
      if (from < to)
	{
	  CERR << "`from' must be higher than `to' in"
	       << " the fertilization area.\n";
	  ok = false;
	}
      return ok;
    }

  ActionFertilizeSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    syntax.add_check (check);
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Apply fertilizer to the soil.\n\
If you want to incorporate the fertilizer directly in the soil, specify\n\
the `from' and `to' parameters.  By default, the fertilizer will be\n\
left on the surface.");
    syntax.add ("am", Librarian<AM>::library (), "\
The type of fertilizer you want to apply.");
    syntax.add ("from", "cm", Syntax::Const, "\
Height where you want to start the incorporation (a negative number).");
    alist.add ("from", 0.0);
    syntax.add ("to", "cm", Syntax::Const, "\
Height where you want to end the incorporation (a negative number).");
    alist.add ("to", 0.0);
    syntax.order ("am");
    Librarian<Action>::add_type ("fertilize", alist, syntax, &make);
  }
} ActionFertilize_syntax;
