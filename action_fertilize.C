// action_fertilize.C

#include "action.h"
#include "daisy.h"
#include "frame.h"
#include "column.h"
#include "am.h"
#include "im.h"

struct ActionFertilize : public Action
{
  const AttributeList& am;
  const double from;
  const double to;

  void doIt (const Frame& frame, Daisy& daisy)
    {
      cout << " [Fertilizing " << am.name ("type") << "]\n";

      for (ColumnList::iterator i = daisy.columns.begin (); 
	   i != daisy.columns.end (); 
	   i++)
	{
	  // Skip unselected columns.
	  if (!frame.match_column (**i))
	    continue;

	  // Add inorganic matter.
	  if (to < from)
	    (*i)->fertilize (IM (am), from, to);
	  else 
	    (*i)->fertilize (IM (am));
      
	  // Add organic matter, if any.
	  if (am.name ("syntax") != "mineral")
	    {
	      if (to < from)
		(*i)->fertilize (am, daisy.time, from, to);
	      else
		(*i)->fertilize (am, daisy.time);
	    }
	}
    }

  bool check (Daisy& daisy) const
    {
      ColumnList& cl = daisy.columns;
      for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
	{
	  if (am.name ("syntax") != "mineral" && !(*i)->check_am (am))
	    return false;
	}
      return true;
    }

  ActionFertilize (const AttributeList& al)
    : Action (al.name ("type")),
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
	  cerr << "You can only fertilize on or below the ground.\n";
	  ok = false;
	}
      if (from < to)
	{
	  cerr << "`from' must be higher than `to' in"
	       << " the fertilization area.\n";
	  ok = false;
	}
      return ok;
    }

  ActionFertilizeSyntax ()
  { 
    Syntax& syntax = *new Syntax (check);
    AttributeList& alist = *new AttributeList ();
    syntax.add ("am", AM::library ());
    syntax.add ("from", Syntax::Number, Syntax::Const);
    alist.add ("from", 0.0);
    syntax.add ("to", Syntax::Number, Syntax::Const);
    alist.add ("to", 0.0);
    syntax.order ("am");
    Librarian<Action>::add_type ("fertilize", alist, syntax, &make);
  }
} ActionFertilize_syntax;
