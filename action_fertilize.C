// action_fertilize.C

#include "action.h"
#include "daisy.h"
#include "column.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "am.h"
#include "im.h"
#include "library.h"
#include <iostream.h>

class ActionFertilize : public Action
{
private:
  const AttributeList& am;
  const double from;
  const double to;

public:
  void doIt (Daisy&);
  bool check (Daisy&) const;

  // Create and Destroy.
private:
  friend class ActionFertilizeSyntax;
  static Action& make (const AttributeList&, const Action *const p);
  ActionFertilize (const AttributeList&, const Action *const p);
public:
  ~ActionFertilize ();
};

void 
ActionFertilize::doIt (Daisy& daisy)
{
  cout << " [Fertilizing " << am.name ("type") << "]";
  ColumnList& cl = daisy.columns;
  for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
    {
      if (!match (**i))
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

bool
ActionFertilize::check (Daisy& daisy) const
{
  ColumnList& cl = daisy.columns;
  for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
    {
      if (am.name ("syntax") != "mineral" && !(*i)->check_am (am))
	return false;
    }
  return true;
}

ActionFertilize::ActionFertilize (const AttributeList& al,
				  const Action *const p)
  : Action (p),
    am (al.list ("am")), 
    from (al.number ("from")),
    to (al.number ("to"))
{ }

ActionFertilize::~ActionFertilize ()
{ }

// Add the ActionFertilize syntax to the syntax table.
Action&
ActionFertilize::make (const AttributeList& al, const Action *const p)
{
  return *new ActionFertilize (al, p);
}

static struct ActionFertilizeSyntax
{
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
	cerr << "`from' must be higher than `to' in the fertilization area.\n";
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
    Action::add_type ("fertilize", alist, syntax, &ActionFertilize::make);
  }
} ActionFertilize_syntax;
