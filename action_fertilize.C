// action_fertilize.C

#include "action.h"
#include "daisy.h"
#include "weather.h"
#include "column.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <iostream.h>
#include "aom.h"
#include "library.h"

class ActionFertilize : public Action
{
private:
  const AttributeList& am;
  const double from;
  const double to;

public:
  void doIt (Daisy&) const;

  // Create and Destroy.
public:
  bool check (Daisy&) const;
private:
  friend class ActionFertilizeSyntax;
  static Action& make (const AttributeList&);
  ActionFertilize (const AttributeList&);
public:
  ~ActionFertilize ();
};

void 
ActionFertilize::doIt (Daisy& daisy) const
{
  ColumnList& cl = daisy.columns;
  for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
    {
      // Add inorganic matter.
      (*i)->fertilize (AOM::im (am), from, to);
      // Add organic matter, if any.
      if (am.number ("total_C_fraction") > 0.0)
	{
	  if ((*i)->check_am (am))
	    {
	      AOM& aom = *new AOM (daisy.time, am);
	      if (aom.check ())
		(*i)->fertilize (aom, from, to);
	      else
		cerr << "Not fertilizing.\n";
	    }
	  else
	    cerr << "Ignoring malformed fertilizer\n";
	}
    }
}

bool
ActionFertilize::check (Daisy&) const
{ 
  bool ok = true;

  if (!AOM::check (am))
    ok = false;
  if (from > 0.0 || to > 0.0)
    {
      cerr << "You can only fertilize on or below the ground.\n";
      ok = false;
    }
  if (from < to)
    {
      cerr << "from must be higher than to in the fertilization area.\n";
      ok = false;
    }
  if (!ok)
    cerr << "in fertilize action\n";

  return ok;
}

ActionFertilize::ActionFertilize (const AttributeList& al)
  : am (al.list ("am")), 
    from (al.number ("from")),
    to (al.number ("to"))
{ }

ActionFertilize::~ActionFertilize ()
{ }

// Add the ActionFertilize syntax to the syntax table.
Action&
ActionFertilize::make (const AttributeList& al)
{
  return *new ActionFertilize (al);
}

static struct ActionFertilizeSyntax
{
  ActionFertilizeSyntax ();
} ActionFertilize_syntax;

ActionFertilizeSyntax::ActionFertilizeSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("am", AOM::library ());
  alist.add ("am", AOM::library ().lookup ("am"));
  syntax.add ("from", Syntax::Number, Syntax::Const);
  alist.add ("from", 0.0);
  syntax.add ("to", Syntax::Number, Syntax::Const);
  alist.add ("to", 0.0);
  Action::add_type ("fertilize", alist, syntax, &ActionFertilize::make);
}
