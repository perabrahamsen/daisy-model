// action_sow.C

#include "action.h"
#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "column.h"

class ActionWithColumn : public Action
{
  const string column;
  vector<Action*>& actions;

public:
  void doIt (Daisy& daisy)
  { 
    for (vector<Action*>::iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	(*i)->doIt (daisy);
      }
  }
  bool match (const Column& c) const
  {
    return column == c.name;
  }

  // Create and Destroy.
public:
  bool check (Daisy& daisy) const
  { 
    bool ok = true;
    for (vector<const Action*>::const_iterator i = actions.begin ();
	 i != actions.end ();
	 i++)
      {
	if (!(*i)->check (daisy))
	  ok = false;
      }
    return ok;
  }
private:
  friend class ActionWithColumnSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  { return *new ActionWithColumn (al, p); }
  ActionWithColumn (const AttributeList& al, const Action *const p)
    : Action (p),
      column (al.name ("column")),
      actions (map_create1<Action, const Action *const>
	       (al.list_sequence ("actions"), this))
  { }
public:
  ~ActionWithColumn ()
  { }
};

static struct ActionWithColumnSyntax
{
  ActionWithColumnSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("column", Syntax::String, Syntax::Const);
    syntax.add ("actions", Action::library (), Syntax::Const,
		Syntax::Sequence);
    syntax.order ("column", "actions");
    Action::add_type ("with-column", alist, syntax, &ActionWithColumn::make);
  }
} ActionWithColumn_syntax;
