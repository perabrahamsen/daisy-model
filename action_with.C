// action_sow.C

#include "action.h"
#include "daisy.h"
#include "frame.h"
#include "syntax.h"
#include "alist.h"
#include "column.h"

struct FrameWith : public Frame 
{
  const Column *const column_;

  const Column* column () const
    { return column_; }

  FrameWith (const Frame& p, const Column* c)
    : Frame (&p),
      column_ (c)
    { }
};

struct ActionWithColumn : public Action
{
  const string column;
  vector<Action*>& actions;

public:
  void doIt (const Frame& frame, Daisy& daisy)
    { 
      FrameWith with (frame, daisy.columns.find (column));
      for (vector<Action*>::iterator i = actions.begin ();
	   i != actions.end ();
	   i++)
	{
	  (*i)->doIt (with, daisy);
	}
    }

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
      if (!daisy.columns.find (column))
	{
	  CERR << "No column `" << column << "'\n";
	  ok = false;
	}
      return ok;
    }

  ActionWithColumn (const AttributeList& al)
    : Action (al.name ("type")),
      column (al.name ("column")),
      actions (map_create<Action> (al.alist_sequence ("actions")))
    { }
public:
  ~ActionWithColumn ()
    { }
};

static struct ActionWithColumnSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionWithColumn (al); }

  ActionWithColumnSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("column", Syntax::String, Syntax::Const);
      syntax.add ("actions", Librarian<Action>::library (), Syntax::Const,
		  Syntax::Sequence);
      syntax.order ("column", "actions");
      Librarian<Action>::add_type ("with-column", alist, syntax, &make);
    }
} ActionWithColumn_syntax;
