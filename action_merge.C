// action_merge.C

#include "action.h"
#include "daisy.h"
#include "field.h"

struct ActionMerge : public Action
{
  const string combine;
  const string remove;

  void doIt (Daisy& daisy)
    {
      COUT << "[Merging " << remove << " into " << combine << "]\n";
      daisy.field.merge (combine, remove);
    }

  ActionMerge (const AttributeList& al)
    : Action (al),
      combine (al.name ("combine")), 
      remove (al.name ("remove"))
    { }
};

static struct ActionMergeSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionMerge (al); }
  ActionMergeSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Merge two columns.  After the merge, only the first column will remain,\n\
but its state will be a average of the the columns, weighted after size.");
      syntax.add ("combine", Syntax::String, Syntax::Const,
		  "Column to merge into.");
      syntax.add ("remove", Syntax::String, Syntax::Const,
		  "Column to remove after merge.");
      syntax.order ("combine", "remove");
      Librarian<Action>::add_type ("merge", alist, syntax, &make);
    }
} ActionMerge_syntax;
