// action_merge.C

#include "action.h"
#include "daisy.h"
#include "field.h"

struct ActionMerge : public Action
{
  const string combine;
  const string remove;
  const double weight;

  void doIt (Daisy& daisy)
    {
      COUT << "[Merging " << remove << " into " << combine << "]\n";
      daisy.field.merge (combine, remove, weight);
    }

  ActionMerge (const AttributeList& al)
    : Action (al.name ("type")),
      combine (al.name ("combine")), 
      remove (al.name ("remove")),
      weight (al.number ("weight"))
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
      syntax.add ("combine", Syntax::String, Syntax::Const,
		  "Column to merge into.");
      syntax.add ("remove", Syntax::String, Syntax::Const,
		  "Column to remove after merge.");
      syntax.add ("weight", Syntax::None (), Syntax::Const,
		  "Relative size of the partition to remove.  \
(2.0 twice as big, 0.5 half as big).");
      alist.add ("weight", 1.0);
      syntax.order ("combine", "remove");
      Librarian<Action>::add_type ("merge", alist, syntax, &make);
    }
} ActionMerge_syntax;
