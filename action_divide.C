// action_divide.C

#include "action.h"
#include "daisy.h"
#include "field.h"

struct ActionDivide : public Action
{
  const string original;
  const string copy;
  const double size;

  void doIt (Daisy& daisy)
    {
      COUT << "[Dividing " << original << " into " << copy << "]\n";
      daisy.field.divide (original, copy, size, daisy.time, daisy.weather);
    }

  ActionDivide (const AttributeList& al)
    : Action (al.name ("type")),
      original (al.name ("original")), 
      copy (al.name ("copy")),
      size (al.number ("size"))
    { }
};

static struct ActionDivideSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionDivide (al); }
  ActionDivideSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Divide an existing column into two, thus creating a new column.\n\
The `size' argument specifies the size of the new column, which must be \n\
smaller than the size of the original column.");
      syntax.add ("original", Syntax::String, Syntax::Const,
		  "Column to divide");
      syntax.add ("copy", Syntax::String, Syntax::Const,
		  "Name of new column.");
      syntax.add ("size", Syntax::Unknown (), Syntax::Const,
		  "Size of the partition to remove.");
      syntax.order ("original", "copy", "size");
      Librarian<Action>::add_type ("divide", alist, syntax, &make);
    }
} ActionDivide_syntax;
