// select_max.C --- Select a state variable.

#include "select.h"

struct SelectMax : public Select
{
  // Content.
  double value;	

  // Output routines.
  void output_number (const string& name, double number)
    { 
      if (!is_active ())
	return;

      if (!valid (name))
	return;

      if (count == 0)
	value = number;
      else if (number > value)
	value = number;
      count++;
    }

  // Print result at end of time step.
  void done (Destination& dest)
    {
      if (count == 0)
	dest.missing (tag);
      else 
	dest.add (tag, value * factor + offset);

      if (!accumulate)
	count = 0;
    }
  // Create and Destroy.
  SelectMax (const AttributeList& al)
    : Select (al),
      value (al.number ("value"))
    { }
};

static struct SelectMaxSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectMax (al); }

  SelectMaxSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);
      alist.add ("description", "Extract maximum value.");

      syntax.add ("value", Syntax::Unknown (), Syntax::State,
		  "The current accumulated value.");
      alist.add ("value", 0.0);

      Librarian<Select>::add_type ("max", alist, syntax, &make);
    }
} Select_syntax;
