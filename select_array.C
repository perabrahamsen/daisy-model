// select_array.C --- Select a state variable.

#include "select.h"

struct SelectArray : public Select
{
  // Content.
  vector<double> value;		// Total array.

  // Output routines.
  void output_array (const string& name, const vector<double>& array, 
		     const Geometry*)
    { 
      if (!is_active ())
	return;

      if (valid (name))
	{
	  if (array.size () > value.size ())
	    value.insert (value.end (), 
			  array.size () - value.size (),
			  0.0);
	  if (count == 0)
	    for (unsigned int i = 0; i < array.size (); i++)
	      value[i] = array[i] * factor + offset;
	  else
	    for (unsigned int i = 0; i < array.size (); i++)
	      value[i] += array[i] * factor + offset;
	  count++;
	}
    }

  // Print result at end of time step.
  void done (Destination& dest)
    {
      if (count == 0)
	dest.missing (tag);
      else 
	dest.add (tag, value);
      
      if (!accumulate)
	count = 0;
    }

  // Create and Destroy.
  SelectArray (const AttributeList& al)
    : Select (al),
      value (al.number_sequence ("value"))
    { }
};

static struct SelectArraySyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectArray (al); }

  SelectArraySyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);

      syntax.add ("value", Syntax::Unknown (), Syntax::State, Syntax::Sequence,
		  "The current accumulated value.");
      vector<double> empty;
      alist.add ("value", empty);

      Librarian<Select>::add_type ("array", alist, syntax, &make);
    }
} Select_syntax;
