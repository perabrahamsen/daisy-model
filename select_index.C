// select_index.C --- Select a state variable.

#include "select.h"

struct SelectIndex : public Select
{
  // Index.
  const int index;
  double value;	

  // Output routines.
  void output (const string& name, const vector<double>& array, 
	       const Geometry*)
    { output (name, array); }
  void output (const string& name, const vector<double>& array)
    { 
      if (!is_active ())
	return;

      if (valid (name))
	{
	  if (count == 0)	 
	    value = array[index];	
	  else
	    value += array[index];	
	  count++;
	}
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
  SelectIndex (const AttributeList& al)
    : Select (al),
      index (al.integer ("index")),
      value (al.number ("value"))
    { }
};

static struct SelectIndexSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectIndex (al); }

  SelectIndexSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);

      alist.add ("description", "Extract content at specified array index.");
      syntax.add ("index", Syntax::Integer, Syntax::Const,
		  "Specify array index to select.");
      syntax.add ("value", Syntax::Unknown (), Syntax::State,
		  "The current accumulated value.");
      alist.add ("value", 0.0);

      Librarian<Select>::add_type ("index", alist, syntax, &make);
    }
} Select_syntax;
