// select_content.C --- Select a state variable.

#include "select.h"
#include "geometry.h"

struct SelectContent : public Select
{
  // Content.
  const double height;
  double value;	

  // Output routines.
  void output (const string& name, const vector<double>& array, 
	       const Geometry* geometry)
    { 
      if (!is_active ())
	return;

      if (valid (name))
	{
	  if (count == 0)	 
	    value = array[geometry->interval_plus (height)];	
	  else
	    value += array[geometry->interval_plus (height)];	
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
  SelectContent (const AttributeList& al)
    : Select (al),
      height (al.number ("height")),
      value (al.number ("value"))
    { }
};

static struct SelectContentSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectContent (al); }

  SelectContentSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);

      alist.add ("description", "Extract content at specified height.");
      syntax.add ("height", "cm", Syntax::Const,
		  "Specify height (negative) to measure content.");
      syntax.add ("value", Syntax::Unknown (), Syntax::State,
		  "The current accumulated value.");
      alist.add ("value", 0.0);

      Librarian<Select>::add_type ("content", alist, syntax, &make);
    }
} Select_syntax;
