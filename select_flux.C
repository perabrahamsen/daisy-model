// select_flux.C --- Select a state variable.

#include "select.h"
#include "geometry.h"

struct SelectFlux : public Select
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
	    value = array[geometry->interval_border (height)];	
	  else
	    value += array[geometry->interval_border (height)];	
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
  SelectFlux (const AttributeList& al)
    : Select (al),
      height (al.number ("height")),
      value (al.number ("value"))
    { }
};

static struct SelectFluxSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectFlux (al); }

  SelectFluxSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);

      alist.add ("description", "Extract flux at specified height.");
      syntax.add ("height", "cm", Syntax::Const,
		  "Specify height (negative) to measure flux.  \
The closest interval border will be used.");
      syntax.add ("value", Syntax::Unknown (), Syntax::State,
		  "The current accumulated value.");
      alist.add ("value", 0.0);

      Librarian<Select>::add_type ("flux", alist, syntax, &make);
    }
} Select_syntax;
