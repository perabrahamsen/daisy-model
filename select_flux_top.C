// select_flux_top.C --- Select a state variable.

#include "select.h"
#include "geometry.h"

struct SelectFluxTop : public Select
{
  // Content.
  double height;
  double value;	

  // Output routines.
  void output_array (const string& name, const vector<double>& array, 
		     const Geometry* geometry)
    { 
      if (!is_active ())
	return;

      if (valid (name))
	{
	  int index = geometry->interval_border (height);

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
  void initialize (const string_map& conv, 
		   double default_from, double default_to,
		   const string& timestep)
    {
      Select::initialize (conv, default_from, default_to, timestep);

      // Overwrite default height.
      if (default_from < 0.0)
	height = default_from;
    }
  SelectFluxTop (const AttributeList& al)
    : Select (al),
      height (0.0),
      value (al.number ("value"))
    { }
};

static struct SelectFluxTopSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectFluxTop (al); }

  SelectFluxTopSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);

      alist.add ("description", "Extract flux at top of specified interval.\n\
By default, log the first member of the sequence.");
      syntax.add ("value", Syntax::Unknown (), Syntax::State,
		  "The current accumulated value.");
      alist.add ("value", 0.0);

      Librarian<Select>::add_type ("flux_top", alist, syntax, &make);
    }
} Select_syntax;
