// select_interval.C --- Select a state variable.

#include "select.h"
#include "geometry.h"

struct SelectInterval : public Select
{
  // Content.
  double from;
  double to;
  double value;	

  // Output routines.

  void output (const string& name, const vector<double>& array, 
	       const Geometry* geometry)
    { 
      if (!is_active ())
	return;

      if (valid (name))
	{
	  if (to > 0.0)
	    to = geometry->zplus (geometry->size () - 1);

	  if (count == 0)	 
	    value = geometry->total (array, from, to);	
	  else
	    value += geometry->total (array, from, to);
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
  void initialize (const string_map conv, 
		   double default_from, double default_to)
    {
      Select::initialize (conv, default_from, default_to);

      // Set default range.
      if (default_from <= 0.0 && from > 0.0)
	from = default_from;
      if (default_to <= 0.0 && to > 0.0)
	to = default_to;
    }
  SelectInterval (const AttributeList& al)
    : Select (al),
      from (al.number ("from")),
      to (al.check ("to") ? al.number ("to") : 1.0),
      value (al.number ("value"))
    { }
};

static struct SelectIntervalSyntax
{
  static Select& make (const AttributeList& al)
    { return *new SelectInterval (al); }

  SelectIntervalSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      Select::load_syntax (syntax, alist);
      alist.add ("description", "Summarize specified interval.");

      syntax.add ("from", "cm", Syntax::Const,
		  "Specify height (negative) to measure from.");
      alist.add ("from", 0.0);
      syntax.add ("to", "cm", Syntax::OptionalConst,
		  "Specify height (negative) to measure interval.\n\
By default, measure to the bottom.");
      syntax.add ("value", Syntax::Unknown (), Syntax::State,
		  "The current accumulated value.");
      alist.add ("value", 0.0);

      Librarian<Select>::add_type ("interval", alist, syntax, &make);
    }
} Select_syntax;
