// select.C --- Select a state variable.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include "select.h"
#include "time.h"
#include "geometry.h"
#include "check.h"
#include <numeric>
#include <set>

EMPTY_TEMPLATE
Librarian<Select>::Content* Librarian<Select>::content = NULL;

const char *const Select::description = "Select part of state.";
  
struct Select::Implementation
{
  // Spec.
  struct Spec
  {
    // Content.
    const string library_name;
    const string model_name;
    const vector<string> submodels_and_attribute;
    
    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog& err);
    static void load_syntax (Syntax& syntax, AttributeList&);
    Spec (const AttributeList&);
    ~Spec ();
  } *spec;

  // Content.
  Condition* condition;		// Should we accumulate now?
  vector<string> path;		// Content of this entry.

  // Intermediate state.
  unsigned int current_path_index;// How nested in open's we are.
  unsigned int last_valid_path_index;	// Remember the last valid level.
  bool is_active;		// Should we be accumulating now?
  vector<bool> maybies;		// Keep track of which maybies 
				// have matched.
  bool valid ();		// If the current path index is valid.
  bool valid (const string& name); // Is the next path index valid?
  void open_maybe (const string& value);
  void close_maybe ();		// Close one level.
  void open_group (const string& name);
  void open (const string& name); // Open one leaf level.
  void close ();		// Close one level.

  bool match (const Daisy& daisy, Treelog&, bool is_printing);

  // Create and Destroy.
  void initialize (const string_map& conv, const string& timestep, 
		   string& dimension);
  Implementation (const AttributeList& al);
  ~Implementation ();
};

bool 
Select::Implementation::Spec::check_alist (const AttributeList& al,
					   Treelog& err)
{
  bool ok = true;
  
  const string library_name = al.name ("library");
  const string model_name = al.name ("model");
  const vector<string> submodels_and_attribute 
    = al.name_sequence ("submodels_and_attribute");

  if (submodels_and_attribute.size () < 1)
    {
      err.entry ("You must specify an attribute");
      ok = false;
    }
  if (!Library::exist (library_name))
    {
      err.entry (string ("'") + library_name + "': no such library");
      ok = false;
    }
  else
    {
      const Library& library = Library::find (library_name);
      
      if (!library.check (model_name))
	{
	  err.entry (string ("'") + model_name + "': no such model");
	  ok = false;
	}
      else
	{
	  const Syntax* syntax = &library.syntax (model_name);
	  
	  for (unsigned int i = 0; i < submodels_and_attribute.size (); i++)
	    {
	      const string& name = submodels_and_attribute[i];
	      bool last = (i + 1 == submodels_and_attribute.size ());
	      const Syntax::type type = syntax->lookup (name);

	      if (!last)
		{
		  if (type != Syntax::AList)
		    {
		      err.entry (string ("'") + name + "': no such submodel");
		      ok = false;
		      break;
		    }
		  syntax = &syntax->syntax (name);
		}
	      else if (type == Syntax::Error)
		{
		  err.entry (string ("'") + name + "': no such attribute");
		  ok = false;
		}
	    }
	}
    }
  return ok;
}

void 
Select::Implementation::Spec::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_check (check_alist);
  syntax.add ("library", Syntax::String, Syntax::Const, "\
Name of library where the attribute belong.");
  syntax.add ("model", Syntax::String, Syntax::Const, "\
Name of model where the attribute belong.");
  syntax.add ("submodels_and_attribute", Syntax::String, 
	      Syntax::Const, Syntax::Sequence, "\
Name of submodels and attribute.");
  syntax.order ("library", "model", "submodels_and_attribute");
}

Select::Implementation::Spec::Spec (const AttributeList& al)
  : library_name (al.name ("library")),
    model_name (al.name ("model")),
    submodels_and_attribute (al.name_sequence ("submodels_and_attribute"))
{ }

Select::Implementation::Spec::~Spec ()
{ }

bool 
Select::Implementation::valid ()
{ 
  // If the current path index is valid.
  return (current_path_index == last_valid_path_index
	  && current_path_index < path.size ()); 
}

bool 
Select::Implementation::valid (const string& name)
{ 
  // Is the next path index valid?
  return valid () && (path[current_path_index] == "*" 
		      || name == path[current_path_index]); 
}

void 
Select::Implementation::open_maybe (const string& value)
{
  const string question_mark = "?";
    
  if (valid ()
      && path[current_path_index][0] == '?'
      && question_mark + value == path[current_path_index])
    {
      maybies.push_back (true);
      last_valid_path_index++;
      current_path_index++;
    }
  else
    maybies.push_back (false);
}

void 
Select::Implementation::close_maybe ()		// Close one level.
{
  assert (!maybies.empty ());
  if (maybies.back ())
    {
      assert (current_path_index == last_valid_path_index);
      last_valid_path_index--;
      current_path_index--;
    }
  maybies.pop_back ();
}

void 
Select::Implementation::open_group (const string& name)
{ 
  // Open one group level.
  if (valid (name))
    {
#if 0
      if (is_active && last_valid_path_index == path.size () -1)
	{
	  names.insert (name);
	  count++;
	}
#endif
      last_valid_path_index++;
    }
  current_path_index++;
}

void 
Select::Implementation::open (const string& name) // Open one leaf level.
{
  if (valid (name))
    last_valid_path_index++;
  current_path_index++;
}

void 
Select::Implementation::close ()		// Close one level.
{
  if (current_path_index == last_valid_path_index)
    last_valid_path_index--;
  current_path_index--;
}

// Reset at start of time step.
bool 
Select::Implementation::match (const Daisy& daisy, Treelog& out,
			       bool is_printing)
{
  assert (current_path_index == 0U);
  assert (last_valid_path_index == 0U);

  if (condition)
    {
      condition->tick (daisy, out);
      is_active = condition->match (daisy);
    }
  else
    is_active = is_printing;
  return is_active;
}

// Create and Destroy.
void 
Select::Implementation::initialize (const string_map& conv, const string& timestep, 
				    string& dimension)
{
  // Convert path according to mapping in 'conv'.
  for (unsigned int i = 0; i < path.size (); i++)
    {
      string_map::const_iterator entry = conv.find (path[i]);
      if (entry != conv.end ())
	path[i] = (*entry).second;
      else if (path[i].size () > 0 && path[i][0] == '$')
	path[i] = "*";
    }
  // Replace '&' with timestep.
  string new_dim;
  for (unsigned int i = 0; i < dimension.length (); i++)
    if (dimension[i] == '&')
      new_dim += timestep;
    else
      new_dim += dimension[i];
  dimension = new_dim;
}

  
Select::Implementation::Implementation (const AttributeList& al)
  : spec (al.check ("spec") ? new Spec (al.alist ("spec")) : NULL),
    condition (al.check ("when") 
	       ? &Librarian<Condition>::create (al.alist ("when"))
	       : NULL),
    path (al.name_sequence ("path")),
    current_path_index (0U),
    last_valid_path_index (0U),
    is_active (false)
{ }
  
Select::Implementation::~Implementation ()
{ 
  if (spec)
    delete spec;
  if (condition)
    delete condition; 
}


const Geometry* 
Select::geometry () const
{ return NULL; }

int 
Select::size () const
{ return -1; }


Select::Destination::Destination ()
{ }

Select::Destination::~Destination ()
{ }

bool
Select::is_active ()
{ return impl.is_active; }

bool 
Select::valid ()
{ return impl.valid (); }

bool 
Select::valid (const string& name)
{ return impl.valid (name); }

void
Select::open_maybe (const string& value)
{ impl.open_maybe (value); }

void 
Select::close_maybe ()		// Close one level.
{ impl.close_maybe (); }

void 
Select::open_group (const string& name) // Open one group level.
{ impl.open_group (name); }

void 
Select::open (const string& name)	// Open one leaf level.
{ impl.open (name); }

void 
Select::close ()		// Close one level.
{ impl.close (); }

  // Output routines.
void 
Select::output_time (const string& name, const Time&)
{ 
  if (is_active () && valid (name))
    throw ("This log selection can't log dates."); 
}

void 
Select::output_number (const string& name, const double)
{ 
  if (is_active () && valid (name))
    throw ("This log selection can't log numbers."); 
}

void 
Select::output_integer (const string& name, const int)
{ 
  if (is_active () && valid (name))
    throw ("This log selection can't log integers."); 
}

void 
Select::output_name (const string& name, const string&)
{ 
  if (is_active () && valid (name))
    throw ("This log selection can't log names.");
}

void 
Select::output_array (const string& name, const vector<double>&,
		const Geometry*)
{ 
  if (is_active () && valid (name))
    throw ("This log selection can't log arrays."); 
}

// Reset at start of time step.
bool 
Select::match (const Daisy& daisy, Treelog& out, bool is_printing)
{ return impl.match (daisy, out, is_printing); }

void 
Select::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("tag", Syntax::String, Syntax::OptionalConst,
	      "Tag to identify the column.\n\
These will be printed in the first line of the log file.\n\
The default tag is the last element in the path.");
  syntax.add ("dimension", Syntax::String, Syntax::Const,
	      "The unit for numbers in this column.\n\
These will be printed in the second line of the log file.\n\
The character '&' will be replaced with the log timestep.");
  alist.add ("dimension", "");
  syntax.add ("description", Syntax::String, Syntax::Const,
	      "A description of this column.");
  alist.add ("description", "\
Each entry represents one column in the log file.");
  syntax.add ("path", Syntax::String, Syntax::Const, 
	      Syntax::Sequence, "\
Sequence of attribute names leading to the variable you want to log in\n\
this column.  The first name should be one of the attributes of the\n\
daisy component itself.  What to specify as the next name depends on\n\
the type of the attribute you selected before.\n\
\n\
If the value of that attribute itself is a fixed component, you should\n\
specify the name of an attribute in that component as the second name.\n\
\n\
If the value is a library component, you should specify the name of\n\
the model or parameterization you are interested in, and then the name\n\
of the attribute inside the model you want to log.\n\
\n\
If the attribute is a date, you should specify 'year', 'month',\n\
'mday', or 'hour'.  These are all integer values.  If you don't specify\n\
any of these, a special ever increasing 'gnuplot' value will be calculated.\n\
\n\
The last attribute in the patch should be a number, a number sequence,\n\
a string, or an integer.  These are the only values which can be\n\
logged by this model.\n\
\n\
You can use the special value \"*\" to match everything at a given\n\
level, for example all crops.  This way the path can specify multiple\n\
values, they will be added before they are printed in the log file.\n\
All values that start with a \"$\" will work like \"*\".  They are intended\n\
to be mapped with the 'set' attribute in the 'table' log model.");
  syntax.add_submodule ("spec", alist, Syntax::OptionalConst, "\
Specification for the attribute to be logged of the form\n\
\n\
  library model submodel* attribute\n\
\n\
Unlike path, the attribute may occur several different places in the\n\
simulation, if the model is used at several places.  Also, there is no\n\
wildcards, so only a single model can be matches.  The spec is used for\n\
helping Daisy establish a unique dimension and description for the\n\
attribute.", Select::Implementation::Spec::load_syntax);
  syntax.add ("when", 
	      Librarian<Condition>::library (),
	      Syntax::OptionalConst, Syntax::Singleton,
	      "\
When to calculate the values in this column.\n\
By default, the values will be calculated once, when the a new log entry\n\
is written.  If you calculate the values more often, they will be\n\
accumulated.  This is useful if you for example want to summarize the\n\
hourly percolation into a daily log.");
  syntax.add ("factor", Syntax::None (), Check::none (), Syntax::Const, "\
Factor to multiply the calculated value with, before logging.");
  alist.add ("factor", 1.0);
  syntax.add ("offset", Syntax::Unknown (), Check::none (), Syntax::Const, "\
Offset to add to the calculated value, before logging.");
  alist.add ("offset", 0.0);
  syntax.add ("accumulate", Syntax::Boolean, Syntax::Const,
	      "Log accumulated values.");
  alist.add ("accumulate", false);
  syntax.add ("count", Syntax::Integer, Syntax::State, "\
Number of times the path has matched a variable since the last log entry.");
  alist.add ("count", 0);
}

void 
Select::initialize (const string_map& conv, double, double, 
		    const string& timestep)
{ impl.initialize (conv, timestep, dimension); }

static string
select_get_tag (const AttributeList& al)
{
  if (al.check ("tag"))
    return al.name ("tag");

  vector<string> path  = al.name_sequence ("path");
  
  if (path.size () > 0)
    return path[path.size () - 1];

  return "<none>";
}

Select::Select (const AttributeList& al)
  : impl (*new Implementation (al)),
    accumulate (al.flag ("accumulate")),
    factor (al.number ("factor")),
    offset (al.number ("offset")),
    count (al.integer ("count")),
    tag (select_get_tag (al)),
    dimension (al.name ("dimension"))
{ }

Select::~Select ()
{ delete &impl; }
