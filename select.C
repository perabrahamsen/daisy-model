// select.C --- Select a state variable.

#include "select.h"
#include "time.h"
#include "geometry.h"
#include <numeric>
#include <set>

EMPTY_TEMPLATE
Librarian<Select>::Content* Librarian<Select>::content = NULL;

const char *const Select::description = "Select part of state.";
  
struct Select::Implementation
{
  // Content.
  Condition* condition;		// Should we accumulate now?
  vector<string> path;		// Content of this entry.

  // Intermediate state.
  unsigned int current_path_index;// How nested in open's we are.
  unsigned int last_valid_path_index;	// Remember the last valid level.
  bool is_active;		// Should we be accumulating now?
  vector<bool> maybies;		// Keep track of which maybies 
                                         // have matched.
  bool valid ()		// If the current path index is valid.
  { 
    return (current_path_index == last_valid_path_index
	    && current_path_index < path.size ()); 
  }

  bool valid (const string& name) // Is the next path index valid?
  { 
    return valid () && (path[current_path_index] == "*" 
			|| name == path[current_path_index]); 
  }

  void open_maybe (const string& value)
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

  void close_maybe ()		// Close one level.
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

  void open_group (const string& name) // Open one group level.
  {
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

  void open (const string& name) // Open one leaf level.
  {
    if (valid (name))
      last_valid_path_index++;
    current_path_index++;
  }

  void close ()		// Close one level.
  {
    if (current_path_index == last_valid_path_index)
      last_valid_path_index--;
    current_path_index--;
  }

  // Reset at start of time step.
  bool match (const Daisy& daisy, bool is_printing)
  {
    assert (current_path_index == 0U);
    assert (last_valid_path_index == 0U);

    if (condition)
      is_active = condition->match (daisy);
    else
      is_active = is_printing;
    return is_active;
  }

  // Create and Destroy.
  void initialize (const string_map& conv, const string& timestep, 
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

  Implementation (const AttributeList& al)
    : condition (al.check ("when") 
		 ? &Librarian<Condition>::create (al.alist ("when"))
		 : NULL),
      path (al.name_sequence ("path")),
      current_path_index (0U),
      last_valid_path_index (0U),
      is_active (false)
  { }
  ~Implementation ()
  { delete &condition; }
};

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
Select::match (const Daisy& daisy, bool is_printing)
{ return impl.match (daisy, is_printing); }

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
  syntax.add ("when", 
	      Librarian<Condition>::library (),
	      Syntax::OptionalConst, Syntax::Singleton,
	      "\
When to calculate the values in this column.\n\
By default, the values will be calculated once, when the a new log entry\n\
is written.  If you calculate the values more often, they will be\n\
accumulated.  This is useful if you for example want to summarize the\n\
hourly percolation into a daily log.");
  syntax.add ("factor", Syntax::None (), Syntax::Const, "\
Factor to multiply the calculated value with, before logging.");
  alist.add ("factor", 1.0);
  syntax.add ("offset", Syntax::Unknown (), Syntax::Const, "\
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
{ }
