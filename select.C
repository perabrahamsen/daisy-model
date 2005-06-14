// select.C --- Select a state variable.
// 
// Copyright 1996-2002, 2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002, 2005 KVL.
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
#include "geometry.h"
#include "check.h"
#include "vcheck.h"
#include "units.h"
#include "symbol.h"
#include "format.h"
#include "submodel.h"
#include <numeric>
#include <set>

using namespace std;

Handle::handle_t
Handle::symbol2handle (symbol s)
{
  static struct sym_set_t : map<symbol,handle_t>
  {
    sym_set_t ()
    {
      static symbol min_symbol ("min");
      insert (pair<symbol,handle_t> (min_symbol, min));
      static symbol max_symbol ("max");
      insert (pair<symbol,handle_t> (max_symbol, max));
      static symbol average_symbol ("average");
      insert (pair<symbol,handle_t> (average_symbol, average));
      static symbol geometric_symbol ("geometric");
      insert (pair<symbol,handle_t> (geometric_symbol, geometric));
      static symbol sum_symbol ("sum");
      insert (pair<symbol,handle_t> (sum_symbol, sum));
      static symbol current_symbol ("current");
      insert (pair<symbol,handle_t> (current_symbol, current));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  

EMPTY_TEMPLATE
Librarian<Select>::Content* Librarian<Select>::content = NULL;

const char *const Select::description = "Select part of state.";
  
struct Select::Implementation
{
  // Spec.
  struct Spec
  {
    // Content.
    const symbol library_name;
    const symbol model_name;
    const vector<symbol> submodels_and_attribute;
    
    // Use.
    const Syntax& leaf_syntax (Syntax&) const;
    const string& leaf_name () const;
    string dimension () const;
    string description () const;
    void refer (Format&) const;

    // Create and Destroy.
    static bool check_path (const vector<symbol>& path,
			    const Syntax* syntax,
			    Treelog& err);
    static bool check_alist (const AttributeList& al, Treelog& err);
    static void load_syntax (Syntax& syntax, AttributeList&);
    Spec (const AttributeList&);
    ~Spec ();
  } *spec;

  // Content.
  const Units::Convert* spec_conv; // Convert value.
  const double factor;		// - || -
  const double offset;		// - || -
  const bool negate;            // - || -
  double convert (double) const; // - || -
  const symbol tag;		// Name of this entry.
  string dimension;		// Physical dimension of this entry.
  const string description;

  // Create and Destroy.
  bool check (const string& spec_tdim, Treelog& err) const;
  static string find_description (const AttributeList&);
  Implementation (const AttributeList& al);
  ~Implementation ();
};

const Syntax&
Select::Implementation::Spec::leaf_syntax (Syntax& buffer) const
{
  const Syntax* syntax;

  if (library_name == symbol ("fixed"))
    {
      AttributeList alist;
      Submodel::load_syntax (model_name.name (), buffer, alist);
      syntax = &buffer;
    }
  else
    {
      const Library& library = Library::find (library_name);
      syntax = &library.syntax (model_name);
    }

  for (unsigned int i = 0; i < submodels_and_attribute.size () - 1; i++)
    syntax = &syntax->syntax (submodels_and_attribute[i].name ());

  return *syntax;
}

const string&
Select::Implementation::Spec::leaf_name () const
{ 
  daisy_assert (submodels_and_attribute.size () > 0);
  return submodels_and_attribute.back ().name (); 
}

string /* can't return reference because buffer is automatic */
Select::Implementation::Spec::dimension () const
{
  Syntax buffer;
  const Syntax& syntax = leaf_syntax (buffer);
  if (syntax.lookup (leaf_name ()) == Syntax::Number)
    return syntax.dimension (leaf_name ());
  else
    return Syntax::Unknown ();
}

string /* can't return reference because buffer is automatic */
Select::Implementation::Spec::description () const
{ 
  Syntax buffer;
  return leaf_syntax (buffer).description (leaf_name ()); 
}

void 
Select::Implementation::Spec::refer (Format& format) const
{
  format.text (" ");
  const bool is_fixed = (library_name == symbol ("fixed"));
  if (!is_fixed)
    {
      format.text (library_name.name ());
      format.special ("nbsp");
    }
  format.text (model_name.name ());
  format.text (" ");
  string aref = library_name.name () + "-" + model_name.name ();
  format.text ("(section");
  format.special ("nbsp");
  if (is_fixed)
    format.ref ("fixed", model_name.name ());
  else
    format.ref ("model", aref);
  format.text (")");
  for (unsigned int i = 0; i < submodels_and_attribute.size (); i++)
    {
      const string name = submodels_and_attribute[i].name ();
      aref += "-" + name;
      format.text (" ");
      format.text (name);
    }
  format.text (" ");
  format.text ("(page");
  format.special ("nbsp");
  format.pageref ("parameter", aref);
  format.text (")");
}

bool 
Select::Implementation::Spec::check_path (const vector<symbol>& path,
					  const Syntax* syntax,
					  Treelog& err)
{
  bool ok = true;
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const symbol name = path[i];
      bool last = (i + 1 == path.size ());
      const Syntax::type type = syntax->lookup (name.name ());

      if (!last)
	{
	  if (type != Syntax::AList)
	    {
	      err.entry ("'" + name + "': no such submodel");
	      ok = false;
	      break;
	    }
	  syntax = &syntax->syntax (name.name ());
	}
      else if (type == Syntax::Error)
	{
	  err.entry ("'" + name + "': no such attribute");
	  ok = false;
	  break;
	}
    }
  return ok;
}

bool 
Select::Implementation::Spec::check_alist (const AttributeList& al,
					   Treelog& err)
{
  bool ok = true;
  
  const symbol library_name = al.identifier ("library");
  const symbol model_name = al.identifier ("model");
  const vector<symbol> submodels_and_attribute 
    = al.identifier_sequence ("submodels_and_attribute");

  if (submodels_and_attribute.size () < 1)
    {
      err.entry ("You must specify an attribute");
      ok = false;
    }
  if (library_name.name () == "fixed")
    {
      if (!Submodel::registered (model_name.name ()))
	{
	  err.entry ("'" + model_name + "': no such submodel");
	  ok = false;
	}
      else
	{
	  Syntax syntax;
	  AttributeList alist;
	  Submodel::load_syntax (model_name.name (), syntax, alist);
	  if (!check_path (submodels_and_attribute, &syntax, err))
	    ok = false;
	}
    }
  else if (!Library::exist (library_name))
    {
      err.entry ("'" + library_name + "': no such library");
      ok = false;
    }
  else
    {
      const Library& library = Library::find (library_name);
      
      if (!library.check (model_name))
	{
	  err.entry ("'" + model_name + "': no such model");
	  ok = false;
	}
      else
	{
	  const Syntax* syntax = &library.syntax (model_name);
	  if (!check_path (submodels_and_attribute, syntax, err))
	    ok = false;
	}
    }
  return ok;
}

void 
Select::Implementation::Spec::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_check (check_alist);
  syntax.add ("library", Syntax::String, Syntax::Const, "\
Name of library where the attribute belong.\n\
Use 'fixed' to denote a fixed component.");
  syntax.add ("model", Syntax::String, Syntax::Const, "\
Name of model or fixed component where the attribute belongs.");
  syntax.add ("submodels_and_attribute", Syntax::String, 
	      Syntax::Const, Syntax::Sequence, "\
Name of submodels and attribute.");
  syntax.order ("library", "model", "submodels_and_attribute");
}

Select::Implementation::Spec::Spec (const AttributeList& al)
  : library_name (al.identifier ("library")),
    model_name (al.identifier ("model")),
    submodels_and_attribute (al.identifier_sequence
			     ("submodels_and_attribute"))
{ }

Select::Implementation::Spec::~Spec ()
{ }

double 
Select::Implementation::convert (double value) const
{ 
  const double result = (spec_conv)
    ? spec_conv->operator() (value)
    :  value * factor + offset; 
  return negate ? -result : result;
}

// Create and Destroy.
bool 
Select::Implementation::check (const string& spec_dim, Treelog& err) const
{
  bool ok = true;
  if (spec && !spec_conv && spec->dimension () != Syntax::Unknown ())
    err.warning (string ("Don't know how to convert [") + spec_dim
                 + "] to [" + dimension + "]");
  return ok;
}
  
string 
Select::Implementation::find_description (const AttributeList& al)
{
  const Library& library = Librarian<Select>::library ();
  if (library.has_interesting_description (al))
    return al.name ("description");
  return string ("");
}

Select::Implementation::Implementation (const AttributeList& al)
  : spec (al.check ("spec") ? new Spec (al.alist ("spec")) : NULL),
    spec_conv (NULL),
    factor (al.number ("factor")),
    offset (al.number ("offset")),
    negate (al.flag ("negate")),
    tag (Select::select_get_tag (al)),
    dimension (al.check ("dimension")
	       ? al.name ("dimension") : Syntax::Unknown ()),
    description (find_description (al))
{ }
  
Select::Implementation::~Implementation ()
{ 
  if (spec)
    delete spec;
}

double 
Select::convert (double value) const
{ return impl.convert (value); }

const string& 
Select::dimension () const
{ return impl.dimension; }

symbol
Select::tag () const
{ return impl.tag; }

const Soil* 
Select::soil () const
{ return NULL; }

int 
Select::size () const
{ return -1; }

symbol
Select::select_get_tag (const AttributeList& al)
{
  if (al.check ("tag"))
    return al.identifier ("tag");

  vector<symbol> path  = al.identifier_sequence ("path");
  
  if (path.size () > 0)
    return path[path.size () - 1];

  static const symbol none_symbol ("<none>");
  return none_symbol;
}

const symbol Select::wildcard ("*");

// Output routines.
void 
Select::output_number (const double)
{ throw ("This log selection can't log numbers."); }

void 
Select::output_integer (const int)
{ throw ("This log selection can't log integers."); }

void 
Select::output_name (const symbol)
{ throw ("This log selection can't log names."); }

void 
Select::output_array (const vector<double>&, const Soil*, Treelog&)
{ throw ("This log selection can't log arrays."); }

void 
Select::output_time (const Time&)
{ throw ("This log selection can't log time values."); }

bool
Select::prevent_printing ()
{ return false; }

static bool check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  if (al.check ("spec"))
    {
      if (al.number ("factor") != 1.0)
	err.warning ("Specifying both 'spec' and 'factor' may conflict");
      else if (al.number ("offset") != 0.0)
	err.warning ("Specifying both 'spec' and 'offset' may conflict");
    }
  static bool has_warned_about_when = false;
  if (!has_warned_about_when && al.check ("when"))
    {
      err.warning ("The 'when' select parametere is obsolete.\n\
Set the 'handle' parameter instead.");
      has_warned_about_when = true;
    }
  return ok;
}

void 
Select::document (Format& format) const
{
  Format::Item item (format, tag ().name ());  
  format.special ("nbsp");
  format.text ("[");
  format.bold (dimension ());
  format.text ("]");
  if (impl.description != "")
    {
      format.hard_linebreak ();
      format.text (impl.description);
      format.soft_linebreak ();
    }
  else if (impl.spec)
    {
      if (impl.negate)
	{
	  format.special ("nbsp");
	  format.text ("(reversed)");
	}
      impl.spec->refer (format);
      format.hard_linebreak ();
      format.text (impl.spec->description ());
      format.soft_linebreak ();
    }
}

void 
Select::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist);
  alist.add ("base_model", "common");
  syntax.add ("tag", Syntax::String, Syntax::OptionalConst,
	      "Tag to identify the column.\n\
These will be printed in the first line of the log file.\n\
The default tag is the last element in the path.");
  syntax.add ("dimension", Syntax::String, Syntax::OptionalConst,
	      "The unit for numbers in this column.\n\
These will be printed in the second line of the log file.\n\
The character '&' will be replaced with the log timestep.\n\
If you do not specify the dimension explicitly, a value will\n\
be interfered from 'spec' if available.");
  syntax.add ("description", Syntax::String, Syntax::Const,
	      "A description of this column.");
  alist.add ("description", "\
This is not a model, but a list of parameters shared by all select models.");
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
The last attribute in the patch should be a number, a number sequence,\n\
a string, or an integer.  These are the only values which can be\n\
logged by this model.\n\
\n\
You can use the special value \"*\" to match everything at a given\n\
level, for example all crops.  This way the path can specify multiple\n\
values, they will be added before they are printed in the log file.\n\
All values that start with a \"$\" will work like \"*\".  They are intended\n\
to be mapped with the 'set' attribute in the 'table' log model.");
  syntax.add_check ("path", VCheck::min_size_1 ());
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
OBSOLETE.  If you set this variable, 'flux' will be set to true.\n\
This overwrites any direct setting of 'flux'.");
  syntax.add ("flux", Syntax::Boolean, Syntax::OptionalConst, "\
OBSOLETE.  This value will be used if 'handle' is not specified.\
A value of true then means 'sum', and false means 'current'.");
  syntax.add ("handle", Syntax::String, Syntax::OptionalConst, "\
This option determine how the specified variable should be logged.  \n\
\n\
min: Log the smallest value seen since last time the variable was logged.\n\
If 'accumulate' is true, use the smallest value ever.\n\
\n\
max: Log the largest value seen since last time the variable was logged.\n\
If 'accumulate' is true, use the largest value ever.\n\
\n\
average: Log the arithmetic average value seen since last time the\n\
variable was logged.\n\
If 'accumulate' is true, use the average of all values.\n\
\n\
geometric: Log the geometic average value seen since last time the\n\
variable was logged.\n\
If 'accumulate' is true, use the average of all values.\n\
\n\
sum: Accumulate value since last time the variable was logged.\n\
If 'accumulate' is true, accumulate since the start of the log.\n\
\n\
current: Log the current value for the variable.\n\
If 'accumulate' is true, the printed values will be accumulated..");
  static VCheck::Enum handle_check ("min", "max", "average", "geometric", 
                                    "sum", "current");
  syntax.add_check ("handle", handle_check);
  syntax.add ("interesting_content", Syntax::Boolean, Syntax::Const, "\
True if the content of this column is interesting enough to warrent an\n\
initial line in the log file.  This only affects non-flux variables.");
  alist.add ("interesting_content", true);
  syntax.add ("factor", Syntax::Unknown (), Check::none (), Syntax::Const, "\
Factor to multiply the calculated value with, before logging.");
  alist.add ("factor", 1.0);
  syntax.add ("offset", Syntax::Unknown (), Check::none (), Syntax::Const, "\
Offset to add to the calculated value, before logging.");
  alist.add ("offset", 0.0);
  syntax.add ("negate", Syntax::Boolean, Syntax::Const, "\
Switch sign of value.  I.e. upward fluxes become downward fluxes.");
  alist.add ("negate", false);
  syntax.add ("accumulate", Syntax::Boolean, Syntax::Const,
	      "Log accumulated values.");
  alist.add ("accumulate", false);
  syntax.add ("count", Syntax::Integer, Syntax::State, "\
Number of times the path has matched a variable since the last log entry.");
  alist.add ("count", 0);
}

const string
Select::default_dimension (const string& spec_dim) const
{ return spec_dim; }

const Units::Convert*
Select::special_convert (const string&, const string&)
{ return NULL; }

void 
Select::add_dest (Destination* d)
{ dest.add_dest (d); }

void 
Select::initialize (const map<symbol, symbol>& conv, double, double, 
		    const string& timestep)
{ 
  string spec_dim;
  if (impl.spec)
    spec_dim = default_dimension (impl.spec->dimension ());
  else
    spec_dim = Syntax::Unknown ();

  // Convert path according to mapping in 'conv'.
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const string& sname = path[i].name ();
      const map<symbol, symbol>::const_iterator entry = conv.find (path[i]);
      if (entry != conv.end ())
	path[i] = (*entry).second;
      else if (sname.size () > 0 && sname[0] == '$')
	path[i] = Select::wildcard;
    }

  if (impl.dimension == Syntax::Unknown ())
    impl.dimension = spec_dim;

  // Attempt to find convertion with original dimension.
  if (impl.spec)
    if (Units::can_convert (spec_dim, impl.dimension))
      impl.spec_conv = &Units::get_convertion (spec_dim, impl.dimension);
    else
      impl.spec_conv = special_convert (spec_dim, impl.dimension);

  // Replace '&' with timestep.
  string new_dim;
  string hour_dim;
  for (unsigned int i = 0; i < impl.dimension.length (); i++)
    if (impl.dimension[i] == '&')
      {
	new_dim += timestep;
	hour_dim += "h";
      }
    else
      {
	new_dim += impl.dimension[i];
	hour_dim += impl.dimension[i];
      }

  // Attempt to find convertion with new dimension.
  if (impl.spec && !impl.spec_conv)
    {
      if (Units::can_convert (spec_dim, hour_dim))
	impl.spec_conv = &Units::get_convertion (spec_dim, hour_dim);
    }

  // Use new dimension.
  impl.dimension = new_dim;
}

bool 
Select::check (Treelog& err) const
{
  string spec_dim;
  if (impl.spec)
    spec_dim = default_dimension (impl.spec->dimension ());
  else
    spec_dim = Syntax::Unknown ();
  return impl.check (spec_dim, err); 
}

bool 
Select::check_border (const Border&, 
                      const double /*from*/, const double /*to*/,
                      Treelog&) const
{ return true; }

Select::Select (const AttributeList& al)
  : name (al.name ("type")),
    impl (*new Implementation (al)),
    accumulate (al.flag ("accumulate")),
    handle (al.check ("handle")
            ? Handle (al.identifier ("handle"))
            : Handle ((al.check ("when") 
                       ||  (al.check ("flux") && al.flag ("flux")))
                      ? Handle::sum : Handle::current)),
    interesting_content (al.flag ("interesting_content")),
    count (al.integer ("count")),
    path (al.identifier_sequence ("path")),
    last_index (path.size () - 1),
    current_name (path[0]),
    is_active (false)
{ }

Select::~Select ()
{ delete &impl; }

static struct SelectSyntax
{
  SelectSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Select::load_syntax (syntax, alist);

    Librarian<Select>::add_base (alist, syntax);
  }
} Select_syntax;
