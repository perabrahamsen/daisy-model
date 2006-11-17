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
#include "number.h"
#include "scope.h"
#include "check.h"
#include "vcheck.h"
#include "units.h"
#include "symbol.h"
#include "format.h"
#include "submodel.h"
#include "mathlib.h"
#include <numeric>
#include <set>

Handle::handle_t
Handle::symbol2handle (symbol s)
{
  static struct sym_set_t : std::map<symbol, handle_t>
  {
    sym_set_t ()
    {
      static symbol min_symbol ("min");
      insert (std::pair<symbol,handle_t> (min_symbol, min));
      static symbol max_symbol ("max");
      insert (std::pair<symbol,handle_t> (max_symbol, max));
      static symbol average_symbol ("average");
      insert (std::pair<symbol,handle_t> (average_symbol, average));
      static symbol geometric_symbol ("geometric");
      insert (std::pair<symbol,handle_t> (geometric_symbol, geometric));
      static symbol sum_symbol ("sum");
      insert (std::pair<symbol,handle_t> (sum_symbol, sum));
      static symbol current_symbol ("current");
      insert (std::pair<symbol,handle_t> (current_symbol, current));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  

template<>
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
    const std::vector<symbol> submodels_and_attribute;
    
    // Use.
    const Syntax& leaf_syntax (Syntax&) const;
    const std::string& leaf_name () const;
    symbol dimension () const;
    std::string description () const;
    void refer (Format&) const;

    // Create and Destroy.
    static bool check_path (const std::vector<symbol>& path,
			    const Syntax& syntax,
			    const AttributeList& alist,
			    Treelog& err);
    static bool check_alist (const AttributeList& al, Treelog& err);
    static void load_syntax (Syntax& syntax, AttributeList&);
    Spec (const AttributeList&);
    ~Spec ();
  };
  std::auto_ptr<Spec> spec;

  // We need a scope for the expression.
  mutable struct ScopeX : public Scope
  {
    // Content.
    double value;
    symbol dim;

    // Interface.
    void tick (const Scope&, Treelog&)
    { }
    bool has_number (symbol name) const
    {
      static const symbol x_symbol ("x");
      return name == x_symbol; 
    }
    double number (symbol) const
    { return value; }
    symbol dimension (symbol) const
    { return dim;}

    // Create.
    ScopeX ()
      : value (-42.42e42),
        dim (Syntax::Unknown ())
    { }
  } scope;
    

  // Content.
  const Units::Convert* spec_conv; // Convert value.
  std::auto_ptr<Number> expr;   // - || -
  const bool negate;            // - || -
  double convert (double) const; // - || -
  const symbol tag;		// Name of this entry.
  symbol dimension;		// Physical dimension of this entry.
  const std::string description;

  // Create and Destroy.
  bool check (symbol spec_dim, Treelog& err) const;
  static std::string find_description (const AttributeList&);
  static Number* get_expr (Block& al);
  Implementation (Block&);
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

const std::string&
Select::Implementation::Spec::leaf_name () const
{ 
  daisy_assert (submodels_and_attribute.size () > 0);
  return submodels_and_attribute.back ().name (); 
}

symbol
Select::Implementation::Spec::dimension () const
{
  Syntax buffer;
  const Syntax& syntax = leaf_syntax (buffer);
  if (syntax.lookup (leaf_name ()) == Syntax::Number)
    return symbol (syntax.dimension (leaf_name ()));
  else
    return Syntax::unknown ();
}

std::string /* can't return reference because buffer is automatic */
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
  std::string aref = library_name.name () + "-" + model_name.name ();
  format.text ("(section");
  format.special ("nbsp");
  if (is_fixed)
    format.ref ("fixed", model_name.name ());
  else
    format.ref ("model", aref);
  format.text (")");
  for (unsigned int i = 0; i < submodels_and_attribute.size (); i++)
    {
      const std::string name = submodels_and_attribute[i].name ();
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
Select::Implementation::Spec::check_path (const std::vector<symbol>& path,
					  const Syntax& top_syntax,
					  const AttributeList& top_alist,
					  Treelog& err)
{
  const Syntax* syntax = &top_syntax;
  const AttributeList* alist = &top_alist;

  bool ok = true;
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const std::string name = path[i].name ();
      bool last = (i + 1 == path.size ());
      const Syntax::type type = syntax->lookup (name);

      if (!last)
	{
	  if (type != Syntax::AList)
	    {
	      err.error ("'" + name + "': no such submodel");
	      ok = false;
	      break;
	    }

          if (syntax->size (name) != Syntax::Singleton || !alist->check (name))
            alist = &syntax->default_alist (name);
          else
            alist = &alist->alist (name);
	  syntax = &syntax->syntax (name);
          
          if (alist->check ("submodel"))
            err.warning ("'" + name + "' is a fixed '" 
                         + alist->name ("submodel") + "' component");
	}
      else if (type == Syntax::Error)
	{
	  err.error ("'" + name + "': no such attribute");
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
  const std::vector<symbol> submodels_and_attribute 
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
	  if (!check_path (submodels_and_attribute, syntax, alist, err))
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
	  const Syntax& syntax = library.syntax (model_name);
	  const AttributeList& alist = library.lookup (model_name);
	  if (!check_path (submodels_and_attribute, syntax, alist, err))
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

  scope.value = value;
  value = expr->value (scope);

  if (spec_conv)
    value =  spec_conv->operator() (value);

  if (negate)
    value = -value;

  return value;
}

// Create and Destroy.
bool 
Select::Implementation::check (const symbol spec_dim, Treelog& err) const
{
  bool ok = true;
  if (spec.get () && !spec_conv && spec->dimension () != Syntax::unknown ())
    err.warning ("Don't know how to convert [" + spec_dim
                 + "] to [" + dimension + "]");
  return ok;
}
  
std::string 
Select::Implementation::find_description (const AttributeList& al)
{
  const Library& library = Librarian<Select>::library ();
  if (library.has_interesting_description (al))
    return al.name ("description");
  return "";
}

Number*
Select::Implementation::get_expr (Block& al)
{
  if (al.check ("expr"))
    return Librarian<Number>::build_item (al, "expr");

  // Support for old factor + offset style.
  struct NumberFactor : public Number
  {
    const double factor;
    void tick (const Scope&, Treelog&)
    { }
    bool missing (const Scope&) const
    { return false; }
    double value (const Scope& scope) const
    { return static_cast<const ScopeX&> (scope).value * factor; }
    symbol dimension (const Scope& scope) const
    { return static_cast<const ScopeX&> (scope).dim; }
    bool initialize (Treelog&)
    { return true; }
    bool check (const Scope&, Treelog&) const
    { return true; }
    explicit NumberFactor (Block& al, const double f)
      : Number (al),
        factor (f)
    { }
  };

  struct NumberLinear : public Number
  {
    const double factor;
    const double offset;
    void tick (const Scope&, Treelog&)
    { }
    bool missing (const Scope&) const
    { return false; }
    double value (const Scope& scope) const
    { return static_cast<const ScopeX&> (scope).value * factor + offset; }
    symbol dimension (const Scope& scope) const
    { return static_cast<const ScopeX&> (scope).dim; }
    bool initialize (Treelog&)
    { return true; }
    bool check (const Scope&, Treelog&) const
    { return true; }
    explicit NumberLinear (Block& al, const double f, const double o)
      : Number (al),
        factor (f),
        offset (o)
    { }
  };
  const double factor = al.number ("factor");
  const double offset = al.number ("offset");

  if (std::isnormal (offset))
    return new NumberLinear (al,factor, offset);

  if (!approximate (factor, 1.0, 1.0e-7))
    return new NumberFactor (al, factor);
  
  // No change.
  struct NumberX : public Number
  {
    void tick (const Scope&, Treelog&)
    { }
    bool missing (const Scope&) const
    { return false; }
    double value (const Scope& scope) const
    { return static_cast<const ScopeX&> (scope).value; }
    symbol dimension (const Scope& scope) const
    { return static_cast<const ScopeX&> (scope).dim; }
    bool initialize (Treelog&)
    { return true; }
    bool check (const Scope&, Treelog&) const
    { return true; }
    explicit NumberX (Block& al)
      : Number (al)
    { }
  };
  return new NumberX (al);
}

static const symbol flux_top_symbol ("flux_top");

Select::Implementation::Implementation (Block& al)
  : spec (al.check ("spec")
	  ? new Spec (al.alist ("spec")) 
	  : NULL),
    spec_conv (NULL),
    expr (get_expr (al)),
    negate (al.flag ("negate")
            // Kludge to negate the meaning of negate for "flux_top".
            != Librarian<Select>::library ()
            /**/ .is_derived_from (al.identifier ("type"), flux_top_symbol)),
    tag (Select::select_get_tag (al.alist ())),
    dimension (al.check ("dimension")
	       ? al.name ("dimension") : Syntax::Unknown ()),
    description (find_description (al.alist ()))
{ }
  
Select::Implementation::~Implementation ()
{ }

double 
Select::convert (double value) const
{ return impl->convert (value); }

symbol
Select::dimension () const
{ return impl->dimension; }

symbol
Select::tag () const
{ return impl->tag; }

const Geometry* 
Select::geometry () const
{ return NULL; }

int 
Select::size () const
{ return -1; }

symbol
Select::select_get_tag (const AttributeList& al)
{
  if (al.check ("tag"))
    return al.identifier ("tag");

  std::vector<symbol> path  = al.identifier_sequence ("path");
  
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
Select::output_array (const std::vector<double>&, 
                      const Geometry*, const Soil*, Treelog&)
{ throw ("This log selection can't log arrays."); }

bool
Select::prevent_printing ()
{ return false; }

static bool check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  if (al.check ("spec"))
    {
      if (!approximate (al.number ("factor"), 1.0, 1e-7))
	err.warning ("Specifying both 'spec' and 'factor' may conflict");
      else if (std::isnormal (al.number ("offset")))
	err.warning ("Specifying both 'spec' and 'offset' may conflict");
    }
  if (al.check ("expr"))
    {
      if (!approximate (al.number ("factor"), 1.0, 1e-7))
        {
          err.error ("Can't specify both 'expr' and 'factor'");
          ok = false;
        }
      else if (std::isnormal (al.number ("offset")))
        {
          err.error ("Can't specify both 'expr' and 'offset'");
          ok = false;
        }
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
  format.bold (dimension ().name ());
  format.text ("]");
  if (impl->description != "")
    {
      format.hard_linebreak ();
      format.text (impl->description);
      format.soft_linebreak ();
    }
  else if (impl->spec.get ())
    {
      if (impl->negate)
	{
	  format.special ("nbsp");
	  format.text ("(reversed)");
	}
      impl->spec->refer (format);
      format.hard_linebreak ();
      format.text (impl->spec->description ());
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
  syntax.add ("expr", Librarian<Number>::library (), 
              Syntax::OptionalConst, Syntax::Singleton, "\
Expression for findig the value for the log file, given the internal\n\
value 'x'.  For example '(expr (ln x))' will give you the natural\n\
logarithm of the value.");  
  syntax.add ("factor", Syntax::Unknown (), Check::none (), Syntax::Const, "\
Factor to multiply the calculated value with, before logging.\n\
OBSOLETE: Use 'expr' instead.");
  alist.add ("factor", 1.0);
  syntax.add ("offset", Syntax::Unknown (), Check::none (), Syntax::Const, "\
Offset to add to the calculated value, before logging.\n\
OBSOLETE: Use 'expr' instead.");
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

symbol
Select::default_dimension (const symbol spec_dim) const
{ return spec_dim; }

const Units::Convert*
Select::special_convert (const symbol, const symbol)
{ return NULL; }

void 
Select::add_dest (Destination* d)
{ dest.add_dest (d); }

bool
Select::initialize (const Volume&, 
		    const std::string& timestep, Treelog& msg)
{ 
  symbol spec_dim;
  if (impl->spec.get ())
    spec_dim = default_dimension (impl->spec->dimension ());
  else
    spec_dim = Syntax::unknown ();

  // Let the expression modify the dimension.
  impl->scope.dim = spec_dim;
  if (impl->expr.get ())
    { 
      if (!impl->expr->initialize (msg) 
          || !impl->expr->check (impl->scope, msg))
        {
          msg.error ("Bad expression");
          return false;
        }
      impl->expr->tick  (impl->scope, msg);
      spec_dim = impl->expr->dimension (impl->scope);
    }

  if (impl->dimension == Syntax::unknown ())
    impl->dimension = spec_dim;

  // Attempt to find convertion with original dimension.
  if (impl->spec.get ())
    if (Units::can_convert (spec_dim, impl->dimension))
      impl->spec_conv = &Units::get_convertion (spec_dim, impl->dimension);
    else
      impl->spec_conv = special_convert (spec_dim, impl->dimension);

  // Replace '&' with timestep.
  std::string new_dim;
  std::string hour_dim;
  const std::string impl_dim = impl->dimension.name ();
  for (unsigned int i = 0; i < impl_dim.length (); i++)
    if (impl_dim[i] == '&')
      {
	new_dim += timestep;
	hour_dim += "h";
      }
    else
      {
	new_dim += impl_dim[i];
	hour_dim += impl_dim[i];
      }

  // Attempt to find convertion with new dimension.
  if (impl->spec.get () && !impl->spec_conv)
    {
      if (Units::can_convert (spec_dim, symbol (hour_dim)))
	impl->spec_conv = &Units::get_convertion (spec_dim, symbol (hour_dim));
    }

  // Use new dimension.
  impl->dimension = symbol (new_dim);

  return true;
}

bool 
Select::check (Treelog& err) const
{
  symbol spec_dim;
  if (impl->expr.get ())
    spec_dim = impl->expr->dimension (impl->scope);
  else 
    spec_dim = impl->scope.dim;
  
  return impl->check (spec_dim, err); 
}

bool 
Select::check_border (const Border&, 
                      const Volume&,
                      Treelog&) const
{ return true; }

Select::Select (Block& al)
  : name (al.name ("type")),
    impl (new Implementation (al)),
    accumulate (al.flag ("accumulate")),
    handle (al.check ("handle")
            ? Handle (al.identifier ("handle"))
            : Handle ((al.check ("when") 
                       ||  (al.check ("flux")
			    && al.flag ("flux")))
                      ? Handle::sum : Handle::current)),
    interesting_content (al.flag ("interesting_content")),
    count (al.integer ("count")),
    path (al.identifier_sequence ("path")),
    last_index (path.size () - 1),
    current_name (path[0]),
    is_active (false)
{ }

Select::~Select ()
{ }

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
