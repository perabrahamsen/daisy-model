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

#define BUILD_DLL

#include "select.h"
#include "condition.h"
#include "block_model.h"
#include "column.h"
#include "geometry.h"
#include "number.h"
#include "scope_id.h"
#include "metalib.h"
#include "library.h"
#include "frame_submodel.h"
#include "frame_model.h"
#include "check.h"
#include "vcheck.h"
#include "format.h"
#include "submodeler.h"
#include "mathlib.h"
#include "librarian.h"
#include "convert.h"
#include "treelog.h"
#include <numeric>
#include <map>

Select::Handle::handle_t
Select::Handle::symbol2handle (symbol s)
{
  static struct sym_set_t : std::map<symbol, handle_t>
  {
    sym_set_t ()
    {
      insert (std::pair<symbol,handle_t> ("min", min));
      insert (std::pair<symbol,handle_t> ("max", max));
      insert (std::pair<symbol,handle_t> ("average", average));
      insert (std::pair<symbol,handle_t> ("sum", sum));
      insert (std::pair<symbol,handle_t> ("content_sum", content_sum));
      insert (std::pair<symbol,handle_t> ("current", current));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  

Select::Multi::handle_t
Select::Multi::symbol2handle (symbol s)
{
  static struct sym_set_t : std::map<symbol, handle_t>
  {
    sym_set_t ()
    {
      insert (std::pair<symbol,handle_t> ("min", min));
      insert (std::pair<symbol,handle_t> ("max", max));
      insert (std::pair<symbol,handle_t> ("sum", sum));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  

const VCheck& 
Select::multi_check ()
{
  static VCheck::Enum check ("min", "max", "sum");
  return check;
}

const char *const Select::description = "Select part of state.";

const char *const Select::component = "select";

symbol
Select::library_id () const
{
  static const symbol id (component);
  return id;
}
  
struct Select::Implementation
{
  // Spec.
  struct Spec
  {
    // Content.
    const Metalib& metalib;
    const symbol library_name;
    const symbol model_name;
    const std::vector<symbol> submodels_and_attribute;
    
    // Use.
    const Frame& leaf_frame () const;
    symbol leaf_name () const;
    symbol dimension () const;
    symbol description () const;
    void refer (Format&) const;

    // Create and Destroy.
    static bool check_path (const std::vector<symbol>& path,
                            const Frame& frame,
                            Treelog& err);
    static bool check_alist (const Metalib&, const Frame&, Treelog&);
    static void load_syntax (Frame&);
    Spec (const Block&);
    ~Spec ();
  };
  std::unique_ptr<Spec> spec;

  // We need a scope for the expression.
  static const symbol x_symbol;
  mutable ScopeID scope;
    
  // Content.
  const Convert* spec_conv; // Convert value.
  std::unique_ptr<Number> expr;   // - || -
  const bool negate;            // - || -
  double convert (double) const; // - || -
  const symbol tag;             // Name of this entry.
  symbol dimension;             // Physical dimension of this entry.
  const symbol documentation;

  // Create and Destroy.
  bool check (symbol spec_dim, Treelog& err) const;
  static Number* get_expr (const BlockModel& al);
  Implementation (const BlockModel&);
  ~Implementation ();
};

const Frame&
Select::Implementation::Spec::leaf_frame () const
{
  const Frame* frame;

  if (library_name == "fixed")
    frame = Librarian::submodel_frame (model_name).get ();
  else
    {
      const Library& library = metalib.library (library_name);
      frame = &library.model (model_name);
    }

  for (unsigned int i = 0; i < submodels_and_attribute.size () - 1; i++)
    frame = frame->default_frame (submodels_and_attribute[i]).get ();

  return *frame;
}

symbol
Select::Implementation::Spec::leaf_name () const
{ 
  daisy_assert (submodels_and_attribute.size () > 0);
  return submodels_and_attribute.back (); 
}

symbol
Select::Implementation::Spec::dimension () const
{
  const Frame& frame = leaf_frame ();
  if (frame.lookup (leaf_name ()) == Attribute::Number)
    return frame.dimension (leaf_name ());
  else
    return Attribute::Unknown ();
}

symbol
Select::Implementation::Spec::description () const
{ 
  return leaf_frame ().description (leaf_name ()); 
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
                                          const Frame& top_frame,
                                          Treelog& err)
{
  const Frame* frame = &top_frame;

  bool ok = true;
  for (unsigned int i = 0; i < path.size (); i++)
    {
      const std::string name = path[i].name ();
      bool last = (i + 1 == path.size ());
      const Attribute::type type = frame->lookup (name);

      if (!last)
        {
          if (type != Attribute::Submodel)
            {
              err.error ("'" + name + "': no such submodel (is a " 
                         + Attribute::type_name (type) + ")");
              ok = false;
              break;
            }
          const symbol submodel_name = frame->submodel_name (name);
          if (submodel_name != Attribute::None ())
            err.warning ("'" + name + "' is a fixed '" 
                         + submodel_name + "' component");

          frame = &frame->submodel (name);
        }
      else if (type == Attribute::Error)
        {
          err.error ("'" + name + "': no such attribute");
          ok = false;
          break;
        }
    }
  return ok;
}

bool 
Select::Implementation::Spec::check_alist (const Metalib& metalib,
                                           const Frame& al,
                                           Treelog& err)
{
  bool ok = true;
  
  const symbol library_name = al.name ("library");
  const symbol model_name = al.name ("model");
  const std::vector<symbol> submodels_and_attribute 
    = al.name_sequence ("submodels_and_attribute");

  if (submodels_and_attribute.size () < 1)
    {
      err.entry ("You must specify an attribute");
      ok = false;
    }
  if (library_name == "fixed")
    {
      if (!Librarian::submodel_registered (model_name))
        {
          err.entry ("'" + model_name + "': no such submodel");
          ok = false;
        }
      else
        {
          const Frame& frame = *Librarian::submodel_frame (model_name).get ();
          if (!check_path (submodels_and_attribute, frame, err))
            ok = false;
        }
    }
  else if (!metalib.exist (library_name))
    {
      err.entry ("'" + library_name + "': no such library");
      ok = false;
    }
  else
    {
      const Library& library = metalib.library (library_name);
      
      if (!library.check (model_name))
        {
          err.entry ("'" + model_name + "': no such model");
          ok = false;
        }
      else
        {
          const FrameModel& frame = library.model (model_name);
          if (!check_path (submodels_and_attribute, frame, err))
            ok = false;
        }
    }
  return ok;
}

void 
Select::Implementation::Spec::load_syntax (Frame& frame)
{ 
  frame.add_check (check_alist);
  frame.declare_string ("library", Attribute::Const, "\
Name of library where the attribute belong.\n\
Use 'fixed' to denote a fixed component.");
  frame.declare_string ("model", Attribute::Const, "\
Name of model or fixed component where the attribute belongs.");
  frame.declare_string ("submodels_and_attribute", 
                 Attribute::Const, Attribute::Variable, "\
Name of submodels and attribute.");
  frame.order ("library", "model", "submodels_and_attribute");
}

Select::Implementation::Spec::Spec (const Block& al)
  : metalib (al.metalib ()),
    library_name (al.name ("library")),
    model_name (al.name ("model")),
    submodels_and_attribute (al.name_sequence
                             ("submodels_and_attribute"))
{ }

Select::Implementation::Spec::~Spec ()
{ }

const symbol
Select::Implementation::x_symbol ("x");

double 
Select::Implementation::convert (double value) const
{ 

  scope.set (x_symbol, value);
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
  if (spec.get () && !spec_conv && spec->dimension () != Attribute::Unknown ())
    err.warning ("Don't know how to convert [" + spec_dim
                 + "] to [" + dimension + "]");
  return ok;
}
  
Number*
Select::Implementation::get_expr (const BlockModel& al)
{
  if (al.check ("expr"))
    return Librarian::build_item<Number> (al, "expr");

  // Support for old factor + offset style.
  struct NumberFactor : public Number
  {
    const double factor;
    void tick (const Units&, const Scope&, Treelog&)
    { }
    bool missing (const Scope&) const
    { return false; }
    double value (const Scope& scope) const
    { return scope.number (x_symbol) * factor; }
    symbol dimension (const Scope& scope) const
    { return scope.dimension (x_symbol); }
    bool initialize (const Units&, const Scope&, Treelog&)
    { return true; }
    bool check (const Units&, const Scope&, Treelog&) const
    { return true; }
    explicit NumberFactor (const BlockModel& al, const double f)
      : Number (al),
        factor (f)
    { }
  };

  struct NumberLinear : public Number
  {
    const double factor;
    const double offset;
    void tick (const Units&, const Scope&, Treelog&)
    { }
    bool missing (const Scope&) const
    { return false; }
    double value (const Scope& scope) const
    { return scope.number (x_symbol) * factor + offset; }
    symbol dimension (const Scope& scope) const
    { return scope.dimension (x_symbol); }
    bool initialize (const Units&, const Scope&, Treelog&)
    { return true; }
    bool check (const Units&, const Scope&, Treelog&) const
    { return true; }
    explicit NumberLinear (const BlockModel& al, const double f, const double o)
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
    void tick (const Units&, const Scope&, Treelog&)
    { }
    bool missing (const Scope&) const
    { return false; }
    double value (const Scope& scope) const
    { return scope.number (x_symbol); }
    symbol dimension (const Scope& scope) const
    { return scope.dimension (x_symbol); }
    bool initialize (const Units&, const Scope&, Treelog&)
    { return true; }
    bool check (const Units&, const Scope&, Treelog&) const
    { return true; }
    explicit NumberX (const BlockModel& al)
      : Number (al)
    { }
  };
  return new NumberX (al);
}

static const symbol flux_top_symbol ("flux_top");

Select::Implementation::Implementation (const BlockModel& al)
  : spec (al.check ("spec")
          ? submodel<Spec> (al, "spec")
          : NULL),
    scope (x_symbol, Attribute::Unknown ()),
    spec_conv (NULL),
    expr (get_expr (al)),
    negate (al.flag ("negate")
            // Kludge to negate the meaning of negate for "flux_top".
            != al.metalib ().library (Select::component)
            /**/ .is_derived_from (al.type_name (), flux_top_symbol)),
    tag (Select::select_get_tag (al)),
    dimension (al.check ("dimension")
               ? al.name ("dimension") : Attribute::Unknown ()),
    documentation (al.name ("documentation", Attribute::None ()))
{ }
  
Select::Implementation::~Implementation ()
{ }

double 
Select::convert (double value) const
{ return impl->convert (value); }

symbol
Select::get_description () const
{
  if (impl->documentation != Attribute::None ())
    return impl->documentation;
  if (impl->spec.get ())
    {
      std::string d = impl->spec->description ().name ();
      if (impl->negate)
        d += " (reversed)";
      return d;
    }
  return Attribute::None ();
}

int
Select::original_size () const
{ 
  if (!impl->spec.get ())
    return Attribute::Unspecified;

  const Frame& frame = impl->spec->leaf_frame ();
  const symbol name = impl->spec->leaf_name ();
  return frame.type_size (name);
}

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
{ return Attribute::Singleton; }

symbol
Select::select_get_tag (const BlockModel& al)
{
  if (al.check ("tag"))
    return al.name ("tag");

  std::vector<symbol> path  = al.name_sequence ("path");
  
  if (path.size () > 0)
    return path[path.size () - 1];

  static const symbol none_symbol ("<none>");
  return none_symbol;
}

symbol
Select::select_get_tag (const Frame& al)
{
  if (al.check ("tag"))
    return al.name ("tag");

  std::vector<symbol> path  = al.name_sequence ("path");
  
  if (path.size () > 0)
    return path[path.size () - 1];

  static const symbol none_symbol ("<none>");
  return none_symbol;
}

const symbol Select::wildcard ("*");

void 
Select::set_column (const Column&, Treelog&)
{ }

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
Select::output_array (const std::vector<double>&)
{ throw ("This log selection can't log arrays."); }

void 
Select::document (Format& format) const
{
  Format::Item item (format, tag ().name ());  
  format.special ("nbsp");
  format.text ("[");
  format.bold (dimension ().name ());
  format.text ("]");
  if (impl->documentation != Attribute::None ())
    {
      format.hard_linebreak ();
      format.text (impl->documentation);
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
      format.text (impl->spec->description ().name ());
      format.soft_linebreak ();
    }
}

symbol
Select::default_dimension (const symbol spec_dim) const
{ return spec_dim; }

const Convert*
Select::special_convert (const Units&, const symbol, const symbol)
{ return NULL; }

void 
Select::add_dest (Destination* d)
{ dest.add_dest (d); }

bool
Select::initialize (const Units& units, const Volume&, 
                    const symbol timestep, Treelog& msg)
{ 
  symbol spec_dim;
  if (impl->spec.get ())
    {
      spec_dim = default_dimension (impl->spec->dimension ());
      if (handle == Handle::sum)
        spec_dim = Units::multiply (spec_dim, Units::h ());
    }
  else
    spec_dim = Attribute::Unknown ();

  // Let the expression modify the dimension.
  impl->scope.set_dimension (Implementation::x_symbol, spec_dim);
  if (impl->expr.get ())
    { 
      if (!impl->expr->initialize (units, impl->scope, msg) 
          || !impl->expr->check (units, impl->scope, msg))
        {
          msg.error ("Bad expression");
          return false;
        }
      impl->expr->tick  (units, impl->scope, msg);
      spec_dim = impl->expr->dimension (impl->scope);
    }

  if (impl->dimension == Attribute::Unknown ())
    impl->dimension = spec_dim;

  // Attempt to find convertion with original dimension.
  if (impl->spec.get ())
    {
      if (units.can_convert (spec_dim, impl->dimension))
        impl->spec_conv = &units.get_convertion (spec_dim, impl->dimension);
      else
        impl->spec_conv = special_convert (units, spec_dim, impl->dimension);
    }

  // Replace '&' with timestep.
  std::string new_dim;
  const std::string impl_dim = impl->dimension.name ();
  for (unsigned int i = 0; i < impl_dim.length (); i++)
    if (impl_dim[i] == '&')
      new_dim += timestep.name ();
    else
      new_dim += impl_dim[i];

  // Attempt to find convertion with new dimension.
  if (impl->spec.get () && !impl->spec_conv)
    {
      if (units.can_convert (spec_dim, new_dim))
        impl->spec_conv = &units.get_convertion (spec_dim, new_dim);
    }

  // Use new dimension.
  impl->dimension = new_dim;

  return true;
}

bool 
Select::check (Treelog& err) const
{
  symbol spec_dim;
  if (impl->expr.get ())
    spec_dim = impl->expr->dimension (impl->scope);
  else 
    spec_dim = impl->scope.dimension (Implementation::x_symbol);
  
  return impl->check (spec_dim, err); 
}

bool 
Select::check_border (const Border&, 
                      const Volume&,
                      Treelog&) const
{ return true; }

Select::Select (const BlockModel& al)
  : name (al.type_name ()),
    impl (new Implementation (al)),
    accumulate (al.flag ("accumulate")),
    handle (al.check ("handle")
            ? Handle (al.name ("handle"))
            : Handle ((al.check ("when") 
                       ||  (al.check ("flux")
                            && al.flag ("flux")))
                      ? Handle::sum : Handle::current)),
    multi (al.name ("multi")),
    interesting_content (al.flag ("interesting_content", 
                                  handle == Handle::current)),
    first_result (true),
    first_small (true),
    dt (0.0),
    path (al.name_sequence ("path")),
    last_index (path.size () - 1),
    current_name (path[0]),
    relative_weight (1.0),
    total_weight (0.0),
    is_active (false)
{ }

Select::~Select ()
{ }

static struct SelectInit : public DeclareComponent 
{
  SelectInit ()
    : DeclareComponent (Select::component, Select::description)
  { }
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
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

  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.add_check (check_alist);
    frame.declare_string ("documentation", Attribute::OptionalConst, "\
Documentation for this entry.");
    frame.declare_string ("tag", Attribute::OptionalConst,
                   "Tag to identify the column.\n\
These will be printed in the first line of the log file.\n\
The default tag is the last element in the path.");
    frame.declare_string ("dimension", Attribute::OptionalConst,
                   "The unit for numbers in this column.\n\
These will be printed in the second line of the log file.\n\
The character '&' will be replaced with the log timestep.\n\
If you do not specify the dimension explicitly, a value will\n\
be interfered from 'spec' if available.");
    frame.declare_string ("path", Attribute::Const, Attribute::Variable, "\
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
    frame.set_check ("path", VCheck::min_size_1 ());
    frame.declare_submodule ("spec", Attribute::OptionalConst, "\
Specification for the attribute to be logged of the form\n\
\n\
  library model submodel* attribute\n\
\n\
Unlike path, the attribute may occur several different places in the\n\
simulation, if the model is used at several places.  Also, there is no\n\
wildcards, so only a single model can be matches.  The spec is used for\n\
helping Daisy establish a unique dimension and description for the\n\
attribute.", Select::Implementation::Spec::load_syntax);
    frame.declare_object ("when", Condition::component,
                          Attribute::OptionalConst, Attribute::Singleton,
                          "\
OBSOLETE.  If you set this variable, 'flux' will be set to true.\n\
This overwrites any direct setting of 'flux'.");
    frame.declare_boolean ("flux", Attribute::OptionalConst, "\
OBSOLETE.  This value will be used if 'handle' is not specified.\
A value of true then means 'sum', and false means 'current'.");
    frame.declare_string ("handle", Attribute::OptionalConst, "\
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
sum: Accumulate flux value since last time the variable was logged.\n\
If 'accumulate' is true, accumulate since the start of the log.\n\
\n\
sum: Accumulate content value since last time the variable was logged.\n\
If 'accumulate' is true, accumulate since the start of the log.\n\
\n\
current: Log the current value for the variable.\n\
If 'accumulate' is true, the printed values will be accumulated.");
    static VCheck::Enum handle_check ("min", "max", "average", 
                                      "sum", "content_sum", "current");
    frame.set_check ("handle", handle_check);
    frame.declare_string ("multi", Attribute::OptionalConst, "\
This option determine how to handle mutiple matches within a timestep.\n\
This could be two crops on the same column, or one crop on two columns.\n \
\n\
min: Use smallest value\n\
\n\
max: Use largest value\n\
\n\
sum: Use the sum of all matches, weighted by relative column area if\n\
the matches are from different columns.");
    frame.set_check ("multi", Select::multi_check ());
    frame.set ("multi", "sum");
    frame.declare_boolean ("interesting_content", Attribute::OptionalConst, "\
True if the content of this column is interesting enough to warrent an\n\
initial line in the log file.\n\
By default, this is true iff 'handle' is 'current'.");
    frame.declare_object ("expr", Number::component, 
                          Attribute::OptionalConst, Attribute::Singleton, "\
Expression for findig the value for the log file, given the internal\n\
value 'x'.  For example '(expr (ln x))' will give you the natural\n\
logarithm of the value.");  
    frame.declare ("factor", Attribute::Unknown (), Check::none (), Attribute::Const, "\
Factor to multiply the calculated value with, before logging.\n\
OBSOLETE: Use 'expr' instead.");
    frame.set ("factor", 1.0);
    frame.declare ("offset", Attribute::Unknown (), Check::none (), Attribute::Const, "\
Offset to add to the calculated value, before logging.\n\
OBSOLETE: Use 'expr' instead.");
    frame.set ("offset", 0.0);
    frame.declare_boolean ("negate", Attribute::Const, "\
Switch sign of value.  I.e. upward fluxes become downward fluxes.");
    frame.set ("negate", false);
    frame.declare_boolean ("accumulate", Attribute::Const,
                   "Log accumulated values.");
    frame.set ("accumulate", false);
  }
} Select_init;

// select.C ends here.
