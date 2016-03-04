// number_const.C -- Simple number objects.
// 
// Copyright 2004, 2005 Per Abrahamsen and KVL.
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

#include "number.h"
#include "block_model.h"
#include "scope.h"
#include "units.h"
#include "unit.h"
#include "assertion.h"
#include "librarian.h"
#include "library.h"
#include "treelog.h"
#include "frame.h"
#include <sstream>

struct NumberConst : public Number
{
  // Parameters.
  const double val;
  const Unit& unit_;

  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  bool missing (const Scope&) const
  { return false; }
  double value (const Scope&) const
  { return val; }
  symbol dimension (const Scope&) const
  { return unit_.native_name (); }
  const Unit& unit () const
  { return unit_; }

  // Create.
  bool initialize (const Units&, const Scope&, Treelog&)
  { return true; }
  bool check (const Units& units, const Scope&, Treelog& msg) const
  { 
    bool ok = true;
    if (units.is_error (unit_))
      {
        msg.error ("Bad unit");
        ok = false;
      }
    return ok;
  }
  explicit NumberConst (const BlockModel& al)
    : Number (al),
      val (al.number ("value")),
      unit_ (al.units ().get_unit (al.name ("value")))
  { 
    if (al.units ().is_error (unit_))
      al.msg ().warning ("Unknown unit '" + al.name ("value") + "'");
  }
  explicit NumberConst (const BlockModel& al, const symbol key)
    : Number (al),
      val (al.number (key)),
      unit_ (al.units ().get_unit (al.find_frame (key).dimension (key)))
  { 
    if (al.units ().is_error (unit_))
      al.msg ().warning ("Unknown unit '"
                         + al.find_frame (key).dimension (key) + "'");
  }
};

static struct NumberConstSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberConst (al); }
  NumberConstSyntax ()
    : DeclareModel (Number::component, "const", 
	       "Always give the specified value.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare ("value", Attribute::User (), Attribute::Const,
		"Fixed value for this number.");
    frame.order ("value");
  }
} NumberConst_syntax;

// The 'x' model.

struct NumberX : public Number
{
  static const symbol name;

  // Parameters.
  symbol title () const
  { return name; }

  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  symbol dimension (const Scope& scope) const
  { 
    return scope.dimension (name); 
  }
  bool missing (const Scope& scope) const
  { return !scope.check (name); }
  double value (const Scope& scope) const
  {  return scope.number ( name); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    if (scope.lookup (name) != Attribute::Number)
      {
        msg.error ("'" + name + "' is not a number");
        return false;
      }
    return true;
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  {
    bool ok = true;
    if (scope.lookup (name) != Attribute::Number)
      {
        msg.error ("'x' is not a number");
        ok = false;
      }
    return ok;
  }

  NumberX (const BlockModel& al)
    : Number (al)
  { }
};

const symbol NumberX::name ("x");

static struct NumberXSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberX (al); }
  NumberXSyntax ()
    : DeclareModel (Number::component, "x", "\
The value of the symbol 'x' in the current scope.")
  { }
  void load_frame (Frame& frame) const
  { }
} NumberX_syntax;

struct NumberGet : public Number
{
  // Data.
  const Unit& unit_;
  const Unit* scope_unit;

  // Parameters.
  const symbol name;
  symbol title () const
  { return name; }

  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  symbol dimension (const Scope&) const
  { return unit_.native_name (); }
  const Unit& unit () const
  { return unit_; }
  bool missing (const Scope& scope) const
  { return !scope.check (name); }
  double value (const Scope& scope) const
  { 
    daisy_assert (scope.check (name));
    daisy_assert (scope_unit);
    const double value = scope.number ( name);
    return Units::unit_convert (*scope_unit, unit (), value);
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    if (scope.lookup (name) != Attribute::Number)
      {
        msg.error ("'" + name + "' is not a number");
        return false;
      }
    const symbol got_dim = scope.dimension (name);
    scope_unit = &units.get_unit (got_dim);
    return true;
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  {
    Treelog::Open nest (msg, name);

    bool ok = true;
    if (units.is_error (unit_))
      {
        msg.error ("Bad unit");
        ok = false;
      }
    if (!scope_unit)
      {
        msg.error ("'" + name + "' is not a number");
        ok = false;
      }
    else if (units.is_error (*scope_unit))
      {
        daisy_assert (scope.lookup (name) == Attribute::Number);
        const symbol got_dim = scope.dimension (name);
        msg.error ("'" + name + "' has unknown dimension [" + got_dim + "]");
        ok = false;
      }
    return ok;
  }

  NumberGet (const BlockModel& al)
    : Number (al),
      unit_ (al.units ().get_unit (al.name ("dimension"))),
      scope_unit (NULL),
      name (al.name ("name"))
  {
    if (al.units ().is_error (unit_))
      al.msg ().warning ("Unknown unit '" + al.name ("dimension") + "'");
  }
  NumberGet (const BlockModel& al, const symbol key)
    : Number (al),
      unit_ (al.units ().get_unit (al.name ("dimension"))),
      scope_unit (NULL),
      name (key)
  { }
};

static struct NumberGetSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberGet (al); }
  NumberGetSyntax ()
    : DeclareModel (Number::component, "get", 
	       "Get the value of symbol in the current scope.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_string ("name", Attribute::Const, 
                "Name of a the symbol.");
    frame.declare_string ("dimension", Attribute::Const, 
                "Expected dimension for the symbol.");
    frame.order ("name", "dimension");
  }
} NumberGet_syntax;

struct NumberFetchGet : public Number
{
  // Data.
  const Unit* scope_unit;

  // Parameters.
  const symbol name;
  symbol title () const
  { return name; }

  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  symbol dimension (const Scope&) const
  { 
    if (scope_unit)
      return scope_unit->native_name (); 

    return Attribute::Unknown ();
  }
  const Unit& unit () const
  { 
    daisy_assert (scope_unit);
    return *scope_unit; 
  }
  bool missing (const Scope& scope) const
  { return !scope.check (name); }
  double value (const Scope& scope) const
  { 
    daisy_assert (scope.check (name));
    return scope.number ( name);
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    if (scope.lookup (name) != Attribute::Number)
      {
        msg.error ("'" + name + "' is not a number");
        return false;
      }
    const symbol got_dim = scope.dimension (name);
    scope_unit = &units.get_unit (got_dim);
    return true;
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  {
    Treelog::Open nest (msg, name);

    bool ok = true;
    if (!scope_unit)
      {
        msg.error ("'" + name + "' is not a number");
        ok = false;
      }
    else if (units.is_error (*scope_unit))
      {
        daisy_assert (scope.lookup (name) == Attribute::Number);
        const symbol got_dim = scope.dimension (name);
        msg.error ("'" + name + "' has unknown dimension [" + got_dim + "]");
        ok = false;
      }
    return ok;
  }

  NumberFetchGet (const BlockModel& al, const symbol key)
    : Number (al),
      scope_unit (NULL),
      name (key)
  { }
};

struct NumberFetch : public Number
{
  // Parameters.
  const std::unique_ptr<Number> child;
  symbol title () const
  { return child->title (); }

  static std::unique_ptr<Number> fetch_child (const BlockModel& al, const symbol key)
  {
    std::unique_ptr<Number> result;
    Attribute::type type = al.lookup (key);
    switch (type)
      {
      case Attribute::Number:
        {
          if (!al.check (key))
            {
              const Frame& frame = al.find_frame (key);
              daisy_assert (frame.lookup (key) == Attribute::Number);
              al.error ("Parameter '" + key 
                        + "' is declared in '" + frame.type_name () 
                        + "' (" + frame.description () 
                        + ") base '" + frame.base_name () 
                        + "', but has no value");
              break;
            }
          if (al.type_size (key) != Attribute::Singleton)
            {
              al.error ("Parameter '" + key 
                         + "' is a sequence, expected singleton");
              break;
            }
          result.reset (new NumberConst (al, key));
        }
        break;
      case Attribute::Model:
        {
          const Frame& frame = al.find_frame (key);
          const symbol component = frame.component (key);
          if (component != Number::component)
            {
              al.error ("'" + key + "' is a '" + component
                         + "' model, expected a '"
                         + Number::component + "'");
              break;
            }
          if (frame.type_size (key) != Attribute::Singleton)
            {
              al.error ("Parameter '" + key 
                        + "' is a model sequence, expected singleton");
              break;
            }
          if (!frame.check (key))
            {
              al.error ("'" + key + "' declared, but has no value");
              break;
            }
          if (!frame.check (al))
            break;
          result.reset (Librarian::build_item<Number> (al, key));
        }
        break;
      case Attribute::Error:
        result.reset (new NumberFetchGet (al, key));
        break;
      default:
        al.error ("'" + key + "' is a " + Attribute::type_name (type)
                  + ", expected a number");
      }
    return result;
  }
  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { child->tick (units, scope, msg); }
  bool missing (const Scope& scope) const
  { return child->missing (scope); }
  double value (const Scope& scope) const
  { return child->value (scope); }
  symbol dimension (const Scope& scope) const
  { return child->dimension (scope); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    if (!child.get ())
      return false;
    return child->initialize (units, scope, msg); 
  }
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  {
    TREELOG_MODEL (msg);

    bool ok = true;
    if (!child.get ())
      {
        msg.error ("Fetch failed");
        ok = false;
      }
    else if (!child->check (units, scope, msg))
      ok = false;
    return ok;
  }
  NumberFetch (const BlockModel& al)
    : Number (al),
      child (fetch_child (al, al.name ("name")))
  { }
  ~NumberFetch ()
  { }
};

static struct NumberFetchSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberFetch (al); }
  NumberFetchSyntax ()
    : DeclareModel (Number::component, "fetch", 
	       "Fetch the value and dimension in the current scope.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_string ("name", Attribute::Const, 
                "Name of a the symbol.");
    frame.order ("name");
  }
} NumberFetch_syntax;

struct NumberChild : public Number
{
  // Parameters.
  const std::unique_ptr<Number> child;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { child->tick (units, scope, msg); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { return child->initialize (units, scope, msg); }
  NumberChild (const BlockModel& al)
    : Number (al),
      child (Librarian::build_item<Number> (al, "value"))
  { }
};

static struct NumberChildSyntax : public DeclareBase
{
  NumberChildSyntax ()
    : DeclareBase (Number::component, "child", "\
Numbers based on another number.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("value", Number::component,
                       "Operand for this function.");
  }
} NumberChild_syntax;

struct NumberIdentity : public NumberChild
{
  const Units& units;

  // Parameters.
  const symbol dim;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return child->missing (scope) 
      || (known (dim) && known (child->dimension (scope))
          && !units.can_convert (child->dimension (scope), dim, 
                                 child->value (scope))); }
  double value (const Scope& scope) const
  { 
    const double v = child->value (scope); 
    if (known (dim) && known (child->dimension (scope)))
      return units.convert (child->dimension (scope), dim, v);
    return v;
  }
  symbol dimension (const Scope& scope) const
  {
    if (known (dim))
      return dim; 
    return child->dimension (scope);
  }

  // Create.
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    bool ok = true;

    if (!child->check (units, scope, msg))
      ok = false;
    
    if (known (dim) && known (child->dimension (scope))
        && !units.can_convert (child->dimension (scope), dim))
      {
        msg.error ("Cannot convert [" + child->dimension (scope) 
                   + "] to [" + dim + "]");
        ok = false;
      }
    return ok;
  }
  NumberIdentity (const BlockModel& al)
    : NumberChild (al),
      units (al.units ()),
      dim (al.name ("dimension", Attribute::Unknown ()))
  { }
};

static struct NumberIdentitySyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberIdentity (al); }
  NumberIdentitySyntax ()
    : DeclareModel (Number::component, "identity", "child", "\
Pass value unchanged.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare_string ("dimension", Attribute::OptionalConst,
		"Dimension of this value.");
  }
} NumberIdentity_syntax;

struct NumberConvert : public NumberChild
{
  const Units& units;

  // Parameters.
  const symbol dim;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return child->missing (scope) 
      || !units.can_convert (child->dimension (scope), dim, 
                              child->value (scope)); }
  double value (const Scope& scope) const
  { 
    const double v = child->value (scope); 
    return units.convert (child->dimension (scope), dim, v);
  }
  symbol dimension (const Scope&) const
  { return dim; }

  // Create.
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    bool ok = true;

    if (!child->check (units, scope, msg))
      ok = false;
    
    if (!units.can_convert (child->dimension (scope), dim))
      {
        msg.error ("Cannot convert [" + child->dimension (scope) 
                   + "] to [" + dim + "]");
        ok = false;
      }
    return ok;
  }
  NumberConvert (const BlockModel& al)
    : NumberChild (al),
      units (al.units ()),
      dim (al.name ("dimension"))
  { }
};

static struct NumberConvertSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberConvert (al); }
  NumberConvertSyntax ()
    : DeclareModel (Number::component, "convert", "child", "\
Convert to specified dimension.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_string ("dimension", Attribute::Const,
		"Dimension to convert to.");
    frame.order ("value", "dimension");
  }
} NumberConvert_syntax;

struct NumberDim : public NumberChild
{
  // Parameters.
  const symbol dim;
  const bool warn_known;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return child->missing (scope); }
  double value (const Scope& scope) const
  { return child->value (scope); }
  symbol dimension (const Scope&) const
  { return dim; }

  // Create.
  bool check (const Units& units, const Scope& scope, Treelog& msg) const
  { 
    TREELOG_MODEL (msg);
    bool ok = true;

    if (!child->check (units, scope, msg))
      ok = false;
    
    if (warn_known && known (child->dimension (scope))
        && child->dimension (scope) != dim)
      msg.warning ("Dimension for child [" + child->dimension (scope)
                   + "] already known, now asserting it is [" + dim + "]");

    return ok;
  }
  NumberDim (const BlockModel& al)
    : NumberChild (al),
      dim (al.name ("dimension")),
      warn_known (al.flag ("warn_known"))
  { }
};

static struct NumberDimSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberDim (al); }
  NumberDimSyntax ()
    : DeclareModel (Number::component, "dim", "child", "\
Specify dimension for number.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_boolean ("warn_known", Attribute::Const,
                "Issue a warning if the dimensions is already known.");
    frame.set ("warn_known", true);
    frame.declare_string ("dimension", Attribute::Const,
		"Dimension to use.");
    frame.order ("value", "dimension");
  }
} NumberDim_syntax;

// number_const.C ends here.
