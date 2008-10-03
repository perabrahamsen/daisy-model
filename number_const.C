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
#include "block.h"
#include "alist.h"
#include "scope.h"
#include "units.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"

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
  { return Units::get_name (unit_); }
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
  NumberConst (Block& al)
    : Number (al),
      val (al.number ("value")),
      unit_ (al.units ().get_unit (al.name ("value")))
  { 
    if (al.units ().is_error (unit_))
      al.msg ().warning ("Unknown unit '" + al.name ("value") + "'");
  }
};

static struct NumberConstSyntax
{
  static Model& make (Block& al)
  { return *new NumberConst (al); }
  NumberConstSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Always give the specified value.");
    syntax.add ("value", Syntax::User (), Syntax::Const,
		"Fixed value for this number.");
    syntax.order ("value");
    Librarian::add_type (Number::component, "const", alist, syntax, &make);
  }
} NumberConst_syntax;

struct NumberLeaf : public Number
{
  // Parameters.
  const Unit& unit_;

  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  symbol dimension (const Scope&) const
  { return Units::get_name (unit_); }
  const Unit& unit () const
  { return unit_; }

  // Create.
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
  NumberLeaf (Block& al)
    : Number (al),
      unit_ (al.units ().get_unit (al.name ("dimension")))
  { 
    if (al.units ().is_error (unit_))
      al.msg ().warning ("Unknown unit '" + al.name ("dimension") + "'");
  }
};

struct NumberGet : public NumberLeaf
{
  // Data.
  const Unit* scope_unit;

  // Parameters.
  const symbol name;
  symbol title () const
  { return name; }

  // Simulation.
  bool missing (const Scope& scope) const
  { return !scope.has_number (name); }
  double value (const Scope& scope) const
  { 
    daisy_assert (scope.has_number (name));
    daisy_assert (scope_unit);
    const double value = scope.number ( name);
    return Units::unit_convert (*scope_unit, unit (), value);
  }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { 
    if (!scope.is_number (name))
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

    bool ok = NumberLeaf::check (units, scope, msg);
    if (!scope_unit)
      {
        msg.error ("'" + name + "' is not a number");
        ok = false;
      }
    else if (units.is_error (*scope_unit))
      {
        daisy_assert (scope.is_number (name));
        const symbol got_dim = scope.dimension (name);
        msg.error ("'" + name + "' has unknown dimension [" + got_dim + "]");
        ok = false;
      }
    return ok;
  }

  NumberGet (Block& al)
    : NumberLeaf (al),
      scope_unit (NULL),
      name (al.name ("name"))
  { }
};

static struct NumberGetSyntax
{
  static Model& make (Block& al)
  { return *new NumberGet (al); }
  NumberGetSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Get the value of symbol in the current scope.");
    syntax.add ("name", Syntax::String, Syntax::Const, 
                "Name of a the symbol.");
    syntax.add ("dimension", Syntax::String, Syntax::Const, 
                "Expected dimension for the symbol.");
    syntax.order ("name", "dimension");
    Librarian::add_type (Number::component, "get", alist, syntax, &make);
  }
} NumberGet_syntax;

struct NumberFetch : public Number
{
  // Parameters.
  const symbol name;
  const double *const default_value;
  const symbol default_dimension;
  symbol title () const
  { return name; }

  static double* fetch_default_value (Block& al, const symbol key_symbol)
  {
    const std::string& key = key_symbol.name ();
    if (al.lookup (key) != Syntax::Number)
      return NULL;
    const AttributeList& alist = al.find_alist (key);
    if (!alist.check (key))
      return NULL;
    const Syntax& syntax = al.find_syntax (key);
    daisy_assert (syntax.lookup (key) == Syntax::Number);
    if (syntax.size (key) != Syntax::Singleton)
      {
	al.msg ().warning ("Parameter '" + key + "' is a sequence, ignored");
	return NULL;
      }
    return new double (alist.number (key));
  }
  static symbol fetch_default_dimension (Block& al, const symbol key_symbol)
  {
    const std::string& key = key_symbol.name ();
    if (al.lookup (key) != Syntax::Number)
      return Syntax::Unknown ();
    const Syntax& syntax = al.find_syntax (key);
    daisy_assert (syntax.lookup (key) == Syntax::Number);
    const symbol dim (syntax.dimension (key));
    if (dim == Syntax::User ())
      {
	const AttributeList& alist = al.find_alist (key);
	if (alist.check (key))
	  return alist.name (key);
      }
    return dim;
  }
  // Simulation.
  void tick (const Units&, const Scope&, Treelog&)
  { }
  bool missing (const Scope& scope) const
  { return !default_value && !scope.has_number (name); }
  double value (const Scope& scope) const
  { 
    if (scope.has_number (name))
      return scope.number (name);
    daisy_assert (default_value);
    return *default_value;
  }
  symbol dimension (const Scope& scope) const
  { 
    if (scope.is_number (name))
      return scope.dimension (name); 
    return default_dimension;
  }

  // Create.
  bool initialize (const Units&, const Scope&, Treelog&)
  { return true; }
  bool check (const Units&, const Scope& scope, Treelog& msg) const
  {
    Treelog::Open nest (msg, "Fetch: " + name);

    bool ok = true;
    if (!default_value && !scope.is_number (name))
      {
        msg.error ("'" + name + "' not in scope");
        ok = false;
      }
    return ok;
  }
  NumberFetch (Block& al)
    : Number (al),
      name (al.name ("name")),
      default_value (fetch_default_value (al, name)),
      default_dimension (fetch_default_dimension (al, name))
  { }
  ~NumberFetch ()
  { delete default_value; }
};

static struct NumberFetchSyntax
{
  static Model& make (Block& al)
  { return *new NumberFetch (al); }
  NumberFetchSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Fetch the value and dimension in the current scope.");
    syntax.add ("name", Syntax::String, Syntax::Const, 
                "Name of a the symbol.");
    syntax.order ("name");
    Librarian::add_type (Number::component, "fetch", alist, syntax, &make);
  }
} NumberFetch_syntax;

struct NumberChild : public Number
{
  // Parameters.
  const std::auto_ptr<Number> child;

  // Simulation.
  void tick (const Units& units, const Scope& scope, Treelog& msg)
  { child->tick (units, scope, msg); }

  // Create.
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  { return child->initialize (units, scope, msg); }
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add_object ("value", Number::component,
                       "Operand for this function.");
  }
  NumberChild (Block& al)
    : Number (al),
      child (Librarian::build_item<Number> (al, "value"))
  { }
};

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
    Treelog::Open nest (msg, name);
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
  NumberIdentity (Block& al)
    : NumberChild (al),
      units (al.units ()),
      dim (al.name ("dimension", Syntax::Unknown ()))
  { }
};

static struct NumberIdentitySyntax
{
  static Model& make (Block& al)
  { return *new NumberIdentity (al); }
  NumberIdentitySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Pass value unchanged.");
    NumberChild::load_syntax (syntax, alist);
    syntax.add ("dimension", Syntax::String, Syntax::OptionalConst,
		"Dimension of this value.");
    Librarian::add_type (Number::component, "identity", alist, syntax, &make);
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
    Treelog::Open nest (msg, name);
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
  NumberConvert (Block& al)
    : NumberChild (al),
      units (al.units ()),
      dim (al.name ("dimension"))
  { }
};

static struct NumberConvertSyntax
{
  static Model& make (Block& al)
  { return *new NumberConvert (al); }
  NumberConvertSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Convert to specified dimension.");
    NumberChild::load_syntax (syntax, alist);
    syntax.add ("dimension", Syntax::String, Syntax::Const,
		"Dimension to convert to.");
    syntax.order ("value", "dimension");
    Librarian::add_type (Number::component, "convert", alist, syntax, &make);
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
    Treelog::Open nest (msg, name);
    bool ok = true;

    if (!child->check (units, scope, msg))
      ok = false;
    
    if (warn_known && known (child->dimension (scope))
        && child->dimension (scope) != dim)
      msg.warning ("Dimension for child [" + child->dimension (scope)
                   + "] already known, now asserting it is [" + dim + "]");

    return ok;
  }
  NumberDim (Block& al)
    : NumberChild (al),
      dim (al.name ("dimension")),
      warn_known (al.flag ("warn_known"))
  { }
};

static struct NumberDimSyntax
{
  static Model& make (Block& al)
  { return *new NumberDim (al); }
  NumberDimSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Specify dimension for number.");
    NumberChild::load_syntax (syntax, alist);
    syntax.add ("warn_known", Syntax::Boolean, Syntax::Const,
                "Issue a warning if the dimensions is already known.");
    alist.add ("warn_known", true);
    syntax.add ("dimension", Syntax::String, Syntax::Const,
		"Dimension to use.");
    syntax.order ("value", "dimension");
    Librarian::add_type (Number::component, "dim", alist, syntax, &make);
  }
} NumberDim_syntax;

