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


#include "number.h"
#include "scope.h"
#include "units.h"

using namespace std;

struct NumberConst : public Number
{
  // Parameters.
  const double val;
  const string dim;

  // Simulation.
  bool missing (const Scope&) const
  { return false; }
  double value (const Scope&) const
  { return val; }
  const string& dimension (const Scope&) const
  { return dim; }

  // Create.
  bool check (const Scope&, Treelog&) const
  { return true; }
  NumberConst (const AttributeList& al)
    : Number (al),
      val (al.number ("value")),
      dim (al.name ("value"))
  { }
};

static struct NumberConstSyntax
{
  static Number& make (const AttributeList& al)
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
    Librarian<Number>::add_type ("const", alist, syntax, &make);
  }
} NumberConst_syntax;

struct NumberLeaf : public Number
{
  // Parameters.
  const string dim;

  // Simulation.
  const string& dimension (const Scope&) const
  { return dim; }

  // Create.
  NumberLeaf (const AttributeList& al)
    : Number (al),
      dim (al.name ("dimension"))
  { }
};

struct NumberGet : public NumberLeaf
{
  // Parameters.
  const string name;
  const std::string& title () const
  { return name; }

  // Simulation.
  bool missing (const Scope& scope) const
  { return !scope.has_number (name); }
  double value (const Scope& scope) const
  { 
    daisy_assert (scope.has_number (name));
    const double value = scope.number ( name);
    const string got_dim = scope.dimension ( name);
    return Units::convert (got_dim, dim, value);
  }

  // Create.
  bool check (const Scope& scope, Treelog& err) const
  {
    Treelog::Open nest (err, name);

    bool ok = true;
    if (!scope.has_number (name))
      {
        err.error ("'" + name + "' not in scope");
        ok = false;
      }
    return ok;
  }

  NumberGet (const AttributeList& al)
    : NumberLeaf (al),
      name (al.name ("name"))
  { }
};

static struct NumberGetSyntax
{
  static Number& make (const AttributeList& al)
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
    Librarian<Number>::add_type ("get", alist, syntax, &make);
  }
} NumberGet_syntax;

struct NumberFetch : public Number
{
  // Parameters.
  const string name;
  const std::string& title () const
  { return name; }

  // Simulation.
  bool missing (const Scope& scope) const
  { return !scope.has_number (name); }
  double value (const Scope& scope) const
  { return scope.number ( name); }
  const std::string& dimension (const Scope& scope) const
  { return scope.dimension ( name); }

  // Create.
  bool check (const Scope& scope, Treelog& err) const
  {
    Treelog::Open nest (err, name);

    bool ok = true;
    if (!scope.has_number (name))
      {
        err.error ("'" + name + "' not in scope");
        ok = false;
      }
    return ok;
  }
  NumberFetch (const AttributeList& al)
    : Number (al),
      name (al.name ("name"))
  { }
};

static struct NumberFetchSyntax
{
  static Number& make (const AttributeList& al)
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
    Librarian<Number>::add_type ("fetch", alist, syntax, &make);
  }
} NumberFetch_syntax;

struct NumberIdentity : public Number
{
  // Parameters.
  const auto_ptr<Number> child;
  const string dim;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return child->missing (scope) 
      || (known (dim) && known (child->dimension (scope))
          && !Units::can_convert (child->dimension (scope), dim, 
                                  child->value (scope))); }
  double value (const Scope& scope) const
  { 
    const double v = child->value (scope); 
    if (known (dim) && known (child->dimension (scope)))
      return Units::convert (child->dimension (scope), dim, v);
    return v;
  }
  const string& dimension (const Scope& scope) const
  {
    if (known (dim))
      return dim; 
    return child->dimension (scope);
  }

  // Create.
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    bool ok = true;

    if (!child->check (scope, err))
      ok = false;
    
    if (known (dim) && known (child->dimension (scope))
        && !Units::can_convert (child->dimension (scope), dim))
      {
        err.error ("Cannot convert [" + child->dimension (scope) 
                   + "] to [" + dim + "]");
        ok = false;
      }
    return ok;
  }
  NumberIdentity (const AttributeList& al)
    : Number (al),
      child (Librarian<Number>::create (al.alist ("value"))),
      dim (al.name ("dimension", Syntax::Unknown ()))
  { }
};

static struct NumberIdentitySyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberIdentity (al); }
  NumberIdentitySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Pass value unchanged.");
    syntax.add ("value", Librarian<Number>::library (),
		"Operand for this function.");
    syntax.add ("dimension", Syntax::String, Syntax::OptionalConst,
		"Dimension of this value.");
    Librarian<Number>::add_type ("identity", alist, syntax, &make);
  }
} NumberIdentity_syntax;

struct NumberConvert : public Number
{
  // Parameters.
  const auto_ptr<Number> child;
  const string dim;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return child->missing (scope) 
      || !Units::can_convert (child->dimension (scope), dim, 
                              child->value (scope)); }
  double value (const Scope& scope) const
  { 
    const double v = child->value (scope); 
    return Units::convert (child->dimension (scope), dim, v);
  }
  const string& dimension (const Scope&) const
  { return dim; }

  // Create.
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    bool ok = true;

    if (!child->check (scope, err))
      ok = false;
    
    if (!Units::can_convert (child->dimension (scope), dim))
      {
        err.error ("Cannot convert [" + child->dimension (scope) 
                   + "] to [" + dim + "]");
        ok = false;
      }
    return ok;
  }
  NumberConvert (const AttributeList& al)
    : Number (al),
      child (Librarian<Number>::create (al.alist ("value"))),
      dim (al.name ("dimension"))
  { }
};

static struct NumberConvertSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberConvert (al); }
  NumberConvertSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Convert to specified dimension.");
    syntax.add ("value", Librarian<Number>::library (),
		"Operand for this function.");
    syntax.add ("dimension", Syntax::String, Syntax::Const,
		"Dimension to convert to.");
    syntax.order ("value", "dimension");
    Librarian<Number>::add_type ("convert", alist, syntax, &make);
  }
} NumberConvert_syntax;

struct NumberDim : public Number
{
  // Parameters.
  const auto_ptr<Number> child;
  const string dim;
  const bool warn_known;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return child->missing (scope); }
  double value (const Scope& scope) const
  { return child->value (scope); }
  const string& dimension (const Scope&) const
  { return dim; }

  // Create.
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    bool ok = true;

    if (!child->check (scope, err))
      ok = false;
    
    if (warn_known && known (child->dimension (scope)))
      err.warning ("Dimension for child already known");

    return ok;
  }
  NumberDim (const AttributeList& al)
    : Number (al),
      child (Librarian<Number>::create (al.alist ("value"))),
      dim (al.name ("dimension")),
      warn_known (al.flag ("warn_known"))
  { }
};

static struct NumberDimSyntax
{
  static Number& make (const AttributeList& al)
  { return *new NumberDim (al); }
  NumberDimSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Specify dimension for number.");
    syntax.add ("value", Librarian<Number>::library (),
		"Operand for this function.");
    syntax.add ("warn_known", Syntax::Boolean, Syntax::Const,
                "Issue a warning if the dimensions is already known.");
    alist.add ("warn_known", true);
    syntax.add ("dimension", Syntax::String, Syntax::Const,
		"Dimension to use.");
    syntax.order ("value", "dimension");
    Librarian<Number>::add_type ("dim", alist, syntax, &make);
  }
} NumberDim_syntax;

