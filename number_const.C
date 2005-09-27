// number_const.C -- Simple number objects.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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

