// boolean.C --- Booleans in Daisy.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#include "boolean.h"

template<>
Librarian<Boolean>::Content* Librarian<Boolean>::content = NULL;

const char *const Boolean::description = "\
Generic representation of booleans.";

const std::string& 
Boolean::title () const
{ return name.name (); }

Boolean::Boolean (Block& al)
  : name (al.identifier ("type"))
{ }

Boolean::~Boolean ()
{ }

struct BooleanStringEqual : public Boolean
{
  // Parameters.
  const std::vector<std::string> values;

  // Simulation.
  bool missing (const Scope&) const
  { return false; }
  bool value (const Scope&) const
  { 
    if (values.size () < 2)
      return true;
    const std::string first = values[0];
    for (size_t i = 1; i < values.size (); i++)
      if (first != values[i])
	return false; 
    return true;
  }

  // Create.
  bool check (const Scope&, Treelog&) const
  { return true; }
  BooleanStringEqual (Block& al)
    : Boolean (al),
      values (al.name_sequence ("values"))
  { }
};

static struct BooleanStringEqualSyntax
{
  static Boolean& make (Block& al)
  { return *new BooleanStringEqual (al); }
  BooleanStringEqualSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "True iff the supplied strings are identical.");
    syntax.add ("values", Syntax::String, Syntax::Const, Syntax::Sequence,
		"Strings to compare.");
    syntax.order ("values");
    Librarian<Boolean>::add_type ("string-equal", alist, syntax, &make);
  }
} BooleanStringEqual_syntax;

struct BooleanTrue : public Boolean
{
  // Simulation.
  bool missing (const Scope&) const
  { return false; }
  bool value (const Scope&) const
  { return true; }

  // Create.
  bool check (const Scope&, Treelog&) const
  { return true; }
  BooleanTrue (Block& al)
    : Boolean (al)
  { }
};

static struct BooleanTrueSyntax
{
  static Boolean& make (Block& al)
  { return *new BooleanTrue (al); }
  BooleanTrueSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Always true.");
    Librarian<Boolean>::add_type ("true", alist, syntax, &make);
  }
} BooleanTrue_syntax;

struct BooleanFalse : public Boolean
{
  // Simulation.
  bool missing (const Scope&) const
  { return false; }
  bool value (const Scope&) const
  { return false; }

  // Create.
  bool check (const Scope&, Treelog&) const
  { return true; }
  BooleanFalse (Block& al)
    : Boolean (al)
  { }
};

static struct BooleanFalseSyntax
{
  static Boolean& make (Block& al)
  { return *new BooleanFalse (al); }
  BooleanFalseSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Always false.");
    Librarian<Boolean>::add_type ("false", alist, syntax, &make);
  }
} BooleanFalse_syntax;
