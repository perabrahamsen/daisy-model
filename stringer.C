// stringer.C --- Strings in Daisy.
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


#include "stringer.h"

template<>
Librarian<Stringer>::Content* Librarian<Stringer>::content = NULL;

const char *const Stringer::description = "\
Generic representation of strings.";

const std::string& 
Stringer::title () const
{ return name.name (); }

Stringer::Stringer (const Block& al)
  : name (al.identifier ("type"))
{ }

Stringer::~Stringer ()
{ }

struct StringerStringEqual : public Stringer
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
  StringerStringEqual (const Block& al)
    : Stringer (al),
      values (al.name_sequence ("values"))
  { }
};

static struct StringerStringEqualSyntax
{
  static Stringer& make (const Block& al)
  { return *new StringerStringEqual (al); }
  StringerStringEqualSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "True iff the supplied strings are identical.");
    syntax.add ("values", Syntax::String, Syntax::Const, Syntax::Sequence,
		"Strings to compare.");
    syntax.order ("values");
    Librarian<Stringer>::add_type ("string-equal", alist, syntax, &make);
  }
} StringerStringEqual_syntax;
