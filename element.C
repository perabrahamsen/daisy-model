// element.C --- A single element of a compound.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "element.h"
#include "block.h"
#include "alist.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>

// The 'element' component.

const char *const Element::component = "element";

symbol
Element::library_id () const
{
  static const symbol id (component);
  return id;
}

Element::Element (Block& al)
  : name (al.identifier ("type"))
{ }

Element::~Element ()
{ }

static Librarian Element_init (Element::component, "\
An element of a compound.");

// The 'atom' model.

struct Atom : public Element
{ 
  const double mass;		// [g/mol]
  double weight () const
  { return mass; }
  symbol dimension () const
  { 
    static const symbol dim = symbol ("g/mol");
    return dim; 
  }
  Atom (Block& al)
    : Element (al),
      mass (al.number ("mass"))
  { }
};

static struct ElementAtomSyntax
{
  static Model& make (Block& al)
  { return *new Atom (al); }
  ElementAtomSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "An atom.");
    
    syntax.add ("mass", "g/mol", Syntax::Const, "Atomic mass.");

    Librarian::add_type (Element::component, "atom", alist, syntax, &make);
  }
} ElementAtom_syntax;

// element.C ends here.
