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
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>

// The 'element' component.

const char *const Element::component = "element";

symbol
Element::library_id () const
{
  static const symbol id (component);
  return id;
}

Element::Element (const BlockModel& al)
  : name (al.type_name ())
{ }

Element::~Element ()
{ }

static struct ElementInit : public DeclareComponent 
{
  ElementInit ()
    : DeclareComponent (Element::component, "\
An element of a compound.")
  { }
} Element_init;

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
  Atom (const BlockModel& al)
    : Element (al),
      mass (al.number ("mass"))
  { }
};

static struct ElementAtomSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new Atom (al); }
  ElementAtomSyntax ()
    : DeclareModel (Element::component, "atom", "An atom.")
  { }
  void load_frame (Frame& frame) const
  {
    
    frame.declare ("mass", "g/mol", Attribute::Const, "Atomic mass.");

  }
} ElementAtom_syntax;

// element.C ends here.
