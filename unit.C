// unit.C -- Specify unit for scalar.
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

#include "unit.h"
#include "check.h"
#include "treelog.h"
#include "metalib.h"
#include "library.h"
#include "librarian.h"
#include "alist.h"
#include "syntax.h"
#include "block.h"

// Component 'unit'.

const char *const Unit::component = "unit";

Unit::Unit (Block& al)
  : name (al.identifier ("type"))
{ }

Unit::~Unit ()
{ }

static Librarian Unit_init (Unit::component, "\
The 'unit' allows you to define convertion to and from SI base units .");

// Base model 'SI'.

class UnitSI : public Unit
{
  // Content.
public:
  const int length;
  const int mass;
  const int time;
  const int electric_current;
  const int thermodynamic_temperature;
  const int amount_of_substance;
  const int luminous_intensity;

  // Create and destroy
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
protected:
  UnitSI (Block& al)
    : Unit (al),
      length (al.integer ("length")),
      mass (al.integer ("mass")),
      time (al.integer ("time")),
      electric_current (al.integer ("electric_current")),
      thermodynamic_temperature (al.integer ("thermodynamic_temperature")),
      amount_of_substance (al.integer ("amount_of_substance")),
      luminous_intensity (al.integer ("luminous_intensity"))
  { }
  ~UnitSI ()
  { }
};

void
UnitSI::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("length", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("length", 0);
  syntax.add ("mass", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("mass", 0);
  syntax.add ("time", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("time", 0);
  syntax.add ("electric_current", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("electric_current", 0);
  syntax.add ("thermodynamic_temperature", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("thermodynamic_temperature", 0);
  syntax.add ("amount_of_substance", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("amount_of_substance", 0);
  syntax.add ("luminous_intensity", Syntax::Integer, Syntax::Const, "\
Dimension.");
  alist.add ("luminous_intensity", 0);
}

// Model 'SIfactor'.

struct UnitSIFactor : public UnitSI
{
  const double factor;
  
  double to_base (double value) const
  { return value * factor; }
  double from_base (double value) const
  { return value / factor; }
  bool in_domain (double) const
  { return true; }
  bool in_range (double) const
  { return true; }

  UnitSIFactor (Block& al)
    : UnitSI (al),
      factor (al.number ("factor"))
  { }
};

static struct UnitSIFactorSyntax
{
  static Model& make (Block& al)
  { return *new UnitSIFactor (al); }
  UnitSIFactorSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Connvert to SI base units by multiplying with a factor.");
    UnitSI::load_syntax (syntax, alist);
    syntax.add ("factor", Syntax::None (), Check::non_zero (), Syntax::Const, "\
Fcator to multiply with to get base unit.");
    Librarian::add_type (Unit::component, "SIfactor", alist, syntax, &make);
  }
} UnitSIFactor_syntax;

// Utilities.

bool
Unit::can_convert (Metalib& metalib, symbol from, symbol to, 
                   Treelog& msg)
{
  bool ok = true;
  Library& library = metalib.library (Unit::component);
  if (!library.complete (metalib, from))
    {
      msg.error ("Unknown source dimension [" + from.name () + "]");
      ok = false;
    }
  if (!library.complete (metalib, from))
    {
      msg.error ("Unknown target dimension [" + to.name () + "]");
      ok = false;
    }
  if (!ok) 
    return false;

  const AttributeList& from_alist = library.lookup (from);
  const std::auto_ptr<Unit> from_unit 
    (Librarian::build_free<Unit> (metalib, msg, from_alist, "unit"));
  const AttributeList& to_alist = library.lookup (to);
  std::auto_ptr<Unit> to_unit 
    (Librarian::build_free<Unit> (metalib, msg, to_alist, "unit"));

  return ok;
}

// unit.C ends here.
