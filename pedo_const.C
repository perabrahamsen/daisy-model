// pedo_const.C -- Simple soil based values.
// 
// Copyright 2002, 2004 Per Abrahamsen and KVL.
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


#include "pedo.h"
#include "soil.h"
#include "units.h"
#include "vcheck.h"

using namespace std;

struct PedotransferConst : public Pedotransfer
{
  // Parameters.
  const double val;
  const string& dim;

  // Simulation.
  double value (const Soil&, int) const
  { return val; }
  const string& dimension () const
  { return dim; }

  // Create.
  bool check_nested (const Soil&, Treelog&) const
  { return true; }
  PedotransferConst (const AttributeList& al)
    : Pedotransfer (al),
      val (al.number ("value")),
      dim (al.name ("value"))
  { }
};

struct PedotransferHumus : public Pedotransfer
{
  const string dim;

  // Simulation.
  double value (const Soil& soil, int i) const
  { 
    return Units::convert (Syntax::Fraction (), dimension (), 
                           soil.humus (i)); 
  }
  const string& dimension () const
  { return dim; }

  // Create.
  bool check_nested (const Soil&, Treelog&) const
  { return true; }
  PedotransferHumus (const AttributeList& al)
    : Pedotransfer (al),
      dim (al.name ("dimension"))
  { }
};

struct PedotransferMineral : public Pedotransfer
{
  const string dim;

  // Simulation.
  double value (const Soil& soil, int i) const
  { 
    return Units::convert (Syntax::Fraction (), dimension (),
                           1.0 - soil.humus (i));
  }
  const string& dimension () const
  { return dim; }

  // Create.
  bool check_nested (const Soil&, Treelog&) const
  { return true; }
  PedotransferMineral (const AttributeList& al)
    : Pedotransfer (al),
      dim (al.name ("dimension"))
  { }
};

struct PedotransferRho_B : public Pedotransfer
{
  const string dim;

  // Simulation.
  double value (const Soil& soil, int i) const
  { 
    return Units::convert ("g/cm^3", dimension (), soil.dry_bulk_density (i));
  }
  const string& dimension () const
  { return dim; }

  // Create.
  bool check_nested (const Soil&, Treelog&) const
  { return true; }
  PedotransferRho_B (const AttributeList& al)
    : Pedotransfer (al),
      dim (al.name ("dimension"))
  { }
};

struct PedotransferBelow : public Pedotransfer
{
  // Parameters.
  const double size;
  const string dim;

  // Simulation.
  double value (const Soil& soil, int i) const
  { 
    return Units::convert (Syntax::Fraction (), dimension (),
                           soil.texture_below (i, size));
  }
  const string& dimension () const
  { return dim; }

  // Create.
  bool check_nested (const Soil&, Treelog&) const
  { return true; }
  PedotransferBelow (const AttributeList& al)
    : Pedotransfer (al),
      size (al.number ("size")),
      dim (al.name ("dimension"))
  { }
};

struct PedotransferGet : public Pedotransfer
{
  // Parameters.
  const string name;
  const string dim;

  // Simulation.
  double value (const Soil& soil, int i) const
  { 
    daisy_assert (soil.has_attribute (i, name));
    const double value = soil.get_attribute (i, name);
    const string got_dim = soil.get_dimension (i, name);
    return Units::convert (got_dim, dim, value);
  }
  const string& dimension () const
  { return dim; }

  // Create.
  bool check_nested (const Soil& soil, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    bool ok = true;

    if (!soil.has_attribute (name))
      {
        err.error ("Required attribute '" 
                   + name + "' is missing from the soil");
        ok = false;
      }
    for (size_t i = 0; i < soil.size () && ok; i++)
      if (!Units::can_convert (soil.get_dimension (i, name), dim))
        {
          err.error ("Cannot convert '" + soil.get_dimension (i, name)
                     + "' to '" + dim + "'");
          ok = false;
        }
    return ok;
  }

  PedotransferGet (const AttributeList& al)
    : Pedotransfer (al),
      name (al.name ("name")),
      dim (al.name ("dimension"))
  { }
};

static struct PedotransferConstSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferConst (al); }
  PedotransferConstSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Ignore the soil, and always give the specified value.");
    syntax.add ("value", Syntax::User (), Syntax::Const,
		"Fixed value of this pedotransfer function.");
    syntax.order ("value");
    Librarian<Pedotransfer>::add_type ("const", alist, syntax, &make);
  }
} PedotransferConst_syntax;

static struct PedotransferHumusSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferHumus (al); }
  PedotransferHumusSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Humus fraction of soil");
    syntax.add ("dimension", Syntax::String, Syntax::Const,
                "Unit used for the humus fraction.");
    syntax.add_check ("dimension", VCheck::fraction ());
    syntax.order ("dimension");

    Librarian<Pedotransfer>::add_type ("humus", alist, syntax, &make);
  }
} PedotransferHumus_syntax;

static struct PedotransferMineralSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferMineral (al); }
  PedotransferMineralSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Mineral fraction of soil");
    syntax.add ("dimension", Syntax::String, Syntax::Const,
                "Unit used for the mineral fraction.");
    syntax.add_check ("dimension", VCheck::fraction ());
    syntax.order ("dimension");

    Librarian<Pedotransfer>::add_type ("mineral", alist, syntax, &make);
  }
} PedotransferMineral_syntax;

static struct PedotransferRho_BSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferRho_B (al); }
  PedotransferRho_BSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", "Dry bulk density.");
    syntax.add ("dimension", Syntax::String, Syntax::Const,
                "Unit used for the dry bulk density.");
    static VCheck::Compatible rho_b_dim ("g/cm^3");
    syntax.add_check ("dimension", rho_b_dim);
    syntax.order ("dimension");

    Librarian<Pedotransfer>::add_type ("rho_b", alist, syntax, &make);
  }
} PedotransferRho_B_syntax;

static struct PedotransferBelowSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferBelow (al); }
  PedotransferBelowSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Fraction of mineral particles below the specified size.");
    syntax.add ("size", "um", Syntax::Const,
		"Particle size limit.");
    syntax.add ("dimension", Syntax::String, Syntax::Const,
                "Unit used for the particle fraction.");
    syntax.add_check ("dimension", VCheck::fraction ());
    syntax.order ("dimension", "size");
    Librarian<Pedotransfer>::add_type ("below", alist, syntax, &make);
  }
} PedotransferBelow_syntax;

static struct PedotransferGetSyntax
{
  static Pedotransfer& make (const AttributeList& al)
  { return *new PedotransferGet (al); }
  PedotransferGetSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Get the value of a horizon attribute.");
    syntax.add ("name", Syntax::String, Syntax::Const, 
                "Name of a soil attribute.");
    syntax.add ("dimension", Syntax::String, Syntax::Const, 
                "Expected dimension for the soil attribute.");
    syntax.order ("name", "dimension");
    Librarian<Pedotransfer>::add_type ("get", alist, syntax, &make);
  }
} PedotransferGet_syntax;

