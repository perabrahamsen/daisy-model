// number_soil.C -- Extract soil properties.
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

#define BUILD_DLL

#include "number.h"
#include "metalib.h"
#include "library.h"
#include "block.h"
#include "column.h"
#include "horizon.h"
#include "hydraulic.h"
#include "weather.h"
#include "output.h"
#include "time.h"
#include "units.h"
#include "librarian.h"
#include "scope.h"
#include <memory>

struct NumberByDepth : public Number
{
  // Parameters.
  const std::auto_ptr<Column> column;
  /* const */ double max_depth;
  const std::auto_ptr<Number> h;
  const std::auto_ptr<Number> z;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { 
    h->tick (scope, msg);
    z->tick (scope, msg);
  }
  bool missing (const Scope& scope) const 
  { 
    if (h->missing (scope) 
        || !Units::can_convert (h->dimension (scope), Units::cm (),
                                h->value (scope))
        || z->missing (scope) 
        || !Units::can_convert (z->dimension (scope), Units::cm (), 
                                z->value (scope)))
      return true;

    const double height = Units::convert (z->dimension (scope), 
                                          Units::cm (), 
                                          z->value (scope));
    if (height > 0 || height < max_depth)
      return true;

    return false;
  }

  // Create.
  bool initialize (Treelog& msg)
  { 
    bool ok = true;
    Treelog::Open nest (msg, name);
    if (!h->initialize (msg))
      ok = false;
    if (!z->initialize (msg))
      ok = false;
    return ok;
  }
  bool check (const Scope& scope, Treelog& msg) const
  { 
    bool ok = true;
    Treelog::Open nest (msg, name);
    if (!h->check (scope, msg))
      ok = false;
    else if (!Units::can_convert (h->dimension (scope), Units::cm ()))
      {
        msg.error ("Cannot convert pressure [" + h->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        ok = false;
      }
    if (!z->check (scope, msg))
      ok = false;
    else if (!Units::can_convert (z->dimension (scope), Units::cm ()))
      {
        msg.error ("Cannot convert height [" + z->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        ok = false;
      }
    return ok;
  }
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add_object ("column", Column::component, "\
The soil column whose properties we want to examine.");
    syntax.add_object ("h", Number::component, "\
The tension we want to compare with.");
    syntax.add_object ("z", Number::component, "\
The height we want to compare with.");
  }
  NumberByDepth (Block& al)
    : Number (al),
      column (Librarian::build_item<Column> (al, "column")),
      h (Librarian::build_item<Number> (al, "h")),
      z (Librarian::build_item<Number> (al, "z"))
  { 
    Output output;
    Time time (9999, 1, 1, 0);
    const Library& wlib = al.metalib ().library (Weather::component);
    const double T = 10.0;
    AttributeList alist = wlib.lookup (symbol ("none"));
    alist.add ("average", T);
    alist.add ("amplitude", 0.0);
    alist.add ("air_temperature", T);
    alist.add ("type", "none");
    std::auto_ptr<Weather> weather (Librarian::build_alist<Weather>
                                    (al, alist, "initialize"));
    column->initialize (al, output, time, weather.get (), Scope::null ());
    max_depth = column->bottom ();
  }
};

struct NumberDepthTheta : public NumberByDepth
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double pressure 
      = Units::convert (h->dimension (scope), Units::cm (), h->value (scope));
    const double height
      = Units::convert (z->dimension (scope), Units::cm (), z->value (scope));
    const Horizon& horizon = column->horizon_at (height, 0.5, 0.5);
    return horizon.hydraulic->Theta (pressure);
  }

  symbol dimension (const Scope&) const 
  { return Syntax::fraction (); }

  // Create.
  NumberDepthTheta (Block& al)
    : NumberByDepth (al)
  { }
};

static struct NumberDepthThetaSyntax
{
  static Model& make (Block& al)
  { return *new NumberDepthTheta (al); }
  NumberDepthThetaSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find water content (Theta) for a given pressure (h).");
    NumberByDepth::load_syntax (syntax, alist);
    Librarian::add_type (Number::component, "depth_Theta", alist, syntax, &make);
  }
} NumberDepthTheta_syntax;

struct NumberDepthK : public NumberByDepth
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const double pressure 
      = Units::convert (h->dimension (scope), Units::cm (), h->value (scope));
    const double height
      = Units::convert (z->dimension (scope), Units::cm (), z->value (scope));
    const Horizon& horizon = column->horizon_at (height, 0.5, 0.5);
    return horizon.hydraulic->K (pressure);
  }

  symbol dimension (const Scope&) const 
  { return Units::cm_per_h (); }

  // Create.
  NumberDepthK (Block& al)
    : NumberByDepth (al)
  { }
};

static struct NumberDepthKSyntax
{
  static Model& make (Block& al)
  { return *new NumberDepthK (al); }
  NumberDepthKSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find water conductivity (K) for a given pressure (h).");
    NumberByDepth::load_syntax (syntax, alist);
    Librarian::add_type (Number::component, "depth_K", alist, syntax, &make);
  }
} NumberDepthK_syntax;

struct NumberByTension : public Number
{
  // Parameters.
  const std::auto_ptr<Horizon> horizon;
  const std::auto_ptr<Number> h;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { h->tick (scope, msg); }
  bool missing (const Scope& scope) const 
  { return h->missing (scope) 
      || !Units::can_convert (h->dimension (scope), Units::cm (),
                              h->value (scope)); }

  // Create.
  bool initialize (Treelog& msg)
  { 
    bool ok = true;
    Treelog::Open nest (msg, name);
    if (!h->initialize (msg))
      ok = false;
    return ok;
  }
  bool check (const Scope& scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    if (!h->check (scope, msg))
      return false;
    if (!Units::can_convert (h->dimension (scope), Units::cm ()))
      {
        msg.error ("Cannot convert [" + h->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        return false;
      }
    return true;
  }
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add_object ("horizon", Horizon::component, "\
The soil horizon whose properties we want to examine.");
    syntax.add_object ("h", Number::component, "\
The tension we want to compare with.");
    syntax.add ("top_soil", Syntax::Boolean, Syntax::Const, "\
Set this to true for the A horizon.");
  }
  NumberByTension (Block& al)
    : Number (al),
      horizon (Librarian::build_item<Horizon> (al, "horizon")),
      h (Librarian::build_item<Number> (al, "h"))
  { horizon->initialize (al.flag ("top_soil"), 2, al.msg ()); }
};

struct NumberSoilTheta : public NumberByTension
{
  // Simulation.
  void tick (const Scope&, Treelog&)
  { }
  double value (const Scope& scope) const
  { 
    return horizon->hydraulic->Theta (Units::convert (h->dimension (scope), 
                                                      Units::cm (), 
                                                      h->value (scope)));
  }
  symbol dimension (const Scope&) const 
  { return Syntax::fraction (); }

  // Create.
  NumberSoilTheta (Block& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilThetaSyntax
{
  static Model& make (Block& al)
  { return *new NumberSoilTheta (al); }
  NumberSoilThetaSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find water content (Theta) for a given pressure (h).");
    NumberByTension::load_syntax (syntax, alist);
    Librarian::add_type (Number::component, "soil_Theta", alist, syntax, &make);
  }
} NumberSoilTheta_syntax;

struct NumberSoilK : public NumberByTension
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    return horizon->hydraulic->K (Units::convert (h->dimension (scope), 
                                                  Units::cm (), 
                                                  h->value (scope)));
  }
  symbol dimension (const Scope&) const 
  { return Units::cm_per_h (); }

  // Create.
  NumberSoilK (Block& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilKSyntax
{
  static Model& make (Block& al)
  { return *new NumberSoilK (al); }
  NumberSoilKSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find hydraulic conductivity (K) for a given pressure (h).");
    NumberByTension::load_syntax (syntax, alist);
    Librarian::add_type (Number::component, "soil_K", alist, syntax, &make);
  }
} NumberSoilK_syntax;

struct NumberSoilHeatCapacity : public NumberByTension
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const symbol my_dim = h->dimension (scope);
    const double my_val = h->value (scope);
    const double my_h = Units::convert (my_dim, Units::cm (), my_val);
    const double Theta = horizon->hydraulic->Theta (my_h);
    return horizon->heat_capacity (Theta, 0.0);
  }
  symbol dimension (const Scope&) const 
  { 
    static const symbol dim ("erg/cm^3/dg C");
    return dim;
  }

  // Create.
  NumberSoilHeatCapacity (Block& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilHeatCapacitySyntax
{
  static Model& make (Block& al)
  { return *new NumberSoilHeatCapacity (al); }
  NumberSoilHeatCapacitySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find heat capacity for a given pressure (h).");
    NumberByTension::load_syntax (syntax, alist);
    Librarian::add_type (Number::component, "soil_heat_capacity", alist, syntax, &make);
  }
} NumberSoilHeatCapacity_syntax;

struct NumberSoilHeatConductivity : public NumberByTension
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const symbol my_dim = h->dimension (scope);
    const double my_val = h->value (scope);
    const double my_h = Units::convert (my_dim, Units::cm (), my_val);
    const double Theta = horizon->hydraulic->Theta (my_h);
    return horizon->heat_conductivity (Theta, 0.0);
  }
  symbol dimension (const Scope&) const 
  { 
    static const symbol dim ("erg/cm/h/dg C");
    return dim;
  }

  // Create.
  NumberSoilHeatConductivity (Block& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilHeatConductivitySyntax
{
  static Model& make (Block& al)
  { return *new NumberSoilHeatConductivity (al); }
  NumberSoilHeatConductivitySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find heat conductivity for a given pressure (h).");
    NumberByTension::load_syntax (syntax, alist);
    Librarian::add_type (Number::component, "soil_heat_conductivity", alist, syntax, &make);
  }
} NumberSoilHeatConductivity_syntax;

struct NumberTensionByTheta : public Number
{
  // Parameters.
  const std::auto_ptr<Horizon> horizon;
  const std::auto_ptr<Number> Theta;

  // Simulation.
  void tick (const Scope& scope, Treelog& msg)
  { Theta->tick (scope, msg); }
  bool missing (const Scope& scope) const 
  { return Theta->missing (scope) 
      || !Units::can_convert (Theta->dimension (scope), Syntax::fraction (),
                              Theta->value (scope)); }
  double value (const Scope& scope) const
  { 
    return horizon->hydraulic->h (Units::convert (Theta->dimension (scope), 
                                                  Syntax::fraction (), 
                                                  Theta->value (scope)));
  }
  symbol dimension (const Scope&) const 
  { return Units::cm (); }

  // Create.
  bool initialize (Treelog& msg)
  {
    Treelog::Open nest (msg, name);
    return Theta->initialize (msg);
  }
  bool check (const Scope& scope, Treelog& msg) const
  { 
    Treelog::Open nest (msg, name);
    if (!Theta->check (scope, msg))
      return false;
    if (!Units::can_convert (Theta->dimension (scope), Syntax::fraction ()))
      {
        msg.error ("Cannot convert [" + Theta->dimension (scope) 
                   + "] to fraction for soil hydraulics");
        return false;
      }
    return true;
  }
  NumberTensionByTheta (Block& al)
    : Number (al),
      horizon (Librarian::build_item<Horizon> (al, "horizon")),
               Theta (Librarian::build_item<Number> (al, "Theta"))
  { horizon->initialize (al.flag ("top_soil"), 2, al.msg ()); }
};

static struct NumberTensionByThetaSyntax
{
  static Model& make (Block& al)
  { return *new NumberTensionByTheta (al); }
  NumberTensionByThetaSyntax()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find pressure (h) for a given water content (Theta).");
    syntax.add_object ("horizon", Horizon::component, "\
The soil horizon whose properties we want to examine.");
    syntax.add_object ("Theta", Number::component, "\
The water content we want to compare with.");
    syntax.add ("top_soil", Syntax::Boolean, Syntax::Const, "\
Set this to true for the A horizon.");
    Librarian::add_type (Number::component, "soil_h", alist, syntax, &make);
  }
} NumberTensionByTheta_syntax;

// number_soil.C ends here
