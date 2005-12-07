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


#include "number.h"
#include "column.h"
#include "horizon.h"
#include "hydraulic.h"
#include "weather.h"
#include "time.h"
#include "units.h"
#include <memory>

struct NumberByDepth : public Number
{
  // Parameters.
  const std::auto_ptr<Column> column;
  /* const */ double max_depth;
  const std::auto_ptr<Number> h;
  const std::auto_ptr<Number> z;

  // Simulation.
  bool missing (const Scope& scope) const 
  { 
    if (h->missing (scope) 
        || !Units::can_convert (h->dimension (scope), "cm", h->value (scope))
        || z->missing (scope) 
        || !Units::can_convert (z->dimension (scope), "cm", z->value (scope)))
      return true;

    const double height = Units::convert (z->dimension (scope), 
                                          "cm", 
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
    else if (!Units::can_convert (h->dimension (scope), "cm"))
      {
        msg.error ("Cannot convert pressure [" + h->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        ok = false;
      }
    if (!z->check (scope, msg))
      ok = false;
    else if (!Units::can_convert (z->dimension (scope), "cm"))
      {
        msg.error ("Cannot convert height [" + z->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        ok = false;
      }
    return ok;
  }
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add ("column", Librarian<Column>::library (), "\
The soil column whose properties we want to examine.");
    syntax.add ("h", Librarian<Number>::library (), "\
The tension we want to compare with.");
    syntax.add ("z", Librarian<Number>::library (), "\
The height we want to compare with.");
  }
  static double find_max_depth (const Column& column)
  {
    const size_t size = column.count_layers ();
    double now = 0;
    for (size_t i = 0; i < size; i++)
      now -= column.get_dz (i);
    return now;
  }
  NumberByDepth (Block& al)
    : Number (al),
      column (Librarian<Column>::build_item (al, "column")),
      h (Librarian<Number>::build_item (al, "h")),
      z (Librarian<Number>::build_item (al, "z"))
  { 
    Time time (9999, 1, 1, 0);
    const Library& wlib = Librarian<Weather>::library ();
    const double T = 10.0;
    AttributeList alist = wlib.lookup (symbol ("none"));
    alist.add ("average", T);
    alist.add ("amplitude", 0.0);
    alist.add ("air_temperature", T);
    alist.add ("type", "none");
    std::auto_ptr<Weather> weather (Librarian<Weather>::build_alist
                                    (al, alist, "initialize"));
    column->initialize (time, al.msg (), weather.get ());
    max_depth = find_max_depth (*column);
  }
};

struct NumberDepthTheta : public NumberByDepth
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const size_t size = column->count_layers ();
    const double pressure = Units::convert (h->dimension (scope), 
                                            "cm", 
                                            h->value (scope));
    std::vector<double> v (size, pressure);
    column->put_water_pressure (v);
    const double height = Units::convert (z->dimension (scope), 
                                          "cm", 
                                          z->value (scope));
    double now = 0;
    for (size_t i = 0; i < size; i++)
      {
        now -= column->get_dz (i);
        if (height > now)
          return column->get_water_content_at (i);
      }
    daisy_assert (size > 0);
    return column->get_water_content_at (size - 1);
  }

  const std::string& dimension (const Scope&) const 
  { return Syntax::Fraction (); }

  // Create.
  NumberDepthTheta (Block& al)
    : NumberByDepth (al)
  { }
};

static struct NumberDepthThetaSyntax
{
  static Number& make (Block& al)
  { return *new NumberDepthTheta (al); }
  NumberDepthThetaSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find water content (Theta) for a given pressure (h).");
    NumberByDepth::load_syntax (syntax, alist);
    Librarian<Number>::add_type ("depth_Theta", alist, syntax, &make);
  }
} NumberDepthTheta_syntax;

struct NumberDepthK : public NumberByDepth
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    const size_t size = column->count_layers ();
    const double pressure = Units::convert (h->dimension (scope), 
                                            "cm", 
                                            h->value (scope));
    std::vector<double> v (size, pressure);
    column->put_water_pressure (v);
    const double height = Units::convert (z->dimension (scope), 
                                          "cm", 
                                          z->value (scope));
    double now = 0;
    for (size_t i = 0; i < size; i++)
      {
        now -= column->get_dz (i);
        if (height > now)
          return column->get_water_conductivity_at (i);
      }
    daisy_assert (size > 0);
    return column->get_water_conductivity_at (size - 1);
  }

  const std::string& dimension (const Scope&) const 
  { 
    static const std::string dim = "cm/h";
    return dim;
  }

  // Create.
  NumberDepthK (Block& al)
    : NumberByDepth (al)
  { }
};

static struct NumberDepthKSyntax
{
  static Number& make (Block& al)
  { return *new NumberDepthK (al); }
  NumberDepthKSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find water conductivity (K) for a given pressure (h).");
    NumberByDepth::load_syntax (syntax, alist);
    Librarian<Number>::add_type ("depth_K", alist, syntax, &make);
  }
} NumberDepthK_syntax;

struct NumberByTension : public Number
{
  // Parameters.
  const std::auto_ptr<Horizon> horizon;
  const std::auto_ptr<Number> h;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return h->missing (scope) 
      || !Units::can_convert (h->dimension (scope), "cm", h->value (scope)); }

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
    if (!Units::can_convert (h->dimension (scope), "cm"))
      {
        msg.error ("Cannot convert [" + h->dimension (scope) 
                   + "] to [cm] for soil hydraulics");
        return false;
      }
    return true;
  }
  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add ("horizon", Librarian<Horizon>::library (), "\
The soil horizon whose properties we want to examine.");
    syntax.add ("h", Librarian<Number>::library (), "\
The tension we want to compare with.");
    syntax.add ("top_soil", Syntax::Boolean, Syntax::Const, "\
Set this to true for the A horizon.");
  }
  NumberByTension (Block& al)
    : Number (al),
      horizon (Librarian<Horizon>::build_item (al, "horizon")),
      h (Librarian<Number>::build_item (al, "h"))
  { horizon->initialize (al.flag ("top_soil"), 2, al.msg ()); }
};

struct NumberSoilTheta : public NumberByTension
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    return horizon->hydraulic->Theta (Units::convert (h->dimension (scope), 
                                                      "cm", 
                                                      h->value (scope)));
  }
  const std::string& dimension (const Scope&) const 
  { return Syntax::Fraction (); }

  // Create.
  NumberSoilTheta (Block& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilThetaSyntax
{
  static Number& make (Block& al)
  { return *new NumberSoilTheta (al); }
  NumberSoilThetaSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find water content (Theta) for a given pressure (h).");
    NumberByTension::load_syntax (syntax, alist);
    Librarian<Number>::add_type ("soil_Theta", alist, syntax, &make);
  }
} NumberSoilTheta_syntax;

struct NumberSoilK : public NumberByTension
{
  // Simulation.
  double value (const Scope& scope) const
  { 
    return horizon->hydraulic->K (Units::convert (h->dimension (scope), 
                                                  "cm", 
                                                  h->value (scope)));
  }
  const std::string& dimension (const Scope&) const 
  { 
    static const std::string dim = "cm/h"; 
    return dim;
  }

  // Create.
  NumberSoilK (Block& al)
    : NumberByTension (al)
  { }
};

static struct NumberSoilKSyntax
{
  static Number& make (Block& al)
  { return *new NumberSoilK (al); }
  NumberSoilKSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find hydraulic conductivity (K) for a given pressure (h).");
    NumberByTension::load_syntax (syntax, alist);
    Librarian<Number>::add_type ("soil_K", alist, syntax, &make);
  }
} NumberSoilK_syntax;

struct NumberTensionByTheta : public Number
{
  // Parameters.
  const std::auto_ptr<Horizon> horizon;
  const std::auto_ptr<Number> Theta;

  // Simulation.
  bool missing (const Scope& scope) const 
  { return Theta->missing (scope) 
      || !Units::can_convert (Theta->dimension (scope), Syntax::Fraction (), 
                              Theta->value (scope)); }
  double value (const Scope& scope) const
  { 
    return horizon->hydraulic->h (Units::convert (Theta->dimension (scope), 
                                                  Syntax::Fraction (), 
                                                  Theta->value (scope)));
  }
  const std::string& dimension (const Scope&) const 
  { 
    static const std::string dim = "cm"; 
    return dim;
  }

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
    if (!Units::can_convert (Theta->dimension (scope), Syntax::Fraction ()))
      {
        msg.error ("Cannot convert [" + Theta->dimension (scope) 
                   + "] to fraction for soil hydraulics");
        return false;
      }
    return true;
  }
  NumberTensionByTheta (Block& al)
    : Number (al),
      horizon (Librarian<Horizon>::build_item (al, "horizon")),
               Theta (Librarian<Number>::build_item (al, "Theta"))
  { horizon->initialize (al.flag ("top_soil"), 2, al.msg ()); }
};

static struct NumberTensionByThetaSyntax
{
  static Number& make (Block& al)
  { return *new NumberTensionByTheta (al); }
  NumberTensionByThetaSyntax()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    alist.add ("description", 
	       "Find pressure (h) for a given water content (Theta).");
    syntax.add ("horizon", Librarian<Horizon>::library (), "\
The soil horizon whose properties we want to examine.");
    syntax.add ("Theta", Librarian<Number>::library (), "\
The water content we want to compare with.");
    syntax.add ("top_soil", Syntax::Boolean, Syntax::Const, "\
Set this to true for the A horizon.");
    Librarian<Number>::add_type ("soil_h", alist, syntax, &make);
  }
} NumberTensionByTheta_syntax;

// number_soil.C ends here
