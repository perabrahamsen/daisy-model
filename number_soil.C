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
#include "horizon.h"
#include "hydraulic.h"
#include "units.h"
#include <memory>

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
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    if (!h->check (scope, err))
      return false;
    if (!Units::can_convert (h->dimension (scope), "cm"))
      {
        err.error ("Cannot convert [" + h->dimension (scope) 
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
  bool check (const Scope& scope, Treelog& err) const
  { 
    Treelog::Open nest (err, name);
    if (!Theta->check (scope, err))
      return false;
    if (!Units::can_convert (Theta->dimension (scope), Syntax::Fraction ()))
      {
        err.error ("Cannot convert [" + Theta->dimension (scope) 
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
