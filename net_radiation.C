// net_radiation.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "net_radiation.h"
#include "log.h"
#include "weather.h"
#include "block.h"
#include "syntax.h"
#include "mathlib.h"
#include "librarian.h"

const char *const NetRadiation::component = "net_radiation";

symbol
NetRadiation::library_id () const
{
  static const symbol id (component);
  return id;
}

// Interface class.

NetRadiation::NetRadiation (Block& al)
  : ModelLogable (al.identifier ("type"))
{ }

NetRadiation::~NetRadiation ()
{ }

// Intermediate class

struct NetRadiationParent : public NetRadiation
{
  static const double SurEmiss;
  static const double SB;       // Stefan-Boltzmann constant [W/m^2/K^4]

  // State
  double net_radiation_;        // [W/m^2]
  double L_n;                   // Net longwave radiation [W/m^2]
  double L_ia;                  // incoming longwave radiation [W/m^2]
  double L_i0;         // clear sky incoming longwave radiation [W/m^2]
  double epsilon_0;             // [unitless]
  double black_body_radiation;  // [W/m^2]
  // Utilities
  virtual double NetLongwaveRadiation (double Cloudiness, // [W/m^2]
				       double Temp,
				       double VapourPressure) const 
  { return SurEmiss * black_body_radiation - L_ia; } 

  virtual double find_epsilon_0 (double Ta, double ea) const = 0;

  static double cloudiness_function (double Cloudiness, double black_body_radiation, 
                                     double epsilon_0)
  {
    const double s = 1 - Cloudiness; // fraction of cloud sky
    return (s + (1 - s) * epsilon_0) * black_body_radiation;   
  }

  // Simulation.
  void output (Log&) const;
  double net_radiation () const
  { return net_radiation_; }
  double incoming_longwave_radiation () const
  { return L_ia; }
  void tick (double Cloudiness, double Temp,
	     double VapourPressure, double Si,
	     double Albedo, Treelog&)
  {
    const double Ta = Temp + 273.15; // [K]
    black_body_radiation = SB * pow(Ta, 4);
    const double ea = VapourPressure / 100.; // Pa -> hPa
    epsilon_0 = find_epsilon_0 (Ta, ea);
    L_i0 = epsilon_0 * black_body_radiation;
    L_ia = cloudiness_function (Cloudiness, black_body_radiation, epsilon_0);
    VapourPressure *= 0.001;	// Pa -> kPa
    L_n  = - NetLongwaveRadiation (Cloudiness, Temp, VapourPressure);
    net_radiation_ = (1.0 - Albedo) * Si + L_n;
  }

  // Create & Destroy.
  static void load_base (Syntax& syntax, AttributeList& alist);
  NetRadiationParent (Block& al)
    : NetRadiation (al),
      net_radiation_ (0.0),
      L_n (0.0),
      L_ia (0.0),
      L_i0 (0.0),
      epsilon_0 (0.0),
      black_body_radiation (0.0)
  { }
};

const double NetRadiationParent::SurEmiss = 0.98;
const double NetRadiationParent::SB = 5.67e-8; // [W/m^2/K^4]

void
NetRadiationParent::output (Log& log) const
{
  output_value (net_radiation (), "net_radiation", log);
  output_variable (L_n, log);
  output_variable (L_ia, log);
  output_variable (L_i0, log);
  output_variable (epsilon_0, log);
  output_variable (black_body_radiation, log);  
}

void
NetRadiationParent::load_base (Syntax& syntax, AttributeList& alist)
{
  // Logs.
  alist.add ("base_model", "common");
  alist.add ("description", "\
This is not a model, but a list of parameters shared by all net radiation models.");

  syntax.add ("net_radiation", "W/m^2", Syntax::LogOnly,
              "The calculated net radiation (positive downwards).");
  syntax.add ("L_n", "W/m^2", Syntax::LogOnly,
              "The calculated net longwave radiation (positive downwards).");
  syntax.add ("L_ia", "W/m^2", Syntax::LogOnly,
              "The calculated incoming longwave radiation (positive downwards).");
  syntax.add ("L_i0", "W/m^2", Syntax::LogOnly,
              "The calculated clear sky incoming longwave radiation (positive downwards).");
  syntax.add ("epsilon_0", Syntax::None(), Syntax::LogOnly,
              "Atmospheric effective clearsky emmisivity (range 0-1).");
  syntax.add ("black_body_radiation", "W/m^2", Syntax::LogOnly,
              "Radiation emitted by black bodies at current air temperature.\n\
Stefan-Boltzmann's law.");
}

static struct NetRadiationParentSyntax
{
  NetRadiationParentSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    NetRadiationParent::load_base (syntax, alist);

    Librarian::add_base (NetRadiation::component, alist, syntax);
  }
} NetRadiationParent_syntax;

// Model classes.

struct NetRadiationBrunt : public NetRadiationParent
{
  const double a;
  const double b;

  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double VapourPressure) const
  {
    const double NetEmiss = a - b * sqrt (VapourPressure);
#if 0
    cout << "NetEmiss       " << NetEmiss << "\n";
    cout << "VapourPressure " << VapourPressure << "\n";
    cout << "Cloudiness     " << Cloudiness << "\n";
#endif
    return (Cloudiness * NetEmiss * SB * pow (Temp + 273, 4));
  }
  
  double find_epsilon_0 (double Ta, double ea) const
  {
    const double A = SurEmiss - a;   // []
    const double B = b / sqrt (10); // [hPa^-½]
    return A + B * sqrt (ea); 
  }

  static void load_syntax (Syntax& syntax, AttributeList& alist);
  NetRadiationBrunt (Block& al)
    : NetRadiationParent (al),
      a (al.number ("a")),
      b (al.number ("b"))
  { }
};

void
NetRadiationBrunt::load_syntax (Syntax& syntax, AttributeList& alist)
{
  NetRadiationParent::load_base (syntax, alist);
  alist.add ("description", "\
Brunt, 1932.  Default parameterization by Jensen et.al., 1990.\n\
FAO recommendation.");
  syntax.add ("a", Syntax::None (), Syntax::Const,
              "Brunt 'a' parameter (offset).");
  alist.add ("a", 0.34);
  syntax.add ("b", "1/sqrt(kPa)", Syntax::Const,
              "Brunt 'b' parameter (vapor pressure factor).");
  alist.add ("b", 0.14);
}

const AttributeList& 
NetRadiation::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      NetRadiationBrunt::load_syntax (dummy, alist);
      alist.add ("type", "brunt");
    }
  return alist;
}

struct NetRadiationIdsoJackson : public NetRadiationParent
{
  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double) const
  {
    const double NetEmiss = 0.261 * exp (-7.77e-4 * pow (Temp, 2)) - 0.02;
    return (Cloudiness * NetEmiss * SB * pow (Temp + 273, 4));
  }

  double find_epsilon_0 (double Ta, double ea) const
  {
    const double A = 0.261;     // []
    const double B = -0.000777; // [K^-2]
    return (1 - A * exp(B * sqr (273.15 - Ta)));
  }
  
  NetRadiationIdsoJackson (Block& al)
    : NetRadiationParent (al)
  { }
};

struct NetRadiationBrutsaert : public NetRadiationParent
{
  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double VapourPressure) const
  {
    const double ea = VapourPressure / 10.0;   /*mb*/
    const double NetEmiss
      = SurEmiss * (1 - 1.24 * pow (ea / (Temp + 273), 1.0 / 7.0));
    return (Cloudiness * NetEmiss * SB * pow (Temp + 273, 4));
  }

  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 1.24;     // [K^1/7 hPa^-1/7]
    return (A * pow(ea/Ta, 1./7.));
  }

  NetRadiationBrutsaert (Block& al)
    : NetRadiationParent (al)
  { }
};

struct NetRadiationSwinbank : public NetRadiationParent
{
  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double) const
  {
    const double NetEmiss = SurEmiss * (1 - 0.92e-5 * pow (Temp + 273, 2));
    return (Cloudiness * NetEmiss * SB * pow (Temp + 273, 4));
  }

  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 9.2E-6;     // [K^-2]
    return (A * sqr (Ta));
  }

  NetRadiationSwinbank (Block& al)
    : NetRadiationParent (al)
  { }
};

struct NetRadiationSatterlund : public NetRadiationParent
{
  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double VapourPressure) const
  {
    const double ea = VapourPressure / 10.0;   /*mb*/
    const double NetEmiss
      = SurEmiss * (1 - 1.08 * (1 - exp (-pow (ea, (Temp + 273) / 2016.0))));
    return (Cloudiness * NetEmiss * SB * pow (Temp + 273, 4));
  }

  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 1.08;      // [?]
    const double B = 2016.;      // [?]
    return (A * (1 - exp(- pow(ea, Ta/B))));
  }

  NetRadiationSatterlund (Block& al)
    : NetRadiationParent (al)
  { }
};

struct NetRadiationPrata : public NetRadiationParent
{
  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 1.2;      // []
    const double B = 3.0;      // []
    const double w = 46.5 * ea / Ta; // []
    return (1 - (1 + w) * exp ( - sqrt( A + B * w)));
  }

  NetRadiationPrata (Block& al)
    : NetRadiationParent (al)
  { }
};

// Add models to library.

static struct NetRadiationSyntax
{
  static Model& make_brunt (Block& al)
  { return *new NetRadiationBrunt (al); }
  static Model& make_idso_jackson (Block& al)
  { return *new NetRadiationIdsoJackson (al); }
  static Model& make_brutsaert (Block& al)
  { return *new NetRadiationBrutsaert (al); }
  static Model& make_swinbank (Block& al)
  { return *new NetRadiationSwinbank (al); }
  static Model& make_satterlund (Block& al)
  { return *new NetRadiationSatterlund (al); }
  static Model& make_prata (Block& al)
  { return *new NetRadiationPrata (al); }

  NetRadiationSyntax ()
  {
    // Brunt.
    Syntax& syntax_brunt = *new Syntax ();
    AttributeList& alist_brunt = *new AttributeList ();
    NetRadiationBrunt::load_syntax (syntax_brunt, alist_brunt);
    Librarian::add_type (NetRadiation::component, "brunt",
                         alist_brunt, syntax_brunt,
                         &make_brunt);
    // Others.
    Syntax& syntax_idso_jackson = *new Syntax ();
    AttributeList& alist_idso_jackson = *new AttributeList ();
    NetRadiationParent::load_base (syntax_idso_jackson, alist_idso_jackson);
    alist_idso_jackson.add ("description", "Idso and Jackson, 1969");
    Librarian::add_type (NetRadiation::component, "idso_jackson",
                         alist_idso_jackson, syntax_idso_jackson,
                         &make_idso_jackson);

    Syntax& syntax_brutsaert = *new Syntax ();
    AttributeList& alist_brutsaert = *new AttributeList ();
    NetRadiationParent::load_base (syntax_brutsaert, alist_brutsaert);
    alist_brutsaert.add ("description", "Brutsaert, 1975");
    Librarian::add_type (NetRadiation::component, "brutsaert",
                         alist_brutsaert, syntax_brutsaert,
                         &make_brutsaert);

    Syntax& syntax_swinbank = *new Syntax ();
    AttributeList& alist_swinbank = *new AttributeList ();
    NetRadiationParent::load_base (syntax_swinbank, alist_swinbank);
    alist_swinbank.add ("description", "Swinbank, 1963");
    Librarian::add_type (NetRadiation::component, "swinbank",
                         alist_swinbank, syntax_swinbank,
                         &make_swinbank);

    Syntax& syntax_satterlund = *new Syntax ();
    AttributeList& alist_satterlund = *new AttributeList ();
    NetRadiationParent::load_base (syntax_satterlund, alist_satterlund);
    alist_satterlund.add ("description", "Satterlund, 1979");
    Librarian::add_type (NetRadiation::component, "satterlund", 
                         alist_satterlund, syntax_satterlund,
                         &make_satterlund);

    Syntax& syntax_prata = *new Syntax ();
    AttributeList& alist_prata = *new AttributeList ();
    NetRadiationParent::load_base (syntax_prata, alist_prata);
    alist_satterlund.add ("description", "Prata, 1996");
    Librarian::add_type (NetRadiation::component, "prata", 
                         alist_prata, syntax_prata,
                         &make_prata);
  }
} NetRadiation_syntax;

static Librarian NetRadiation_init (NetRadiation::component, "\
The purpose of this component is to calculate the net radiation from\n\
other meteorological data.");

// net_radiation.C ends here
