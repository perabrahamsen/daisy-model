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
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"

const char *const NetRadiation::component = "net_radiation";

symbol
NetRadiation::library_id () const
{
  static const symbol id (component);
  return id;
}

// Interface class.

NetRadiation::NetRadiation (Block& al)
  : ModelLogable (al.name ("type"))
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

  NetRadiationBrunt (Block& al)
    : NetRadiationParent (al),
      a (al.number ("a")),
      b (al.number ("b"))
  { }
};

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

static struct NetRadiationBruntSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NetRadiationBrunt (al); }

  NetRadiationBruntSyntax ()
    : DeclareModel (NetRadiation::component, "brunt", "\
Brunt, 1932.  Default parameterization by Jensen et.al., 1990.\n\
FAO recommendation.")
  { }
  void load_frame (Frame& frame) const
  {
    // Brunt.
  frame.add ("a", Value::None (), Value::Const,
              "Brunt 'a' parameter (offset).");
  frame.add ("a", 0.34);
  frame.add ("b", "1/sqrt(kPa)", Value::Const,
              "Brunt 'b' parameter (vapor pressure factor).");
  frame.add ("b", 0.14);
  }
} NetRadiationBrunt_syntax;

static struct NetRadiationIJSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NetRadiationIdsoJackson (al); }

  NetRadiationIJSyntax ()
    : DeclareModel (NetRadiation::component, "idso_jackson", "Idso and Jackson, 1969")
  { }
  void load_frame (Frame& frame) const
  { }
} NetRadiationIH_syntax;

static struct NetRadiationBrutsaertSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NetRadiationBrutsaert (al); }

  NetRadiationBrutsaertSyntax ()
    : DeclareModel (NetRadiation::component, "brutsaert", "Brutsaert, 1975")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationBrutsaert_syntax;

static struct NetRadiationSwinbankSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NetRadiationSwinbank (al); }

  NetRadiationSwinbankSyntax ()
    : DeclareModel (NetRadiation::component, "swinbank", "Swinbank, 1963")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationSwinbank_syntax;

static struct NetRadiationSatterlundSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NetRadiationSatterlund (al); }

  NetRadiationSatterlundSyntax ()
    : DeclareModel (NetRadiation::component, "satterlund", "Satterlund, 1979")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationSatterlund_syntax;

static struct NetRadiationPrataSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new NetRadiationPrata (al); }

  NetRadiationPrataSyntax ()
    : DeclareModel (NetRadiation::component, "prata", "Prata, 1996")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationPrata_syntax;

static struct NetRadiationInit : public DeclareComponent 
{
  NetRadiationInit ()
    : DeclareComponent (NetRadiation::component, "\
The purpose of this component is to calculate the net radiation from\n\
other meteorological data.")
  { }
  void load_frame (Frame& frame) const
  {
    // Logs.
    frame.add ("net_radiation", "W/m^2", Value::LogOnly,
                "The calculated net radiation (positive downwards).");
    frame.add ("L_n", "W/m^2", Value::LogOnly,
                "The calculated net longwave radiation (positive downwards).");
    frame.add ("L_ia", "W/m^2", Value::LogOnly,
                "The calculated incoming longwave radiation (positive downwards).");
    frame.add ("L_i0", "W/m^2", Value::LogOnly,
                "The calculated clear sky incoming longwave radiation (positive downwards).");
    frame.add ("epsilon_0", Value::None(), Value::LogOnly,
                "Atmospheric effective clearsky emmisivity (range 0-1).");
    frame.add ("black_body_radiation", "W/m^2", Value::LogOnly, "\
Radiation emitted by black bodies at current air temperature.\n\
Stefan-Boltzmann's law.");
  }
} NetRadiation_init;

// net_radiation.C ends here
