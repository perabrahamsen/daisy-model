// net_radiation.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2021 KU
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
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"

// The 'net_radiation' component.

const char *const NetRadiation::component = "net_radiation";

symbol
NetRadiation::library_id () const
{
  static const symbol id (component);
  return id;
}

const double NetRadiation::SurEmiss = 0.98;
const double NetRadiation::SB = 5.67e-8; // [W/m^2/K^4]

double
NetRadiation::cloudiness_function (double Cloudiness,
				   double black_body_radiation, 
				   double epsilon_0)
{
  const double s = 1 - Cloudiness; // fraction of cloud sky
  return (s + (1 - s) * epsilon_0) * black_body_radiation;   
}

void
NetRadiation::output (Log&) const
{ }

NetRadiation::NetRadiation (const BlockModel& al)
  : ModelDerived (al.type_name ())
{ }

NetRadiation::~NetRadiation ()
{ }

static struct NetRadiationInit : public DeclareComponent 
{
  NetRadiationInit ()
    : DeclareComponent (NetRadiation::component, "\
Calculate net radiation from meteorological data.")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiation_init;

// The 'brunt' model.

struct NetRadiationBrunt : public NetRadiation
{
  const double a;
  const double b;

  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double VapourPressure, double) const
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

  NetRadiationBrunt (const BlockModel& al)
    : NetRadiation (al),
      a (al.number ("a")),
      b (al.number ("b"))
  { }
};

static struct NetRadiationBruntSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationBrunt (al); }

  NetRadiationBruntSyntax ()
    : DeclareModel (NetRadiation::component, "brunt", "\
Brunt, 1932.  Default parameterization by Jensen et.al., 1990.\n\
FAO recommendation.")
  { }
  void load_frame (Frame& frame) const
  {
    // Brunt.
  frame.declare ("a", Attribute::None (), Attribute::Const,
              "Brunt 'a' parameter (offset).");
  frame.set ("a", 0.34);
  frame.declare ("b", "1/sqrt(kPa)", Attribute::Const,
              "Brunt 'b' parameter (vapor pressure factor).");
  frame.set ("b", 0.14);
  }
} NetRadiationBrunt_syntax;

// The 'idso_jackson' model.

struct NetRadiationIdsoJackson : public NetRadiation
{
  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double, double) const
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
  
  NetRadiationIdsoJackson (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationIJSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationIdsoJackson (al); }

  NetRadiationIJSyntax ()
    : DeclareModel (NetRadiation::component, "idso_jackson", "Idso and Jackson, 1969")
  { }
  void load_frame (Frame& frame) const
  { }
} NetRadiationIH_syntax;

// The 'brutsaert' model.

struct NetRadiationBrutsaert : public NetRadiation
{
  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double VapourPressure, double) const
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

  NetRadiationBrutsaert (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationBrutsaertSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationBrutsaert (al); }

  NetRadiationBrutsaertSyntax ()
    : DeclareModel (NetRadiation::component, "brutsaert", "Brutsaert, 1975")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationBrutsaert_syntax;

// The 'swinbank' model.

struct NetRadiationSwinbank : public NetRadiation
{
  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double, double) const
  {
    const double NetEmiss = SurEmiss * (1 - 0.92e-5 * pow (Temp + 273, 2));
    return (Cloudiness * NetEmiss * SB * pow (Temp + 273, 4));
  }

  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 9.2E-6;     // [K^-2]
    return (A * sqr (Ta));
  }

  NetRadiationSwinbank (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationSwinbankSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationSwinbank (al); }

  NetRadiationSwinbankSyntax ()
    : DeclareModel (NetRadiation::component, "swinbank", "Swinbank, 1963")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationSwinbank_syntax;

// The 'satterlund' model.

struct NetRadiationSatterlund : public NetRadiation
{
  double NetLongwaveRadiation(double Cloudiness, // [W/m2]
			      double Temp,
			      double VapourPressure, double) const
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

  NetRadiationSatterlund (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationSatterlundSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationSatterlund (al); }

  NetRadiationSatterlundSyntax ()
    : DeclareModel (NetRadiation::component, "satterlund", "Satterlund, 1979")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationSatterlund_syntax;

// The 'prata' model.

struct NetRadiationPrata : public NetRadiation
{
  double NetLongwaveRadiation (double, 
			       double Temp,
			       double,
			       double L_ia) const // [W/m^2]
  {
    const double Ta = Temp + 273.15; // [K]
    const double black_body_radiation = SB * std::pow (Ta, 4.0);
    return SurEmiss * black_body_radiation - L_ia;
  } 

  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 1.2;      // []
    const double B = 3.0;      // []
    const double w = 46.5 * ea / Ta; // []
    return (1 - (1 + w) * exp ( - sqrt( A + B * w)));
  }

  NetRadiationPrata (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationPrataSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationPrata (al); }

  NetRadiationPrataSyntax ()
    : DeclareModel (NetRadiation::component, "prata", "Prata, 1996")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationPrata_syntax;

// net_radiation.C ends here
