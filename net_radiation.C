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


#include "net_radiation.h"
#include "log.h"
#include "weather.h"

EMPTY_TEMPLATE
Librarian<NetRadiation>::Content* Librarian<NetRadiation>::content = NULL;

const char *const NetRadiation::description = "\
The purpose of this component is to calculate the net radiation from\n\
other meteorological data.";

// Interface class.

void
NetRadiation::output (Log& log) const
{
  log.output ("net_radiation", net_radiation ());
}

NetRadiation::NetRadiation (const AttributeList& al)
  : name (al.name ("type"))
{ }

NetRadiation::~NetRadiation ()
{ }

// Intermediate class

struct NetRadiationParent : public NetRadiation
{
  static const double SurEmiss;
  static const double SB;

  // State
  double net_radiation_;

  // Utilities
  virtual double NetLongwaveRadiation (double Cloudiness, // [W/m2]
				       double Temp,
				       double VapourPressure) const = 0;
  // Simulation.
  double net_radiation () const
    { return net_radiation_; }
  void tick (double Cloudiness, double Temp,
	     double VapourPressure, double Si,
	     double Albedo, Treelog&)
    {
      VapourPressure *= 0.001;	// Pa -> kPa
      const double Ln
	= NetLongwaveRadiation (Cloudiness, Temp, VapourPressure);
      net_radiation_ = (1.0 - Albedo) * Si - Ln;
#if 0
      if (net_radiation_>750)
      {
        cout << "Albedo     " << Albedo << "\n";
        cout << "Si         " << Si     << "\n";
        cout << "Ln         " << Ln     << "\n";
        cout << "Rn         " << net_radiation_ << "\n";
      }
#endif
    }

  // Create & Destroy.
  NetRadiationParent (const AttributeList& al)
    : NetRadiation (al),
      net_radiation_ (0.0)
    { }
};

const double NetRadiationParent::SurEmiss = 0.98;
const double NetRadiationParent::SB = 5.67e-8; // [W/m^2/K^4]

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
  
  NetRadiationBrunt (const AttributeList& al)
    : NetRadiationParent (al),
      a (al.check ("a") ? al.number ("a") : 0.34),
      b (al.check ("b") ? al.number ("b") : 0.14)
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
  
  NetRadiationIdsoJackson (const AttributeList& al)
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

  NetRadiationBrutsaert (const AttributeList& al)
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

  NetRadiationSwinbank (const AttributeList& al)
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

  NetRadiationSatterlund (const AttributeList& al)
    : NetRadiationParent (al)
    { }
};

// Add models to library.

static struct NetRadiationSyntax
{
  static NetRadiation&
  make_brunt (const AttributeList& al)
    { return *new NetRadiationBrunt (al); }
  static NetRadiation&
  make_idso_jackson (const AttributeList& al)
    { return *new NetRadiationIdsoJackson (al); }
  static NetRadiation&
  make_brutsaert (const AttributeList& al)
    { return *new NetRadiationBrutsaert (al); }
  static NetRadiation&
  make_swinbank (const AttributeList& al)
    { return *new NetRadiationSwinbank (al); }
  static NetRadiation&
  make_satterlund (const AttributeList& al)
    { return *new NetRadiationSatterlund (al); }

  NetRadiationSyntax ()
    {
      // Brunt.
      Syntax& syntax_brunt = *new Syntax ();
      syntax_brunt.add ("net_radiation", "W/m^2", Syntax::LogOnly,
			"The calculated net radiation.");
      // We make them optional, so other code doesn't have to set them.
      syntax_brunt.add ("a", Syntax::None (), Syntax::OptionalConst,
			"Brunt 'a' parameter (offset).");
      syntax_brunt.add ("b", "1/sqrt(kPa)", Syntax::OptionalConst,
			"Brunt 'b' parameter (vapor pressure factor).");
      AttributeList& alist_brunt = *new AttributeList ();
      alist_brunt.add ("description", "\
Brunt, 1932.  Default parameterization by Jensen et.al., 1990.\n\
FAO recommendation.");
      // We add the values here so they appear in the manual.
      alist_brunt.add ("a", 0.34);
      alist_brunt.add ("b", 0.14);
      Librarian<NetRadiation>::add_type ("brunt",
					 alist_brunt, syntax_brunt,
					 &make_brunt);
      // Others.
      Syntax& syntax = *new Syntax ();
      syntax.add ("net_radiation", "W/m^2", Syntax::LogOnly,
		  "The calculated net radiation.");
      AttributeList& alist_idso_jackson = *new AttributeList ();
      alist_idso_jackson.add ("description", "Idso and Jackson, 1969");
      AttributeList& alist_brutsaert = *new AttributeList ();
      alist_brutsaert.add ("description", "Brutsaert, 1975");
      AttributeList& alist_swinbank = *new AttributeList ();
      alist_swinbank.add ("description", "Swinbank, 1963");
      AttributeList& alist_satterlund = *new AttributeList ();
      alist_satterlund.add ("description", "Satterlund, 1979");
      Librarian<NetRadiation>::add_type ("idso_jackson",
					 alist_idso_jackson, syntax,
					 &make_idso_jackson);
      Librarian<NetRadiation>::add_type ("brutsaert",
					 alist_brutsaert, syntax,
					 &make_brutsaert);
      Librarian<NetRadiation>::add_type ("swinbank",
					 alist_swinbank, syntax,
					 &make_swinbank);
      Librarian<NetRadiation>::add_type ("satterlund", 
					 alist_satterlund, syntax,
					 &make_satterlund);
    }
} NetRadiation_syntax;

// net_radiation.C ends here
