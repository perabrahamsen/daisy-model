// net_radiation.C 

#include "net_radiation.h"
#include "log.h"
#include "weather.h"

Librarian<NetRadiation>::Content* Librarian<NetRadiation>::content = NULL;

// Interface class.

void
NetRadiation::output (Log& log, Filter& filter) const
{
  log.output ("net_radiation", filter, net_radiation (), true);
}

void 
NetRadiation::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("net_radiation", Syntax::Number, Syntax::LogOnly);
  alist.add ("type", "brunt");
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
  virtual double NetLongwaveRadiation (double Cloudiness, // [MJ/m2/d] 
				       double Temp,
				       double VapourPressure) const = 0;
  // Simulation.
  double net_radiation () const
    { return net_radiation_; }
  void tick (double Cloudiness, double Temp, 
	     double VapourPressure, double Si, 
	     double Albedo)
    {
      const double Ln
	= NetLongwaveRadiation (Cloudiness, Temp, VapourPressure);
      net_radiation_ = (1.0 - Albedo) * Si - Ln;
    }
  void tick (const Weather& weather, double Albedo)
    { tick (weather.cloudiness (), weather.daily_air_temperature (),
	    weather.vapor_pressure (), weather.daily_global_radiation (),
	    Albedo); }

  // Create & Destroy.
  NetRadiationParent (const AttributeList& al)
    : NetRadiation (al),
      net_radiation_ (0.0)
    { }
};

const double NetRadiationParent::SurEmiss = 0.98;
const double NetRadiationParent::SB = 4.90e-9;

// Model classes.

struct NetRadiationBrunt : public NetRadiationParent
{
  double NetLongwaveRadiation(double Cloudiness, // [MJ/m2/d] 
			      double Temp,
			      double VapourPressure) const
    {
      const double NetEmiss = 0.34 - 0.14 * sqrt (VapourPressure);
      return (Cloudiness * NetEmiss * SB * pow (Temp + 273, 4));
    }
  
  NetRadiationBrunt (const AttributeList& al)
    : NetRadiationParent (al)
    { }
};

struct NetRadiationIdsoJackson : public NetRadiationParent
{
  double NetLongwaveRadiation(double Cloudiness, // [MJ/m2/d] 
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
  double NetLongwaveRadiation(double Cloudiness, // [MJ/m2/d] 
			      double Temp,
			      double VapourPressure) const
    {
      const double ea = VapourPressure / 10;   /*mb*/
      const double NetEmiss
	= SurEmiss * (1 - 1.24 * pow (ea / (Temp + 273), 1.0 / 7));
      return (Cloudiness * NetEmiss * SB * pow (Temp + 273, 4));
    }
  
  NetRadiationBrutsaert (const AttributeList& al)
    : NetRadiationParent (al)
    { }
};

struct NetRadiationSwinbank : public NetRadiationParent
{
  double NetLongwaveRadiation(double Cloudiness, // [MJ/m2/d] 
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
  double NetLongwaveRadiation(double Cloudiness, // [MJ/m2/d] 
			      double Temp,
			      double VapourPressure) const
    {
      const double ea = VapourPressure / 10;   /*mb*/
      const double NetEmiss 
	= SurEmiss * (1 - 1.08 * (1 - exp (-pow (ea, (Temp + 273) / 2016))));
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
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      NetRadiation::load_syntax (syntax, alist);
      Librarian<NetRadiation>::add_type ("brunt", alist, syntax, &make_brunt);
      Librarian<NetRadiation>::add_type ("idso_jackson", alist, syntax,
				&make_idso_jackson);
      Librarian<NetRadiation>::add_type ("brutsaert", alist, syntax,
					 &make_brutsaert);
      Librarian<NetRadiation>::add_type ("swinbank", alist, syntax,
					 &make_swinbank);
      Librarian<NetRadiation>::add_type ("satterlund", alist, syntax,
					 &make_satterlund);
    }
} PetMakkink_syntax;

// net_radiation.C ends here
