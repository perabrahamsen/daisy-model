// net_radiation.h

#ifndef NET_RADIATION_H
#define NET_RADIATION_H

#include "librarian.h"

class Weather;

class NetRadiation
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Simulation.
public:
  virtual void output (Log&, Filter&) const;
  virtual double net_radiation () const = 0; // [MJ/m2/d]
  virtual void tick (double Cloudiness /* [0-1] */,
		     double Temp /* [dg C] */, 
		     double VapourPressure /* [kPa] */,
		     double Si /* [MJ/m2/d] */, 
		     double Albedo /* [0-1] */) = 0;
  void tick (const Weather& weather, double Albedo);

  // Create and Destroy.
protected:
  NetRadiation (const AttributeList&);
public:
  virtual ~NetRadiation ();
};

static Librarian<NetRadiation> NetRadiation_init ("net_radiation");

#endif NET_RADIATION_H
