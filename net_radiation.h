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
  virtual void output (Log&) const;
  virtual double net_radiation () const = 0; // [W/m2]
  virtual void tick (double Cloudiness /* [0-1] */,
		     double Temp /* [dg C] */, 
		     double VapourPressure /* [kPa] */,
		     double Si /* [W/m2] */, 
		     double Albedo /* [0-1] */) = 0;

  // Create and Destroy.
protected:
  NetRadiation (const AttributeList&);
public:
  virtual ~NetRadiation ();
};

static Librarian<NetRadiation> NetRadiation_init ("net_radiation");

#endif // NET_RADIATION_H
