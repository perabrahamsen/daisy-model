// transport.h

#ifndef TRANSPORT_H
#define TRANSPORT_H

#include "librarian.h"

class Soil;
class SoilWater;
class Solute;

class Transport
{
  // Content.
public:
  const string name;

  // Simulation.
public:
  virtual void tick (const Soil&, const SoilWater&, const Solute&,
		     vector<double>& M, 
		     vector<double>& C,
		     vector<double>& S,
		     double J_in) = 0;
  virtual void output (Log&, Filter&) const = 0;

  // Create and Destroy.
protected:
  Transport (const string name);
public:
  virtual ~Transport ();
};

static Librarian<Transport> Transport_init ("transport");

#endif TRANSPORT_H
