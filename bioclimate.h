// bioclimate.h

#ifndef BIOCLIMATE_H
#define BIOCLIMATE_H

#include <std/string.h>
#include <vector.h>

struct Surface;
struct Weather;
struct AttributeList;
struct CropList;

class Bioclimate
{ 
  struct Implementation;
  Implementation& impl;
public:
  // Simulation
  void tick (Surface&, const Weather&, const CropList&);

  // Canopy.
public:
  int NumberOfIntervals () const;
  double height (int) const;
  double PAR (int) const;
  double AirTemperature () const;
  double DayLength () const;

  // Create.
public:
  Bioclimate (const AttributeList& par, const AttributeList& var);
  ~Bioclimate ();
};

#endif BIOCLIMATE_H
