// bioclimate.h

#ifndef BIOCLIMATE_H
#define BIOCLIMATE_H

#include <std/string.h>
#include <vector.h>
#include "column.h"

struct Surface;
struct Weather;
struct AttributeList;
struct CropList;
struct Soil;
struct Syntax;
struct SoilWater;
struct Log;
struct Filter;
struct SoluteMatter;

class Bioclimate
{ 
  struct Implementation;
  Implementation& impl;
public:
  // Simulation
  void tick (Surface&, const Weather&, const CropList&, 
	     const Soil&, SoilWater&);
  void output (Log&, const Filter&) const;

  // Canopy.
public:
  int NumberOfIntervals () const;
  double height (int) const;
  double PAR (int) const;
  double AirTemperature () const;
  double DayLength () const;

  // Manager.
public:
  void Irrigate (double flux, double temp, 
		 const SoluteMatter& sm,
		 Column::irrigation_from from);
  
  // Create.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Bioclimate (const AttributeList&);
  ~Bioclimate ();
};

#endif BIOCLIMATE_H
