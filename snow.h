// snow.h

#ifndef SNOW_H
#define SNOW_H

struct AttributeList;
struct Weather;
struct Bioclimate;
struct Syntax;
struct Log;
struct Filter;
struct Soil;
struct SoilWater;
struct SoilHeat;

class Snow
{ 
  struct Implementation;
  Implementation& impl;

  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, const SoilHeat&,
	     double Si, double q_h, double Prain,
	     double Psnow, double& T, double Epot);
  void output (Log&, Filter&) const;
  double percolation ();
  double temperature ();
  double evaporation ();

  // Communication with external model.
  double get_h2o () const;

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Snow (const AttributeList& al);
  ~Snow ();
};

#endif SNOW_H
