// snow.h

#ifndef SNOW_H
#define SNOW_H

struct AttributeList;
struct Weather;
struct Bioclimate;
struct Syntax;
struct Log;
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
	     double Psnow, double Pond, double T, double Epot);
  void output (Log&) const;

  // Queries.
public:
  double percolation () const;
  double temperature () const;
  double evaporation () const;
  double storage () const;

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Snow (const AttributeList& al);
  ~Snow ();
};

#endif // SNOW_H
