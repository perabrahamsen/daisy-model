// snow.h

#ifndef SNOW_H
#define SNOW_H

struct AttributeList;
struct Weather;
struct Bioclimate;
struct Syntax;
struct Log;
struct Filter;

class Snow
{ 
  struct Implementation;
  Implementation& impl;

  // Simulation.
public:
  void tick (double Si, double q_h, double Prain,
	     double Psnow, double Tair, double Epot);
  void output (Log&, const Filter&) const;
  double percolation ();
  double temperature ();
  double evaporation ();

  // Create & Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Snow (const AttributeList& al);
  ~Snow ();
};

#endif SNOW_H
