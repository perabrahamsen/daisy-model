// solute.h

#ifndef SOLUTE_H
#define SOLUTE_H

#include <std/string.h>
#include <vector.h>

struct Log;
struct Filter;
struct Syntax;
struct AttributeList;
struct Soil;
struct SoilWater;

class Solute
{
  // Content.
protected:
  // State variables.
  vector<double> M;		// Concentration in soil [g / cm^3]
  vector<double> C;		// Concentration in soil solution [g / cm^3]

  // Flux variables.
  vector<double> S;		// Sink-source term [kg / m^3 / s]
  vector<double> J;		// Flux density [kg / m^2 / s]

  // Substance specific constants.
  virtual double beta (const Soil&, const SoilWater&,
		       int i, double C) const = 0; // dA/dC
  virtual double diffusion_coefficient () const = 0; // in free solu. [m^2 / s]
  virtual double C_to_M (const Soil&, const SoilWater&,
			 int i, double C) const = 0;
  virtual double M_to_C (const Soil&, const SoilWater&,
			 int i, double M) const = 0;

  // Sink.
public:
  void clear ();
  void add_to_source (const vector<double>&);

  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, double J_in);
  bool check (unsigned n) const;
  void output (Log&, const Filter*) const;

  // Create and destroy.
protected:
  static void load_syntax (Syntax&, AttributeList&);
  Solute ();
  void initialize (const Soil&, const SoilWater&, const AttributeList&);
public:
  virtual ~Solute ();
};

#endif SOLUTE_H
