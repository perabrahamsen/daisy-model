// solute.h

#ifndef SOLUTE_H
#define SOLUTE_H

#include <string>
#include <vector>
#include "common.h"

struct Log;
struct Filter;
struct Syntax;
struct AttributeList;
struct Soil;
struct SoilWater;

class Solute
{
  // State variables.
protected:
  vector<double> M_;		// Concentration in soil [g / cm³]
  vector<double> C_;		// Concentration in soil solution [g / cm³]

  // Flux variables.
protected:
  vector<double> S;		// Sink-source term 
  vector<double> J;		// Flux density 

  // FYI variables.
  double ddt;			// Calculated time step.

  // Substance specific constants.
  virtual double beta (const Soil&, const SoilWater&,
		       int i, double C) const = 0; // dA/dC
public:
  virtual double diffusion_coefficient () const = 0; // in free solu. 
protected:
  virtual double C_to_M (const Soil&, double Theta, int i, double C) const = 0;
  virtual double M_to_C (const Soil&, double Theta, int i, double M) const = 0;

public:
  double M (int i) const
  { return M_[i]; }
  double C (int i) const
  { return C_[i]; }
  double M_left (int i) const
  { return M_[i] + S[i] * dt; }

  // Sink.
public:
  void clear ();
  void add_to_source (const vector<double>&);
  void add_to_sink (const vector<double>&);

  // Simulation.
protected:
  
public:
  virtual void tick (const Soil&, const SoilWater&, double J_in);
  bool check (unsigned n) const;
  void output (Log&, const Filter&) const;
  void add (const Soil&, const SoilWater&,
	    double amount, double from, double to);
  void mix (const Soil&, const SoilWater&, double from, double to);
  void swap (const Soil&, const SoilWater&, double from, double middle, double to);

  // Create and destroy.
protected:
  static void load_syntax (Syntax&, AttributeList&);
  Solute ();
  void initialize (const Soil&, const SoilWater&, const AttributeList&);
public:
  virtual ~Solute ();
};

#endif SOLUTE_H
