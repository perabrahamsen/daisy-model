// solute.h

#ifndef SOLUTE_H
#define SOLUTE_H

#include "adsorbtion.h"

struct Log;
struct Filter;
struct Syntax;
struct AttributeList;
struct Soil;
struct SoilWater;
struct Transport;

class Solute
{
  // State variables.
protected:
  vector<double> M_;		// Concentration in soil [g / cm³]
  vector<double> C_;		// Concentration in soil solution [g / cm³]

  // Flux variables.
protected:
  vector<double> S;		// Sink-source term.
  Transport& transport;		// Solute transport.
  Adsorbtion& adsorbtion;	// Solute adsorbtion.

public:
  virtual double diffusion_coefficient () const = 0; // in free solute. 
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    { return adsorbtion.C_to_M (soil, Theta, i, C); }
  double M_to_C (const Soil& soil, double Theta, int i, double M) const
    { return adsorbtion.M_to_C (soil, Theta, i, M); }

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
  void tick (const Soil&, const SoilWater&, double J_in);
  bool check (unsigned n) const;
  void output (Log&, Filter&) const;
  void add (const Soil&, const SoilWater&,
	    double amount, double from, double to);
  void mix (const Soil&, const SoilWater&, double from, double to);
  void swap (const Soil&, const SoilWater&, double from, double middle, double to);

  // Create and destroy.
protected:
  static void load_syntax (Syntax&, AttributeList&);
  Solute (const AttributeList& al);
public:
  void initialize (const AttributeList&, const Soil&, const SoilWater&);
public:
  virtual ~Solute ();
};

#endif SOLUTE_H
