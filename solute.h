// solute.h

#ifndef SOLUTE_H
#define SOLUTE_H

// These must be included in the header file, for `load_syntax' to work.
#include "adsorption.h"
#include "transport.h"
#include "mactrans.h"

struct Log;
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
  vector<double> S;		// Combined source term.
  vector<double> S_p;		// Source term for macropores only.
  vector<double> J;		// Solute transport log in matrix.
  vector<double> J_p;		// Solute transport log in macropores.
  Transport& transport;		// Solute transport model in matrix.
  Mactrans& mactrans;		// Solute transport model in macropores.
  Adsorption& adsorption;	// Solute adsorption.
  
public:
  virtual double diffusion_coefficient () const = 0; // in free solute. 
  double C_to_M (const Soil& soil, double Theta, int i, double C) const
    { return adsorption.C_to_M (soil, Theta, i, C); }
  double M_to_C (const Soil& soil, double Theta, int i, double M) const
    { return adsorption.M_to_C (soil, Theta, i, M); }

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
  virtual void output (Log&) const;
  void add (const Soil&, const SoilWater&,
	    double amount, double from, double to);
  void mix (const Soil&, const SoilWater&, double from, double to);
  void swap (const Soil&, const SoilWater&, double from, double middle, double to);

  // Communication with external model.
  void put_M (const Soil& soil, const SoilWater& soil_water,
	      const vector<double>& v);

  // Create and destroy.
protected:
  static void load_syntax (Syntax&, AttributeList&);
  Solute (const AttributeList& al);
public:
  virtual void initialize (const AttributeList&,
			   const Soil&, const SoilWater&);
public:
  virtual ~Solute ();
};

#endif SOLUTE_H
