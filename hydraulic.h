// hydraulic.h

#ifndef HYDRAULIC_H
#define HYDRAULIC_H

#include "librarian.h"

struct PLF;

class Hydraulic 
{
  // Content.
public:
  const string name;
  static const char *const description;

  // Standard parameters.
public:
  double Theta_sat;
  const double Theta_res;
  inline double porosity () const
  { return Theta_sat; }
  virtual void set_porosity (double Theta);

  // Convertion functions.
public:
  virtual double Theta (double h) const = 0;
  virtual double K (double h) const = 0;
  virtual double Cw2 (double h) const = 0;
  virtual double h (double Theta) const = 0;
  virtual double M (double h) const = 0;

  // Simulation.
public:
  virtual void output (Log&) const;

  // Tools for derived classes.
protected:
  void K_to_M (PLF&, int) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
protected:
  void initialize ();
  Hydraulic (const AttributeList&);
public:
  virtual ~Hydraulic ();
};

static Librarian<Hydraulic> Hydraulic_init ("hydraulic");

#endif HYDRAULIC_H
