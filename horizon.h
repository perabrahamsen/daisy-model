// horizon.h

#ifndef HORIZON_H
#define HORIZON_H

#include "librarian.h"

struct Hydraulic;

class Horizon 
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
public:
  double clay () const;
  double C () const;
  double N () const;
  double C_per_N () const;

  // Water.
public:
  const Hydraulic& hydraulic;
  double heat_conductivity (double Theta, double Ice) const;
  double heat_capacity (double Theta, double Ice) const;
  
  // Texture.
public:
  virtual double tortuosity_factor (double Theta) const;
  
  // Absorbtion.
public:
  double K_planar () const;	// Half saturation constant [ g / cm³ ]
  double K_edge () const;	// Same for edges.
  double v_planar () const;
  double v_edge () const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Horizon (const AttributeList&);
  virtual ~Horizon ();
};

static Librarian<Horizon> Horizon_init ("horizon");

#endif HORIZON_H
