// horizon.h

#ifndef HORIZON_H
#define HORIZON_H

#include "librarian.h"

struct Hydraulic;
struct Tortuosity;

// Weigth of mineral particles. [g / cm³]
GLOBAL_CONSTANT const double rho_mineral = 2.65;	
// Weight of humus. [g / cm³]
GLOBAL_CONSTANT const double rho_humus = 1.3; 
GLOBAL_CONSTANT const double c_fraction_in_humus = 0.587;

class Horizon 
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;
public:

  // Water.
public:
  const Hydraulic& hydraulic;
  double heat_conductivity (double Theta, double Ice) const;
  double heat_capacity (double Theta, double Ice) const;
  
  // Texture.
public:
  const Tortuosity& tortuosity;
  double dry_bulk_density () const;
  double clay () const;
  double SOM_C (unsigned int pool) const;
  double SOM_C_per_N (unsigned int pool) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Horizon (const AttributeList&);
  virtual ~Horizon ();
};

static Librarian<Horizon> Horizon_init ("horizon");

#endif HORIZON_H
