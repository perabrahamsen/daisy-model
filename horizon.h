// horizon.h

#ifndef HORIZON_H
#define HORIZON_H

#include <string>

struct AttributeList;
struct Library;
struct Syntax;
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
  double K_planar () const;	// Half saturation constant [ g / cm^3 ]
  double K_edge () const;	// Same for edges.
  double v_planar () const;
  double v_edge () const;

  // Library.
public:
  static const Library& library ();
  typedef Horizon& (*constructor) (const AttributeList&);
  static void add_type (string name, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (string name, const AttributeList&, string super);
  static Horizon& create (const AttributeList&);

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  Horizon (const AttributeList&);
  virtual ~Horizon ();
};

// Ensure the Horizon library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Horizon_init
{
  static int count;
public:
  Horizon_init ();
  ~Horizon_init ();
} horizon_init;

#endif HORIZON_H
