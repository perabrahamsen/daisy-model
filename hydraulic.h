// hydraulic.h

#ifndef HYDRAULIC_H
#define HYDRAULIC_H

#include <string>

struct AttributeList;
struct Library;
struct Syntax;
struct CSMP;

class Hydraulic 
{
  // Standard parameters.
public:
  const double Theta_sat;
  const double Theta_res;
  const double lambda;
  inline double porosity () const
  { return Theta_sat; }

  // Convertion functions.
public:
  virtual double Theta (double h) const = 0;
  virtual double K (double h) const = 0;
  virtual double Cw2 (double h) const = 0;
  virtual double h (double Theta) const = 0;
  virtual double M (double h) const = 0;
  virtual bool compact () const;

  // Tools for derived classes.
protected:
  void K_to_M (CSMP&, int) const;

  // Library.
public:
  static const Library& library ();
  typedef Hydraulic& (*constructor) (const AttributeList&);
  static void add_type (string name, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (string name, const AttributeList&, string super);
  static Hydraulic& create (const AttributeList&);

  // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
protected:
  void initialize ();
  Hydraulic (const AttributeList&);
private:
  Hydraulic () { };
public:
  virtual ~Hydraulic ();
};

// Ensure the Hydraulic library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Hydraulic_init
{
  static int count;
public:
  Hydraulic_init ();
  ~Hydraulic_init ();
} hydraulic_init;

#endif HYDRAULIC_H
