// horizon.h

#ifndef HORIZON_H
#define HORIZON_H

#include <std/string.h>

struct AttributeList;
struct Library;
struct Syntax;
struct CSMP;

class Horizon 
{
  // Use.
public:
  virtual double Theta (double h) const = 0;
  virtual double Theta_res () const;
  virtual double K (double h) const = 0;
  virtual double Cw2 (double h) const = 0;
  virtual double h (double Theta) const = 0;
  virtual double M (double h) const = 0;
  virtual bool compact () const;

  // Tools for derived classes.
  void K_to_M (CSMP&, int) const;

  // Library.
public:
  static const Library& library ();
  typedef Horizon& (*constructor) (const AttributeList&);
  static void add_type (string name, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (string name, const AttributeList&, string super);
  static Horizon& create (const AttributeList&);

  // Create and Destroy.
protected:
  Horizon ();
public:
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
