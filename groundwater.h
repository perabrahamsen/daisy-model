// groundwater.h

#ifndef GROUNDWATER_H
#define GROUNDWATER_H

#include "time.h"
#include "uzmodel.h"

class AttributeList;
class Input;
class Library;
class Syntax;

class Groundwater : public UZbottom
{
  // Content.
protected:
  const Time& time;

  // Simulation.
public:
  virtual void tick () = 0;
  virtual double table () const = 0;

  // Library.
public:
  static const Library& library ();
  static Groundwater& create (const Time&, const AttributeList&);
  typedef Groundwater& (*constructor) (const Time&, const AttributeList&);
  static void add_type (const string, const AttributeList&, const Syntax&,
			constructor);
  static void derive_type (const string, const AttributeList&, string super);

    // Create and Destroy.
protected:
  Groundwater (const Time&);
public:
  virtual ~Groundwater ();
};

// Ensure the Groundwater library is initialized.
// See TC++PL, 2ed, 10.5.1, for an explanation.
static class Groundwater_init
{
  static int count;
public:
  Groundwater_init ();
  ~Groundwater_init ();
} Groundwater_init;

#endif GROUNDWATER_H
