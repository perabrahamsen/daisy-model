// groundwater.h

#ifndef GROUNDWATER_H
#define GROUNDWATER_H

#include "uzmodel.h"
#include "librarian.h"

struct Time;

class Groundwater : public UZbottom
{
  // Content.
public:
  const string name;

  // Simulation.
public:
  virtual void tick (const Time& time) = 0;
  virtual void output (Log&, Filter&) const;

  // Accessors.
public:
  virtual double table () const = 0;

    // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual void initialize (const Time& time);
protected:
  Groundwater (const string&);
public:
  virtual ~Groundwater ();
};

static Librarian<Groundwater> Groundwater_init ("groundwater");

#endif GROUNDWATER_H
