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
  static const char *const description;

  // Simulation.
public:
  virtual void tick (const Time& time) = 0;
  virtual void update_water (const Soil&,
			     vector<double>& S_sum,
			     vector<double>& h,
			     vector<double>& h_ice,
			     vector<double>& Theta,
			     vector<double>& q,
			     vector<double>& q_p);
  virtual void output (Log&) const;

  // Accessors.
public:
  virtual double table () const = 0;

    // Create and Destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  virtual void initialize (const Time& time, const Soil&);
protected:
  Groundwater (const AttributeList& al);
public:
  virtual ~Groundwater ();
};

static Librarian<Groundwater> Groundwater_init ("groundwater");

#endif GROUNDWATER_H
