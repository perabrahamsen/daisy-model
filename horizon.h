// horizon.h

#ifndef HORIZON_H
#define HORIZON_H

#include "daisy.h"

struct AttributeList;

class Horizon 
{
  // Content.
  struct Implementation;
  Implementation& impl;

  // Use.
public:
  virtual double Theta (double h) const;
  virtual double K (double h) const;
  virtual double Cw1 (double h) const;
  virtual double Cw2 (double h) const;
  virtual double h (double Theta) const;
  virtual bool compact () const;

  // Create and Destroy.
public:
  static const Horizon& get (string name, const AttributeList&);
  Horizon (string name, const AttributeList&);
  virtual ~Horizon ();
};

class HorizonList
{
  // Content.
  struct Implementation;
  Implementation& impl;

  // Examine.
public:
  const Horizon& horizon (double z) const;

  // Create and Destroy.
public:
  void add (double zplus, const Horizon&);
  HorizonList ();
  ~HorizonList ();
};

#endif HORIZON_H
