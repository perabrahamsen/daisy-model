// groundwater.h

#ifndef GROUNDWATER_H
#define GROUNDWATER_H

#include "uzmodel.h"

class AttributeList;

class Groundwater : public UZbottom
{
  // UZbottom.
public:
  bool flux_bottom () const;
  double q_bottom () const;
  bool accept_bottom (double) const;

  // Simulation.
public:
  Groundwater (const AttributeList& al);
};


#endif GROUNDWATER_H
