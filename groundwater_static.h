// groundwater_static.h

#ifndef GROUNDWATER_STATIC_H
#define GROUNDWATER_STATIC_H

#include "groundwater.h"

class GroundwaterStatic : public Groundwater
{
  // Content.
private:
  const double depth;
  
  // UZbottom.
public:
  bool flux_bottom () const;
  bool accept_bottom (double) const;

  // Simulation.
public:
  void tick ();
  double table () const;

  // Create and Destroy.
private:
  friend class GroundwaterStaticSyntax;
  static Groundwater& make (const Time&, const AttributeList&);
  GroundwaterStatic (const Time&, const AttributeList&);
public:
  ~GroundwaterStatic ();
};

#endif GROUNDWATER_STATIC_H
