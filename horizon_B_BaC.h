// horizon_B_BaC.h

#ifndef HORIZON_B_BaC_H
#define HORIZON_B_BaC_H

#include "horizon.h"

struct AttributeList;

class HorizonB_BaC : public Horizon
{
  // Content.
  const double Theta_sat;
  const double Theta_rel;
  const double lambda;
  const double h_b;
  const double K_sat;

  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
private:
  double Se (double h) const;
  
  // Create and Destroy.
private:
  friend class HorizonB_BaCSyntax;
  static Horizon& make (AttributeList& al);
  HorizonB_BaC (const AttributeList&);
public:
  virtual ~HorizonB_BaC ();
};

#endif HORIZON_B_BaC_H
