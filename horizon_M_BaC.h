// horizon_M_BaC.h

#ifndef HORIZON_M_BaC_H
#define HORIZON_M_BaC_H

#include "horizon.h"

struct AttributeList;

class HorizonM_BaC : public Horizon
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
  friend class HorizonM_BaCSyntax;
  static Horizon& make (AttributeList& al);
  HorizonM_BaC (const AttributeList&);
public:
  virtual ~HorizonM_BaC ();
};

#endif HORIZON_M_BaC_H
