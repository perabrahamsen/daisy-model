// horizon_B_vG.h

#ifndef HORIZON_B_vG_H
#define HORIZON_B_vG_H

#include "horizon.h"

struct AttributeList;

class HorizonB_vG : public Horizon
{
  // Content.
  const double Theta_sat;
  const double Theta_ref;
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double m;		// 1 - 2/n
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
  friend class HorizonB_vGSyntax;
  static Horizon& make (AttributeList& al);
  HorizonB_vG (const AttributeList&);
public:
  ~HorizonB_vG ();
};

#endif HORIZON_B_vG_H
