// horizon_M_vG.h

#ifndef HORIZON_M_vG_H
#define HORIZON_M_vG_H

#include "horizon.h"

struct AttributeList;

class HorizonM_vG : public Horizon
{
  // Content.
  const double Theta_sat;
  const double Theta_ref;
  const double alpha;
  const double a;		// - alpha
  const double n;
  const double m;		// 1 - 1/n
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
  friend class HorizonM_vGSyntax;
  static Horizon& make (AttributeList& al);
  HorizonM_vG (const AttributeList&);
public:
  virtual ~HorizonM_vG ();
};

#endif HORIZON_M_vG_H
