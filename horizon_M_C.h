// horizon_M_C.h

#ifndef HORIZON_M_C_H
#define HORIZON_M_C_H

#include "horizon.h"

struct AttributeList;

class HorizonM_C : public Horizon
{
  // Content.
  const double Theta_sat;
  const double h_b;
  const double b;
  const double K_sat;

  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
private:
  double Sr (double h) const;
  
  // Create and Destroy.
private:
  friend class HorizonM_CSyntax;
  static Horizon& make (AttributeList& al);
  HorizonM_C (const AttributeList&);
public:
  virtual ~HorizonM_C ();
};

#endif HORIZON_M_C_H
