// horizon_B_C.h

#ifndef HORIZON_B_C_H
#define HORIZON_B_C_H

#include "horizon.h"

struct AttributeList;

class HorizonB_C : public Horizon
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
  friend class HorizonB_CSyntax;
  static Horizon& make (AttributeList& al);
  HorizonB_C (const AttributeList&);
public:
  virtual ~HorizonB_C ();
};

#endif HORIZON_B_C_H
