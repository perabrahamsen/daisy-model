// horizon_yolo.h

#ifndef HORIZON_YOLO_H
#define HORIZON_YOLO_H

#include "horizon.h"

struct AttributeList;

class HorizonYolo : public Horizon
{
  // Use.
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw1 (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;

  // Create and Destroy.
public:
  HorizonYolo (const AttributeList&);
  virtual ~HorizonYolo ();
};

#endif HORIZON_YOLO_H
