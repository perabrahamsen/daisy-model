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
  double Cw2 (double h) const;
  double h (double Theta) const;

  // Create and Destroy.
private:
  friend class HorizonYoloSyntax;
  static Horizon& make (AttributeList& al);
  HorizonYolo (const AttributeList&);
public:
  virtual ~HorizonYolo ();
};

#endif HORIZON_YOLO_H
