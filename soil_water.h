// soil_water.h

#ifndef SOIL_WATER_H
#define SOIL_WATER_H

#include <vector.h>

class AttributeList;
class UZmodel;
class Surface;
class Groundwater;

class SoilWater
{
  vector<double> Theta;
  vector<double> h;
  UZmodel* top;
  UZmodel* bottom;
public:
  void tick (Surface&, const Groundwater&);

  SoilWater (const AttributeList& par, const AttributeList& var);
};

#endif SOIL_WATER_H
