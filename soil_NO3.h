// soil_NO3.h

#ifndef SOIL_NO3_H
#define SOIL_NO3_H

class AttributeList;
class Syntax;

class SoilNO3
{
public:
  static void load_syntax (Syntax&, AttributeList&);
  SoilNO3 (const AttributeList&);
};


#endif SOIL_NO3_H
