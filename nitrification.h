// nitrification.h

#ifndef NITRIFICATION_H
#define NITRIFICATION_H

class AttributeList;
class Syntax;

class Nitrification
{
public:
  static void load_syntax (Syntax&, AttributeList&);
  Nitrification (const AttributeList&);
};


#endif NITRIFICATION_H
