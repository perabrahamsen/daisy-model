// denitrification.h

#ifndef DENITRIFICATION_H
#define DENITRIFICATION_H

class AttributeList;
class Syntax;

class Denitrification
{
public:
  static void load_syntax (Syntax&, AttributeList&);
  Denitrification (const AttributeList&);
};


#endif DENITRIFICATION_H
