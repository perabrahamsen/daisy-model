// syntax.h

#ifndef SYNTAX_H
#define SYNTAX_H

#include <std/string.h>

struct FTable;
struct AttributeList;
struct Log;

class Syntax 
{ 
  struct Implementation;
  Implementation& impl;
public:
  enum type 
  { Number, List, Rules, CSMP, Function, Array, Boolean, String,
    Date, Crops, Columns, Integer, Soil, UZmodel, Error };
  enum required
  { Mandatory, Optional, LogOnly };
  bool check (string, const AttributeList&, const Log&) const;
  required status (string) const;
  type lookup (string) const;
  const Syntax& syntax (string) const;
  const FTable* function (string) const;
  int  size (string) const;
  void add (string, type, required = Mandatory);
  void add (string, const Syntax*, required = Mandatory);
  void add (string, const FTable*, required = Mandatory);
  void add (string, const int, required = Mandatory);
  Syntax ();
  ~Syntax ();
};

#endif SYNTAX_H
