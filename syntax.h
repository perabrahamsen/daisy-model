// syntax.h

#ifndef SYNTAX_H
#define SYNTAX_H

#include <std/string.h>

struct FTable;
struct AttributeList;
struct Log;
struct Library;

typedef void (*derive_fun) (string, const AttributeList&, string);

class Syntax 
{ 
  struct Implementation;
  Implementation& impl;
public:
  enum type 
  { Number, List, Rules, CSMP, Function, Array, Boolean, String,
    Date, Integer, Output, Class, Object, Sequence, Layers, Error };
  enum required
  { Mandatory, Sparse, Optional, LogOnly };
  bool check (string, const AttributeList&, const Log&, 
	      bool sparse = false) const;
  bool check (const AttributeList&) const;
  required status (string) const;
  type lookup (string) const;
  const Syntax& syntax (string) const;
  const FTable* function (string) const;
  const Library& library (string) const;
  derive_fun derive (string) const;
  int  size (string) const;
  void add (string, type, required = Mandatory);
  void add (string, const Syntax&, required = Mandatory);
  void add (string, const FTable*, required = Mandatory);
  void add (string, const int, required = Mandatory);
  void add_output (string, const Syntax&, required = Sparse);
  void add_class (string, const Library&, derive_fun);
  void add_object (string, const Library&, required = Mandatory);
  void add_sequence (string, const Library&, required = Mandatory);
  void add_layers (string, const Library&, required = Mandatory);
  Syntax ();
  ~Syntax ();
};

// Ugly ugly UGLY!  Must change!!! 
#define ADD_SUBMODULE(s, a, m) \
do \
  { \
    Syntax& _s = *new Syntax (); \
    AttributeList& _a = *new AttributeList (); \
    m::load_syntax (_s, _a); \
    s.add (#m, _s); \
    a.add (#m, _a); \
  } \
while (false)



#endif SYNTAX_H
