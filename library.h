// library.h

#ifndef LIBRARY_H
#define LIBRARY_H

#include "common.h"

struct Syntax;
struct AttributeList;

class Library
{
  struct Implementation;
  Implementation& impl;
public:
  const char* name () const;
  const AttributeList& lookup (string) const;
  bool check (string) const;
  void add (string, const AttributeList&, const Syntax&);
  const Syntax& syntax (string) const;

  void dump (int indent) const;
private: 
  Library (const Library&);
public:
  Library (const char*);
  ~Library ();
};

#endif LIBRARY_H
