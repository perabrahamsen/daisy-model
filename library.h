// library.h

#ifndef LIBRARY_H
#define LIBRARY_H

#include "common.h"
#include <vector>

struct Syntax;
struct AttributeList;

class Library
{
  // Content.
  struct Implementation;
  Implementation& impl;

public:
  // Find a specific library.
  static Library& find (string name);
  static void all (vector<string>& libraries);
  // 
  const string& name () const;
  AttributeList& lookup (string) const;
  bool check (string) const;
  void add (string, AttributeList&, const Syntax&);
  void remove (string);
  const Syntax& syntax (string) const;

  void dump (int indent) const;
  void entries (vector<string>&) const;
private: 
  Library (const Library&);
public:
  Library (const char*);
  ~Library ();
};

#endif LIBRARY_H
