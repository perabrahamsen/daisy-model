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
  static Library& find (const string& name);

  static void all (vector<string>& libraries);
  static int get_sequence ();

  const string& name () const;
  AttributeList& lookup (const string&) const;
  bool check (const string&) const;
  void add (const string&, AttributeList&, const Syntax&);
  void remove (const string&);
  const Syntax& syntax (const string&) const;

  void dump (int indent) const;
  void entries (vector<string>&) const;
private: 
  Library (const Library&);
public:
  Library (const char*);
  ~Library ();
};

#endif LIBRARY_H
