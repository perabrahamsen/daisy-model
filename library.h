// library.h

#ifndef LIBRARY_H
#define LIBRARY_H

#include "common.h"
#include <vector>

struct Syntax;
struct AttributeList;

typedef void (*derive_fun) (const string& name, AttributeList& al,
			    const string& super);

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
  const char* description () const;
  AttributeList& lookup (const string&) const;
  bool check (const string&) const;
  void add (const string&, AttributeList&, const Syntax&);
  void add_derived (const string& name, AttributeList& al,
		    const string& super);
  void remove (const string&);
  const Syntax& syntax (const string&) const;

  void entries (vector<string>&) const;
  static void clear_all_parsed ();
  static void refile_parsed (const string& from, const string& to);
  static void load_syntax (Syntax&, AttributeList&);
private: 
  Library (const Library&);
public:
  Library (const char* name, derive_fun derive, const char* description);
  ~Library ();
};

#endif LIBRARY_H
