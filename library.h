// library.h
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#ifndef LIBRARY_H
#define LIBRARY_H

#include <vector>
using namespace std;

class Syntax;
class AttributeList;
class Treelog;

typedef void (*derive_fun) (const string& name, AttributeList& al,
			    const string& super);

class Library
{
  // Content.
public:
  struct Implementation;
private:
  Implementation& impl;

public:
  // Find a specific library.
  static bool exist (const string& name);
  static Library& find (const string& name);
  static void all (vector<string>& libraries);
  static int get_sequence ();

  // Use.
  const string& name () const;
  const char* description () const;
  AttributeList& lookup (const string&) const;
  bool check (const string&) const;
  void add (const string&, AttributeList&, const Syntax&);
  void add_derived (const string& name, AttributeList& al,
		    const string& super);
  const Syntax& syntax (const string&) const;
  void entries (vector<string>&) const;
  bool is_derived_from (const string& a, const string& b) const;
  const string base_model (const string& parameterization) const;

  // Dependencies.
  void remove (const string&);

  // File handling.
  static void clear_all_parsed ();
  static void refile_parsed (const string& from, const string& to);

  // Create and destroy.
  static void load_syntax (Syntax&, AttributeList&);
private: 
  Library (const Library&);
public:
  Library (const char* name, derive_fun derive, const char* description);
  ~Library ();
};

#endif // LIBRARY_H
