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

#include "symbol.h"
#include <string>
#include <vector>
using namespace std;

class Syntax;
class AttributeList;
class Treelog;

typedef void (*derive_fun) (symbol name, AttributeList& al, symbol super);

class Library
{
  // Content.
public:
  struct Implementation;
private:
  Implementation& impl;

public:
  // Find a specific library.
  static bool exist (symbol name);
  static Library& find (symbol name);
  static void all (vector<symbol>& libraries);
  static int get_sequence ();

  // Use.
  symbol name () const;
  const char* description () const;
  AttributeList& lookup (symbol) const;
  bool check (symbol) const;
  void add (symbol, AttributeList&, const Syntax&);
  void add_derived (symbol name, AttributeList& al,
		    symbol super);
  const Syntax& syntax (symbol) const;
  void entries (vector<symbol>&) const;
  bool is_derived_from (symbol a, symbol b) const;
  const symbol base_model (symbol parameterization) const;

  // Dependencies.
  void remove (symbol);

  // Command line parsing.
  typedef void (*command_fun) (int& argc, char**& argv, Treelog& out);
  void command (int& argc, char**& argv, Treelog& out) const;
  void add_command (symbol name, command_fun fun);

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
