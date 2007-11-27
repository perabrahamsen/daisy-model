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
#include <set>
#include <memory>

class Metalib;
class Block;
class Syntax;
class AttributeList;
class Treelog;
class Format;
class Model;

#ifdef BUILD_DLL
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT Library
{
  // Types.
public:
  typedef void (*doc_fun) (Format&, Metalib&, Treelog&, 
                           const AttributeList& al);

  typedef Model& (*builder) (Block&);

  // Content.
private:
  struct Implementation;
  std::auto_ptr<Implementation> impl;

  // Metalib
public:
  void clear_parsed ();
  void refile_parsed (const std::string& from, const std::string& to);

  // Use.
public:
  symbol name () const;
  const char* description () const;
  AttributeList& lookup (symbol) const;
  bool check (symbol) const;
  bool complete (const Metalib&, symbol) const;
  void add_base (AttributeList&, const Syntax&);
  void add (symbol, AttributeList&, const Syntax&, builder);
  void add_derived (symbol name, AttributeList& al,
		    symbol super);
  void add_derived (symbol name, const Syntax&, AttributeList& al,
		    symbol super);
  const Syntax& syntax (symbol) const;
  void entries (std::vector<symbol>&) const;
  const std::set<symbol>& ancestors (symbol) const;
  bool is_derived_from (symbol a, symbol b) const;
  const symbol base_model (symbol parameterization) const;
  bool has_interesting_description (const AttributeList& alist) const;
  void add_doc_fun (doc_fun);
  std::vector<doc_fun>& doc_funs () const;

  // Dependencies.
  void remove (symbol);

  // Build a model.
  Model* build_raw (const symbol type, Block& block) const;

  void set_description (const char*);
  Library* clone () const;
private:
  Library ();
  Library (const Library&);
public:
  Library (const char* name);
  ~Library ();
};

#endif // LIBRARY_H
