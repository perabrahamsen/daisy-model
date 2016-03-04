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
#include <boost/shared_ptr.hpp>

class Metalib;
class BlockModel;
class Treelog;
class Format;
class Model;
class Frame;
class FrameModel;
class Declare;

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
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
  typedef void (*doc_fun) (Format&, const Metalib&, Treelog&, symbol name);
  typedef Model& (*builder) (const BlockModel&);

  // Content.
private:
  struct Implementation;
  std::unique_ptr<Implementation> impl;

  // Metalib
public:
  void clear_parsed ();
#if 0
  void refile_parsed (const std::string& from, const std::string& to);
#endif

  // Use.
public:
  symbol name () const;
  symbol description () const;
  const FrameModel& model (symbol) const;
  bool check (symbol) const;
  bool complete (const Metalib&, symbol) const;
  void add_model (symbol, boost::shared_ptr<const FrameModel>);
  void entries (std::vector<symbol>&) const;
  const std::set<symbol>& ancestors (symbol) const;
  bool is_derived_from (symbol a, symbol b) const;
  const symbol base_model (symbol parameterization) const;
  bool has_interesting_description (const Frame&) const;
  void add_doc_fun (doc_fun);
  std::vector<doc_fun>& doc_funs () const;

  // Dependencies.
public:
  void remove (symbol);

public:
  void set_description (symbol);
  Library* clone () const;
private:
  Library ();
  Library (const Library&);
public:
  Library (symbol name);
  ~Library ();
};

#endif // LIBRARY_H
