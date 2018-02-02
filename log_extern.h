// log_extern.h --- Logging to external model.
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

#ifndef LOG_EXTERN_H
#define LOG_EXTERN_H

#include "log_select.h"
#include "destination.h"
#include "scope.h"
#include "symbol.h"
#include <map>
#include <vector>

class Daisy;

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT LogExtern : public LogSelect,
                         public Destination, 
                         public Scope
{
public:
  class NumEntry;

  // Scopesel id.
private:
  const symbol title_;
  
  // Destination Content.
  typedef enum { Missing, Number, Name, Array } intern_type;
  typedef std::map<symbol, intern_type> type_map;
  typedef std::map<symbol, double> number_map;
  typedef std::map<symbol, symbol> name_map;
  typedef std::map<symbol, int> int_map;
  typedef std::map<symbol, const std::vector<double>*> array_map;
  type_map types;
  number_map numbers;
  name_map names;
  array_map arrays;
  int_map sizes;
  name_map dimensions;

  // Log.
  void find_scopes (std::vector<const Scope*>&) const;
  symbol last_done;
  void done_print  (const std::vector<Time::component_t>& time_columns,
                    const Time& time);

  // Self use.
  void output (Log&) const;

  // Select::Destination
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

  // Scope
public:
  symbol title () const;
  void tick (const Scope&, Treelog&);
  void entries (std::set<symbol>&) const;
  Attribute::type lookup (symbol tag) const;
  int type_size (symbol tag) const;
  int value_size (symbol tag) const;
  using LogSelect::check;
  bool check (symbol tag) const;
  double number (symbol) const;
  symbol dimension (symbol) const;
  symbol name (symbol tag) const;
  symbol description (symbol) const;

  // Create and destroy.
  void initialize (const symbol log_dir, const symbol suffix, Treelog&);
public:
  LogExtern (const BlockModel&);
  ~LogExtern ();
};

#endif // LOG_EXTERN_H
