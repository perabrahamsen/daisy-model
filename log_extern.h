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
#include "scope.h"
#include "symbol.h"
#include <map>
#include <vector>

class Daisy;

class LogExtern : public LogSelect,
                  public Destination, 
                  public Scope
{
  // Destination Content.
  typedef enum { Error, Missing, Number, Name, Array } type;
  typedef std::map<symbol, type> type_map;
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
  name_map descriptions;
  std::vector<symbol> all_numbers_;

  // Log.
  symbol last_done;
  void done (const Time&, double dt);
  bool initial_match (const Daisy&, Treelog&)
    // No initial line.
  { return false; }

  // Self use.
  void output (Log&) const;

  // Select::Destination
  void error ();
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

  // Scope
  void tick (const Scope&, Treelog&);
  const std::vector<symbol>& all_numbers () const;
  bool has_number (symbol) const;
  double number (symbol) const;
  symbol dimension (symbol) const;
  bool has_identifier (symbol tag) const;
  symbol identifier (symbol tag) const;
  symbol get_description (symbol) const;

  // Scope to be?
  type lookup (symbol tag) const;
  const std::vector<double>& array (symbol tag) const;
  int size (symbol tag) const;

  // Create and destroy.
  void initialize (Treelog&);
public:
  LogExtern (Block&);
private:
  ~LogExtern ();
};

#endif // LOG_EXTERN_H
