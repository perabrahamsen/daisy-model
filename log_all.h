// log_all.h
// 
// Copyright 2003 Per Abrahamsen and KVL.
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


#ifndef LOG_ALL_H
#define LOG_ALL_H

#include "log_select.h"

class LogAll : public LogSelect
{
  // Content.
private:
  std::vector<LogSelect*> slaves;
  std::stack<std::vector<Select*>/**/> active_leafs;
  std::stack<std::vector<Select*>/**/> active_interiors;
  Treelog* msg;

  // Filter functions.
  bool check_leaf (symbol) const;
  bool check_interior (symbol) const;

  // Use.
private:
  
public:
  void insert_active ();
  bool match (const Daisy& daisy, Treelog&);
  void done (const Time& time);

  // Initial line.
  bool initial_match (const Daisy&, Treelog&);
  void initial_done (const Time& time);

  // Open normal items.
  void open (symbol name);
  void close ();

  void output_entry (symbol name, bool);
  void output_entry (symbol name, double);
  void output_entry (symbol name, int);
  void output_entry (symbol name, symbol);
  void output_entry (symbol name, const std::vector<double>& value);
  void output_entry (symbol name, const PLF&);

  // Create and destroy.
  void initialize (Treelog&);
private:
  static Block& get_block ();
public:
  LogAll (const std::vector<Log*>& logs);
  ~LogAll ();
};

#endif // LOG_ALL_H
