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

  void output (symbol name, bool);
  void output (symbol name, double);
  void output (symbol name, int);
  void output (symbol name, symbol);
  void output (symbol name, const std::vector<double>& value);
  void output (symbol name, const PLF&);
  void output (symbol name, const Time&); // Obsolete.

  // Create and destroy.
private:
  static const AttributeList& get_alist ();
public:
  LogAll (const std::vector<Log*>& logs);
  ~LogAll ();
};

#endif // LOG_ALL_H
