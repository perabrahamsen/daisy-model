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
  vector<LogSelect*> slaves;
  stack<vector<Select*>/**/> active_leafs;
  stack<vector<Select*>/**/> active_interiors;

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

  void output (symbol name, const bool);
  void output (symbol name, const double value);
  void output (symbol name, const int value);
  void output (symbol name, const string& value);
  void output (symbol name, const vector<double>& value);
  void output (symbol name, const PLF&);
  void output (symbol name, const Time&); // Obsolete.

  // Create and destroy.
private:
  static const AttributeList& get_alist ();
public:
  LogAll (const vector<Log*>& logs);
  ~LogAll ();
};

#endif // LOG_ALL_H
