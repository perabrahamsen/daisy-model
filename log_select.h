// log_select.h
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


#ifndef LOG_SELECT_H
#define LOG_SELECT_H

#include "log.h"
#include "select.h"		// Need to be here to ensure proper
#include "condition.h"		// initialization in derived classes.

struct Select;
struct Condition;

struct LogSelect : public Log
{
  // State. 
  bool is_printing;		// True iff this time step should be logged.
  bool is_active;		// True iff we need values for this time step.

  // Filter functions.
  bool check_member (const string&) const;
  bool check_derived (const string& field, const string& name,
		      const Library&) const;
  const string description;	// Description of log file.
  Condition& condition;	// Should we print a log now?

  vector<Select*> entries;

  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy, Treelog&);

  // Open normal items.
  void open (const string& name);
  void close ();

  // Open named items.
  void open_named (const string& name);
  void close_named ();

  // Open ordered items.
  void open_ordered (int index);
  void close_ordered ();

  // Unnamed items.
  void open_unnamed ();
  void close_unnamed ();

  // Derived items.
  void open_derived (const string& field, const string& type);
  void close_derived ();

  // Derived items in a list.
  void open_entry (const string& type, const AttributeList&);
  void close_entry ();

  // Named derived items in a list.
  void open_named_entry (const string& name, const string& type, 
			 const AttributeList&);
  void close_named_entry ();

  void output (const string& name, const Time& value);
  void output (const string&, const bool);
  void output (const string& name, const double value);
  void output (const string& name, const int value);
  void output (const string& name, const string& value);
  void output (const string& name, const vector<double>& value);
  void output (const string&, const PLF&);

  // Create and Destroy.
  bool check (const Syntax&, Treelog& err) const;
  static void load_syntax (Syntax&, AttributeList&);
  LogSelect (const AttributeList& al);
  ~LogSelect ();
};

#endif // LOG_SELECT_H
