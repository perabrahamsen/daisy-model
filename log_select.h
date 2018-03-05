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
#include "memutils.h"
#include <vector>
#include <memory>

class Select;
class Condition;
class Format;
class Volume;
class Summary;

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

struct EXPORT LogSelect : public Log
{
  // Parameters.
  const symbol description;	// Description of log file.
  const symbol file;       // Filename.
  const std::vector<std::pair<symbol, symbol>/**/> parameters;      // Par vals.
  std::unique_ptr<Condition> condition;	// Should we print a log now?
  std::unique_ptr<Condition> active;	// Should we look at data now?
  auto_vector<Select*> entries;
  std::unique_ptr<const Volume> volume;
  const bool print_initial;     // Set if initial values should be printed.

  // Summary.
  Time begin;                   // First log entry.
  Time end;                     // Last log entry.
  const auto_vector<Summary*> summary;

  // State. 
  bool is_printing;		// True iff this time step should be logged.
  bool is_active;		// True iff we need values for this time step.

  // Scopes.
  void find_scopes (std::vector<const Scope*>&) const;

  // Filter functions.
  bool check_leaf (symbol) const;
  bool check_interior (symbol) const;
  bool check_derived (symbol field, symbol name, symbol component) const;

  // Checking to see if we should log this time step.
  bool match (const Daisy& daisy, Treelog&);
  void done (const std::vector<Time::component_t>& time_columns,
	     const Time&, const double dt, Treelog& msg);

  // Initial line.
  bool initial_match (const Daisy&, const Time& previous, Treelog&);
  void initial_done (const std::vector<Time::component_t>& time_columns,
		     const Time&, Treelog&);

  // Print line.
  virtual void done_print (const std::vector<Time::component_t>& time_columns,
                           const Time& time) = 0;

  // Open a derived type (for LogAll to overwrite).
  virtual void open_derived_type (symbol type, const symbol library);

  // Open normal items.
  void open (symbol name);
  void close ();

  // Open named items.
  void open_named (symbol name);
  void close_named ();

  // Open ordered items.
  void open_ordered (int index);
  void close_ordered ();

  // Unnamed items.
  void open_unnamed ();
  void close_unnamed ();

  // Derived items.
  void open_derived (symbol field, symbol type, const symbol library);
  void close_derived ();

  // Model singletons with alist.
  void open_object (symbol field, symbol type, const Frame&,
		    symbol library); 
  void close_object ();

  // Derived items in a list.
  void open_entry (symbol type, const Frame&, symbol library);
  void close_entry ();

  // Named derived items in a list.
  void open_named_entry (symbol name, symbol type, 
			 const Frame&);
  void close_named_entry ();

  // Object names.
  void open_shallow (symbol type, const symbol library);
  void close_shallow ();

  void output_entry (symbol name, bool);
  void output_entry (symbol name, double);
  void output_entry (symbol name, int);
  void output_entry (symbol name, symbol);
  void output_entry (symbol name, const std::vector<double>&);
  void output_entry (symbol name, const PLF&);

  // Create and Destroy.
  void initialize (const symbol log_dir, const symbol suffix, Treelog&);
  bool check (const Border&, Treelog& err) const;
  static void document_entries (Format&, const Metalib&, Treelog&, symbol);
  static std::vector<std::pair<symbol, symbol>/**/>
  /**/ build_parameters (const Block& al);
  static bool default_print_initial (const std::vector<Select*>& entries);
  LogSelect (const BlockModel& al);
  LogSelect (const char* id);
  void summarize (Treelog&);
  ~LogSelect ();
};

#endif // LOG_SELECT_H
