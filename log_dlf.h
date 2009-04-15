// log_dlf.h -- Log selected data in dlf format.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2009 Per Abrahamsen and KU.
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

#ifndef LOG_DLF_H
#define LOG_DLF_H

#define BUILD_DLL

#include "log_select.h"
#include "dlf.h"
#include "symbol.h"
#include <fstream>
#include <vector>

struct LogDLF : public LogSelect
{
  // File Content.
  const symbol parsed_from_file; // Defined in...
  const symbol file;       // Filename.
  std::ofstream out;            // Output stream.
  const bool flush;             // Flush after each time step.
  const symbol record_separator; // String to print on records (time steps).
  const symbol field_separator; // String to print between fields.
  const symbol error_string; // String to print on errors.
  const symbol missing_value; // String to print for missing values.
  const symbol array_separator; // String to print between array entries.
  DLF print_header;             // How much header should be printed?
  std::vector<std::pair<symbol, symbol>/**/> parameters;      // Par vals.
  bool print_tags;              // Set if tags should be printed.
  bool print_dimension;         // Set if dimensions should be printed.
  const bool print_initial;     // Set if initial values should be printed.
  const bool std_time_columns;  // Add year, month, day and hour columns.
  Time begin;                   // First log entry.
  Time end;                     // Last log entry.

  // Log.
  void common_match (const Daisy& daisy, Treelog& out);
  void common_done (const std::vector<Time::component_t>& time_columns,
                    const Time& time, double dt);
  virtual void process_entry (size_t) = 0;

  // Log.
  bool match (const Daisy& daisy, Treelog& out);
  void done (const std::vector<Time::component_t>& time_columns,
             const Time& time, double dt);

  // Initial line.
  bool initial_match (const Daisy&, Treelog&);
  void initial_done (const std::vector<Time::component_t>& time_columns,
                     const Time& time, const double dt);

  // Create and destroy.
  bool check (const Border&, Treelog& msg) const;
  static bool contain_time_columns (const std::vector<Select*>& entries);
  void initialize (Treelog&);
  static std::vector<std::pair<symbol, symbol>/**/>
  /**/ build_parameters (Block& al);
  explicit LogDLF (Block& al);
  ~LogDLF ();
};

#endif // LOG_DLF_H

