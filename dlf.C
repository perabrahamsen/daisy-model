// dlf.h -- Printing Daisy Log File headers.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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

#include "dlf.h"
#include "volume.h"
#include "alist.h"
#include "assertion.h"
#include "version.h"
#include "daisy.h"
#include <time.h>
#include <ostream>

DLF::type 
DLF::string2type (const std::string& s)
{ 
  if (s == "true")
    return Full;
  if (s == "false")
    return None;
  daisy_assert (s == "fixed");
  return Terse;
}

void
DLF::start (std::ostream& out, const symbol name,
            const std::string& file,
            const std::string& parsed_from_file) const
{
  if (value == DLF::None)
    return;

  out << "dlf-0.0 -- " << name;
  if (parsed_from_file != "")
    out << " (defined in '" << parsed_from_file << "').";
  out << "\n";
  out << "\n";
  out << "VERSION: " << version  << "\n";
  out << "LOGFILE: " << file  << "\n";
  time_t now = time (NULL);
  out << "RUN: " << ctime (&now);
  if (value != Terse)
    out << "\n";
}

void
DLF::interval (std::ostream& out, const Volume& volume) const
{ out << "INTERVAL: " << volume.one_line_description () << "\n"; }

void
DLF::log_description (std::ostream& out, const std::string& description) const
{
  if (value != DLF::Full)
    return;

  out << "\nLOG: ";
  for (unsigned int i = 0; i < description.size (); i++)
    if (description[i] != '\n')
      out << description[i];
    else
      out << "\nLOG: ";
  out << "\n\n";
}

void
DLF::finish (std::ostream& out, const Daisy& daisy)
{
  // No (additional) header.
  if (value == None)
    return;

  const AttributeList& al = daisy.alist;

  // SIMFILE:
  if (al.check ("parser_files"))
    {
      const std::vector<symbol>& files 
        = al.identifier_sequence ("parser_files");
      if (value == Terse)
        {
          out << "SIMFILE:";
          for (unsigned int i = 0; i < files.size (); i++)
            out << " " << files[i];
          out << "\n";
        }
      else
        for (unsigned int i = 0; i < files.size (); i++)
          out << "SIMFILE: " << files[i] << "\n";
    }
  else if (value == Terse)
    out << "SIMFILE:\n";

  // SIM:
  const std::string sim_description = al.name ("description");
  if (sim_description != Daisy::default_description || value == Terse)
    {
      out << "SIM: ";
      for (unsigned int i = 0; i < sim_description.size (); i++)
        if (sim_description[i] != '\n')
          out << sim_description[i];
        else if (value == Terse)
          out << " ";
        else
          out << "\nSIM: ";
      out << "\n";
    }
  
  // End of header.
  out << "\n--------------------\n";

  // Only do this once.
  value = None;
}
