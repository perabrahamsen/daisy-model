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

#define BUILD_DLL

#include "dlf.h"
#include "volume.h"
#include "assertion.h"
#include "version.h"
#include "daisy.h"
#include "toplevel.h"
#include "metalib.h"
#include "frame_model.h"
#include <time.h>
#include <ostream>

DLF::type 
DLF::string2type (const symbol s)
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
            const symbol file, const symbol parsed_from_file) const
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
DLF::parameter (std::ostream& out, 
                const symbol name, const symbol value) const
{ out << name << ": " << value << "\n"; }

void
DLF::interval (std::ostream& out, const Volume& volume) const
{ out << "INTERVAL: " << volume.one_line_description () << "\n"; }

void
DLF::log_description (std::ostream& out, const symbol description_s) const
{
  const std::string description = description_s.name ();
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
  
  const Metalib& global_alist = daisy.metalib;
  const FrameModel& daisy_alist = daisy.frame;

  // SIMFILE:
  const std::vector<symbol>& files = global_alist.parser_files ();
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

  // SIM:
  const symbol sim_description_s = daisy_alist.description ();
  if ((sim_description_s != Daisy::default_description
       && sim_description_s != Toplevel::default_description)
      || value == Terse)
    {
      const std::string sim_description = sim_description_s.name ();
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
