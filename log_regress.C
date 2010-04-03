// log_regress.C -- Append summary data to a DLF file.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2009 Copenhagen University.
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

#include "log_dlf.h"
#include "destination.h"
#include "select.h"
#include "library.h"
#include "block_model.h"
#include "timestep.h"
#include "memutils.h"
#include "librarian.h"
#include "scope_block.h"
#include "treelog.h"
#include "frame.h"
#include "assertion.h"
#include <sstream>

struct LogRegress : public LogDLF, public Destination
{
  // destination Content.
  enum { Error, Missing, Number, Name, Array } type;
  double dest_number;
  symbol dest_name;
  const std::vector<double>* dest_array;
  
  // LogDLF.
  void process_entry (size_t i);

  // Initial line.
  bool initial_match (const Daisy&, const Time& previous, Treelog&);

  // Select::Destination
  void error ();
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

  // Create and destroy.
  void initialize (Treelog&);
  explicit LogRegress (const BlockModel& al);
  void summarize (Treelog&);
  ~LogRegress ();
};

void 
LogRegress::process_entry (const size_t i)
{ 
  switch (type)
    {
    case Error:
      out << error_string;
      break;
    case Missing: 
      out << missing_value;
      break;
    case Number:
      out << dest_number;
      break;
    case Name: 
      out << dest_name;
      break;
    case Array:
      {
        daisy_assert (dest_array);
        const std::vector<double> array = *dest_array;
        for (unsigned int i = 0; i < array.size (); i++)
          {
            if (i != 0)
              out << array_separator;
            out << array[i];
          }
      }
      break;
    default:
      daisy_notreached ();
    }
}

bool 
LogRegress::initial_match (const Daisy& daisy, const Time& previous, 
                           Treelog& msg)
{
  return LogDLF::initial_match (daisy, previous, msg);
}

void 
LogRegress::error ()
{ 
  type = Error;
}

void 
LogRegress::missing ()
{ 
  type = Missing;
}

void 
LogRegress::add (const std::vector<double>& value)
{ 
  type = Array;
  dest_array = &value;
}

void 
LogRegress::add (const double value)
{ 
  type = Number;
  dest_number = value;
}

void 
LogRegress::add (const symbol value)
{ 
  type = Name;
  dest_name = value;
}


void
LogRegress::initialize (Treelog& msg)
{
  LogDLF::initialize (msg);
}

LogRegress::LogRegress (const BlockModel& al)
  : LogDLF (al),
    type (Error),
    dest_number (-42.42e42),
    dest_name ("Daisy bug"),
    dest_array (NULL)
{
  if (!al.ok ())
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->add_dest (this);
}

void
LogRegress::summarize (Treelog& msg)
{ }

LogRegress::~LogRegress ()
{ }

static struct LogRegressSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LogRegress (al); }

  LogRegressSyntax ()
    : DeclareModel (Log::component, "regress", "DLF", "\
Maintain a DLF file containing simulation results from multiple runs.\n\
Warn if result from this run diverge from previous runs.  The intention\n\
is that model should be used for regression testing.")
  { }
  void load_frame (Frame&) const
  { }
} LogRegress_syntax;

// log_regress.C ends here.
