// log_regress.C -- Log selected data in tabular format.
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

#define BUILD_DLL

#include "log_dlf.h"
#include "destination.h"
#include "select.h"
#include "library.h"
#include "block.h"
#include "summary.h"
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
  // Summarize this log file.
  const auto_vector<Summary*> summary;

  // destination Content.
  enum { Error, Missing, Number, Name, Array } type;
  double dest_number;
  symbol dest_name;
  const std::vector<double>* dest_array;
  
  // LogDLF.
  void process_entry (size_t i);

  // Initial line.
  bool initial_match (const Daisy&, Treelog&);

  // Select::Destination
  void error ();
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

  // Create and destroy.
  void initialize (Treelog&);
  explicit LogRegress (Block& al);
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
LogRegress::initial_match (const Daisy& daisy, Treelog& msg)
{
  for (unsigned int i = 0; i < summary.size (); i++)
    summary[i]->clear ();

  return LogDLF::initial_match (daisy, msg);
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

  Treelog::Open nest (msg, name);
  for (unsigned int i = 0; i < summary.size (); i++)
    summary[i]->initialize (entries, msg);
}

LogRegress::LogRegress (Block& al)
  : LogDLF (al),
    summary (Librarian::build_vector<Summary> (al, "summary")),
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
{
  if (summary.size () > 0)
    {
      Treelog::Open nest (msg, name);
      std::ostringstream tmp;

      tmp << "LOGFILE: " << file  << "\n";
      tmp << "VOLUME: " << volume->one_line_description () << "\n";
      tmp << "TIME: " << begin.print () << " to " << end.print ();
      msg.message (tmp.str ());
      const Timestep step = end - begin;
      for (size_t i = 0; i < summary.size (); i++)
        summary[i]->summarize (Time::hours_between (begin, end), msg);
    }
}

LogRegress::~LogRegress ()
{ }

static struct LogRegressSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new LogRegress (al); }

  LogRegressSyntax ()
    : DeclareModel (Log::component, "regress", "DLF", "\
Each selected variable is represented by a column in the specified log file.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_object ("summary", Summary::component,
                          Value::Const, Value::Variable,
                          "Summaries for this log file.");
    frame.set_empty ("summary");
  }
} LogRegress_syntax;

// log_regress.C ends here.
