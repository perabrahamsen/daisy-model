// fetch_pretty.C --- Fetch data from a log model to a summary model.
// 
// Copyright 2003-2004 Per Abrahamsen and KVL
// Copyright 2009 University of Copenhagen
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

#include "fetch_pretty.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "mathlib.h"
#include <ostream>

int 
FetchPretty::width (const double value)
{
  if (!std::isnormal (value))
    return 1;

  const int absolute 
    = double2int (std::max (0.0, floor (log10 (fabs (value))))) + 1;
  if (value < 0)
    return absolute + 1;
  else
    return absolute;
}

size_t 
FetchPretty::name_size () const
{ 
  if (type != Content)
    return name.name ().size ();
  else if (add_delta)
    return name.name ().size () + 6;
  else
    return name.name ().size () + 9;
}

int 
FetchPretty::value_size (double& total) const
{
  double value = 0.0;
  switch (type)
    {
    case Error:
    case NewContent:
      break;
    case Content:
      value = (last - initial) * factor;
      break;
    case Flux:
      value = sum * factor;
      break;
    }
  total += value;
  return width (value);
}

void 
FetchPretty::summarize (std::ostream& out, const int width) const
{
  if (type == Content && add_delta)
    out << "delta ";
  out << name;
  if (type == Content && !add_delta)
    out << " increase";
  out << " = ";
  out.width (width);
  switch (type)
    {
    case NewContent:
      out << "no data";
      break;
    case Content:
      {
        const double value = (last - initial) * factor;
        out << value;
      }
      break;
    case Flux:
      {
        const double value = sum * factor;
        out << value;
      }
      break;
    case Error:
      out << "bogus data";
      break;
    }
  out << " " << "[" << dimension () << "]\n";
}

void
FetchPretty::clear (const std::vector<FetchPretty*>& fetch)
{ 
  for (size_t i = 0; i != fetch.size (); i++)
    fetch[i]->clear ();
}

void
FetchPretty::initialize (const std::vector<FetchPretty*>& fetch,
                         std::vector<Select*>& select, Treelog& msg)
{ 
  for (size_t i = 0; i != fetch.size (); i++)
    fetch[i]->initialize (select, msg);
}

FetchPretty::FetchPretty (const FrameSubmodel& al)
  : Fetch (al),
    name (al.name ("name", tag)),
    factor (al.number ("factor")),
    add_delta (true)
{ }

FetchPretty::FetchPretty (const symbol key)
  : Fetch (key),
    name (tag),
    factor (1.0),
    add_delta (false)
{ }

void
FetchPretty::load_syntax (Frame& frame)
{
  Fetch::load_syntax (frame); 
  frame.declare_string ("name", Attribute::OptionalConst, "\
Name to use for this line.  By default use the tag.");
  frame.declare ("factor", Attribute::None (), Attribute::Const, "\
Factor to multiply with to get the sum.\n\
Typically 1.0 to add this line, or -1.0 to subtract it.");
  frame.set ("factor", 1.0);
}

static DeclareSubmodel 
fetch_prettysubmodel (FetchPretty::load_syntax, "FetchPretty", "\
A summary file line.");

// fetch_pretty.C ends here.
