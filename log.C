// log.C
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


#include "log.h"
#include "daisy.h"
#include <sstream>

using namespace std;

EMPTY_TEMPLATE
Librarian<Log>::Content* Librarian<Log>::content = NULL;

const char *const Log::description = "\
Running a simulation is uninteresting, unless you can get access to\n\
the results in one way or another.  The purpose of the 'log' component\n\
is to provide this access.  Most 'log' models does this by writing a\n\
summary of the state to a log file.";

struct Log::Implementation
{
  list<const Soil*> geometries;
};

bool 
Log::initial_match (const Daisy&, Treelog&)
{ return false; }

void
Log::initial_done (const Time&)
{ }

bool
Log::check_entry (symbol name, const Library& library) const
{
  bool looking = true;

  while (looking && !check_interior (name))
    {
      if (library.check (name))
	{
	  const AttributeList alist = library.lookup (name);
	  if (alist.check ("type"))
	    name = alist.identifier ("type");
	  else
	    looking = false;
	}
      else
	looking = false;
   }
  return looking;
}


void 
Log::open_named (symbol)
{ open_unnamed (); }

void 
Log::close_named ()
{ close_unnamed (); }

void 
Log::open_ordered (int)
{ open_unnamed (); }

void 
Log::close_ordered ()
{ close_unnamed (); }

void 
Log::open_alist (symbol name, const AttributeList&)
{ open (name); }

void 
Log::close_alist ()
{ close (); }

void 
Log::open_soil (const Soil& g)
{ 
  impl.geometries.push_back (&g);
}

void 
Log::close_soil ()
{
  impl.geometries.pop_back ();
}

const Soil*
Log::soil ()
{
  daisy_assert (!impl.geometries.empty ());
  return impl.geometries.back ();
}

void 
Log::done (const Time&)
{ }

void
Log::print_dlf_header (std::ostream& out, const AttributeList& al)
{
  if (al.check ("parser_files"))
  {
    const vector<symbol>& files = al.identifier_sequence ("parser_files");
    for (unsigned int i = 0; i < files.size (); i++)
      out << "SIMFILE: " << files[i] << "\n";
  }

  const string sim_description = al.name ("description");
  if (sim_description != Daisy::default_description)
    {
      out << "SIM: ";
      for (unsigned int i = 0; i < sim_description.size (); i++)
	if (sim_description[i] != '\n')
	  out << sim_description[i];
	else
	  out << "\nSIM: ";
      out << "\n";
    }
  out << "\n--------------------\n";
}

Log::Log (const AttributeList& al)
  : impl (*new Implementation ()),
    name (al.identifier ("type"))
{ }

void
Log::summarize (Treelog&)
{ }

Log::~Log ()
{
  delete &impl;
}
