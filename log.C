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
#include "tmpstream,h"

EMPTY_TEMPLATE
Librarian<Log>::Content* Librarian<Log>::content = NULL;

const char *const Log::description = "\
Running a simulation is uninteresting, unless you can get access to\n\
the results in one way or another.  The purpose of the 'log' component\n\
is to provide this access.  Most 'log' models does this by writing a\n\
summary of the state to a log file.";

struct Log::Implementation
{
  list<const Geometry*> geometries;
};

bool
Log::check_entry (const string& name, const Library& library) const
{
  const string* type = &name;
  bool looking = true;

  while (looking && !check_member (*type))
    {
      if (library.check (*type))
	{
	  const AttributeList alist = library.lookup (*type);
	  if (alist.check ("type"))
	    type = &alist.name ("type");
	  else
	    looking = false;
	}
      else
	looking = false;
    }
  return looking;
}


void 
Log::open_named (const string&)
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
Log::open_alist (const string& name, const AttributeList&)
{ open (name); }

void 
Log::close_alist ()
{ close (); }

void 
Log::open_geometry (const Geometry& g)
{ 
  impl.geometries.push_back (&g);
}

void 
Log::close_geometry ()
{
  impl.geometries.pop_back ();
}

const Geometry*
Log::geometry ()
{
  daisy_assert (!impl.geometries.empty ());
  return impl.geometries.back ();
}

void 
Log::done ()
{ }

void
Log::print_dlf_header (ostream& out, const AttributeList& al)
{
  if (al.check ("parser_files"))
  {
    const vector<string>& files = al.name_sequence ("parser_files");
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
    name (al.name ("type"))
{ }

Log::~Log ()
{
  delete &impl;
}
