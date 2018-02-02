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

#define BUILD_DLL

#include "log.h"
#include "library.h"
#include "metalib.h"
#include "block_model.h"
#include "daisy.h"
#include "assertion.h"
#include "librarian.h"
#include "frame_model.h"
#include <sstream>
#include <list>
#include <string>

const char *const Log::component = "log";

symbol 
Log::library_id () const
{
  static const symbol id (component);
  return id;
}

struct Log::Implementation
{
  const Metalib* metalib;

  Implementation ()
    : metalib (NULL)
  { }
};

const Metalib&
Log::metalib () const
{
  daisy_assert (impl->metalib);
  return *impl->metalib;
}

void 
Log::find_scopes (std::vector<const Scope*>&) const
{ }

bool
Log::check_entry (symbol name, const symbol component) const
{
  bool looking = true;
  const Library& library = metalib ().library (component);

  // TODO: We should have check_interior use Library::ancestors for speed.
  while (looking && !check_interior (name))
    {
      if (library.check (name))
	{
	  const FrameModel& frame = library.model (name);
          const symbol base = frame.base_name ();
          daisy_assert (base != name);
	  if (base != Attribute::None ())
	    name = base;
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
Log::open_alist (symbol name, const Frame&)
{ open (name); }

void 
Log::close_alist ()
{ close (); }

void 
Log::column_clear ()
{ }

void 
Log::column_add_to_total (const Column&)
{ }

void 
Log::column_select (const Column&)
{ }

void
Log::output (Log&) const
{ }

void
Log::initialize_common (const symbol log_dir, const symbol suffix,
                        const Metalib& metalib, Treelog& msg)
{
  daisy_assert (!impl->metalib);
  impl->metalib = &metalib;
  initialize (log_dir, suffix, msg);
}

Log::Log (const BlockModel& al)
  : ModelFramed (al),
    impl (new Implementation ())
{ }

Log::Log (const char *const id)
  : ModelFramed (symbol (id)),
    impl (new Implementation ())
{ }

void
Log::summarize (Treelog&)
{ }

Log::~Log ()
{ }

static struct LogInit : public DeclareComponent 
{
  LogInit ()
    : DeclareComponent (Log::component, "\
Running a simulation is uninteresting, unless you can get access to\n\
the results in one way or another.  The purpose of the 'log' component\n\
is to provide this access.  Most 'log' models does this by writing a\n\
summary of the state to a log file.")
  { }
} Log_init;
