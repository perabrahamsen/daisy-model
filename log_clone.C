// log_clone.C
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

//
// Clone an object by using its log function.

#include "log_clone.h"
#include "block.h"

bool
LogClone::match (const Daisy&, Treelog&)
{ daisy_notreached (); }

bool 
LogClone::check_leaf (symbol) const
{ daisy_notreached (); }

bool 
LogClone::check_interior (symbol) const
{ daisy_notreached (); }

void
LogClone::done (const Time&, double)
{ daisy_notreached (); }

bool 
LogClone::initial_match (const Daisy&, Treelog&) 
{ daisy_notreached (); }

void 
LogClone::initial_done (const Time&, double)
{ daisy_notreached (); }

const AttributeList& 
LogClone::result ()
{ return alist (); }

void 
LogClone::initialize (Treelog&)
{ }

LogClone::LogClone (const std::string& name, 
		    Block& al)
  : LogAList (al)
{ 
  push (symbol (name), al.syntax (), al.alist ());
}

LogClone::~LogClone ()
{
  // Check stacks.
  daisy_assert (syntax_stack.size () == 1U);
  daisy_assert (alist_stack.size () == 1U);
  daisy_assert (library_stack.size () == 1U);
  
  // Cleanup.
  delete &alist ();
  pop ();
  daisy_assert (nested == 0);
}
