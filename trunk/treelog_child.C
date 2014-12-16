// treelog_child.h -- Modify treelog stream.
// 
// Copyright 2007 Per Abrahamsen and KVL.
// Copyright 2011 KU.
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
#include "treelog_child.h"

void
TreelogChild::open (const std::string& msg)
{ child.open (msg); }

void
TreelogChild::close ()
{ child.close (); }

void
TreelogChild::debug (const std::string& msg)
{ child.debug (msg); }

void
TreelogChild::entry (const std::string& msg)
{ child.entry (msg); }

void
TreelogChild::message (const std::string& msg)
{ child.message (msg); }

void
TreelogChild::warning (const std::string& msg)
{ child.warning (msg); }

void
TreelogChild::error (const std::string& msg)
{ child.error (msg); }

void
TreelogChild::bug (const std::string& msg)
{ child.bug (msg); }

void
TreelogChild::touch ()
{ child.touch (); }

void
TreelogChild::flush ()
{ child.flush (); }

TreelogChild::TreelogChild (Treelog& msg)
  : child (msg)
{ }
TreelogChild::~TreelogChild ()
{ }

void
TreelogSilent::message (const std::string& msg)
{ child.debug (msg); }

TreelogSilent::TreelogSilent (Treelog& msg)
  : TreelogChild (msg)
{ }

TreelogSilent::~TreelogSilent ()
{ }

// treelog_child.C ends here.

