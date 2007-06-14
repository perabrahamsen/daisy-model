// treelog.C -- Log hierarchical information.
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

#include "treelog.h"
#include <sstream>

Treelog::Open::Open (Treelog& l, const symbol name)
  : log (l)
{ log.open (name.name ()); }

Treelog::Open::Open (Treelog& l, const std::string& name)
  : log (l)
{ log.open (name); }

Treelog::Open::~Open ()
{ log.close (); }

void
Treelog::open (const symbol name)
{ open (name.name ()); }

void
Treelog::message (const std::string& str)
{ entry (str); }

void
Treelog::warning (const std::string& str)
{ entry (str + " (warning)"); }

void
Treelog::error (const std::string& str)
{ entry (str + " (error)"); }

class TreelogNull : public Treelog
{
  // Nesting.
public:
  void open (const std::string&)
  { }
  void close ()
  { }

  // Use.
  void debug (const std::string&)
  { }
  void entry (const std::string&)
  { }
  void touch ()
  { }
  void flush ()
  { }

  // Create and Destroy.
public:
  TreelogNull ()
  { }
  ~TreelogNull ()
  { }
};

static TreelogNull nulllog;

Treelog&
EXPORT Treelog::null () 
{ return nulllog; }

Treelog::Treelog ()
{ }

Treelog::~Treelog ()
{ }
