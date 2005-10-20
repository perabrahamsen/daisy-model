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


#include "treelog.h"
#include <sstream>

using namespace std;

std::string 
sequence_id (std::string key, size_t index)
{
  std::ostringstream tmp;
  tmp << key << "[" << index << "]";
  return tmp.str ();
}

Treelog::Open::Open (Treelog& l, const symbol name)
  : log (l)
{ log.open (name.name ()); }

Treelog::Open::Open (Treelog& l, const string& name)
  : log (l)
{ log.open (name); }

Treelog::Open::~Open ()
{ log.close (); }

void
Treelog::open (const symbol name)
{ open (name.name ()); }

void
Treelog::debug (const string&)
{ }

void
Treelog::entry (const string&)
{ count++; }

void
Treelog::message (const string& str)
{ entry (str); }

void
Treelog::warning (const string& str)
{ entry (str); }

void
Treelog::error (const string& str)
{ entry (str); }

void
Treelog::lazy (const string& str)
{ entry (str); }

class TreelogNull : public Treelog
{
  // Nesting.
public:
  void open (const string&)
  { }
  void close ()
  { }

  // Use.
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
Treelog::null ()
{ return nulllog; }

Treelog::Treelog ()
  : count (0)  
{ }

Treelog::~Treelog ()
{ }
