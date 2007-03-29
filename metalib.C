// metalib.C -- A library of libraries.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "metalib.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"

struct Metalib::Implementation
{
  Syntax syntax_;
  AttributeList alist_;

  Implementation ()
  { }
};
  
Syntax& 
Metalib::syntax () const
{ return impl->syntax_; }

AttributeList&
Metalib::alist () const
{ return impl->alist_; }

bool
Metalib::exist (symbol name) const
{ return Library::metalib_exist (name); }

Library& 
Metalib::library (const symbol name) const
{ return Library::metalib_find (name); }

void 
Metalib::all (std::vector<symbol>& libraries) const
{ Library::metalib_all (libraries); }

int 
Metalib::get_sequence ()
{ return Library::metalib_get_sequence (); }

void 
Metalib::clear_all_parsed ()
{ Library::metalib_clear_all_parsed (); }

void 
Metalib::refile_parsed (const std::string& from, const std::string& to)
{ Library::metalib_refile_parsed (from, to); }

Metalib::Metalib ()
  : impl (new Implementation ())
{ }

Metalib::~Metalib ()
{ }

// metalib.C ends here

