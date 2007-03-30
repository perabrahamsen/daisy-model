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
#include "intrinsics.h"
#include "librarian.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"
#include <map>

struct Metalib::Implementation
{
  Syntax syntax_;
  AttributeList alist_;

  typedef std::map<symbol, Library*> library_map;
  library_map all;

  int sequence;

  Implementation ()
    : all (BuildBase::intrinsics ().all),
      sequence (0)
  { }
};
  
Syntax& 
Metalib::syntax () const
{ return impl->syntax_; }

AttributeList&
Metalib::alist () const
{ return impl->alist_; }

bool
Metalib::exist (const symbol name) const
{ return impl->all.find (name) != impl->all.end (); }

Library& 
Metalib::library (const symbol name) const
{ return *impl->all[name]; }

Library& 
Metalib::library (const char *const name) const
{ return library (symbol (name)); }

void
Metalib::all (std::vector<symbol>& libraries) const
{ 
  for (Implementation::library_map::const_iterator i = impl->all.begin (); 
       i != impl->all.end ();
       i++)
    libraries.push_back (symbol ((*i).first)); 
}

void 
Metalib::clear_all_parsed ()
{
  for (Implementation::library_map::iterator i = impl->all.begin (); 
       i != impl->all.end (); 
       i++)
    (*i).second->clear_parsed ();
}

void 
Metalib::refile_parsed (const std::string& from, const std::string& to)
{
  for (Implementation::library_map::iterator i = impl->all.begin (); 
       i != impl->all.end (); 
       i++)
    (*i).second->refile_parsed (from, to);
}

int 
Metalib::get_sequence ()
{ 
  impl->sequence++;
  // Nobody will ever need more than two billion objects --- Per 1998.
  daisy_assert (impl->sequence > 0);
  return impl->sequence;
}

Metalib::Metalib ()
  : impl (new Implementation ())
{ }

Metalib::~Metalib ()
{ }

// metalib.C ends here

