// anystate.C --- A class able to store any state.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#include "anystate.h"
#include "assertion.h"

Anystate::Content::Content ()
{ }

Anystate::Content::~Content ()
{ }

const Anystate::Content& 
Anystate::inspect () const
{ return *content; }

Anystate
Anystate::none ()
{
  struct ContentNone : public Content
  {
    std::auto_ptr<Content> clone () const
    { 
      std::auto_ptr<Content> copy (new ContentNone ());
      return copy; 
    }
  };
  
  std::auto_ptr<Content> copy (new ContentNone ());
  return Anystate (copy);
}

Anystate& 
Anystate::operator= (const Anystate& other)
{
  daisy_assert (this != &other);
  content = other.content->clone (); 
  return *this;
}

Anystate::Anystate (const Anystate& other)
{
  daisy_assert (this != &other);
  content = other.content->clone ();
}

Anystate::Anystate (const std::auto_ptr<Content> other)
{ 
  daisy_assert (content.get () != other.get ());
  content = other->clone (); 
}

Anystate::~Anystate ()
{ }

// anystate.C ends here.
