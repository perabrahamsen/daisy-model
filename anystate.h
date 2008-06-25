// anystate.h --- A class able to store any state.
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
// 
// The idea is that Anystate specifies the generic interface to the
// state of a model, while the relevant model will derive a class from
// Anystate::Content to store the actual content of the model.

#ifndef ANYSTATE_H
#define ANYSTATE_H

#include <memory>

class Anystate
{
  // Content.
public:
  class Content
  {
  private:                      // Disable
    Content& operator= (const Content&);
    Content (const Content&);
  public:
    virtual std::auto_ptr<Content> clone () const = 0;
    Content ();
    virtual ~Content ();
  };
  std::auto_ptr<Content> content;
  const Content& inspect () const;

  // Create and Destroy.
public:
  static Anystate none ();
private:
  Anystate ();                   // Disable.
public:
  Anystate& operator= (const Anystate&);
  Anystate (const Anystate&);
  Anystate (std::auto_ptr<Content>);
  ~Anystate ();
};

#endif // ANYSTATE_H
