// block_nested.h -- Support for blocks nested in other blocks.
// 
// Copyright 2005, 2009 Per Abrahamsen and KVL.
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

#include "block_nested.h"
#include "frame.h"

const Metalib& 
BlockNested::metalib () const
{ return context.metalib (); }

Treelog&
BlockNested::msg () const
{ return context.msg (); }

void
BlockNested::set_error () const
{ 
  Block::set_error ();
  context.set_error (); 
}

const Frame& 
BlockNested::find_frame (const symbol key) const
{
  if (frame ().check (key) || frame ().lookup (key) != Attribute::Error)
    return frame ();

  return context.find_frame (key);
}

Attribute::type 
BlockNested::lookup (const symbol key) const
{
  Attribute::type type = Block::lookup (key); 
  if (type != Attribute::Error)
    return type;

  return context.lookup (key);
}

void 
BlockNested::entries (std::set<symbol>& all) const
{
  Block::entries (all);
  context.entries (all);
}

BlockNested::BlockNested (const Block& block, symbol scope_tag)
  : context (block),
    msg_nest (block.msg (), scope_tag)
{ }

BlockNested::~BlockNested ()
{ }

// block_nested.C ends here.

