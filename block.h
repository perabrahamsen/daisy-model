// block.h -- Support for block scoped variables.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#ifndef BLOCK_H
#define BLOCK_H

#include "syntax.h"
#include <string>

class Block
{
  struct Implementation;
  std::auto_ptr<Implementation> impl;

  // Use.
public:
  const Syntax& syntax () const;
  const AttributeList& alist () const;

  Syntax::type lookup (const std::string&) const;

  const Syntax& syntax (const std::string& key) const;
  const AttributeList& alist (const std::string& key) const;

  // Shortcuts.
public:
  const std::string expand (const std::string& name) const;

  // Create and Destroy.
public:
  explicit Block (const Syntax&, const AttributeList&);
  explicit Block (const Block&, const Syntax&, const AttributeList&);
  ~Block ();
};

#endif // BLOCK_H
