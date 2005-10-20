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
#include "symbol.h"
#include "plf.h"
#include <string>

class Treelog;

class Block
{
  struct Implementation;
  std::auto_ptr<Implementation> impl;
public:
  const AttributeList& alist () const;
  const Syntax& syntax () const;

  // Error handling.
  void error (const std::string&);
  bool ok () const;
  void set_error ();

  // AList emulation.
public:
  bool check (const std::string& key) const;
  double number (const std::string&) const;
  double number (const std::string&, double default_value) const;
  const std::string name (const std::string&) const;
  const std::string name (const std::string&,
			  const std::string& default_value) const;
  symbol identifier (const std::string&) const;
  bool flag (const std::string&) const;
  bool flag (const std::string&, bool default_value) const;
  const PLF& plf (const std::string&) const;
  AttributeList& alist (const std::string&) const;
  int integer (const std::string&) const;
  int integer (const std::string&, int default_value) const;
  const std::vector<double>& number_sequence (const std::string&) const;
  const std::vector<symbol>
  /**/ identifier_sequence (const std::string& key) const;
  std::vector<std::string> name_sequence (const std::string& key) const;
  const std::vector<bool>& flag_sequence (const std::string& key) const;
  const std::vector<int>& integer_sequence (const std::string& key) const;
  const std::vector<const PLF*>& plf_sequence (const std::string& key) const;
  const std::vector<AttributeList*>& 
  /**/ alist_sequence (const std::string& key) const;

  // Create and Destroy.
public:
  explicit Block (const Syntax&, const AttributeList&, Treelog& msg, 
		  const std::string& scope_id);
  explicit Block (Block&, const Syntax&, const AttributeList&, 
		  const std::string& scope_tag);
  explicit Block (Block&, const Syntax&, const AttributeList&, 
		  const std::string& scope_tag, size_t index);
  ~Block ();
};

#endif // BLOCK_H
