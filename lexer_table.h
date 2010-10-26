// lexer_table.h --- Lexical analysis for Daisy table files.
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


#ifndef LEXER_TABLE_H
#define LEXER_TABLE_H

#include "block_model.h"
#include "symbol.h"
#include <string>
#include <vector>
#include <boost/scoped_ptr.hpp>
#include <boost/noncopyable.hpp>

class Frame;
class Treelog;
class Time;

class LexerTable : private boost::noncopyable
{
  // Content.
private:
  class Implementation;
  boost::scoped_ptr<Implementation> impl;

  // Use.
public:
  bool good ();
  bool read_header (Treelog& msg);
  bool read_header_with_keywords (Frame& keywords, Treelog& msg);
  const std::string& type () const;
  const std::vector<symbol>& tag_names () const;
  symbol dimension (size_t tag_c) const;
  int find_tag (const symbol tag) const;
  bool get_entries (std::vector<std::string>& entries) const;
  static bool get_time (const std::string& entry, Time& time, 
                        int default_hour); 
  bool get_time (const std::vector<std::string>& entries, Time& time, 
                 int default_hour) const;
  bool is_missing (const std::string& value) const;
  double convert_to_double (const std::string& value) const;

  // Messages.
public:
  void debug (const std::string& str) const;
  void warning (const std::string& str) const;
  void error (const std::string& str) const;

  // Create and Destroy.
public:
  static void load_syntax (Frame&);
  explicit LexerTable (const BlockModel&);
  ~LexerTable ();
};

#endif // LEXER_TABLE_H
