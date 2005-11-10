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

#include "block.h"
#include <string>
#include <vector>
#include <map>

class Treelog;
class LexerData;
class Time;

class LexerTable
{
  // Content.
private:
  const std::string filename;  
  LexerData* lex;
  std::string field_sep;
  std::string type_;
  const std::vector<std::string> missing;
  std::vector<std::string> tag_names;
  std::map<std::string,int> tag_pos;
  std::vector<size_t> fil_col;
  struct Filter;
  std::vector<const Filter*> filter;
  int year_c;
  int month_c;
  int mday_c;
  int hour_c;
  int time_c;
  const std::vector<std::string> original;
  const bool dim_line;
  std::vector<std::string> dim_names;
  
  // Use.
public:
  bool good ();
  bool read_header (Treelog& msg);
  const std::string& type () const;
  const std::string& dimension (size_t tag_c) const;
  int find_tag (const std::string& tag) const;
private:
  int find_tag (const std::string& tag1, const std::string& tag2) const;
private:
  std::string get_entry () const;
  void get_entries_raw (std::vector<std::string>& entries) const;
public:
  bool get_entries (std::vector<std::string>& entries) const;
private:
  int get_date_component (const std::vector<std::string>& entries, 
                          int column, int default_value) const;
  static bool get_time (const std::string& entry, Time& time); 
public:
  bool get_time (const std::vector<std::string>& entries, Time& time) const;
  bool is_missing (const std::string& value) const;
  double convert_to_double (const std::string& value) const;

  // Messages.
public:
  void debug (const std::string& str) const;
  void warning (const std::string& str) const;
  void error (const std::string& str) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList&);
private:
  LexerTable (const LexerTable&);
  LexerTable ();
public:
  explicit LexerTable (Block&);
  ~LexerTable ();
};

#endif // LEXER_TABLE_H
