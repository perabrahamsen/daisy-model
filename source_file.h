// source_file.h -- File source for gnuplot interface 
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

#ifndef SOURCE_FILE_H
#define SOURCE_FILE_H

#include "source.h"

class LexerData;

static int
find_tag (const std::map<std::string,int>& tag_pos, const std::string& tag)
{
  if (tag_pos.find (tag) == tag_pos.end ())
    return -1;
  return tag_pos.find (tag)->second;
}

class SourceFile : public Source
{
  // Content.
protected:
  const std::string filename;  
  std::string with_;
  const int style_;
  const std::vector<std::string> missing;
  std::vector<Time> times;
  std::vector<double> values;
  
  // Filter.
private:
  struct Filter;
  std::vector<const Filter*> filter;
  
  // Interface.
public:
  const std::string& with () const
  { return with_; }
  int style () const 
  { return style_; }
  const std::vector<Time>& time () const
  { return times; }
  const std::vector<double>& value () const
  { return values; }

  // Read.
private:
  std::string get_entry (LexerData& lex) const;
protected:
  std::vector<std::string> get_entries (LexerData& lex) const;
private:
  static int get_date_component (LexerData& lex,
                                 const std::vector<std::string>& entries, 
                                 int column, 
                                 int default_value);
  static Time get_time (const std::string& entry);
protected:
  static double convert_to_double (LexerData& lex, const std::string& value);
private:
  std::string field_sep;
protected:
  std::vector<std::string> tag_names;
  std::map<std::string,int> tag_pos;
private:
  std::vector<size_t> fil_col;
  int year_c;
  int month_c;
  int mday_c;
  int hour_c;
  int time_c;
protected:
  bool read_header (LexerData&);
  bool read_entry (LexerData&, std::vector<std::string>&, Time&) const;

  // Create.
public:
  static void load_syntax (Syntax&, AttributeList&);
protected:
  explicit SourceFile (const AttributeList&);
public:
  ~SourceFile ();
};

#endif // SOURCE_H

