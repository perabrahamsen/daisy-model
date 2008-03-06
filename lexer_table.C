// lexer_data.C --- Read tabular data from a file.
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

#include "lexer_table.h"
#include "lexer_data.h"

bool 
LexerTable::good ()
{ return lex->good (); }

bool
SourceFile::read_header ()
{
  // Open errors?
  if (!lex->good ())
    return false;

  // Read first line.
  type_ = lex->get_word ();
  if (type == "dwf-0.0")
    field_sep = "";
  else if (type == "dlf-0.0")
    field_sep = "\t";
  else if (type == "ddf-0.0")
    field_sep = "\t";
  else
    {
      lex->error ("Unknown file type '" + type + "'");
      field_sep = "\t";
    }
  lex->skip_line ();
  lex->next_line ();

  // Skip keywords.
  while (lex->good () && lex->peek () != '-')
    {
      lex->skip_line ();
      lex->next_line ();
    }

  // Skip hyphens.
  while (lex->good () && lex->peek () == '-')
    lex->get ();
  lex->skip_space ();

  return lex->good ();
}  

const std::string&
LexerTable::type ()
{ return type_; }

std::string
LexerTable::get_entry () const
{
  std::string tmp_term;  // Data storage.
  const char* field_term;

  switch (field_sep.size ())
    { 
    case 0:
      // Whitespace
      field_term = " \t\n";
      break;
    case 1:
      // Single character field seperator.
      tmp_term = field_sep + "\n";
      field_term = tmp_term.c_str ();
      break;
    default:
      // Multi-character field seperator.
      daisy_assert (false);
    }

  // Find it.
  std::string entry = "";
  while (lex->good ())
    {
      int c = lex->peek ();
      if (strchr (field_term, c))
	break;
      entry += int2char (lex->get ());
    }
  return entry;
}

std::vector<std::string>
LexerTable::get_entries () const
{
  lex->skip ("\n");
  std::vector<std::string> entries;

  while (lex->good ())
    {
      entries.push_back (get_entry (lex));

      if (lex->peek () == '\n')
        break;

      if (field_sep == "")
	lex->skip_space ();
      else
	lex->skip(field_sep.c_str ());
    }
  return entries;
}

void
LexerTable::warning (const std::string& str)
{ lex->warning (str); }

void 
LexerTable::error (const std::string& str)
{ lex->error (str); }

LexerTable::LexerTable (const std::string& name, Treelog& msg)
  : lex (new LexerData (name, msg)),
    field_sep ("UNINITIALIZED"),
    type ("UNINITIALIZED")
{ }

LexerTable::~LexerTable ()
{ }
