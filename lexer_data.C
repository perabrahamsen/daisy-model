// lexer_data.C --- Reda data from a file.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "lexer_data.h"
#include "time.h"
#include "mathlib.h"
#include <ctype.h>

string
LexerData::get_word ()
{
  string tmp;
  while (good () && !isspace (peek ()))
    tmp += int2char (get ());
  return tmp;
}

double
LexerData::get_number ()
{
  string str;
  int c = peek ();

  while (good () && (isdigit (c) 
		     || c == '.' || c == '-' || c == '+' 
		     || c == 'e' || c == 'E'))
    {
      str += int2char (c);
      get ();
      c = peek ();
    }
  // Empty number?
  if (str.size () < 1U)
    {
      error ("Number expected");
      return -42.42e42;
    }
  const char *c_str = str.c_str ();
  const char *endptr = c_str;
  const double value = strtod (c_str, const_cast<char**> (&endptr));
  
  if (*endptr != '\0')
    error (string ("Junk at end of number '") + endptr + "'");

  return value;
}

int
LexerData::get_cardinal ()
{
  string str;
  int c = peek ();

  while (isdigit (c))
    {
      str += int2char (c);
      get ();
      c = peek ();
    }
  // Empty number?
  if (str.size () < 1U)
    {
      error ("Integer expected");
      return -42;
    }
  return atoi (str.c_str ());
}

void 
LexerData::read_date (Time& time)
{
  int year = get_cardinal ();
  skip ("-");
  int month = get_cardinal ();
  skip ("-");
  int mday = get_cardinal ();
  int hour = 0;
  if (peek () == 'T' || peek () == ':')
    {
      get ();
      hour = get_cardinal ();
    }
  if (Time::valid (year, month, mday, hour))
    time = Time (year, month, mday, hour);
  else
    error ("Invalid date");
}

void
LexerData::skip (const char* str)
{ 
  for (const char* p = str; *p; p++)
    if (*p != peek ())
      {
	error (string("Expected '") + str + "'");
	break;
      }
    else
      get ();
}

void
LexerData::skip_line ()
{
  while (good () && peek () != '\n')
    get ();
}

void
LexerData::skip_space ()
{
  while (good () && (peek () == ' ' || peek () == '\t'))
    get ();
}

void
LexerData::skip_hyphens ()
{
  while (good () && peek () == '-')
    get ();
  if (get () != '\n')
    {
      error ("Expected line of hyphens only");
      skip_line ();
      get ();
    }
}

void
LexerData::next_line ()
{
  skip_space ();
  if (peek () != '\n')
    {
      error ("Expected end of line");
      skip_line ();
    }
  get ();

  while (good ())
    {
      skip_space ();
      if (peek () == '#')
	skip_line ();
      else if (peek () != '\n')
	break;
      else
	get ();
    }
}

LexerData::LexerData (const string& name, Treelog& out)
  : Lexer (name, out)
{ }

LexerData::~LexerData ()
{ }
