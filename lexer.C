// lexer.C --- Lexical analysis.
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


#include "lexer.h"
#include <sstream>
#include "treelog.h"
#include "path.h"

#include <vector>
#include <iostream>

using namespace std;

struct Lexer::Implementation
{
  std::istream& in;
  int line;
  int column;
  
  int get ();
  bool good ();

  Implementation (const string& name);
  ~Implementation ();
};

int
Lexer::Implementation::get ()
{
  int c = in.get ();

  switch (c)
    {
    case '\n':
      column = 0;
      line++;
      break;
    case '\t':
      column += 8 - column % 8;
      break;
    case '\r':
      // Ignore carriage return for DOS files under Unix.
      return get ();
    default:
      column++;
    }
  return c;
}

bool
Lexer::Implementation::good ()
{
#if 1 
  // BCC and GCC 3.0 requires that you try to read beyond the eof
  // to detect eof.
  char c;
  in.get (c);
  bool ok = in.good ();
  if (ok)
    in.putback (c);
  return ok;
#else
  return in.good ();
#endif
}

Lexer::Implementation::Implementation (const string& name)
  : in (Path::open_file (name)),
    line (1),
    column (0)
{}
  
Lexer::Implementation::~Implementation ()
{
  if (&in != &cin)
    delete &in;
}

bool 
Lexer::Position::operator== (const Position& pos) const
{ return column == pos.column && line == pos.line; }

bool 
Lexer::Position::operator< (const Position& pos) const
{ return line < pos.line || (column < pos.column && line == pos.line); }

const Lexer::Position& 
Lexer::Position::operator= (const Position& pos)
{ line = pos.line; column = pos.column; return *this; }

Lexer::Position::Position (const Position& pos)
{ *this = pos; }

Lexer::Position::Position (int l, int c)
  : line (l),
    column (c)
{ }
  
Lexer::Position::Position ()
  : line (-42),
    column (-42)
{ }
   
Lexer::Position::~Position ()
{ }

Lexer::Position 
Lexer::position ()
{ return Position (impl.line, impl.column); }

const Lexer::Position& 
Lexer::no_position ()
{ 
  static Position none;
  return none;
}

void Lexer::seek (const Lexer::Position& pos)
{
  if (pos == position ())
    return;

  impl.in.seekg (0, ios::beg);
  impl.column = 0;
  impl.line = 1;
  while (good () && position () < pos)
    (void) get ();
}


int
Lexer::get ()
{ return impl.get (); }

int
Lexer::peek ()
{ 
  const int c = impl.in.peek ();
  if (c != '\r')
    return c;
  
  // Skip DOS carriage return on Unix.
  (void) impl.in.get ();
  return peek ();
}

bool
Lexer::good ()
{ return impl.good (); }

void 
Lexer::warning (const string& str, const Position& pos)
{
  std::ostringstream tmp;
  tmp << file << ":" << pos.line << ":"
	 << (pos.column + 1) << ": " << str;
  err.warning (tmp.str ());
}

void 
Lexer::error (const string& str, const Position& pos)
{
  std::ostringstream tmp;
  tmp << file << ":" << pos.line << ":"
	 << (pos.column + 1) << ": " << str;
  err.error (tmp.str ());
  error_count++;
}

void 
Lexer::debug (const string& str)
{
  std::ostringstream tmp;
  tmp << file << ":" << impl.line << ":"
	 << (impl.column + 1) << ": " << str;
  err.debug (tmp.str ());
}

void 
Lexer::warning (const string& str)
{
  std::ostringstream tmp;
  tmp << file << ":" << impl.line << ":"
	 << (impl.column + 1) << ": " << str;
  err.warning (tmp.str ());
}

void 
Lexer::error (const string& str)
{
  std::ostringstream tmp;
  tmp << file << ":" << impl.line << ":"
	 << (impl.column + 1) << ": " << str;
  err.error (tmp.str ());
  error_count++;
}

void
Lexer::eof ()
{ 
  if (!impl.in.eof ())
    error ("Expected end of file");
}
    
Lexer::Lexer (const string& name, Treelog& out)
  : impl (*new Implementation (name)),
    err (out),
    file (name),
    error_count (0)
{  
  if (&impl.in == &cin || !impl.in.good ())
    err.entry (string ("Open '") + file + "' failed");
}

Lexer::~Lexer ()
{
  if (impl.in.bad ())
    err.entry (string ("There were trouble parsing '") + file  + "'");
  delete &impl;
}
