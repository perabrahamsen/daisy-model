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
#include "tmpstream.h"
#include "treelog.h"
#include "path.h"

#include <vector>
#include <iostream>

using namespace std;

struct Lexer::Implementation
{
  istream& in;
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
Lexer::Position::operator== (const Position& pos)
{ return column == pos.column && line == pos.line; }

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

int
Lexer::get ()
{ return impl.get (); }

int
Lexer::peek ()
{ return impl.in.peek (); }

bool
Lexer::good ()
{ return impl.good (); }

void 
Lexer::warning (const string& str, const Position& pos)
{
  TmpStream tmp;
  tmp () << file << ":" << pos.line << ":"
	 << (pos.column + 1) << ": " << str;
  err.entry (tmp.str ());
}

void 
Lexer::error (const string& str, const Position& pos)
{
  error_count++;
  warning (str, pos);
}

void 
Lexer::warning (const string& str)
{
  TmpStream tmp;
  tmp () << file << ":" << impl.line << ":"
	 << (impl.column + 1) << ": " << str;
  err.entry (tmp.str ());
}

void 
Lexer::error (const string& str)
{
  error_count++;
  warning (str);
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
  if (&impl.in == &cin || impl.in.bad ())
    err.entry (string ("Open '") + file + "' failed");
}

Lexer::~Lexer ()
{
  if (impl.in.bad ())
    err.entry (string ("There were trouble parsing '") + file  + "'");
  delete &impl;
}
