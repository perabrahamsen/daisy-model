// lexer.h --- Lexical analysis
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


#ifndef LEXER_H
#define LEXER_H

#include "common.h"

#if defined (__BORLANDC__) && __BORLANDC__ < 0x0550
struct istream;
#else
#include <iosfwd>
#endif

using namespace std;

#if defined (__BORLANDC__) && __BORLANDC__ == 0x0550
#define istread std::istream
#endif

class Treelog;

class Lexer
{
  // State.
private:
  static istream& open_file (const string& name);
  istream& in;
public:
  Treelog& err;
private:
  int line;
  int column;
public:
  const string file;
  int error_count;

  // Operations.
public:
  int get ();
  int peek ();
  bool good ();
  void error (const string& str);
  void eof ();

  // Create and destroy.
public:
  Lexer (const string&, Treelog&);
  ~Lexer ();
};

#endif // LEXER_H
