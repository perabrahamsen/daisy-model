// lexer_data.h --- Lexical analysis for data files.
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


#ifndef LEXER_DATA_H
#define LEXER_DATA_H

#include "lexer.h"

class Time;

class LexerData : public Lexer
{
  // Lex me!
public:
  string get_word ();
  double get_number ();
  int get_cardinal ();
  void read_date (Time&);
  void skip (const char* str);
  void skip_line ();
  void skip_space ();
  void skip_hyphens ();
  void next_line ();

  // Create and Destroy.
public:
  LexerData (const string& name, Treelog&);
  ~LexerData ();
};

#endif // LEXER_H
