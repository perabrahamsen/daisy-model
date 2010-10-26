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

#include "symbol.h"
#include "filepos.h"
#include <string>
#include <memory>
#include <iosfwd>
#include <boost/noncopyable.hpp>

class Treelog;

class Lexer : private boost::noncopyable
{
  // Content.
private:
  std::istream& in;
  int line;
  int column;
public:
  Treelog& err;
  const symbol file;
private:
  int error_count;
public:
  int get_error_count () const;
  void add_errors (int);
  Filepos position ();
  void seek (const Filepos&);

  // Operations.
public:
  int get ();
  int peek ();
  bool good ();
  void warning (const std::string& str, const Filepos&);
  void error (const std::string& str, const Filepos&);
  void debug (const std::string& str);
  void warning (const std::string& str);
  void error (const std::string& str);
  void eof ();

  // Create and destroy.
public:
  Lexer (symbol name, std::istream&, Treelog&);
  virtual ~Lexer ();
};

#endif // LEXER_H
