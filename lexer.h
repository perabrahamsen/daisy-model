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

#include <string>
#include <memory>
#include <iosfwd>

class Treelog;

class Lexer
{
  // Content.
private:
  std::istream& in;
  int line;
  int column;
public:
  Treelog& err;
  const std::string file;
  int error_count;

  // Position
  class Position
  {
    // Content.
  private:
    int line;
    int column;

    // Use.
  public:
    bool operator== (const Position&) const;
    bool operator!= (const Position& pos) const
    { return !(*this == pos); }
    bool operator< (const Position&) const;

    // Create and Destroy.
    friend class Lexer;
  public:
    const Position& operator= (const Position&);
    Position (const Position&);
    Position (int line, int column);
    Position ();
    ~Position ();
  };
  Position position ();
  static const Position& no_position ();
  void seek (const Position&);

  // Operations.
public:
  int get ();
  int peek ();
  bool good ();
  void warning (const std::string& str, const Position&);
  void error (const std::string& str, const Position&);
  void debug (const std::string& str);
  void warning (const std::string& str);
  void error (const std::string& str);
  void eof ();

  // Create and destroy.
public:
  Lexer (const std::string& name, std::istream&, Treelog&);
  virtual ~Lexer ();
};

#endif // LEXER_H
