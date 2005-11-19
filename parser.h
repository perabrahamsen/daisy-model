// parser.h
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


#ifndef PARSER_H
#define PARSER_H

#include "librarian.h"

class Treelog;

class Parser
{
  // Content.
public:
  const symbol name;
  static const char *const description;

  // Interface.
public:
  virtual void load_nested (AttributeList&) = 0;
  virtual void load (AttributeList&) = 0;
  virtual int error_count () const = 0;
 
  // Create and Destroy.
public:
  virtual void initialize (Syntax&, Treelog&) = 0;
protected:
  explicit Parser (Block&);
public:
  virtual ~Parser ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Parser>::Content* Librarian<Parser>::content;
#endif

static Librarian<Parser> Parser_init ("parser");

#endif // PARSER_H
