// parser_file.h
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


#ifndef PARSER_FILE_H
#define PARSER_FILE_H

#include "parser.h"

class ParserFile : public Parser
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;

  // Use.
public:
  void load_nested (AttributeList&);
  void load (AttributeList&);
  int error_count () const;

  // Create and Destroy.
public:
  void initialize (const Syntax&, Treelog&);
  ParserFile (const AttributeList&);
  ParserFile (const Syntax& syntax, const string& n, Treelog&);
  ~ParserFile ();
};

#endif // PARSER_FILE_H
