// format.h -- Text formatting component.
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


#ifndef FORMAT_H
#define FORMAT_H

#include "librarian.h"
#include <iosfwd>

class Format
{
  // Content.
public:
  const symbol name;
  static const char *const description;
private:
  std::ostream* output;
protected:
  std::ostream& out ();

  // Use.

  // Create and Destroy.
public:
  void initialize (std::ostream&);
protected:
  Format (const AttributeList& al);
public:
  virtual ~Format ();
};

#ifdef FORWARD_TEMPLATES
EMPTY_TEMPLATE
Librarian<Format>::Content* Librarian<Format>::content;
#endif

static Librarian<Format> Format_init ("format");

#endif // FORMAT_H
