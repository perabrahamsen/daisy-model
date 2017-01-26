// filepos.h --- Position in a file.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2009 Per Abrahamsen and KU.
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


#ifndef FILEPOS_H
#define FILEPOS_H

#include "symbol.h"

// Filepos
class Filepos 
{
  // Content.
private:
  symbol file_;
  int line_;
  int column_;
public:
  symbol filename () const;
  int line () const;
  int column () const;

  // Use.
public:
  bool operator== (const Filepos&) const;
  bool operator!= (const Filepos& pos) const
  { return !(*this == pos); }
  bool operator< (const Filepos&) const;

  // Create and Destroy.
public:
  static const Filepos& none ();
  Filepos (const symbol file, int line, int column);
  Filepos ();
  ~Filepos ();
};

#endif // FILEPOS_H
