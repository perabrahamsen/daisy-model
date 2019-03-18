// filepos.C --- Position in a file.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2009 Per Abrahamsen and KVL.
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

#define BUILD_DLL

#include "filepos.h"
#include "attribute.h"

symbol
Filepos::filename () const
{ return file_; }

int 
Filepos::line () const
{ return line_; }

int 
Filepos::column () const
{ return column_; }

bool 
Filepos::operator== (const Filepos& pos) const
{ return file_ == pos.file_ && column_ == pos.column_ && line_ == pos.line_; }

bool 
Filepos::operator< (const Filepos& pos) const
{ return file_ == pos.file_ 
    && (line_ < pos.line_ || (column_ < pos.column_ && line_ == pos.line_)); }

const Filepos& 
Filepos::none ()
{ 
  static Filepos none;
  return none;
}

Filepos::Filepos (const symbol f, int l, int c)
  : file_ (f),
    line_ (l),
    column_ (c)
{ }
  
Filepos::Filepos ()
  : file_ (Attribute::None ()),
    line_ (-42),
    column_ (-42)
{ }
   
Filepos::~Filepos ()
{ }

// filepos.C ends here.
