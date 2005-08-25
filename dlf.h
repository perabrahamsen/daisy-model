// dlf.h -- Printing Daisy Log File headers.
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


#ifndef DLF_H
#define DLF_H

#include <string>

class DLF
{
public:
  enum type { 
    None,                       // No dlf header used.
    Full,                       // Futypell dlf header.
    Terse                       // Fixed size dlf header.
  } value;
  operator type& ()
  { return value; }
  DLF& operator= (const DLF& other)
  { value = other.value; return *this; }
  type& operator= (const type v)
  { value = v; return *this; }
  DLF (type v)
    : value (v)
  { }
  static type string2type (const std::string& s);
  DLF (const std::string& name)
    : value (string2type (name))
  { }
}; 

#endif // DLF_H
