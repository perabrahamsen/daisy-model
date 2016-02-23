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

#include "symbol.h"
#include <string>
#include <vector>

class Volume;
class Metalib;
class FrameModel;
class Frame;

class DLF
{
  // Content.
private:
  enum type { 
    None,                       // No dlf header used.
    Full,                       // Futypell dlf header.
    Terse                       // Fixed size dlf header.
  } value;

  // Simulation.
public:
  void start (std::ostream& out, const symbol name,
              const symbol file, const symbol parsed_from_file) const;
  void parameter (std::ostream& out,
                  const symbol name, const symbol value) const;
  void parameter (std::ostream& out,
                  const symbol name, const double value,
		  const symbol dim) const;
  void interval (std::ostream& out, const Volume&) const;
  void log_description (std::ostream& out, symbol description) const;
  void finish (std::ostream& out, const Metalib&, const FrameModel&);
  void finish (std::ostream& out);

  // Create and destroy.
private:
  static type string2type (const symbol s);
public:
  static void add_syntax (Frame&, symbol key);
  explicit DLF (const symbol name)
    : value (string2type (name))
  { }
}; 

#endif // DLF_H
