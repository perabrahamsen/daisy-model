// treelog_dual.h -- Log hierarchical information in two ostreams.
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


#ifndef TREELOG_DUAL_H
#define TREELOG_DUAL_H

#include "treelog.h"
#include <memory>

#if defined (__BORLANDC__) && __BORLANDC__ < 0x0550
#include <iostream>
namespace std { typedef ostream ostream; }
#else
#include <iosfwd>
#endif

class TreelogDual : public Treelog
{
  // Content.
private:
  struct Implementation;
  std::auto_ptr<Implementation> impl;

  // Nesting.
public:
  void open (const std::string& name);
  void close ();

  // Use.
public:
  void debug (const std::string&);
  void entry (const std::string&);
  void lazy (const std::string&);
  void touch ();
  void flush ();

  // Create and Destroy.
public:
  TreelogDual (const std::string& file, std::ostream&);
  ~TreelogDual ();
};

#endif // TREELOG_DUAL_H
