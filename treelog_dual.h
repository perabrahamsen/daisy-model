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
#include "common.h"

class TreelogDual : public Treelog
{
  // Content.
private:
  struct Implementation;
  Implementation& impl;

  // Nesting.
public:
  void open (const string& name);
  void close ();

  // Use.
public:
  void entry (const string&);
  
  // Create and Destroy.
public:
  TreelogDual (const string& file, ostream&);
  ~TreelogDual ();
};

#endif // TREELOG_DUAL_H
