// condition.h -- Logic expressions
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


#ifndef CONDITION_H
#define CONDITION_H

#include "model.h"
#include "symbol.h"
#include "alist.h"

class Daisy;
class Log;
class Block;
class Treelog;
class Scope;

class Condition : public Model
{  
  // Content.
public:
  const symbol name;
  const AttributeList alist;
  static const char *const component;
  virtual const std::string timestep ();

  // Simulation.
public:
  virtual void tick (const Daisy&, const Scope&, Treelog&) = 0;
  virtual void initiate_log (const Daisy& daisy);
  virtual bool match (const Daisy&, const Scope&, Treelog&) const = 0;
  virtual void output (Log&) const = 0;

  // Create & Destroy.
public:
  virtual void initialize (const Daisy&, const Scope&, Treelog&) = 0;
  virtual bool check (const Daisy&, const Scope&, Treelog&) const = 0;
  static std::auto_ptr<Condition> create_true ();
protected:
  Condition (Block& al);
  Condition (const char* id);
public:
  ~Condition ();
};

#endif // CONDITION_H
