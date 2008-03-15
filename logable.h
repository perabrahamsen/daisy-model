// logable.h -- Base class for all logable model in Daisy.
// 
// Copyright 2008 Per Abrahamsen and KVL.
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

#include "model.h"
#include "symbol.h"
#include "alist.h"

class Log;

#ifndef LOGABLE_H
#define LOGABLE_H

class ModelNamed : public Model
{
  // Content.
public:
  const symbol name;            // Remember name for logs.
  virtual symbol library_id () const = 0;

  // Use.
public:
  virtual void output (Log& log) const = 0;

  // Create and Destroy.
private:
  ModelNamed (const ModelNamed&);
  ModelNamed ();
protected:
  ModelNamed (symbol);
};

class ModelAListed : public ModelNamed
{
  // Content.
public:
  const AttributeList alist;	// Remember attributes for checkpoint.

  // Use.
public:
  void output_as_object (symbol, Log&) const;
  void output_as_entry (Log&) const;
  
  // Create and Destroy.
private:
  ModelAListed (const ModelAListed&);
  ModelAListed ();
protected:
  ModelAListed (const AttributeList&);
  ModelAListed (symbol);
};

#endif // LOGABLE_H
