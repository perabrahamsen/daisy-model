// model.h -- Base class for all model in Daisy.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef MODEL_H
#define MODEL_H

#include "symbol.h"
#include "alist.h"

class Log;

// 'Model' is the base class for all models.
// Inheriting from model is needed in order to be put in libraries.
class Model
{
  // Create and Destroy.
private:
  Model (const Model&);
protected:
  Model ();
public:
  virtual ~Model ();
};

// 'ModelLogable' is the base class for all logable models.
// It has a 'name' parameter for idnetifying it, and 
class ModelLogable : public Model
{
  // Content.
public:
  const symbol name;            // Remember name for logs.
  virtual symbol library_id () const = 0;

  // Use.
public:
  virtual void output (Log& log) const = 0;
  void output_as_derived (symbol, Log&) const;

  // Create and Destroy.
private:
  ModelLogable (const ModelLogable&);
  ModelLogable ();
protected:
  ModelLogable (symbol);
};

// 'ModelAlisted' is the base class for models that needs to keep
// track of its own attribute list.  The includes models that are part
// of a variable length list, or are created ad-hoc.  Other models
// will not need to maintain their own attribute lists, as the
// attribute list of the enclosing frame will be sufficient.
class ModelAListed : public ModelLogable
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

#endif // MODEL_H
