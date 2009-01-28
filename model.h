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
#include <memory>
#include <boost/noncopyable.hpp>

class Log;
class Frame;
class FrameModel;
class Block;

// 'Model' is the base class for all models.
// Inheriting from model is needed in order to be put in libraries.
class Model : private boost::noncopyable
{
  // Create and Destroy.
public:
  static void load_model (Frame&);
protected:
  Model ();
public:
  virtual ~Model ();
};

// 'ModelLogable' is the base class for all logable models.
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
  ModelLogable ();
protected:
  ModelLogable (symbol);
};

// 'ModelFrame' is the base class for models that needs to keep
// track of its own attributes.  The includes models that are part
// of a variable length list, or are created ad-hoc.  Other models
// will not need to maintain their own attribute lists, as the
// attribute list of the enclosing frame will be sufficient.
class ModelFramed : public ModelLogable
{
  // Content.
public:
  std::auto_ptr<FrameModel> frame; // Remember attributes for checkpoint.

  // Use.
public:
  void output_as_object (symbol, Log&) const;
  void output_as_entry (Log&) const;
  
  // Create and Destroy.
protected:
  ModelFramed (Block&);
  ModelFramed (symbol);
  ~ModelFramed ();
};

#endif // MODEL_H
