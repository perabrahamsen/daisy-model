// model_framed.h -- Base class for framed models in Daisy.
// 
// Copyright 2007, 2009 Per Abrahamsen and KVL.
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
//
// 'ModelFrame' is the base class for models that needs to keep
// track of its own attributes.  The includes models that are part
// of a variable length list, or are created ad-hoc.  Other models
// will not need to maintain their own attribute lists, as the
// attribute list of the enclosing frame will be sufficient.

#ifndef MODEL_FRAMED_H
#define MODEL_FRAMED_H

#include "model_logable.h"
#include <memory>

class FrameModel;
class BlockModel;

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class ModelFramed : public ModelLogable
{
  // Content.
private:
  std::unique_ptr<const FrameModel> my_frame; // Remember for checkpoint.
public:
  EXPORT virtual const FrameModel& frame () const;

  // Use.
public:
  void output_as_object (symbol, Log&) const;
  void output_as_entry (Log&) const;
  
  // Create and Destroy.
protected:
  ModelFramed (const BlockModel&);
  ModelFramed (symbol);
  ~ModelFramed ();
};

#endif // MODEL_FRAMED_H
