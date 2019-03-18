// frame_model.h -- Model and parameterizations.
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

#ifndef FRAME_MODEL_H
#define FRAME_MODEL_H

#include "frame.h"

class Model;
class Block;

class FrameModel : public Frame
{
  // Inheritance.
private:
  mutable const FrameModel* parent_;
public:
  const FrameModel* parent () const;
private:
  void replace_parent (const Frame* new_parent) const;

  // Use.
public:
  using Frame::subset;
  bool subset (const Metalib&, const FrameModel& other) const;
private:
  using Frame::component;
  virtual symbol component () const;

  // Construct.
public:
  virtual bool buildable () const;
protected:
  virtual Model* construct (const Block&, const symbol, const FrameModel&) const;
public:
  Model* construct (const Block& context, const symbol key) const;

  // Create and Destroy.
private:
  FrameModel ();
public:
  static const FrameModel& root ();
  FrameModel (const FrameModel&, parent_link_t);
  FrameModel (const FrameModel&, parent_clone_t);
  FrameModel& clone () const;
  ~FrameModel ();
};

#endif // FRAME_MODEL_H
