// unit_model.h -- The 'unit' component.
// 
// Copyright 2007, 2008 Per Abrahamsen and KVL.
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

#ifndef UNIT_MODEL_H
#define UNIT_MODEL_H

#include "unit.h"
#include "model.h"

class BlockModel;

// Component 'unit'.

class MUnit : public Model, public Unit
{
  // Identity.
public:
  const symbol name;
  static const char *const component;
  symbol library_id () const;
private:
  const symbol base_name_;
public:

  // Unit interface.
public:
  symbol base_name () const
  { return base_name_; }
  symbol native_name () const
  { return name; }

  // Convert.
public:
  const Convert* create_convertion (const Unit& to) const;

  // Create and Destroy.
protected:
  MUnit (const BlockModel& al, symbol base);
public:
  virtual ~MUnit ();
};

#endif // UNIT_MODEL_H

