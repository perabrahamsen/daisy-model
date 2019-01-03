// cstage.h --- Alternative crop stage definitions.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyrigth 2018 KU.
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


#ifndef CSTAGE_H
#define CSTAGE_H

#include "model_derived.h"
#include "symbol.h"

class Treelog;
class BlockModel;

class CStage : public ModelDerived
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // Simulation.
public:
  virtual void tick (const double DS, Treelog& msg) = 0;
  virtual double stage () const = 0;

  // Create and Destroy.
protected:
  CStage (const BlockModel&);
public:
  ~CStage ();
};

#endif // CSTAGE_H
