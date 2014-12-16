// deposition.h --- Deposition of inorganic material from atmosphere.
// 
// Copyright 2013 KU.
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

#ifndef DEPOSITION_H
#define DEPOSITION_H

#include "model_derived.h"
#include "symbol.h"
#include "im.h"

class Vegetation;
class Weather;
class Treelog;
class BlockModel;

// The 'deposition' component.

class Deposition : public ModelDerived
{
  // Content.
public:
  const symbol name;
  symbol library_id () const;
  static const char *const component;

  IM my_deposit;                // [g [stuff] /cm²/h]
  
  // Use.
public:
  virtual void tick (const Vegetation&, const Weather&, Treelog& msg) = 0;
  const IM& deposit () const;
  void output (Log&) const;

  // Create and Destroy.
protected:
  explicit Deposition (const BlockModel&);
public:
  virtual ~Deposition ();
};

#endif // DEPOSITION_H
