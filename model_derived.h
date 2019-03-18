// model_derived.h -- Base class for derived models in Daisy.
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
// Derived models which rely on enclsing scope for frame.

#ifndef MODEL_DERIVED_H
#define MODEL_DERIVED_H

#include "model_logable.h"

class ModelDerived : public ModelLogable
{
  // Use.
public:
  void output_as_derived (symbol, Log&) const;

  // Create and Destroy.
private:
  ModelDerived ();
protected:
  ModelDerived (symbol);
};

#endif // MODEL_DERIVED_H
