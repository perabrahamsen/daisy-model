// scope_model.h -- Scope as a model.
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


#ifndef SCOPE_MODEL_H
#define SCOPE_MODEL_H

#include "scope.h"
#include "model.h"
#include <vector>

class BlockModel;

class MScope : public Model, public WScope
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;

  // For selecting with the 'scopesel' model.
private:
  const symbol title_;
public:
  symbol title () const;

  // Create and Destroy.
public:
  explicit MScope (symbol title);
  explicit MScope (const BlockModel&);
  ~MScope ();
};

#endif // SCOPE_MODEL_H
