// uifilter.h -- Abstract user interface details.
// 
// Copyright 2012 KU.
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

#ifndef UIFILTER_H
#define UIFILTER_H

#include "model.h"
#include "symbol.h"
#include <vector>

class BlockModel;
class Metalib;

class UIFilter : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const component;

  // Utilities.
  static symbol default_filter ();

  // Use.
public:
  virtual symbol default_component (const Metalib& metalib, 
                                    const symbol file) const;
  virtual void find_components (const Metalib& metalib, const symbol file,
                                std::vector<symbol>& components) const;
  virtual symbol default_model (const Metalib& metalib, const symbol file,
                                symbol component) const;
  virtual void find_models (const Metalib& metalib, const symbol file,
                            symbol component, 
                            std::vector<symbol>& models) const;
  
  // Create and Destroy.
protected:
  explicit UIFilter (const BlockModel&);
public:
  virtual ~UIFilter ();
};


#endif // UIFILTER_H
