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

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

// The 'UIItem' interface. 

class EXPORT UIItem
{
  // Content.
public:
  static const char *const component;
  const symbol name;

  // Create and Destroy.
protected:
  UIItem (const symbol name);
public:
  virtual ~UIItem ();
};

// The 'uifilter' component.

class EXPORT UIFilter : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const component;

  // Utilities.
  static symbol default_filter ();

  // Use.
public:
  virtual symbol default_component_all () const = 0;
  virtual const std::vector<symbol>& find_components_all () const = 0;
  virtual symbol default_model_all (symbol component) const = 0;
  virtual const std::vector<symbol>&
    find_models_all (symbol component) const = 0;
  virtual symbol default_component_editable () const = 0;
  virtual const std::vector<symbol>& find_components_editable () const = 0;
  virtual symbol default_model_editable (symbol component) const = 0;
  virtual const std::vector<symbol>& 
    find_models_editable (symbol component) const = 0;
  virtual const std::vector<const UIItem*>& 
    find_items (symbol component, symbol model) = 0;

  // Create and Destroy.
public:
  virtual void reset (const Metalib& metalib, symbol file) = 0;
protected:
  explicit UIFilter (const BlockModel&);
public:
  virtual ~UIFilter ();
};

#endif // UIFILTER_H
