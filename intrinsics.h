// intrinsics.h -- The build in models of Daisy.
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

#ifndef INTRINSICS_H
#define INTRINSICS_H

#include "memutils.h"
#include "symbol.h"
#include <boost/shared_ptr.hpp>

class Library;
class Declare;
class Frame;
class FrameSubmodel;

class Intrinsics 
{
  // Models.
  typedef std::map<symbol, std::vector<const Declare*>/**/> declare_lib_map_t;
  typedef std::map<symbol, declare_lib_map_t> declare_map_t;
  mutable declare_map_t delayed;
public:
  auto_map<symbol, Library*> all;
  std::map<symbol, Library*> clone () const;
  Library& add (symbol component);
  Library& library (symbol component) const;
  void declare_model (symbol component, symbol model, const Declare&);
  void instantiate (symbol component, symbol model) const;

  // Submodels.
private:
  typedef void (*load_syntax_t) (Frame&);
  typedef std::map<load_syntax_t, boost::shared_ptr<const FrameSubmodel>/**/> submodel_load_frame_t;
  typedef std::map<symbol, load_syntax_t> submodel_name_load_t;
  typedef std::map<load_syntax_t, symbol> submodel_load_name_t;
  typedef std::map<symbol, symbol> submodel_name_desc_t;
  mutable submodel_load_frame_t submodel_load_frame;
  mutable submodel_name_load_t submodel_name_load;
  mutable submodel_load_name_t submodel_load_name;
  mutable submodel_name_desc_t submodel_name_desc;
public:
  void submodel_instantiate (load_syntax_t);
  bool submodel_registered (symbol) const;
  boost::shared_ptr<const FrameSubmodel> submodel_frame (symbol);
  boost::shared_ptr<const FrameSubmodel> submodel_frame (load_syntax_t);
  symbol submodel_name (load_syntax_t);
  symbol submodel_description (symbol) const;
  load_syntax_t submodel_load (symbol) const;
  void submodel_declare (load_syntax_t load_syntax, symbol name, symbol desc);
  void submodel_all (std::vector<symbol>&) const;

  // Intrinsics.
public:
  int count;
  mutable int closed;


  // Create and Destroy.
public:
  Intrinsics ();
  ~Intrinsics ();
};

#endif // INTRINSICS_H
