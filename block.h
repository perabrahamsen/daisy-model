// block.h -- Support for block scoped variables.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#ifndef BLOCK_H
#define BLOCK_H

#include "scope.h"
#include "symbol.h"
#include "attribute.h"
#include <string>
#include <vector>
#include <set>
#include <stack>
#include <boost/shared_ptr.hpp>

class Treelog;
class Metalib;
class Path;
class Units;
class Frame;
class FrameModel;
class FrameSubmodel;
class PLF;

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif


class EXPORT Block : public Scope 
{
public:
  virtual const Metalib& metalib () const = 0;
  const Units& units () const;
  Path& path () const;
  virtual const Frame& frame () const = 0;
  virtual Treelog& msg () const = 0;
  symbol type_name () const;

  // Variables.
private:
  symbol expand_string (symbol) const;
  symbol expand_string (const symbol value_s, std::set<symbol>& outer) const;
  symbol expand_reference (const symbol key) const;

  // Error handling.
private:
  mutable bool is_ok;
public:  
  void error (const std::string&) const;
  bool ok () const;
  virtual void set_error () const;

  // Nested scope handling.
public:
  virtual const Frame& find_frame (const symbol key) const = 0;

  // Syntax emulation.
public:
  void entries (std::set<symbol>&) const;
  Attribute::type lookup (symbol) const;
  bool can_extract_as (symbol, Attribute::type) const;
  int type_size (symbol tag) const;
  symbol dimension (symbol) const;
  symbol description (symbol) const;

  // Submodel emulation.
public:
  bool check (const symbol key) const;
  int value_size (symbol tag) const;
  double number (symbol) const;
  double number (symbol, double default_value) const;
  using Scope::name;
  symbol name (symbol) const;
  symbol name (symbol, symbol default_value) const;
  bool flag (symbol) const;
  bool flag (symbol, bool default_value) const;
  const PLF& plf (symbol) const;
  boost::shared_ptr<const PLF> plf_ptr (symbol) const;
  const FrameModel& model (symbol) const;
  boost::shared_ptr<const FrameModel> model_ptr (symbol) const;
  const FrameSubmodel& submodel (symbol) const;
  boost::shared_ptr<const FrameSubmodel> submodel_ptr (symbol) const;
  int integer (symbol) const;
  int integer (symbol, int default_value) const;
  const std::vector<double>& number_sequence (symbol) const;
  const std::vector<symbol> name_sequence (symbol key) const;
  const std::vector<bool>& flag_sequence (symbol key) const;
  const std::vector<int>& integer_sequence (symbol key) const;
  const std::vector<boost::shared_ptr<const FrameModel>/**/>& model_sequence (symbol key) const;
  const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>&
    submodel_sequence (symbol key) const;
  const std::vector<boost::shared_ptr<const PLF>/**/>& 
    plf_sequence (symbol key) const;

  // Create and Destroy.
protected:
  static symbol sequence_id (symbol key, size_t index);
  Block ();
public:
  ~Block ();
};

#endif // BLOCK_H
