// alist.h -- attribute list
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#ifndef ALIST_H
#define ALIST_H

#include "symbol.h"
#include <vector>

class PLF;
class Metalib;
class FrameSubmodel;
class FrameModel;

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT AttributeList
{
  // Content.
  struct Implementation;
  Implementation& impl;

public:
  // Is 'key' an element of this alist?
  bool check (const symbol key) const;
  // Is the element 'key' in this alist a subset of the other entry.
  bool subset (const Metalib&, const AttributeList& other, symbol key) const;
  int size (symbol key) const;

  // References.
  void set_reference (symbol key, symbol val);
  bool is_reference (symbol key) const;
  symbol get_reference (symbol key) const;

  // Extract values.
  double number (symbol) const;
  double number (symbol, double default_value) const;
  symbol name (symbol) const;
  symbol name (symbol, symbol default_value) const;
  bool flag (symbol) const;
  bool flag (symbol, bool default_value) const;
  const PLF& plf (symbol) const;
  const FrameModel& model (symbol) const;
  const FrameSubmodel& submodel (symbol) const;
  int integer (symbol) const;
  int integer (symbol, int default_value) const;
  const std::vector<double>& number_sequence (symbol) const;
  const std::vector<symbol>& name_sequence (symbol key) const;
  const std::vector<bool>& flag_sequence (symbol key) const;
  const std::vector<int>& integer_sequence (symbol key) const;
  const std::vector<const PLF*>& plf_sequence (symbol key) const;
  const std::vector<const FrameModel*>& model_sequence (symbol key) const;
  const std::vector<const FrameSubmodel*>& submodel_sequence (symbol key) const;

  // Create and Destroy.
  void set (symbol, double);
  void set (symbol, double, symbol);
  void set (symbol, symbol);
  void set (symbol key, const char *const value)
  // This one is needed to avoid calling the bool version.
  { set (key, symbol (value)); }
  void set (symbol, bool);
  void set (symbol, int);
  void set (symbol, const FrameModel&);
  void set (symbol, const FrameSubmodel&);
  void set (symbol, const PLF&);
  void set (symbol, const std::vector<double>&);
  void set (symbol, const std::vector<symbol>&);
  void set_strings (symbol key);
  void set_strings (symbol key, symbol a);
  void set_strings (symbol key,
                    symbol a, symbol b);
  void set_strings (symbol key,
                    symbol a, symbol b,
                    symbol c);
  void set (symbol, const std::vector<bool>&);
  void set (symbol, const std::vector<int>&);
  void set (symbol, const std::vector<const FrameModel*>&);
  void set (symbol, const std::vector<const FrameSubmodel*>&);
  void set (symbol, const std::vector<const PLF*>&);

  void remove (symbol);
  void operator += (const AttributeList&);
  void operator = (const AttributeList&);
  void clear ();
  AttributeList (const AttributeList& old);
  AttributeList ();
  ~AttributeList ();
};

#endif // ALIST_H

// alist.h ends here
