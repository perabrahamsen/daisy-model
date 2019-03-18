// imvec.h -- Keep track of vectors of inorganic matter.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#ifndef IMVEC_H
#define IMVEC_H

#include "symbol.h"
#include "attribute.h"
#include <map>
#include <vector>
#include <boost/noncopyable.hpp>

class Log;
class BlockModel;
class Unit;
class Frame;

class IMvec : private boost::noncopyable
{
  // Content.
private:
  typedef std::map<symbol, std::vector<double>/**/> map_type;
  map_type content;
  const Unit& unit_;

  // Accessors.
public:
  const Unit& unit () const
  { return unit_; }
  const std::vector<double>& get_array (symbol chem) const;
  std::vector<double>& get_array (symbol chem);
  void set_array (symbol chem, const std::vector<double>& value);
  double get_value (symbol chem, size_t index) const;
  void add_value (symbol chem, size_t index, double value);
  void clear ();
  void multiply (double factor);

  // Iterate.
public:
  struct const_iterator
  {
    map_type::const_iterator i;

    symbol operator* () const
    { return (*i).first; }
    bool operator!= (const_iterator j)
    { return i != j.i; }
    const_iterator operator++(int)
    { const_iterator old = *this; i++; return old; }
    const_iterator operator++()
    { i++; return *this; }
    explicit const_iterator (map_type::const_iterator x)
      : i (x)
    { }
  };
  const_iterator begin () const
  { return const_iterator (content.begin ()); }
  const_iterator end () const
  { return const_iterator (content.end ()); }

  // Operations.
public:
  void output (Log&) const;
  const std::string print () const;

  // Create and Destroy. 
public:
  static void add_syntax (Frame& parent,
			  Attribute::category cat, int size,
			  const symbol dimension);
  explicit IMvec (const BlockModel&, const char*);
  ~IMvec ();
};

#endif // IMVEC_H
