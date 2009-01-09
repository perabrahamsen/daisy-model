// declare.h -- Manage components and models.
// 
// Copyright 2009 Per Abrahamsen and KVL.
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


#ifndef DECLARE_H
#define DECLARE_H

#include "symbol.h"

#include <boost/noncopyable.hpp>

class Model;
class Frame;
class Block;

struct Declare : private boost::noncopyable
{
  const symbol component;
  const symbol super;
  const symbol description;

  void load (Frame& frame) const;
protected:
  virtual void load_frame (Frame&) const = 0;
public:

  Declare (symbol component, symbol name, symbol super, symbol description);
  Declare (symbol component, symbol name, symbol description);
  virtual ~Declare ();
};

struct DeclareModel : public Declare
{
  virtual Model* make (Block&) const = 0;
  DeclareModel (symbol component, symbol name, symbol super, 
                symbol description);
  DeclareModel (symbol component, symbol name, symbol description);
  ~DeclareModel ();
};

#endif // DECLARE_H
