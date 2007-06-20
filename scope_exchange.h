// scope_exchange.h -- Exchange values with through a scope.
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

#include "model.h"
#include "symbol.h"

class Exchange : public Model
{
  // Content.
public:
  static const char *const description;
  static const char *const component;

  // Use.
public:
  virtual bool is_number () const;
  virtual symbol name () const = 0;
  virtual double number () const;
  virtual bool has_number () const;
  virtual symbol dimension () const;
  virtual bool has_identifier () const;
  virtual symbol identifier () const;
  virtual symbol get_description () const = 0;
  virtual void set_number (const double)

  // Create and Destroy.
public:
  Exchange ();
  ~Exchange ()
};

// scope_exchange.h ends here.
