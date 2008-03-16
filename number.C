// number.C --- Numbers in Daisy.
// 
// Copyright 2004 Per Abrahamsen and KVL.
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

#define BUILD_DLL

#include "number.h"
#include "block.h"
#include "librarian.h"
#include "units.h"
#include <sstream>

const char *const Number::component = "number";

symbol
Number::library_id () const
{
  static const symbol id (component);
  return id;
}

symbol
Number::title () const
{ return name; }

bool 
Number::known (symbol dim)
{ return dim != Syntax::unknown (); }

bool 
Number::tick_value (double& value, symbol want, const Scope& scope, 
		    Treelog& msg)
{ 
  this->tick (scope, msg);
  if (this->missing (scope))
    {
      // msg.warning ("Expression '" + name + "' is missing in scope");
      return false;
    }

  value = this->value (scope);
  const symbol has = this->dimension (scope);
      
  if (!Units::can_convert (has, want, value))
    {
      std::ostringstream tmp;
      tmp << "Cannot convert " << value << " [" << has
	  << "] to [" << want << "]";
      msg.warning (tmp.str ());
    }
  else
    value = Units::convert (has, want, value);
  
  return true;
}

bool 
Number::check_dim (const Scope& scope, const symbol want, Treelog& msg) const
{
  if (!this->check (scope, msg))
    return false;

  const symbol has = this->dimension (scope);
  if (Units::can_convert (has, want))
    return true;

  msg.error ("Cannot convert [" + has + "] to [" + want + "]");
  return false;
}

Number::Number (Block& al)
  : name (al.identifier ("type"))
{ }

Number::~Number ()
{ }

static Librarian Number_init (Number::component, "\
Generic representation of numbers.");
