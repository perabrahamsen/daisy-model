// select_value.C --- Select a state variable.
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


#include "select_value.h"
#include "mathlib.h"

void 
SelectValue::add_result (double result)
{
    if (count == 0)
      {
        if (handle == Handle::geometric)
          value = log (result);
        else
          value = result;
      }
    else switch (handle)
      {
      case Handle::min:
        value = std::min (value, result);
        break;
      case Handle::max:
        value = std::max (value, result);
        break;
      case Handle::current:    
        // We may have count > 0 && Handle::current when selecting
        // multiple items with "*", e.g. multiple SOM pools.  
        // In that case, we use the sum.
      case Handle::average:
      case Handle::sum:
        value += result;
        break;
      case Handle::geometric:
        value += log (result);
        break;
      }
    count++;
}


// Print result at end of time step.
void 
SelectValue::done ()
{
  if (count == 0)
    dest.missing ();
  else 
    {
      double result = value;
      switch (handle)
        {
        case Handle::average:
          result /= (count + 0.0);
          break;
        case Handle::geometric:
          result /= (count + 0.0);
          result = exp (result);
          break;
	case Handle::min:
	case Handle::max:
	case Handle::sum:
	case Handle::current:
	  break;
        }
      dest.add (convert (result));
    }
  if (!accumulate)
    count = 0;
}

void 
SelectValue::load_syntax (Syntax& syntax, AttributeList& alist)
{
  Select::load_syntax (syntax, alist);
  syntax.add ("value", Syntax::Unknown (), Syntax::State,
	      "The current accumulated value.");
  alist.add ("value", 0.0);
}

// Create and Destroy.
SelectValue::SelectValue (const AttributeList& al)
  : Select (al),
    value (al.number ("value"))
{ }
