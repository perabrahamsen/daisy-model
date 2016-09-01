// select_value.C --- Select a state variable.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2010 KU
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

#include "select_value.h"
#include "block_model.h"
#include "frame.h"
#include "librarian.h"
#include "check.h"
#include <cmath>

void 
SelectValue::add_result (double result)
{
  switch (multi)
    {
    case Multi::min:
      if (first_result)
        small_value = result;
      else
        small_value = std::min (small_value, result);
      break;
    case Multi::max:
      if (first_result)
        small_value = result;
      else
        small_value = std::max (small_value, result);
      break;
    case Multi::sum:
      small_value += result * relative_weight;
      break;
    }
  first_result = false;
}

void 
SelectValue::done_initial ()
{
  if (first_result)
    return;

  switch (handle)
    {
    case Handle::average:
    case Handle::sum:
      break;
    case Handle::content_sum:
      value += small_value;
      first_small = false;
      break;
    case Handle::min:
      if (first_small)
        value = small_value;
      else
        {
          value = std::min (value, small_value);
          first_small = false;
        }
      break;
    case Handle::max:
      if (first_small)
        value = small_value;
      else
        {
          value = std::max (value, small_value);
          first_small = false;
        }
       break;
     case Handle::current:
       value = small_value;
       first_small = false;
       break;
     }

  small_value = 0.0;
  first_result = true;
 }

void 
SelectValue::done_small (const double ddt)
{
  dt += ddt;
  if (first_result)
    return;

  switch (handle)
    {
    case Handle::average:
    case Handle::sum:
      value += small_value * ddt;
      break;
    case Handle::content_sum:
      value += small_value;
      first_small = false;
      break;
    case Handle::min:
      if (first_small)
        value = small_value;
      else
        value = std::min (value, small_value);
      break;
    case Handle::max:
      if (first_small)
        value = small_value;
      else
        value = std::max (value, small_value);
      break;
     case Handle::current:
       value = small_value;
       break;
     }

  small_value = 0.0;
  first_result = true;
  first_small = false;
 }

 void 
 SelectValue::done_print ()
 {
   // Missing value.
   if (first_small)
     {
       dest.missing ();
       return;
     }

   // Use it.
   double result = value;
   switch (handle)
     {
     case Handle::average:
       if (dt > 0.0)
         result /= dt;
       else
         {
           dest.missing ();
           return;
         }
       break;
     case Handle::sum:
     case Handle::content_sum:
     case Handle::min:
     case Handle::max:
     case Handle::current:
       break;
     }
   dest.add (convert (result));
   
   // Clear for next.
   if (!accumulate)
     {
       dt = 0.0;
       value = 0.0;
       first_small = true;
    }
 }

// Create and Destroy.
SelectValue::SelectValue (const BlockModel& al)
  : Select (al),
    small_value (0.0),
    value (0.0)
{ }

static struct SelectValueSyntax : public DeclareBase
{
  SelectValueSyntax ()
    : DeclareBase (Select::component, "value", "\
Log a single numeric value.")
  { }
  void load_frame (Frame& frame) const
  { }
} SelectValue_syntax;

// select_value.C ends here.
