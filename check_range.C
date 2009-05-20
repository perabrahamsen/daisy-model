// check_range.C -- Check range of numeric alist members.
// 
// Copyright 2001 Per Abrahamsen and KVL.
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

#include "check_range.h"
#include "treelog.h"
#include <sstream>

bool
RangeII::verify (const double value, Treelog& msg) const
{
  if (value < lower || value > upper)
    {
      std::ostringstream str;
      str << "Value not in interval [" << lower << "; " << upper << "]";
      msg.error (str.str ());
      return false;
    }
    return true;
}

RangeII::RangeII (const double l, const double u)
  : lower (l),
    upper (u)
{ }

bool
RangeIE::verify (const double value, Treelog& msg) const
{
  if (value < lower || value >= upper)
    {
      std::ostringstream str;
      str << "Value not in interval [" << lower << "; " << upper << "[";
      msg.error (str.str ());
      return false;
    }
    return true;
}

RangeIE::RangeIE (const double l, const double u)
  : lower (l),
    upper (u)
{ }

bool
RangeEI::verify (const double value, Treelog& msg) const
{
  if (value <= lower || value > upper)
    {
      std::ostringstream str;
      str << "Value not in interval ]" << lower << "; " << upper << "]";
      msg.error (str.str ());
      return false;
    }
    return true;
}

RangeEI::RangeEI (const double l, const double u)
  : lower (l),
    upper (u)
{ }

bool
RangeEE::verify (const double value, Treelog& msg) const
{
  if (value <= lower || value >= upper)
    {
      std::ostringstream str;
      str << "Value not in interval ]" << lower << "; " << upper << "[";
      msg.error (str.str ());
      return false;
    }
    return true;
}

RangeEE::RangeEE (const double l, const double u)
  : lower (l),
    upper (u)
{ }

bool
Below::verify (const double value, Treelog& msg) const
{
  if (value >= upper)
    {
      std::ostringstream str;
      str << "Value should be < " << upper;
      msg.error (str.str ());
      return false;
    }
    return true;
}

Below::Below (const double u)
  : upper (u)
{ }

bool
BelowOrEqual::verify (const double value, Treelog& msg) const
{
  if (value > upper)
    {
      std::ostringstream str;
      str << "Value should be <= " << upper;
      msg.error (str.str ());
      return false;
    }
    return true;
}

BelowOrEqual::BelowOrEqual (const double u)
  : upper (u)
{ }

bool
Above::verify (const double value, Treelog& msg) const
{
  if (value <= lower)
    {
      std::ostringstream str;
      str << "Value should be > " << lower;
      msg.error (str.str ());
      return false;
    }
    return true;
}

Above::Above (const double l)
  : lower (l)
{ }

bool
AboveOrEqual::verify (const double value, Treelog& msg) const
{
  if (value < lower)
    {
      std::ostringstream str;
      str << "Value should be >= " << lower;
      msg.error (str.str ());
      return false;
    }
    return true;
}

AboveOrEqual::AboveOrEqual (const double l)
  : lower (l)
{ }

const AboveOrEqual Temperature (-273.15);

// check_range.C ends here.
