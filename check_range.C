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


#include "check_range.h"
#include <sstream>

void
RangeII::check (const double value) const throw (std::string)
{
  if (value < lower || value > upper)
    {
      std::ostringstream str;
      str << "Value not in interval [" << lower << "; " << upper << "]";
      throw std::string (str.str ());
    }
}

RangeII::RangeII (const double l, const double u)
  : lower (l),
    upper (u)
{ }

void
RangeIE::check (const double value) const throw (std::string)
{
  if (value < lower || value >= upper)
    {
      std::ostringstream str;
      str << "Value not in interval [" << lower << "; " << upper << "[";
      throw std::string (str.str ());
    }
}

RangeIE::RangeIE (const double l, const double u)
  : lower (l),
    upper (u)
{ }

void
RangeEI::check (const double value) const throw (std::string)
{
  if (value <= lower || value > upper)
    {
      std::ostringstream str;
      str << "Value not in interval ]" << lower << "; " << upper << "]";
      throw std::string (str.str ());
    }
}

RangeEI::RangeEI (const double l, const double u)
  : lower (l),
    upper (u)
{ }

void
RangeEE::check (const double value) const throw (std::string)
{
  if (value <= lower || value >= upper)
    {
      std::ostringstream str;
      str << "Value not in interval ]" << lower << "; " << upper << "[";
      throw std::string (str.str ());
    }
}

RangeEE::RangeEE (const double l, const double u)
  : lower (l),
    upper (u)
{ }

void
Below::check (const double value) const throw (std::string)
{
  if (value >= upper)
    {
      std::ostringstream str;
      str << "Value should be < " << upper;
      throw std::string (str.str ());
    }
}

Below::Below (const double u)
  : upper (u)
{ }

void
BelowOrEqual::check (const double value) const throw (std::string)
{
  if (value > upper)
    {
      std::ostringstream str;
      str << "Value should be <= " << upper;
      throw std::string (str.str ());
    }
}

BelowOrEqual::BelowOrEqual (const double u)
  : upper (u)
{ }

void
Above::check (const double value) const throw (std::string)
{
  if (value <= lower)
    {
      std::ostringstream str;
      str << "Value should be > " << lower;
      throw std::string (str.str ());
    }
}

Above::Above (const double l)
  : lower (l)
{ }

void
AboveOrEqual::check (const double value) const throw (std::string)
{
  if (value < lower)
    {
      std::ostringstream str;
      str << "Value should be >= " << lower;
      throw std::string (str.str ());
    }
}

AboveOrEqual::AboveOrEqual (const double l)
  : lower (l)
{ }

const AboveOrEqual Temperature (-273.15);
