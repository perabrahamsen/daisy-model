// check.C -- Check validity of numeric alist members.
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
#include "check.h"
#include "mathlib.h"

// GCC 2.95 -O2 dislike declaring these classes local.
struct None : public Check
{
  void check (const double) const throw (std::string)
  { }
};

const Check& 
Check::none ()
{
  static None none;
  return none;
}

struct Unknown : public Check
{
  void check (const double) const throw (std::string)
  { }
};

const Check& 
Check::unknown ()
{
  static Unknown unknown;
  return unknown;
}

struct NonZero : public Check
{
  void check (const double value) const throw (std::string)
  {
    if (iszero (value))
      throw std::string ("Zero value not permitted");
  }
};

const Check& 
Check::non_zero ()
{
  static NonZero non_zero;
  return non_zero;
}

struct NonNegative : public Check
{
  void check (const double value) const throw (std::string)
  {
    if (value < 0.0)
      throw std::string ("Negative value not permitted");
  }
};

const Check& 
Check::non_negative ()
{
  static NonNegative non_negative;
  return non_negative;
}

struct NonPositive : public Check
{
  void check (const double value) const throw (std::string)
  {
    if (value > 0.0)
      throw std::string ("Positive value not permitted");
  }
};

const Check& 
Check::non_positive ()
{
  static NonPositive non_positive;
  return non_positive;
}

struct Negative : public Check
{
  void check (const double value) const throw (std::string)
  {
    if (value >= 0.0)
      throw std::string ("Value must be negative");
  }
};

const Check& 
Check::negative ()
{
  static Negative negative;
  return negative;
}

struct Positive : public Check
{
  void check (const double value) const throw (std::string)
  {
    if (value <= 0.0)
      throw std::string ("Value must be positive");
  }
};

const Check& 
Check::positive ()
{
  static Positive positive;
  return positive;
}

struct Fraction : public Check
{
  void check (const double value) const throw (std::string)
  {
    if (value < 0.0 || value > 1.0)
      throw std::string ("Value must be fraction [0;1]");
  }
};

const Check& 
Check::fraction ()
{
  static Fraction fraction;
  return fraction;
}

Check::Check ()
{ }

Check::~Check ()
{ }
