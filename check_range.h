// check_range.h -- Check range of numeric alist members.
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


#ifndef CHECK_RANGE_H
#define CHECK_RANGE_H

#include "check.h"

class RangeII : public Check
{
  // Parameters.
private:
  const double lower;
  const double upper;

  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  RangeII (double lower, double upper);
};

class RangeIE : public Check
{
  // Parameters.
private:
  const double lower;
  const double upper;

  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  RangeIE (double lower, double upper);
};

class RangeEI : public Check
{
  // Parameters.
private:
  const double lower;
  const double upper;

  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  RangeEI (double lower, double upper);
};

class RangeEE : public Check
{
  // Parameters.
private:
  const double lower;
  const double upper;

  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  RangeEE (double lower, double upper);
};

class Below : public Check
{
  // Parameters.
private:
  const double upper;

  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  Below (double upper);
};

class BelowOrEqual : public Check
{
  // Parameters.
private:
  const double upper;

  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  BelowOrEqual (double upper);
};

class Above : public Check
{
  // Parameters.
private:
  const double lower;

  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  Above (double lower);
};

class AboveOrEqual : public Check
{
  // Parameters.
private:
  const double lower;

  // Use.
private:
  void check (double value) const throw (string);

  // Create and Destroy.
public:
  AboveOrEqual (double lower);
};

#endif // CHECK_RANGE_H
