// vcheck.h -- Check validity of arbitrary alist member.
// 
// Copyright 2001, 2003 Per Abrahamsen and KVL.
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


#ifndef VCHECK_H
#define VCHECK_H

#include <string>
using namespace std;

class AttributeList;
class Syntax;
class PLF;

class VCheck
{ 
  // Use.
public:
  virtual void check (const Syntax&, const AttributeList&, const string& key)
    const throw (string) = 0;

  // Integer or Integer sequence.
public:
  static const VCheck& valid_year ();

  // PLF or Integer sequence.
public:
  static const VCheck& increasing ();
  static const VCheck& non_decreasing ();
  static const VCheck& non_increasing ();
  static const VCheck& decreasing ();
  static const VCheck& sum_equal_0 ();
  static const VCheck& sum_equal_1 ();
  
  // Create and Destroy.
private:
  VCheck (VCheck&);
  VCheck& operator= (VCheck&);
protected:
  VCheck ();
public:
  virtual ~VCheck ();
};

class SumEqual : public VCheck
{
  // Parameters.
private:
  const double sum;

  // Use.
private:
  void validate (double value) const throw (string);
  void validate (const PLF& value) const throw (string);
  virtual void check (const Syntax&, const AttributeList&, const string& key)
    const throw (string);

  // Create and Destroy.
public:
  SumEqual (double value);
};

#endif // VCHECK_H
