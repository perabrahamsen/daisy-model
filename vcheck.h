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

#include "symbol.h"
#include <string>
#include <vector>
#include <set>
#include <boost/noncopyable.hpp>

class Metalib;
class Frame;
class PLF;
class Units;
class Treelog;

class VCheck : private boost::noncopyable
{ 
  // Use.
public:
  virtual bool verify (const Metalib&, const Frame&, const symbol key, Treelog&)
    const = 0;

  // Integer or Integer sequence.
public:
  static const VCheck& valid_year ();
  static const VCheck& valid_month ();
  static const VCheck& valid_mday ();
  static const VCheck& valid_hour ();
  static const VCheck& valid_minute ();
  static const VCheck& valid_second ();
  static const VCheck& negative ();
  static const VCheck& non_negative ();
  static const VCheck& non_positive ();
  static const VCheck& positive ();

  class Integer;
  class IRange;
  class LargerThan;
  class SmallerThan;

  // PLF or Number sequence.
public:
  static const VCheck& increasing ();
  static const VCheck& non_decreasing ();
  static const VCheck& non_increasing ();
  static const VCheck& decreasing ();
  static const VCheck& sum_equal_0 ();
  static const VCheck& sum_equal_1 ();

  // PLF or PLF sequence.
public:
  static const VCheck& season ();

  // Any sequence.
public:
  static const VCheck& min_size_1 ();

  class SumEqual;
  class StartValue;
  class EndValue;
  class MinSize;

  // PLF.
public:
  class FixedPoint;

  // String.
protected:
  class String;
public:
  class Compatible;
  static const VCheck& fraction ();
  class Enum;
  class InLibrary;
  class Buildable;

  // Sequence.
public:
  static const VCheck& unique ();
  class MultiSize;

  // Logic.
public:
  class All;
  class Any;
  
  // Create and Destroy.
protected:
  VCheck ();
public:
  virtual ~VCheck ();
};

class VCheck::Integer : public VCheck
{
private:
  virtual bool valid (int value, Treelog&) const = 0;
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
protected:
  Integer ();
};

class VCheck::IRange : public VCheck::Integer
{
  // Parameters.
private:
  const int min;
  const int max;

  // Use.
private:
  bool valid (int value, Treelog&) const;

  // Create and Destroy.
public:
  IRange (int min, int max);
};

class VCheck::LargerThan : public VCheck::Integer
{
  // Parameters.
private:
  const int below;

  // Use.
private:
  bool valid (int value, Treelog&) const;

  // Create and Destroy.
public:
  LargerThan (int below);
};

class VCheck::SmallerThan : public VCheck::Integer
{
  // Parameters.
private:
  const int above;

  // Use.
private:
  bool valid (int value, Treelog&) const;

  // Create and Destroy.
public:
  SmallerThan (int above);
};

class VCheck::SumEqual : public VCheck
{
  // Parameters.
private:
  const double sum;

  // Use.
private:
  bool valid (double value, Treelog&) const;
  bool valid (const PLF& value, Treelog&) const;
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  SumEqual (double value);
};

class VCheck::StartValue : public VCheck
{
  // Parameters.
private:
  const double fixed;

  // Use.
private:
  bool valid (double value, Treelog&) const;
  bool valid (const PLF& value, Treelog&) const;
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  StartValue (double value);
};

class VCheck::EndValue : public VCheck
{
  // Parameters.
private:
  const double fixed;

  // Use.
private:
  bool valid (double value, Treelog&) const;
  bool valid (const PLF& value, Treelog&) const;
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  EndValue (double value);
};

class VCheck::FixedPoint : public VCheck
{
  // Parameters.
private:
  const double fixed_x;
  const double fixed_y;

  // Use.
private:
  bool valid (const PLF& value, Treelog&) const;
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  FixedPoint (double x, double y);
};

class VCheck::MinSize : public VCheck
{
  // Parameters.
private:
  const unsigned int min_size;

  // Use.
private:
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  MinSize (unsigned int siz);
};

class VCheck::String : public VCheck
{
  // Use.
private:
  virtual bool valid (const symbol value, Treelog&) const = 0;
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;
};

class VCheck::Compatible : public VCheck
{
  // Parameters.
private:
  const symbol dimension;

  // Use.
private:
  bool valid (const Units&, symbol value, Treelog&) const;
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  Compatible (const symbol dim);
};

class VCheck::Enum : public VCheck::String
{
  // Parameters.
private:
  /*const*/ std::set<symbol> ids;

  // Use.
private:
  bool valid (const symbol value, Treelog&) const;

  // Create and Destroy.
public:
  Enum ();
  void add (const symbol a);
  size_t size () const;
  Enum (const symbol a);
  Enum (const symbol a, const symbol b);
  Enum (const symbol a, const symbol b, const symbol c);
  Enum (const symbol a, const symbol b, const symbol c,
        const symbol d);
  Enum (const symbol a, const symbol b, const symbol c,
        const symbol d, const symbol e);
  Enum (const symbol a, const symbol b, const symbol c,
        const symbol d, const symbol e, const symbol f);
};

class VCheck::InLibrary : public VCheck
{
  // Parameters.
protected:
  const symbol lib_name;

  // Use.
private:
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;
protected:
  bool valid (const Metalib&, const symbol value, Treelog&) const;

  // Create and Destroy.
public:
  InLibrary (const symbol lib);
};

class VCheck::Buildable : public VCheck::InLibrary
{
  // Use.
private:
  bool valid (const Metalib&, const symbol value, Treelog&) const;

  // Create and Destroy.
public:
  Buildable (const symbol lib);
};

class VCheck::MultiSize : public VCheck
{
  // Parameters.
private:
  std::set<size_t> sizes;

  // Use.
private:
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  MultiSize (size_t a, size_t b);
};

class VCheck::All : public VCheck
{
  // Parameters.
private:
  std::vector<const VCheck*> checks;

  // Use.
private:
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  All (const VCheck& a, const VCheck& b);
  All (const VCheck& a, const VCheck& b, const VCheck& c);
  All (const VCheck& a, const VCheck& b, const VCheck& c, const VCheck& d);
  All (const VCheck& a, const VCheck& b, const VCheck& c, const VCheck& d, 
       const VCheck& e);
};

class VCheck::Any : public VCheck
{
  // Parameters.
private:
  std::vector<const VCheck*> checks;

  // Use.
private:
  bool verify (const Metalib&, const Frame&, const symbol key, Treelog&) const;

  // Create and Destroy.
public:
  Any (const VCheck& a, const VCheck& b);
  Any (const VCheck& a, const VCheck& b, const VCheck& c);
  Any (const VCheck& a, const VCheck& b, const VCheck& c, const VCheck& d);
  Any (const VCheck& a, const VCheck& b, const VCheck& c, const VCheck& d, 
       const VCheck& e);
};

#endif // VCHECK_H
