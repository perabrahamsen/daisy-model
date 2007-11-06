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
#include <vector>
#include <set>

class AttributeList;
class Syntax;
class PLF;

class VCheck
{ 
  // Use.
public:
  virtual void check (const Syntax&, const AttributeList&,
                      const std::string& key) const throw (std::string) = 0;

  // Integer or Integer sequence.
public:
  static const VCheck& valid_year ();
  static const VCheck& valid_month ();
  static const VCheck& valid_mday ();
  static const VCheck& valid_hour ();

  class IRange;

  // PLF or Number sequence.
public:
  static const VCheck& increasing ();
  static const VCheck& non_decreasing ();
  static const VCheck& non_increasing ();
  static const VCheck& decreasing ();
  static const VCheck& sum_equal_0 ();
  static const VCheck& sum_equal_1 ();
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

  // Sequence.
public:
  static const VCheck& unique ();

  // Logic.
public:
  class All;
  
  // Create and Destroy.
private:
  VCheck (VCheck&);
  VCheck& operator= (VCheck&);
protected:
  VCheck ();
public:
  virtual ~VCheck ();
};

class VCheck::IRange : public VCheck
{
  // Parameters.
private:
  const int min;
  const int max;

  // Use.
private:
  void validate (int value) const throw (std::string);
  void check (const Syntax&, const AttributeList&,
              const std::string& key) const throw (std::string);

  // Create and Destroy.
public:
  IRange (int min, int max);
};

class VCheck::SumEqual : public VCheck
{
  // Parameters.
private:
  const double sum;

  // Use.
private:
  void validate (double value) const throw (std::string);
  void validate (const PLF& value) const throw (std::string);
  void check (const Syntax&, const AttributeList&,
              const std::string& key) const throw (std::string);

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
  void validate (double value) const throw (std::string);
  void validate (const PLF& value) const throw (std::string);
  void check (const Syntax&, const AttributeList&,
              const std::string& key) const throw (std::string);

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
  void validate (double value) const throw (std::string);
  void validate (const PLF& value) const throw (std::string);
  void check (const Syntax&, const AttributeList&,
              const std::string& key) const throw (std::string);

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
  void validate (const PLF& value) const throw (std::string);
  void check (const Syntax&, const AttributeList&,
              const std::string& key) const throw (std::string);

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
  void check (const Syntax&, const AttributeList&,
              const std::string& key) const throw (std::string);

  // Create and Destroy.
public:
  MinSize (unsigned int siz);
};

class VCheck::String : public VCheck
{
  // Use.
private:
  virtual void validate (const std::string& value) 
    const throw (std::string) = 0;
  void check (const Syntax&, const AttributeList&,
              const std::string& key) const throw (std::string);
};

class VCheck::Compatible : public VCheck::String
{
  // Parameters.
private:
  const std::string dimension;

  // Use.
private:
  void validate (const std::string& value) const throw (std::string);

  // Create and Destroy.
public:
  Compatible (const std::string& dim);
};

class VCheck::Enum : public VCheck::String
{
  // Parameters.
private:
  /*const*/ std::set<std::string> ids;

  // Use.
private:
  void validate (const std::string& value) const throw (std::string);

  // Create and Destroy.
public:
  Enum ();
  void add (const std::string& a);
  size_t size () const;
  Enum (const std::string& a);
  Enum (const std::string& a, const std::string& b);
  Enum (const std::string& a, const std::string& b, const std::string& c);
  Enum (const std::string& a, const std::string& b, const std::string& c,
        const std::string& d);
  Enum (const std::string& a, const std::string& b, const std::string& c,
        const std::string& d, const std::string& e);
  Enum (const std::string& a, const std::string& b, const std::string& c,
        const std::string& d, const std::string& e, const std::string& f);
};

class VCheck::All : public VCheck
{
  // Parameters.
private:
  std::vector<const VCheck*> checks;

  // Use.
private:
  void check (const Syntax&, const AttributeList&,
              const std::string& key) const throw (std::string);

  // Create and Destroy.
public:
  All (const VCheck& a, const VCheck& b);
  All (const VCheck& a, const VCheck& b, const VCheck& c);
  All (const VCheck& a, const VCheck& b, const VCheck& c, const VCheck& d);
  All (const VCheck& a, const VCheck& b, const VCheck& c, const VCheck& d, 
       const VCheck& e);
};

#endif // VCHECK_H
