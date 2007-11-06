// check.C -- Check validity of arbitrary alist members.
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

#define BUILD_DLL

#include "vcheck.h"
#include "units.h"
#include "syntax.h"
#include "alist.h"
#include "time.h"
#include "plf.h"
#include <sstream>
#include "assertion.h"
#include "mathlib.h"
#include <algorithm>
#include <numeric>

// GCC 2.95 -O2 dislike declaring these classes local.
struct ValidYear : public VCheck
{
  static void validate (int year) throw (std::string)
  {
    if (!Time::valid (year, 1, 1, 1))
      {
	std::ostringstream tmp;
	tmp << year << " is not a valid year";
	throw std::string (tmp.str ());
      }
  }

  void check (const Syntax& syntax, const AttributeList& alist, 
	      const std::string& key) const throw (std::string)
  { 
    daisy_assert (alist.check (key));
    daisy_assert (syntax.lookup (key) == Syntax::Integer);
    daisy_assert (!syntax.is_log (key));

    if (syntax.size (key) == Syntax::Singleton)
      validate (alist.integer (key));
    else
      {
	const std::vector<int> years = alist.integer_sequence (key);
	for_each (years.begin (), years.end (), validate);
      }
  }
};

void 
VCheck::IRange::validate (const int value) const throw (std::string)
{
  if (value < min)
    {
      std::ostringstream tmp;
      tmp << "Value is " << value << " but should be >= " << min;
      throw std::string (tmp.str ());
    }
  if (value > max)
    {
      std::ostringstream tmp;
      tmp << "Value is " << value << " but should be <= " << max;
      throw std::string (tmp.str ());
    }
}

void
VCheck::IRange::check (const Syntax& syntax, const AttributeList& alist, 
		       const std::string& key) const throw (std::string)
{
  daisy_assert (alist.check (key));
  daisy_assert (!syntax.is_log (key));
  daisy_assert (syntax.lookup (key) == Syntax::Integer);

  if (syntax.size (key) == Syntax::Singleton)
    validate (alist.integer (key));
  else
    {
      const std::vector<int> integers = alist.integer_sequence (key);
      for (unsigned int i = 0; i < integers.size (); i++)
	validate (integers[i]);
    }
}

VCheck::IRange::IRange (const int min_, const int max_)
  : min (min_),
    max (max_)
{ }

struct LocalOrder : public VCheck
{
  virtual void validate (double last, double next) const throw (std::string) = 0;

  void validate_plf (const PLF& plf) const throw (std::string)
  {
    const int end = plf.size () - 1;
    daisy_assert (end >= 0);
    double last = plf.y (0);

    for (int i = 1; i < end; i++)
      {
	const double next = plf.y (i);
	validate (last, next);
	last = next;
      }
  }

  void check (const Syntax& syntax, const AttributeList& alist, 
	      const std::string& key) const throw (std::string)
  { 
    daisy_assert (alist.check (key));
    daisy_assert (!syntax.is_log (key));
  
    switch (syntax.lookup (key))
      {
      case Syntax::Number:
	{
	  daisy_assert (syntax.size (key) != Syntax::Singleton);
	  const std::vector<double>& numbers = alist.number_sequence (key);
	  if (numbers.size () < 2)
	    return;
	  double last = numbers[0];
	  for (int i = 1; i < numbers.size (); i++)
	    {
	      const double next = numbers[i];
	      validate (last, next);
	      last = next;
	    }
	}
	break;
      case Syntax::PLF:
	if (syntax.size (key) == Syntax::Singleton)
	  validate_plf (alist.plf (key));
	else
	  {
	    const std::vector<const PLF*>& plfs = alist.plf_sequence (key);
	    for (unsigned int i = 0; i < plfs.size (); i++)
	      validate_plf (*plfs[i]);
	  }
	break;
      default:
	daisy_notreached ();
      }
  }
};

struct Increasing : public LocalOrder
{
  void validate (double last, double next) const throw (std::string)
  {
    if (last >= next)
      {
	std::ostringstream tmp;
	tmp << last << " >= " << next << ", must be increasing";
	throw std::string (tmp.str ());
      }
  }
};

struct NonDecreasing : public LocalOrder
{
  void validate (double last, double next) const throw (std::string)
  {
    if (last > next)
      {
	std::ostringstream tmp;
	tmp << last << " > " << next << ", must be non-decreasing";
	throw std::string (tmp.str ());
      }
  }
};

struct NonIncreasing : public LocalOrder
{
  void validate (double last, double next) const throw (std::string)
  {
    if (last < next)
      {
	std::ostringstream tmp;
	tmp << last << " < " << next << ", must be non-increasing";
	throw std::string (tmp.str ());
      }
  }
};

struct Decreasing : public LocalOrder
{
  void validate (double last, double next) const throw (std::string)
  {
    if (last <= next)
      {
	std::ostringstream tmp;
	tmp << last << " <= " << next << ", must be decreasing";
	throw std::string (tmp.str ());
      }
  }
};

const VCheck& 
VCheck::valid_year ()
{
  static ValidYear valid_year;
  return valid_year;
}

const VCheck& 
VCheck::valid_month ()
{
  static IRange valid_month (1, 12);
  return valid_month;
}

const VCheck& 
VCheck::valid_mday ()
{
  static IRange valid_mday (1, 31);
  return valid_mday;
}

const VCheck& 
VCheck::valid_hour ()
{
  static IRange valid_hour (0, 23);
  return valid_hour;
}

const VCheck& 
VCheck::increasing ()
{
  static Increasing increasing;
  return increasing;
}

const VCheck& 
VCheck::non_decreasing ()
{
  static NonDecreasing non_decreasing;
  return non_decreasing;
}

const VCheck& 
VCheck::non_increasing ()
{
  static NonIncreasing non_increasing;
  return non_increasing;
}

const VCheck& 
VCheck::decreasing ()
{
  static Decreasing decreasing;
  return decreasing;
}

const VCheck& 
VCheck::sum_equal_0 ()
{
  static SumEqual sum_equal (0.0);
  return sum_equal;
}

const VCheck& 
VCheck::sum_equal_1 ()
{
  static SumEqual sum_equal (1.0);
  return sum_equal;
}

const VCheck& 
VCheck::min_size_1 ()
{
  static MinSize min_size (1);
  return min_size;
}

VCheck::VCheck ()
{ }

VCheck::~VCheck ()
{ }

void 
VCheck::SumEqual::validate (double value) const throw (std::string)
{
  if (!approximate (value, sum))
    {
      std::ostringstream tmp;
      tmp << "Sum is " << value << " but should be " << sum;
      throw std::string (tmp.str ());
    }
}

void 
VCheck::SumEqual::validate (const PLF& plf) const throw (std::string)
{
  const int end = plf.size () - 1;
  daisy_assert (end >= 0);
  if (std::isnormal (plf.y (0)))
    throw (std::string ("Value at start of PLF should be 0.0"));
  if (std::isnormal (plf.y (end)))
    throw (std::string ("Value at end of PLF should be 0.0"));
  validate (plf.integrate (plf.x (0), plf.x (end)));
}

void
VCheck::SumEqual::check (const Syntax& syntax, const AttributeList& alist, 
			 const std::string& key) const throw (std::string)
{
  daisy_assert (alist.check (key));
  daisy_assert (!syntax.is_log (key));
  
  switch (syntax.lookup (key))
    {
    case Syntax::Number:
      {
	daisy_assert (syntax.size (key) != Syntax::Singleton);
	const std::vector<double>& numbers = alist.number_sequence (key);
	validate (accumulate (numbers.begin (), numbers.end (), 0.0));
      }
      break;
    case Syntax::PLF:
      if (syntax.size (key) == Syntax::Singleton)
	validate (alist.plf (key));
      else
	{
	  const std::vector<const PLF*> plfs = alist.plf_sequence (key);
	  for (unsigned int i = 0; i < plfs.size (); i++)
	    validate (*plfs[i]);
	}
      break;
    default:
      daisy_notreached ();
    }
}

VCheck::SumEqual::SumEqual (double value)
  : sum (value)
{ }

void 
VCheck::StartValue::validate (double value) const throw (std::string)
{
  if (!approximate (value, fixed))
    {
      std::ostringstream tmp;
      tmp << "Start value is " << value << " but should be " << fixed;
      throw std::string (tmp.str ());
    }
}

void 
VCheck::StartValue::validate (const PLF& plf) const throw (std::string)
{ validate (plf.y (0)); }

void
VCheck::StartValue::check (const Syntax& syntax, const AttributeList& alist, 
			   const std::string& key) const throw (std::string)
{
  daisy_assert (alist.check (key));
  daisy_assert (!syntax.is_log (key));
  
  switch (syntax.lookup (key))
    {
    case Syntax::Number:
      {
	daisy_assert (syntax.size (key) != Syntax::Singleton);
	const std::vector<double>& numbers = alist.number_sequence (key);
	if (numbers.size () > 0)
	  validate (numbers[0]);
      }
      break;
    case Syntax::PLF:
      if (syntax.size (key) == Syntax::Singleton)
	validate (alist.plf (key));
      else
	{
	  const std::vector<const PLF*> plfs = alist.plf_sequence (key);
	  for (unsigned int i = 0; i < plfs.size (); i++)
	    validate (*plfs[i]);
	}
      break;
    default:
      daisy_notreached ();
    }
}

VCheck::StartValue::StartValue (double value)
  : fixed (value)
{ }

void 
VCheck::EndValue::validate (double value) const throw (std::string)
{
  if (!approximate (value, fixed))
    {
      std::ostringstream tmp;
      tmp << "End value is " << value << " but should be " << fixed;
      throw std::string (tmp.str ());
    }
}

void 
VCheck::EndValue::validate (const PLF& plf) const throw (std::string)
{ 
  daisy_assert (plf.size () > 0);
  validate (plf.y (plf.size () - 1)); 
}

void
VCheck::EndValue::check (const Syntax& syntax, const AttributeList& alist, 
			   const std::string& key) const throw (std::string)
{
  daisy_assert (alist.check (key));
  daisy_assert (!syntax.is_log (key));
  
  switch (syntax.lookup (key))
    {
    case Syntax::Number:
      {
	daisy_assert (syntax.size (key) != Syntax::Singleton);
	const std::vector<double>& numbers = alist.number_sequence (key);
	if (numbers.size () > 0)
	  validate (numbers[numbers.size () - 1]);
      }
      break;
    case Syntax::PLF:
      if (syntax.size (key) == Syntax::Singleton)
	validate (alist.plf (key));
      else
	{
	  const std::vector<const PLF*> plfs = alist.plf_sequence (key);
	  for (unsigned int i = 0; i < plfs.size (); i++)
	    validate (*plfs[i]);
	}
      break;
    default:
      daisy_notreached ();
    }
}

VCheck::EndValue::EndValue (double value)
  : fixed (value)
{ }

void 
VCheck::FixedPoint::validate (const PLF& plf) const throw (std::string)
{ 
  if (!approximate (plf (fixed_x), fixed_y))
    {
      std::ostringstream tmp;
      tmp << "Value at " << fixed_x << " should be " << fixed_y 
	     << " but is << " << plf (fixed_x);
      throw (std::string (tmp.str ()));
    }
}

void
VCheck::FixedPoint::check (const Syntax& syntax, const AttributeList& alist, 
			   const std::string& key) const throw (std::string)
{
  daisy_assert (alist.check (key));
  daisy_assert (!syntax.is_log (key));
  
  switch (syntax.lookup (key))
    {
    case Syntax::PLF:
      if (syntax.size (key) == Syntax::Singleton)
	validate (alist.plf (key));
      else
	{
	  const std::vector<const PLF*> plfs = alist.plf_sequence (key);
	  for (unsigned int i = 0; i < plfs.size (); i++)
	    validate (*plfs[i]);
	}
      break;
    default:
      daisy_notreached ();
    }
}

VCheck::FixedPoint::FixedPoint (double x, double y)
  : fixed_x (x),
    fixed_y (y)
{ }

void
VCheck::MinSize::check (const Syntax& syntax, const AttributeList& alist, 
			const std::string& key) const throw (std::string)
{
  daisy_assert (alist.check (key));
  daisy_assert (!syntax.is_log (key));
  daisy_assert (syntax.size (key) == Syntax::Sequence);
  if (alist.size (key) < min_size)
    {
      std::ostringstream tmp;
      tmp << "Need at least " << min_size << " elements, got " 
	     << alist.size (key);
      throw std::string (tmp.str ());
    }
}

VCheck::MinSize::MinSize (unsigned int size)
  : min_size (size)
{ }

void
VCheck::String::check (const Syntax& syntax, const AttributeList& alist, 
                       const std::string& key) const throw (std::string)
{
  daisy_assert (alist.check (key));
  daisy_assert (!syntax.is_log (key));
  if (syntax.size (key) == Syntax::Singleton)
    validate (alist.name (key));
  else
    {
      const std::vector<symbol> names = alist.identifier_sequence (key);
      for (size_t i = 0; i < names.size (); i++)
        validate (names[i].name ());
    }
}

void 
VCheck::Compatible::validate (const std::string& value) const throw (std::string)
{
  if (!Units::can_convert (dimension, value))
    {
      std::ostringstream tmp;
      tmp << "Cannot convert [" << dimension << "] to [" << value << "]";
      throw std::string (tmp.str ());
    }
}

VCheck::Compatible::Compatible (const std::string& dim)
  : dimension (dim)
{ }

const VCheck& 
VCheck::fraction ()
{
  static Compatible fraction (Syntax::Fraction ());
  return fraction;
}

void 
VCheck::Enum::validate (const std::string& value) const throw (std::string)
{
  if (ids.find (value) == ids.end ())
    throw std::string ("Invalid value '" + value + "'");
}

VCheck::Enum::Enum ()
{ }

void 
VCheck::Enum::add (const std::string& a)
{ ids.insert (a); }

size_t 
VCheck::Enum::size () const
{ return ids.size (); }

VCheck::Enum::Enum (const std::string& a)
{
  ids.insert (a);
}

VCheck::Enum::Enum (const std::string& a, const std::string& b)
{
  ids.insert (a);
  ids.insert (b);
}

VCheck::Enum::Enum (const std::string& a, const std::string& b, const std::string& c)
{
  ids.insert (a);
  ids.insert (b);
  ids.insert (c);
}

VCheck::Enum::Enum (const std::string& a, const std::string& b, const std::string& c, 
		  const std::string& d)
{
  ids.insert (a);
  ids.insert (b);
  ids.insert (c);
  ids.insert (d);
}

VCheck::Enum::Enum (const std::string& a, const std::string& b, const std::string& c,
		  const std::string& d, const std::string& e)
{
  ids.insert (a);
  ids.insert (b);
  ids.insert (c);
  ids.insert (d);
  ids.insert (e);
}

VCheck::Enum::Enum (const std::string& a, const std::string& b, const std::string& c,
		  const std::string& d, const std::string& e, const std::string& f)
{
  ids.insert (a);
  ids.insert (b);
  ids.insert (c);
  ids.insert (d);
  ids.insert (e);
  ids.insert (f);
}

template<class T> 
void unique_validate (const std::vector<T>& list)
{
  for (int i = 0; i < list.size (); i++)
    for (int j = i + 1; j < list.size (); j++)
      if (list[i] == list[j])
        {
          std::ostringstream tmp;
          tmp << "Entry " << i << " and " << j << " are both '" 
              << list[j] << "'";
          throw std::string (tmp.str ());
        }
}

const VCheck& 
VCheck::unique ()
{
  static struct Unique : public VCheck
  {
    void check (const Syntax& syntax, const AttributeList& alist, 
                const std::string& key) const throw (std::string)
    { 
      daisy_assert (alist.check (key));
      daisy_assert (syntax.size (key) != Syntax::Singleton);
      daisy_assert (!syntax.is_log (key));
      
      switch (syntax.lookup (key))
        {
        case Syntax::Number:
          unique_validate (alist.number_sequence (key));
          break;
        case Syntax::PLF:
          unique_validate (alist.plf_sequence (key));
          break;
        case Syntax::Boolean:
          unique_validate (alist.flag_sequence (key));
          break;
        case Syntax::String:
          unique_validate (alist.identifier_sequence (key));
          break;
        case Syntax::Integer:
          unique_validate (alist.integer_sequence (key));
          break;
        default:
          daisy_panic ("Unhandled list type "
                       + Syntax::type_name (syntax.lookup (key)));
        }
    }      
  } unique;

  return unique;
}

void
VCheck::All::check (const Syntax& syntax, const AttributeList& alist, 
		    const std::string& key) const throw (std::string)
{
  for (int i = 0; i < checks.size (); i++)
    checks[i]->check (syntax, alist, key);
}

VCheck::All::All (const VCheck& a, const VCheck& b)
{
  checks.push_back (&a);
  checks.push_back (&b);
}

VCheck::All::All (const VCheck& a, const VCheck& b, const VCheck& c)
{
  checks.push_back (&a);
  checks.push_back (&b);
  checks.push_back (&c);
}

VCheck::All::All (const VCheck& a, const VCheck& b, const VCheck& c, 
		  const VCheck& d)
{
  checks.push_back (&a);
  checks.push_back (&b);
  checks.push_back (&c);
  checks.push_back (&d);
}

VCheck::All::All (const VCheck& a, const VCheck& b, const VCheck& c,
		  const VCheck& d, const VCheck& e)
{
  checks.push_back (&a);
  checks.push_back (&b);
  checks.push_back (&c);
  checks.push_back (&d);
  checks.push_back (&e);
}
