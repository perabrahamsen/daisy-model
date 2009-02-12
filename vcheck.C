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
#include "metalib.h"
#include "library.h"
#include "time.h"
#include "plf.h"
#include "assertion.h"
#include "mathlib.h"
#include "treelog.h"
#include "frame_model.h"
#include <sstream>
#include <algorithm>
#include <numeric>
#include <map>

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

  void check (Metalib&, const Frame& frame, const symbol key)
    const throw (std::string)
  { 
    daisy_assert (frame.check (key));
    daisy_assert (frame.lookup (key) == Value::Integer);
    daisy_assert (!frame.is_log (key));

    if (frame.type_size (key) == Value::Singleton)
      validate (frame.integer (key));
    else
      {
	const std::vector<int> years = frame.integer_sequence (key);
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
VCheck::IRange::check (Metalib&, const Frame& frame, 
		       const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  daisy_assert (frame.lookup (key) == Value::Integer);

  if (frame.type_size (key) == Value::Singleton)
    validate (frame.integer (key));
  else
    {
      const std::vector<int> integers = frame.integer_sequence (key);
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
  virtual void validate (double last, double next)
    const throw (std::string) = 0;

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

  void check (Metalib&, const Frame& frame, const symbol key)
    const throw (std::string)
  { 
    daisy_assert (frame.check (key));
    daisy_assert (!frame.is_log (key));
  
    switch (frame.lookup (key))
      {
      case Value::Number:
	{
	  daisy_assert (frame.type_size (key) != Value::Singleton);
	  const std::vector<double>& numbers = frame.number_sequence (key);
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
      case Value::PLF:
	if (frame.type_size (key) == Value::Singleton)
	  validate_plf (frame.plf (key));
	else
	  {
	    const std::vector<const PLF*>& plfs = frame.plf_sequence (key);
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
VCheck::valid_minute ()
{
  static IRange valid_minute (0, 59);
  return valid_minute;
}

const VCheck& 
VCheck::valid_second ()
{
  static IRange valid_second (0, 59);
  return valid_second;
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
VCheck::SumEqual::check (Metalib&, const Frame& frame, 
			 const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  
  switch (frame.lookup (key))
    {
    case Value::Number:
      {
	daisy_assert (frame.type_size (key) != Value::Singleton);
	const std::vector<double>& numbers = frame.number_sequence (key);
	validate (accumulate (numbers.begin (), numbers.end (), 0.0));
      }
      break;
    case Value::PLF:
      if (frame.type_size (key) == Value::Singleton)
	validate (frame.plf (key));
      else
	{
	  const std::vector<const PLF*> plfs = frame.plf_sequence (key);
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
VCheck::StartValue::check (Metalib&, const Frame& frame, 
			   const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  
  switch (frame.lookup (key))
    {
    case Value::Number:
      {
	daisy_assert (frame.type_size (key) != Value::Singleton);
	const std::vector<double>& numbers = frame.number_sequence (key);
	if (numbers.size () > 0)
	  validate (numbers[0]);
      }
      break;
    case Value::PLF:
      if (frame.type_size (key) == Value::Singleton)
	validate (frame.plf (key));
      else
	{
	  const std::vector<const PLF*> plfs = frame.plf_sequence (key);
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
VCheck::EndValue::check (Metalib&, const Frame& frame, 
			   const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  
  switch (frame.lookup (key))
    {
    case Value::Number:
      {
	daisy_assert (frame.type_size (key) != Value::Singleton);
	const std::vector<double>& numbers = frame.number_sequence (key);
	if (numbers.size () > 0)
	  validate (numbers[numbers.size () - 1]);
      }
      break;
    case Value::PLF:
      if (frame.type_size (key) == Value::Singleton)
	validate (frame.plf (key));
      else
	{
	  const std::vector<const PLF*> plfs = frame.plf_sequence (key);
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
VCheck::FixedPoint::check (Metalib&, const Frame& frame, 
			   const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  
  switch (frame.lookup (key))
    {
    case Value::PLF:
      if (frame.type_size (key) == Value::Singleton)
	validate (frame.plf (key));
      else
	{
	  const std::vector<const PLF*> plfs = frame.plf_sequence (key);
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
VCheck::MinSize::check (Metalib&, const Frame& frame, 
			const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  daisy_assert (frame.type_size (key) == Value::Sequence);
  if (frame.value_size (key) < min_size)
    {
      std::ostringstream tmp;
      tmp << "Need at least " << min_size << " elements, got " 
	     << frame.value_size (key);
      throw std::string (tmp.str ());
    }
}

VCheck::MinSize::MinSize (unsigned int size)
  : min_size (size)
{ }

void
VCheck::String::check (Metalib&, const Frame& frame, 
                       const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  if (frame.type_size (key) == Value::Singleton)
    validate (frame.name (key));
  else
    {
      const std::vector<symbol> names = frame.name_sequence (key);
      for (size_t i = 0; i < names.size (); i++)
        validate (names[i].name ());
    }
}

void 
VCheck::Compatible::validate (const Units& units, symbol value)
  const throw (std::string)
{
  if (!units.can_convert (dimension, value))
    {
      std::ostringstream tmp;
      tmp << "Cannot convert [" << dimension << "] to [" << value << "]";
      throw std::string (tmp.str ());
    }
}

void
VCheck::Compatible::check (Metalib& metalib,
                           const Frame& frame, 
                           const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  const Units& units = metalib.units ();
  if (frame.type_size (key) == Value::Singleton)
    validate (units, frame.name (key));
  else
    {
      const std::vector<symbol> names = frame.name_sequence (key);
      for (size_t i = 0; i < names.size (); i++)
        validate (units, names[i]);
    }
}

VCheck::Compatible::Compatible (const symbol dim)
  : dimension (dim)
{ }

const VCheck& 
VCheck::fraction ()
{
  static Compatible fraction (Value::Fraction ());
  return fraction;
}

void 
VCheck::Enum::validate (const symbol value) const throw (std::string)
{
  if (ids.find (value) == ids.end ())
    throw std::string ("Invalid value '" + value + "'");
}

VCheck::Enum::Enum ()
{ }

void 
VCheck::Enum::add (const symbol a)
{ ids.insert (a); }

size_t 
VCheck::Enum::size () const
{ return ids.size (); }

VCheck::Enum::Enum (const symbol a)
{
  ids.insert (a);
}

VCheck::Enum::Enum (const symbol a, const symbol b)
{
  ids.insert (a);
  ids.insert (b);
}

VCheck::Enum::Enum (const symbol a, const symbol b, const symbol c)
{
  ids.insert (a);
  ids.insert (b);
  ids.insert (c);
}

VCheck::Enum::Enum (const symbol a, const symbol b, const symbol c, 
		  const symbol d)
{
  ids.insert (a);
  ids.insert (b);
  ids.insert (c);
  ids.insert (d);
}

VCheck::Enum::Enum (const symbol a, const symbol b, const symbol c,
		  const symbol d, const symbol e)
{
  ids.insert (a);
  ids.insert (b);
  ids.insert (c);
  ids.insert (d);
  ids.insert (e);
}

VCheck::Enum::Enum (const symbol a, const symbol b, const symbol c,
		  const symbol d, const symbol e, const symbol f)
{
  ids.insert (a);
  ids.insert (b);
  ids.insert (c);
  ids.insert (d);
  ids.insert (e);
  ids.insert (f);
}

void
VCheck::InLibrary::check (Metalib& metalib, 
			  const Frame& frame, 
			  const symbol key) const throw (std::string)
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  if (frame.type_size (key) == Value::Singleton)
    validate (metalib, frame.name (key));
  else
    {
      const std::vector<symbol> names = frame.name_sequence (key);
      for (size_t i = 0; i < names.size (); i++)
        validate (metalib, names[i]);
    }
}

void
VCheck::InLibrary::validate (Metalib& metalib,
			     const symbol type) const throw (std::string)
{
  daisy_assert (metalib.exist (lib_name));
  const Library& library = metalib.library (lib_name);
  
  if (!library.check (type))
    throw "Unknown '" + lib_name + "' type '" + type + "'";

  const FrameModel& frame = library.model (type);
  if (!frame.check (metalib, Treelog::null ()))
    throw "Incomplete type '" + type + "'";
}

VCheck::InLibrary::InLibrary (const symbol lib)
  : lib_name (lib)
{ }

template<class T> 
void unique_validate (const std::vector<T>& list)
{
  std::map<T, size_t> found;
  for (size_t i = 0; i < list.size (); i++)
    {
      T v = list[i];
      typename std::map<T, size_t>::const_iterator f = found.find (v);
      if (f != found.end ())
	{
	  std::ostringstream tmp;
	  tmp << "Entry " << (*f).second << " and " << i 
	      << " are both '" << v << "'";
	  throw std::string (tmp.str ());
	}
      found[v] = i;
    }
}

const VCheck& 
VCheck::unique ()
{
  static struct Unique : public VCheck
  {
    void check (Metalib&,
		const Frame& frame, 
                const symbol key) const throw (std::string)
    { 
      daisy_assert (frame.check (key));
      daisy_assert (frame.type_size (key) != Value::Singleton);
      daisy_assert (!frame.is_log (key));
      
      switch (frame.lookup (key))
        {
        case Value::Number:
          unique_validate (frame.number_sequence (key));
          break;
        case Value::PLF:
          unique_validate (frame.plf_sequence (key));
          break;
        case Value::Boolean:
          unique_validate (frame.flag_sequence (key));
          break;
        case Value::String:
          unique_validate (frame.name_sequence (key));
          break;
        case Value::Integer:
          unique_validate (frame.integer_sequence (key));
          break;
	case Value::Object:
	  {
	    const std::vector<const FrameModel*>& list 
              = frame.model_sequence (key);
	    std::map<symbol, size_t> found;
	    for (size_t i = 0; i < list.size (); i++)
	      {
		const symbol type = list[i]->type_name ();
		std::map<symbol, size_t>::const_iterator f = found.find (type);
		if (f != found.end ())
		  {
		    std::ostringstream tmp;
		    tmp << "Entry " << (*f).second << " and " << i 
			<< " are both '" << type << "'";
		    throw std::string (tmp.str ());
		  }
		found[type] = i;
	      }
	    break;
	  }
        default:
          daisy_panic ("Unhandled list type "
                       + Value::type_name (frame.lookup (key)));
        }
    }      
  } unique;

  return unique;
}

void
VCheck::All::check (Metalib& metalib, const Frame& frame, const symbol key)
  const throw (std::string)
{
  for (int i = 0; i < checks.size (); i++)
    checks[i]->check (metalib, frame, key);
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

// vcheck.C ends here.
