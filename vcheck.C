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
#include "treelog.h"
#include <sstream>
#include <algorithm>
#include <numeric>
#include <map>

// GCC 2.95 -O2 dislike declaring these classes local.
struct ValidYear : public VCheck
{
  static bool valid (int year, Treelog& msg)
  {
    if (!Time::valid (year, 1, 1, 1))
      {
	std::ostringstream tmp;
	tmp << year << " is not a valid year";
        msg.error (tmp.str ());
        return false;
      }
    return true;
  }

  bool verify (Metalib&, const Frame& frame, const symbol key, 
               Treelog& msg) const
  { 
    daisy_assert (frame.check (key));
    daisy_assert (frame.lookup (key) == Value::Integer);
    daisy_assert (!frame.is_log (key));
    
    if (frame.type_size (key) == Value::Singleton)
      return valid (frame.integer (key), msg);

    bool ok = true;
    const std::vector<int> years = frame.integer_sequence (key);
    for (size_t i = 0; i < years.size (); i++)
      if (!valid (years[i], msg))
        ok = false;
    return ok;
  }
};

bool
VCheck::IRange::valid (const int value, Treelog& msg) const
{
  if (value < min)
    {
      std::ostringstream tmp;
      tmp << "Value is " << value << " but should be >= " << min;
      msg.error (tmp.str ());
      return false;
    }
  if (value > max)
    {
      std::ostringstream tmp;
      tmp << "Value is " << value << " but should be <= " << max;
      msg.error (tmp.str ());
      return false;
    }
  return true;
}

bool
VCheck::IRange::verify (Metalib&, const Frame& frame, const symbol key, 
                        Treelog& msg) const
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  daisy_assert (frame.lookup (key) == Value::Integer);

  if (frame.type_size (key) == Value::Singleton)
    return valid (frame.integer (key), msg);

  bool ok = true;
  const std::vector<int> integers = frame.integer_sequence (key);
  for (unsigned int i = 0; i < integers.size (); i++)
    if (!valid (integers[i], msg))
      ok = false;
        
  return ok;
}

VCheck::IRange::IRange (const int min_, const int max_)
  : min (min_),
    max (max_)
{ }

struct LocalOrder : public VCheck
{
  virtual bool valid (double last, double next, Treelog&) const = 0;

  bool validate_plf (const PLF& plf, Treelog& msg) const
  {
    const int end = plf.size () - 1;
    daisy_assert (end >= 0);
    double last = plf.y (0);

    bool ok = true;
    for (int i = 1; i < end; i++)
      {
	const double next = plf.y (i);
	if (!valid (last, next, msg))
          ok = false;
	last = next;
      }
    return ok;
  }

  bool verify (Metalib&, const Frame& frame, const symbol key, 
               Treelog& msg) const
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
	    return true;
	  double last = numbers[0];
          bool ok = true;
	  for (int i = 1; i < numbers.size (); i++)
	    {
	      const double next = numbers[i];
	      if (!valid (last, next, msg))
                ok = false;
	      last = next;
	    }
          return ok;
	}
      case Value::PLF:
        {
          if (frame.type_size (key) == Value::Singleton)
            return validate_plf (frame.plf (key), msg);
          bool ok = true;
          const std::vector<const PLF*>& plfs = frame.plf_sequence (key);
          for (unsigned int i = 0; i < plfs.size (); i++)
          if (!validate_plf (*plfs[i], msg))
            ok = false;
          return ok;
        }
      default:
        daisy_notreached ();
      }
  }
};

struct Increasing : public LocalOrder
{
  bool valid (double last, double next, Treelog& msg) const
  {
    if (last < next)
      return true;

    std::ostringstream tmp;
    tmp << last << " >= " << next << ", must be increasing";
    msg.error (tmp.str ());
    return false;
  }
};

struct NonDecreasing : public LocalOrder
{
  bool valid (double last, double next, Treelog& msg) const
  {
    if (last <= next)
      return true;

    std::ostringstream tmp;
    tmp << last << " > " << next << ", must be non-decreasing";
    msg.error (tmp.str ());
    return false;
  }
};

struct NonIncreasing : public LocalOrder
{
  bool valid (double last, double next, Treelog& msg) const
  {
    if (last >= next)
      return true;

    std::ostringstream tmp;
    tmp << last << " < " << next << ", must be non-increasing";
    msg.error (tmp.str ());
    return false;
  }
};

struct Decreasing : public LocalOrder
{
  bool valid (double last, double next, Treelog& msg) const
  {
    if (last > next)
      return true;

    std::ostringstream tmp;
    tmp << last << " <= " << next << ", must be decreasing";
    msg.error (tmp.str ());
    return false;
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

bool
VCheck::SumEqual::valid (double value, Treelog& msg) const
{
  if (approximate (value, sum))
    return true;

  std::ostringstream tmp;
  tmp << "Sum is " << value << " but should be " << sum;
  msg.error (tmp.str ());
  return false;
}

bool
VCheck::SumEqual::valid (const PLF& plf, Treelog& msg) const
{
  const int end = plf.size () - 1;
  daisy_assert (end >= 0);
  bool ok = true;
  if (std::isnormal (plf.y (0)))
    {
      msg.error ("Value at start of PLF should be 0.0");
      ok = false;
    }
  if (std::isnormal (plf.y (end)))
    {
      msg.error ("Value at end of PLF should be 0.0");
      ok = false;
    }
  if (!valid (plf.integrate (plf.x (0), plf.x (end)), msg))
    ok = false;

  return ok;
}

bool
VCheck::SumEqual::verify (Metalib&, const Frame& frame, 
                          const symbol key, Treelog& msg) const
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  
  switch (frame.lookup (key))
    {
    case Value::Number:
      {
	daisy_assert (frame.type_size (key) != Value::Singleton);
	const std::vector<double>& numbers = frame.number_sequence (key);
	return valid (accumulate (numbers.begin (), numbers.end (), 0.0), msg);
      }
      break;
    case Value::PLF:
      if (frame.type_size (key) == Value::Singleton)
	return valid (frame.plf (key), msg);
      else
	{
          bool ok = true;
	  const std::vector<const PLF*> plfs = frame.plf_sequence (key);
	  for (unsigned int i = 0; i < plfs.size (); i++)
	    if (!valid (*plfs[i], msg))
              ok = false;
          return ok;
	}
      break;
    default:
      daisy_notreached ();
    }
}

VCheck::SumEqual::SumEqual (double value)
  : sum (value)
{ }

bool
VCheck::StartValue::valid (double value, Treelog& msg) const
{
  if (approximate (value, fixed))
    return true;

  std::ostringstream tmp;
  tmp << "Start value is " << value << " but should be " << fixed;
  msg.error (tmp.str ());
  return false;
}

bool
VCheck::StartValue::valid (const PLF& plf, Treelog& msg) const
{ return valid (plf.y (0), msg); }

bool
VCheck::StartValue::verify (Metalib&, const Frame& frame, 
                            const symbol key, Treelog& msg) const
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
	  return valid (numbers[0], msg);
      }
      return true;
    case Value::PLF:
      if (frame.type_size (key) == Value::Singleton)
	return valid (frame.plf (key), msg);
      {
        const std::vector<const PLF*> plfs = frame.plf_sequence (key);
        bool ok = true;
        for (unsigned int i = 0; i < plfs.size (); i++)
          if (!valid (*plfs[i], msg))
            ok = false;
        return ok;
      }
      break;
    default:
      daisy_notreached ();
    }
}

VCheck::StartValue::StartValue (double value)
  : fixed (value)
{ }

bool
VCheck::EndValue::valid (double value, Treelog& msg) const
{
  if (approximate (value, fixed))
    return true;

  std::ostringstream tmp;
  tmp << "End value is " << value << " but should be " << fixed;
  msg.error (tmp.str ());
  return false;
}

bool
VCheck::EndValue::valid (const PLF& plf, Treelog& msg) const
{ 
  daisy_assert (plf.size () > 0);
  return valid (plf.y (plf.size () - 1), msg); 
}

bool
VCheck::EndValue::verify (Metalib&, const Frame& frame, 
                          const symbol key, Treelog& msg) const
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
	  return valid (numbers[numbers.size () - 1], msg);
      }
      return true;
    case Value::PLF:
      if (frame.type_size (key) == Value::Singleton)
	return valid (frame.plf (key), msg);
      {
        const std::vector<const PLF*> plfs = frame.plf_sequence (key);
        bool ok = true;
        for (unsigned int i = 0; i < plfs.size (); i++)
          if (!valid (*plfs[i], msg))
            ok = false;
        return ok;
      }
      break;
    default:
      daisy_notreached ();
    }
}

VCheck::EndValue::EndValue (double value)
  : fixed (value)
{ }

bool
VCheck::FixedPoint::valid (const PLF& plf, Treelog& msg) const
{ 
  if (approximate (plf (fixed_x), fixed_y))
    return true;

  std::ostringstream tmp;
  tmp << "Value at " << fixed_x << " should be " << fixed_y 
      << " but is << " << plf (fixed_x);
  msg.error (tmp.str ());
  return false;
}

bool
VCheck::FixedPoint::verify (Metalib&, const Frame& frame, 
                            const symbol key, Treelog& msg) const
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  
  switch (frame.lookup (key))
    {
    case Value::PLF:
      if (frame.type_size (key) == Value::Singleton)
	return valid (frame.plf (key), msg);
      {
        const std::vector<const PLF*> plfs = frame.plf_sequence (key);
        bool ok = true;
        for (unsigned int i = 0; i < plfs.size (); i++)
          if (!valid (*plfs[i], msg))
            ok = false;
        return ok;
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

bool
VCheck::MinSize::verify (Metalib&, const Frame& frame, 
                         const symbol key, Treelog& msg) const
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  daisy_assert (Value::flexible_size (frame.type_size (key)));
  if (frame.value_size (key) >= min_size)
    return true;
  std::ostringstream tmp;
  tmp << "Need at least " << min_size << " elements, got " 
      << frame.value_size (key);
  msg.error (tmp.str ());
  return false;
}

VCheck::MinSize::MinSize (unsigned int size)
  : min_size (size)
{ }

bool
VCheck::String::verify (Metalib&, const Frame& frame, 
                        const symbol key, Treelog& msg) const
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  if (frame.type_size (key) == Value::Singleton)
    return valid (frame.name (key), msg);
  {
    const std::vector<symbol> names = frame.name_sequence (key);
    bool ok = true;
    for (size_t i = 0; i < names.size (); i++)
      if (!valid (names[i].name (), msg))
        ok = false;
    return ok;
  }
}

bool
VCheck::Compatible::valid (const Units& units, symbol value, Treelog& msg) const
{
  if (units.can_convert (dimension, value))
    return true;
  
  std::ostringstream tmp;
  tmp << "Cannot convert [" << dimension << "] to [" << value << "]";
  msg.error (tmp.str ());
  return false;
}

bool
VCheck::Compatible::verify (Metalib& metalib, const Frame& frame, 
                            const symbol key, Treelog& msg) const
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  const Units& units = metalib.units ();
  if (frame.type_size (key) == Value::Singleton)
    return valid (units, frame.name (key), msg);

  const std::vector<symbol> names = frame.name_sequence (key);
  bool ok = true;
  for (size_t i = 0; i < names.size (); i++)
    if (!valid (units, names[i], msg))
      ok = false;
  return ok;
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

bool
VCheck::Enum::valid (const symbol value, Treelog& msg) const
{
  if (ids.find (value) != ids.end ())
    return true;

  msg.error ("Invalid value '" + value + "'");
  return false;
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

bool
VCheck::InLibrary::verify (Metalib& metalib, const Frame& frame, 
                           const symbol key, Treelog& msg) const
{
  daisy_assert (frame.check (key));
  daisy_assert (!frame.is_log (key));
  if (frame.type_size (key) == Value::Singleton)
    return valid (metalib, frame.name (key), msg);

  const std::vector<symbol> names = frame.name_sequence (key);
  bool ok = true;
  for (size_t i = 0; i < names.size (); i++)
    if (!valid (metalib, names[i], msg))
      ok = false;
  return ok;
}

bool
VCheck::InLibrary::valid (Metalib& metalib, const symbol type, 
                          Treelog& msg) const
{
  daisy_assert (metalib.exist (lib_name));
  const Library& library = metalib.library (lib_name);
  
  if (!library.check (type))
    {
      msg.error ("Unknown '" + lib_name + "' type '" + type + "'");
      return false;
    }

  const FrameModel& frame = library.model (type);
  if (!frame.check (metalib, Treelog::null ()))
    {
      msg.error ("Incomplete type '" + type + "'");
      return false;
    }
  return true;
}

VCheck::InLibrary::InLibrary (const symbol lib)
  : lib_name (lib)
{ }

template<class T> 
bool unique_validate (const std::vector<T>& list, Treelog& msg)
{
  bool ok = true;
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
	  msg.error (tmp.str ());
          ok = false;
	}
      found[v] = i;
    }
  return ok;
}

const VCheck& 
VCheck::unique ()
{
  static struct Unique : public VCheck
  {
    bool verify (Metalib&, const Frame& frame, const symbol key, 
                 Treelog& msg) const
    { 
      daisy_assert (frame.check (key));
      daisy_assert (frame.type_size (key) != Value::Singleton);
      daisy_assert (!frame.is_log (key));
      
      switch (frame.lookup (key))
        {
        case Value::Number:
          return unique_validate (frame.number_sequence (key), msg);
        case Value::PLF:
          return unique_validate (frame.plf_sequence (key), msg);
        case Value::Boolean:
          return unique_validate (frame.flag_sequence (key), msg);
        case Value::String:
          return unique_validate (frame.name_sequence (key), msg);
        case Value::Integer:
          return unique_validate (frame.integer_sequence (key), msg);
	case Value::Object:
	  {
            bool ok = true;
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
		    msg.error (tmp.str ());
                    ok = false;
		  }
		found[type] = i;
	      }
            return ok;
	  }
        default:
          daisy_panic ("Unhandled list type "
                       + Value::type_name (frame.lookup (key)));
        }
    }      
  } unique;

  return unique;
}

bool
VCheck::All::verify (Metalib& metalib, const Frame& frame, const symbol key,
                     Treelog& msg) const
{
  bool ok = true;
  for (int i = 0; i < checks.size (); i++)
    if (!checks[i]->verify (metalib, frame, key, msg))
      ok = false;
  return ok;
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
