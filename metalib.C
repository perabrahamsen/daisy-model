// metalib.C -- A library of libraries.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "metalib.h"
#include "intrinsics.h"
#include "librarian.h"
#include "library.h"
#include "block.h"
#include "syntax.h"
#include "alist.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "path.h"
#include "units.h"
#include "unit.h"
#include "oldunits.h"
#include <map>
#include <sstream>

struct Metalib::Implementation : public Unitc
{
  // Content.
  Path path;
  Syntax syntax;
  AttributeList alist;

  typedef std::map<symbol, Library*> library_map;
  library_map all;

  int sequence;

  // Units.
  typedef std::map<symbol, const Unit*> unit_map;
  unit_map units;
  bool has_unit (symbol name) const;
  const Unit& get_unit (symbol name) const;
  void add_unit (Metalib&, const symbol name);
  void add_all_units (Metalib&);
  bool allow_old () const;

  // Units public interface.
  bool can_convert (symbol from, symbol to, Treelog&) const;
  bool can_convert (symbol from, symbol to) const;
  bool can_convert (symbol from, symbol to, double) const;
  double convert (symbol from, symbol to, double) const;

  // Conversions.
  typedef std::map<symbol, const Convert*> convert_map;
  mutable convert_map conversions;
  const Convert& get_convertion (symbol from, symbol to) const;

  // Create and destroy.
  Implementation ()
    : all (Librarian::intrinsics ().clone ()),
      sequence (0)
  { }
  ~Implementation ()
  { 
    map_delete (all.begin (), all.end ()); 
    map_delete (units.begin (), units.end ()); 
    map_delete (conversions.begin (), conversions.end ()); 
  }
};

bool
Metalib::Implementation::has_unit (symbol name) const
{ return this->units.find (name) != this->units.end (); }

const Unit&
Metalib::Implementation::get_unit (symbol name) const
{ 
  Implementation::unit_map::const_iterator i = this->units.find (name); 
  if (i != this->units.end () && (*i).second)
    return *(*i).second;
  
  throw "No unit [" + name + "]";
}

void
Metalib::Implementation::add_unit (Metalib& metalib, const symbol name)
{
  // Do we already have it?
  Implementation::unit_map::iterator i = this->units.find (name); 
  if (i != this->units.end ())
    {
      // Delete old copy.
      delete (*i).second;
      (*i).second = NULL;
    }
  
  // Is it defined?
  const Library& library = metalib.library (Unit::component);
  if (!library.complete (metalib, name))
    return;

  // Build it.
  AttributeList alist (library.lookup (name));
  alist.add ("type", name);
  this->units[name] = Librarian::build_free<Unit> (metalib, Treelog::null (),
                                                   alist, "unit");
}

void
Metalib::Implementation::add_all_units (Metalib& metalib)
{
  const Library& library = metalib.library (Unit::component);
  std::vector<symbol> entries;
  library.entries (entries);
  for (size_t i = 0; i < entries.size (); i++)
    add_unit (metalib, entries[i]);
}

bool 
Metalib::Implementation::allow_old () const
{
  daisy_assert (alist.check ("allow_old_units"));
  const bool allow_old_units = alist.flag ("allow_old_units");
  return allow_old_units;
}

bool
Metalib::Implementation::can_convert (const symbol from, const symbol to, 
                                      Treelog& msg) const
{
  if (from == to)
    return true;

  // Defined?
  if (!has_unit(from) || !has_unit (to))
    {
      if (!allow_old ())
        {
          if (has_unit (from))
            msg.message ("Original dimension [" + from + "] known.");
          else
            msg.message ("Original dimension [" + from + "] not known.");
          if (has_unit (to))
            msg.message ("Target dimension [" + to + "] known.");
          else
            msg.message ("Target dimension [" + to + "] not known.");
          return false;
        }
      msg.message (std::string ("Trying old conversion of ") 
                   + (has_unit (from) ? "" : "unknown ") + "[" + from + "] to " 
                   + (has_unit (to) ? "" : "unknown ") + "[" + to + "]." );
      return Oldunits::can_convert (from, to);
    }

  const Unit& from_unit = get_unit (from);
  const Unit& to_unit = get_unit (to);

  if (compatible (from_unit, to_unit))
    return true;

  // Not compatible.
  std::ostringstream tmp;
  tmp << "Cannot convert [" << from 
      << "] with base [" << from_unit.base_name () << "] to [" << to
      << "] with base [" << to_unit.base_name () << "]";
  msg.message (tmp.str ());
  if (!allow_old ())
    return false;

  msg.message ("Trying old conversion.");
  return Oldunits::can_convert (from, to);
}

bool 
Metalib::Implementation::can_convert (const symbol from, const symbol to) const
{
  if (from == to)
    return true;

  // Defined?
  if (!has_unit(from) || !has_unit (to))
    if (!allow_old ())
      return false;
    else
      return Oldunits::can_convert (from, to);
  
  const Unit& from_unit = get_unit (from);
  const Unit& to_unit = get_unit (to);

  if (compatible (from_unit, to_unit))
    return true;

  if (!allow_old ())
    return false;

  return Oldunits::can_convert (from, to);
}

bool 
Metalib::Implementation::can_convert (const symbol from, const symbol to, 
                                      const double value) const
{ 
  if (from == to)
    return true;

  // Defined?
  if (!has_unit(from) || !has_unit (to))
    if (!allow_old ())
      return false;
    else
      return Oldunits::can_convert (from, to, value);
  
  const Unit& from_unit = get_unit (from);
  const Unit& to_unit = get_unit (to);

  if (!compatible (from_unit, to_unit))
    return false;
  if (!from_unit.in_native  (value))
    return false;
  const double base = from_unit.to_base (value);
  // We don't have to worry about [cm] and [hPa] as all values are valid.
  return to_unit.in_base (base);
}

double 
Metalib::Implementation::convert (const symbol from, const symbol to, 
                                  const double value) const
{ 
  if (from == to)
    return value;

  // Defined?
  if (!has_unit(from) || !has_unit (to))
    {
      if (allow_old ())
        return Oldunits::convert (from, to, value);
      if (!has_unit (from))
        throw "Cannot convert from unknown dimension [" + from 
          + "] to [" + to + "]";
      throw "Cannot convert from [" + from 
        + "] to unknown dimension [" + to + "]";
    }
  
  return Unitc::unit_convert (get_unit (from), get_unit (to), value);
}

const Convert& 
Metalib::Implementation::get_convertion (const symbol from, 
                                         const symbol to) const
{
  if (from == to)
    {
      static struct ConvertIdentity : public Convert
      {
        double operator()(const double value) const
        { return value; }
        bool valid (const double) const
        { return true; }
      } identity;
      return identity;
    }
  const symbol key (from.name () + " -> " + to.name ());

  // Already known.
  Implementation::convert_map::const_iterator i
    = this->conversions.find (key); 
  if (i != this->conversions.end ())
    return *(*i).second;

  // Defined?
  if (!has_unit (from) || !has_unit (to))
    {
      if (allow_old ())
        {
          struct ConvertOld : Convert
          {
            const Oldunits::Convert& old;
            double operator()(const double value) const
            { return old (value); }
            bool valid (const double value) const
            { return old.valid (value); }
            ConvertOld (const Oldunits::Convert& o)
              : old (o)
            { }
          };
          const Oldunits::Convert& old
            = Oldunits::get_convertion (from.name (), to.name ());
          
          const Convert* convert = new ConvertOld (old);
          daisy_assert (convert);
          conversions[key] = convert;
          return *convert;
        }
      if (!has_unit (from))
        throw "Cannot get conversion from unknown dimension [" + from 
          + "] to [" + to + "]";
      if (!has_unit (to))
        throw "Cannot get conversion from [" + from 
          + "] to unknown dimension [" + to + "]";
    }
  
  const Unit& from_unit = get_unit (from);
  const Unit& to_unit = get_unit (to);
  if (!compatible (from_unit, to_unit))
    throw "Cannot get conversion from [" + from 
      + "] to dimension [" + to + "]";

  const Convert* convert = create_convertion (from_unit, to_unit);
  daisy_assert (convert);
  conversions[key] = convert;
  return *convert;
}

const Unitc& 
Metalib::unitc () const
{ return *impl; }

const Unit& 
Metalib::get_unit (const symbol name) const
{ return impl->get_unit (name); }

Path& 
Metalib::path () const
{ return impl->path; }

Syntax& 
Metalib::syntax () const
{ return impl->syntax; }

AttributeList&
Metalib::alist () const
{ return impl->alist; }

bool
Metalib::exist (const symbol name) const
{ return impl->all.find (name) != impl->all.end (); }

Library& 
Metalib::library (const symbol name) const
{ return *impl->all[name]; }

Library& 
Metalib::library (const char *const name) const
{ return library (symbol (name)); }

void
Metalib::all (std::vector<symbol>& libraries) const
{ 
  for (Implementation::library_map::const_iterator i = impl->all.begin (); 
       i != impl->all.end ();
       i++)
    libraries.push_back (symbol ((*i).first)); 
}

void 
Metalib::clear_all_parsed ()
{
  for (Implementation::library_map::iterator i = impl->all.begin (); 
       i != impl->all.end (); 
       i++)
    (*i).second->clear_parsed ();
}

void 
Metalib::refile_parsed (const std::string& from, const std::string& to)
{
  for (Implementation::library_map::iterator i = impl->all.begin (); 
       i != impl->all.end (); 
       i++)
    (*i).second->refile_parsed (from, to);
}

void 
Metalib::added_object (const symbol library, const symbol object)
{
  // Make sur ewe can use units right after we defined them.
  if (library == symbol (Unit::component))
    impl->add_unit (*this, object);
}

int 
Metalib::get_sequence ()
{ 
  impl->sequence++;
  // Nobody will ever need more than two billion objects --- Per 1998.
  daisy_assert (impl->sequence > 0);
  return impl->sequence;
}

void
Metalib::reset ()
{ 
  impl.reset (new Implementation ()); 
  impl->add_all_units (*this);
}

Metalib::Metalib ()
  : impl (new Implementation ())
{
  impl->add_all_units (*this);
}

Metalib::~Metalib ()
{ }

// metalib.C ends here

