// symbol.C -- assign unique integers to different strings.
// 
// Copyright 2003 Per Abrahamsen and KVL.
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

#include "symbol.h"
#include "assertion.h"
#include <sstream>
#include <map>
#include <iostream>

struct symbol::DB
{
  static symbol::DB* data;
  static const int fast_ints;

  typedef std::map<std::string, int> name_map_t;
  typedef std::map<int, int> int_map_t;
  typedef std::map<int, std::string> reverse_map_t;
  name_map_t name_map;
  int_map_t int_map;
  reverse_map_t reverse_map;
  int counter;

  int name2id (const std::string& name);
  int name2id (const char *const s)
  { return name2id (std::string (s)); }
  int int2id (const int i);
  const std::string& id2name (const int id)
  { 
    daisy_assert (reverse_map.find (id) != reverse_map.end ());
    return reverse_map[id]; 
  }

  DB ();
  ~DB ();
};

symbol::DB::~DB ()
{ }

symbol::DB::DB ()
  : counter (0)
{ 
  for (int i = 0; i < fast_ints; i++)
    {
      std::ostringstream tmp;
      tmp << i;
      const std::string name (tmp.str ());
      name_map[name] = i;
      reverse_map[i] = name;
      int_map[i] = i;
      counter++;
    }
  daisy_assert (name_map.size () == counter);
  daisy_assert (reverse_map.size () == counter);
}

symbol::DB* symbol::data = NULL;
const int symbol::DB::fast_ints = 100;

int 
symbol::DB::name2id (const std::string& name)
{
  name_map_t::const_iterator i = name_map.find (name);
  if (i == name_map.end ())
    {
      daisy_assert (name_map.size () == counter);
      daisy_assert (reverse_map.size () == counter);
      name_map[name] = counter;
      daisy_assert (reverse_map.find (counter) == reverse_map.end ());
      reverse_map[counter] = name;
      counter++;
      daisy_assert (name_map.size () == counter);
      daisy_assert (reverse_map.size () == counter);
      return counter - 1;
    }
  return (*i).second;
}

int
symbol::DB::int2id (const int value)
{
  if (value >= 0 && value < fast_ints)
    return value;
  
  int_map_t::const_iterator i = int_map.find (value);
  if (i == int_map.end ())
    {
      std::ostringstream tmp;
      tmp << value;
      const std::string name (tmp.str ());
      daisy_assert (name_map.find (name) == name_map.end ());
      name_map[name] = counter;
      daisy_assert (reverse_map.find (counter) == reverse_map.end ());
      reverse_map[counter] = name;
      int_map[value] = counter;
      counter++;
      daisy_assert (name_map.size () == counter);
      daisy_assert (reverse_map.size () == counter);
      return counter - 1;
    }
  return (*i).second;
}

const std::string&
symbol::name () const
{ return data->id2name (id); }

bool
symbol::alphabetical (symbol a, symbol b)
{ return a.name () < b.name (); }

symbol::symbol (const std::string& name)
  : id (data->name2id (name))
{ }

symbol::symbol (const char *const name)
  : id (data->name2id (name))
{ }

symbol::symbol (const int i)
  : id (data->int2id (i))
{ }

int symbol::Init::count = 0;

symbol::Init::Init ()
{
  if (count == 0)
    {
      daisy_assert (symbol::data == 0);
      symbol::data = new symbol::DB ();
    }
  count++;
}

symbol::Init::~Init ()
{
  daisy_assert (count > 0);
  count--;
  if (count == 0)
    {
      delete symbol::data;
      symbol::data = NULL;
    }
}

std::string operator+ (const symbol sym, const char *const str)
{ return sym.name () + str; }

std::string operator+ (const char *const str, const symbol sym)
{ return str + sym.name (); }

std::string operator+ (const symbol sym, const std::string& str)
{ return sym.name () + str; }

std::string operator+ (const std::string& str, const symbol sym)
{ return str + sym.name (); }

std::string operator+ (const symbol s1, const symbol s2)
{ return s1.name () + s2.name (); }

std::ostream& 
operator<< (std::ostream& out, const symbol sym)
{ out << sym.name (); return out; }
