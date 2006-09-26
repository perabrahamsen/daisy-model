// log_extern.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006 Per Abrahamsen and KVL.
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


#include "log_extern.h"
#include "log_select.h"
#include "scope.h"
#include "block.h"
#include <map>
#include <vector>

struct LogExtern : public LogSelect,
		   public Destination, 
		   public Scope
{
  // Global register.
  typedef std::map<symbol, const Scope*> log_extern_map_type; 
  static log_extern_map_type* log_extern_map;
  static int log_extern_count;

  // Destination Content.
  typedef enum { Error, Missing, Number, Name, Array } type;
  typedef std::map<symbol, type> type_map;
  typedef std::map<symbol, double> number_map;
  typedef std::map<symbol, symbol> name_map;
  typedef std::map<symbol, int> int_map;
  typedef std::map<symbol, const std::vector<double>*> array_map;
  type_map types;
  number_map numbers;
  name_map names;
  array_map arrays;
  int_map sizes;
  name_map dimensions;
  
  // Log.
  symbol tag;
  void done (const Time&);
  bool initial_match (const Daisy&, Treelog&)
    // No initial line.
  { return false; }

  // Self use.
  using LogSelect::output;
  void output (Log&) const;

  // Select::Destination
  void error ();
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

  // Scope
  void tick (const Scope&, Treelog&);
  bool has_number (symbol) const;
  double number (symbol) const;
  symbol dimension (symbol) const;

  // Scope to be?
  type lookup (symbol tag) const;
  symbol name (symbol tag) const;
  const std::vector<double>& array (symbol tag) const;
  int size (symbol tag) const;

  // Create and destroy.
  void initialize (Treelog&);
  LogExtern (Block&);
  ~LogExtern ();
};

LogExtern::log_extern_map_type* LogExtern::log_extern_map = NULL;
int LogExtern::log_extern_count = 0;

void 
LogExtern::done (const Time& time)
{ 
  LogSelect::done (time);

  if (!is_printing)
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    {
      tag = entries[i]->tag ();
      entries[i]->done ();
    }
}

void 
LogExtern::output (Log& log) const
{
  // output_vector
  static const symbol numbers_symbol ("numbers");
  if (log.check_interior (numbers_symbol))
    {
      Log::Open open (log, numbers_symbol);
      for (number_map::const_iterator item = numbers.begin ();
	   item != numbers.end ();
	   item++)
	{
	  Log::Unnamed unnamed (log);
          output_value ((*item).first, "name", log);
          output_value ((*item).second, "value", log);
	}
    }
}

void 
LogExtern::error ()
{ 
  types[tag] = Error;
}

void 
LogExtern::missing ()
{ 
  types[tag] = Missing;
}

void 
LogExtern::add (const std::vector<double>& value)
{ 
  types[tag] = Array;
  arrays[tag] = &value;
}

void 
LogExtern::add (double value)
{ 
  types[tag] = Number;
  numbers[tag] = value;
}

void 
LogExtern::add (symbol value)
{ 
  types[tag] = Name;
  names[tag] = value;
}

void 
LogExtern::tick (const Scope&, Treelog&)
{ }

bool 
LogExtern::has_number (symbol tag) const
{ return lookup (tag) == Number; }

#if 0
bool 
LogExtern::has_number (symbol tag) const
{ 
  const type_map::const_iterator i = types.find (tag);
  if (i != types.end ())
    return (*i).second == Number;

  // For initialization, we need to claim we have a number before it is ready.

  const int_map::const_iterator j = sizes.find (tag);
  if (i != types.end ())
    return (*i).second == Syntax::Singleton;

  return false; 
}
#endif

double 
LogExtern::number (symbol tag) const
{
  daisy_assert (has_number (tag));
  const number_map::const_iterator i = numbers.find (tag);
  daisy_assert (i != numbers.end ());
  return (*i).second;
}

int
LogExtern::size (symbol tag) const
{
  const int_map::const_iterator i = sizes.find (tag);
  daisy_assert (i != sizes.end ());
  return (*i).second;
}

symbol 
LogExtern::dimension (symbol tag) const
{
  const name_map::const_iterator i = dimensions.find (tag);
  daisy_assert (i != dimensions.end ());
  return (*i).second;
}

LogExtern::type 
LogExtern::lookup (symbol tag) const
{ 
  const type_map::const_iterator i = types.find (tag);

  if (i == types.end ())
    return Error;

  return (*i).second;
}

symbol
LogExtern::name (symbol tag) const
{ 
  const name_map::const_iterator i = names.find (tag);
  daisy_assert (i != names.end ());
  return (*i).second;
}

const std::vector<double>&
LogExtern::array (symbol tag) const
{ 
  const array_map::const_iterator i = arrays.find (tag);
  daisy_assert (i != arrays.end ());
  return *(*i).second;
}

void 
LogExtern::initialize (Treelog&)
{
  for (unsigned int i = 0; i < entries.size (); i++)
    {
      tag = entries[i]->tag ();
      sizes[tag] = entries[i]->size ();
      dimensions[tag] = entries[i]->dimension ();
#if 0
      if (entries[i]->has_default_value ())
        numbers[tag] = entries[i]->default_value ();
#endif 
    }
}

LogExtern::LogExtern (Block& al)
  : LogSelect (al)
{ 
  if (al.check ("numbers"))
    {
      const std::vector<AttributeList*>& alists 
        = al.alist_sequence ("numbers");
      for (size_t i = 0; i < alists.size (); i++)
        {
          const symbol id = alists[i]->identifier ("name");
          numbers[id] = alists[i]->number ("value");
          types[id] = Number;
        }
    }

  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->add_dest (this);

  // Register extern access.
  const symbol name = al.check ("where") 
    ? al.identifier ("where") 
    : al.identifier ("type");

  if (!log_extern_map)
    {
      daisy_assert (log_extern_count == 0);
      log_extern_map = new log_extern_map_type;
    }
  (*log_extern_map)[name] = this;
  log_extern_count++;
}

LogExtern::~LogExtern ()
{
  
  // Unregister extern access.
  daisy_assert (log_extern_count > 0);
  log_extern_count--;
  if (log_extern_count == 0)
    {
      delete log_extern_map;
      log_extern_map = NULL;
    }
}

static struct LogExternSyntax
{
  static Log& make (Block& al)
  { return *new LogExtern (al); }

  static void load_numbers (Syntax& syntax, AttributeList&)
  {
    syntax.add ("name", Syntax::String, Syntax::State, "\
Name to refer to number with.");
    syntax.add ("value", Syntax::Unknown (), Syntax::State, "\
Numeric value.");
  }

  LogExternSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      LogSelect::load_syntax (syntax, alist);
      syntax.add_submodule_sequence ("numbers", Syntax::OptionalState, "\
Inititial numeric values.  By default, none.", load_numbers);
      syntax.add ("where", Syntax::String, Syntax::OptionalConst,
		  "Name of the extern log to use.\n\
By default, use the model name.");

      Librarian<Log>::add_type ("extern", alist, syntax, &make);
    }
} LogExtern_syntax;

// Extern access.
const Scope*
find_extern_scope (const symbol name)
{ 
  if (!LogExtern::log_extern_map)
    return NULL;
  if (LogExtern::log_extern_count < 1)
    return NULL;
  return (*LogExtern::log_extern_map)[name];
}
