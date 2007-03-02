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
  static std::vector<const LogExtern*> log_extern_table;

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
  std::vector<symbol> all_numbers_;

  // Log.
  symbol last_done;
  void done (const Time&, double dt);
  bool initial_match (const Daisy&, Treelog&)
    // No initial line.
  { return false; }

  // Self use.
  void output (Log&) const;

  // Select::Destination
  void error ();
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

  // Scope
  void tick (const Scope&, Treelog&);
  const std::vector<symbol>& all_numbers () const;
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

std::vector<const LogExtern*> LogExtern::log_extern_table;

void 
LogExtern::done (const Time& time, const double dt)
{ 
  LogSelect::done (time, dt);

  if (!is_printing)
    return;

  for (size_t i = 0; i < entries.size (); i++)
    {
      last_done = entries[i]->tag ();
      entries[i]->done (dt);
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
  types[last_done] = Error;
}

void 
LogExtern::missing ()
{ 
  types[last_done] = Missing;
}

void 
LogExtern::add (const std::vector<double>& value)
{ 
  types[last_done] = Array;
  arrays[last_done] = &value;
}

void 
LogExtern::add (double value)
{ 
  types[last_done] = Number;
  numbers[last_done] = value;
}

void 
LogExtern::add (symbol value)
{ 
  types[last_done] = Name;
  names[last_done] = value;
}

void 
LogExtern::tick (const Scope&, Treelog&)
{ }

const std::vector<symbol>&
LogExtern::all_numbers () const
{ return all_numbers_; }

bool 
LogExtern::has_number (symbol tag) const
{ return lookup (tag) == Number; }

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
  for (size_t i = 0; i < entries.size (); i++)
    {
      const Select::type_t type = entries[i]->type ();
      const symbol tag = entries[i]->tag ();
      const int size = entries[i]->size ();
      sizes[tag] = size;
      if (type == Select::NumberSingleton)
        all_numbers_.push_back (tag);
      dimensions[tag] = entries[i]->dimension ();
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

  for (size_t i = 0; i < entries.size (); i++)
    entries[i]->add_dest (this);

  // Register extern access.
  log_extern_table.push_back (this);
}

LogExtern::~LogExtern ()
{ }

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
size_t
extern_scope_size ()
{ 
  return LogExtern::log_extern_table.size ();
}

const Scope*
extern_scope_get (size_t i)
{ 
  daisy_assert (i < LogExtern::log_extern_table.size ());
  return LogExtern::log_extern_table[i];
}

const Scope*
extern_scope_find (const symbol target)
{ 
  for (size_t i = 0; i < LogExtern::log_extern_table.size (); i++)
    {
      const LogExtern* scope = LogExtern::log_extern_table[i];
      const symbol name = scope->alist.check ("where") 
        ? scope->alist.identifier ("where") 
        : scope->alist.identifier ("type");

      if (name == target)
        return scope;
    }
  return NULL;
}
