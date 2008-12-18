// log_extern.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006-2007 Per Abrahamsen and KVL.
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

#include "log_extern.h"
#include "select.h"
#include "scope_block.h"
#include "block.h"
#include "assertion.h"
#include "librarian.h"
#include "submodeler.h"

void 
LogExtern::done (const std::vector<Time::component_t>& time_columns,
		 const Time& time, const double dt)
{ 
  LogSelect::done (time_columns, time, dt);

  if (!is_printing)
    return;

  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    {
      last_done = LogSelect::entries[i]->tag ();
      LogSelect::entries[i]->done (dt);
    }
}

bool 
LogExtern::initial_match (const Daisy&, Treelog&)
  // No initial line.
{ return false; }

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
  sizes[last_done] = value.size ();
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

symbol 
LogExtern::title () const
{ return title_; }

void 
LogExtern::tick (const Scope&, Treelog&)
{ }

void 
LogExtern::entries (std::vector<symbol>& all) const
{
  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    all.push_back (LogSelect::entries[i]->tag ());
}

Value::type 
LogExtern:: lookup (const symbol tag) const
{
  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    if (LogSelect::entries[i]->tag () == tag)
      {
        switch (LogSelect::entries[i]->type ())
          {
          case Select::NumberSingleton:
          case Select::NumberSequence:
            return Value::Number;
          }
        daisy_notreached ();
      }
  return Value::Error;
}

int
LogExtern::type_size (symbol tag) const
{
  const int_map::const_iterator i = sizes.find (tag);
  if (i == sizes.end ())
    return Value::Singleton;
  return (*i).second;
}

int
LogExtern::value_size (symbol tag) const
{
  const array_map::const_iterator i = arrays.find (tag);
  if (i == arrays.end ())
    return type_size (tag);
  return (*i).second->size ();
}

bool 
LogExtern::check (const symbol tag) const
{
  const type_map::const_iterator i = types.find (tag);
  if (i == types.end ())
    return false;
  switch ((*i).second)
    {
    case Number:
    case Name:
    case Array:
      return true;
    case Missing:
    case Error:
      return false;
    }
  daisy_notreached ();
}

double 
LogExtern::number (symbol tag) const
{
  const number_map::const_iterator i = numbers.find (tag);
  daisy_assert (i != numbers.end ());
  return (*i).second;
}

symbol 
LogExtern::dimension (symbol tag) const
{
  const name_map::const_iterator i = dimensions.find (tag);
  daisy_assert (i != dimensions.end ());
  return (*i).second;
}

symbol
LogExtern::description (const symbol tag) const
{
  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    if (LogSelect::entries[i]->tag () == tag)
      return symbol (LogSelect::entries[i]->get_description ());

  const name_map::const_iterator i = descriptions.find (tag);
  if (i == dimensions.end ())
    return symbol ("No such tag");

  return (*i).second;
}

LogExtern::intern_type 
LogExtern::intern_lookup (symbol tag) const
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
  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    {
      const symbol tag = LogSelect::entries[i]->tag ();
      sizes[tag] = LogSelect::entries[i]->size ();
      dimensions[tag] = LogSelect::entries[i]->dimension ();
    }
}

struct LogExtern::NumEntry
{
  const symbol name;
  const double value;

  static void load_syntax (Syntax& syntax, AttributeList&)
  {
    syntax.add ("name", Value::String, Value::State, "\
Name to refer to number with.");
    syntax.add ("value", Value::Unknown (), Value::State, "\
Numeric value.");
  }

  NumEntry (Block& al)
    : name (al.name ("name")),
      value (al.number ("value"))
  { }
};

void
LogExtern::load_syntax (Syntax& syntax, AttributeList& alist)
{
  LogSelect::load_syntax (syntax, alist);

  syntax.add_submodule_sequence ("numbers", Value::OptionalState, "\
Inititial numeric values.  By default, none.", NumEntry::load_syntax);
  syntax.add ("where", Value::String, Value::OptionalConst,
              "Name of the extern log to use.\n\
By default, use the model name.");
  syntax.add ("parameter_names", Value::String, 
              Value::Const, Value::Sequence, "\
List of parameters to export.\n\
\n\
For example, if you have defined 'column' and 'crop' parameters for\n\
this extern log parameterization, you can export them to through the\n\
API interface by specifying '(names column crop)'.");
  alist.add ("parameter_names", std::vector<symbol> ());
}

LogExtern::LogExtern (Block& al)
  : LogSelect (al),
    title_ (al.name ("where", al.name ("type")))
{ 
  std::vector<symbol> par_names = al.name_sequence ("parameter_names");
  ScopeBlock scope_block (al);

  for (size_t i = 0; i < par_names.size (); i++)
    {
      const symbol id = par_names[i];
      if (scope_block.has_name (id))
        {
          types[id] = Name;
          names[id] = scope_block.name (id);
          descriptions[id] = scope_block.description (id);
        }
      else
        al.msg ().warning ("Parameter name " + id + " not found"); 
    }

  if (al.check ("numbers"))
    {
      auto_vector<const NumEntry*> nums
        = map_submodel_const<NumEntry> (al, "numbers");
      for (size_t i = 0; i < nums.size (); i++)
        {
          const symbol id = nums[i]->name;
          numbers[id] = nums[i]->value;
          types[id] = Number;
        }
    }

  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    LogSelect::entries[i]->add_dest (this);
}

LogExtern::~LogExtern ()
{ }

static struct LogExternSyntax
{
  static Model& make (Block& al)
  { return dynamic_cast<Log&> (*new LogExtern (al)); }

  LogExternSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    LogExtern::load_syntax (syntax, alist);
    alist.add ("description", "Log simulation state for extern use.");
    Librarian::add_type (Log::component, "extern", alist, syntax, &make);
  }
} LogExtern_syntax;
