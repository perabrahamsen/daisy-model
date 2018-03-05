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
#include "block_model.h"
#include "assertion.h"
#include "librarian.h"
#include "submodeler.h"
#include "treelog.h"
#include "frame_model.h"
#include "daisy.h"
#include <sstream>

void 
LogExtern::find_scopes (std::vector<const Scope*>& scopes) const
{ 
  LogSelect::find_scopes (scopes);
  scopes.push_back (this); 
}

void 
LogExtern::done_print (const std::vector<Time::component_t>&, const Time&)
{ 
  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    {
      last_done = LogSelect::entries[i]->tag ();
      LogSelect::entries[i]->done_print ();
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
          const symbol key = (*item).first;
          const type_map::const_iterator i = types.find (key);
          daisy_assert (i != types.end ());
          if ((*i).second == Missing)
            continue;
          const double value = (*item).second;
	  Log::Unnamed unnamed (log);
          output_value (key, "name", log);
          output_value (value, "value", log);
	}
    }
}

void 
LogExtern::missing ()
{ 
#ifdef DEBUG_PROTOCOL
  Assertion::message (__FUNCTION__);
#endif
  types[last_done] = Missing;
}

void 
LogExtern::add (const std::vector<double>& value)
{ 
#ifdef DEBUG_PROTOCOL
  Assertion::message (__FUNCTION__);
#endif
  types[last_done] = Array;
  arrays[last_done] = &value;
  sizes[last_done] = value.size ();
}

void 
LogExtern::add (double value)
{ 
  types[last_done] = Number;
  numbers[last_done] = value;
#ifdef DEBUG_PROTOCOL
  std::ostringstream tmp;
  tmp << "Set '" << last_done << "' = " << value;
  Assertion::message (tmp.str ());
#endif
}

void 
LogExtern::add (symbol value)
{ 
#ifdef DEBUG_PROTOCOL
  Assertion::message (__FUNCTION__);
#endif
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
LogExtern::entries (std::set<symbol>& all) const
{
  for (type_map::const_iterator i = types.begin ();
       i != types.end ();
       i++)
    all.insert ((*i).first);
}

Attribute::type 
LogExtern::lookup (const symbol tag) const
{
  const type_map::const_iterator i = types.find (tag);

  if (i == types.end ())
    return Attribute::Error;

  switch ((*i).second)
    {
    case Number: 
    case Array:
      return Attribute::Number;
    case Name:
      return Attribute::String;
    case Missing:
      for (size_t j = 0; j < LogSelect::entries.size (); j++)
        if (LogSelect::entries[j]->tag () == tag)
          {
#if 0
            switch (LogSelect::entries[j]->type ())
              {
              case Select::NumberSingleton:
              case Select::NumberSequence:
                return Attribute::Number;
              }
            daisy_notreached ();
#else
            return LogSelect::entries[j]->type ();
#endif
          }
      return Attribute::Error;
    }
  daisy_notreached ();
}

int
LogExtern::type_size (symbol tag) const
{
  const int_map::const_iterator i = sizes.find (tag);
  if (i == sizes.end ())
    return Attribute::Singleton;
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
      return false;
    }
  daisy_notreached ();
}

double 
LogExtern::number (symbol tag) const
{
  const number_map::const_iterator i = numbers.find (tag);
  daisy_assert (i != numbers.end ());

#ifdef DEBUG_PROTOCOL
  std::ostringstream tmp;
  tmp << "Get '" << tag << "' = " << (*i).second;
  Assertion::message (tmp.str ());
#endif
  return (*i).second;
}

symbol 
LogExtern::dimension (symbol tag) const
{
  const name_map::const_iterator i = dimensions.find (tag);
  daisy_assert (i != dimensions.end ());
#ifdef DEBUG_PROTOCOL
  std::ostringstream tmp;
  tmp << "Dim '" << tag << "' = " << (*i).second;
  Assertion::message (tmp.str ());
#endif
  return (*i).second;
}

symbol
LogExtern::description (const symbol tag) const
{
  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    if (LogSelect::entries[i]->tag () == tag)
      return symbol (LogSelect::entries[i]->get_description ());

  return frame ().description (tag);
}

symbol
LogExtern::name (symbol tag) const
{ 
  const name_map::const_iterator i = names.find (tag);
  daisy_assert (i != names.end ());
  return (*i).second;
}

void 
LogExtern::initialize (const symbol log_dir, const symbol suffix, Treelog& msg)
{
  TREELOG_MODEL (msg);
  LogSelect::initialize (log_dir, suffix, msg);
  for (size_t i = 0; i < LogSelect::entries.size (); i++)
    {
      const symbol tag = LogSelect::entries[i]->tag ();
      sizes[tag] = LogSelect::entries[i]->size ();
      if (types.find (tag) == types.end ())
        // May already have been initialized with "numbers".
        types[tag] = Missing;
      dimensions[tag] = LogSelect::entries[i]->dimension ();
    }
}

class LogExtern::NumEntry
{
public:
  const symbol name;
  const double value;

  static void load_syntax (Frame& frame)
  {
    frame.declare_string ("name", Attribute::State, "\
Name to refer to number with.");
    frame.declare ("value", Attribute::Unknown (), Attribute::State, "\
Numeric value.");
  }

  NumEntry (const Block& al)
    : name (al.name ("name")),
      value (al.number ("value"))
  { }
};

LogExtern::LogExtern (const BlockModel& al)
  : LogSelect (al),
    title_ (al.name ("where", al.type_name ()))
{ 
  for (size_t i = 0; i < parameters.size (); i++)
    {
      const symbol id = parameters[i].first;
      const symbol value = parameters[i].second;
      types[id] = Name;
      names[id] = value;
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

static struct LogExternSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LogExtern (al); }

  LogExternSyntax ()
    : DeclareModel (Log::component, "extern", "select", "\
Log simulation state for extern use.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_submodule_sequence ("numbers", Attribute::OptionalState, "\
Inititial numeric values.  By default, none.", 
                                  LogExtern::NumEntry::load_syntax);
    frame.declare_string ("where", Attribute::OptionalConst,
                "Name of the extern log to use.\n\
By default, use the model name.");
    // Disable initial line as it might put "missing" values in
    // initialized flux variables.  TODO: Make 'numbers' be default
    // values, rather than initial values.
    frame.set ("print_initial", false);
  }
} LogExtern_syntax;

// log_extern.C ends here.
