// log_extern.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "log_select.h"
#include "log_extern.h"
#include "select.h"
#include <map>

using namespace std;

typedef map<symbol, const LogExternSource*> log_extern_map_type; 
log_extern_map_type* log_extern_map = NULL;
int log_extern_count = 0;

const LogExternSource& 
LogExternSource::find (const symbol name)
{ 
  daisy_assert (log_extern_map);
  daisy_assert (log_extern_count > 0);
  return *((*log_extern_map)[name]);
}

LogExternSource::LogExternSource (const AttributeList& al)
{ 
  const symbol name 
    = al.check ("where") ? al.identifier ("where") : al.identifier ("type");

  if (!log_extern_map)
    {
      daisy_assert (log_extern_count == 0);
      log_extern_map = new log_extern_map_type;
    }
  (*log_extern_map)[name] = this;
  log_extern_count++;
}

LogExternSource::~LogExternSource ()
{ 
  daisy_assert (log_extern_count > 0);
  log_extern_count--;
  if (log_extern_count == 0)
    {
      delete log_extern_map;
      log_extern_map = NULL;
    }
}


struct LogExtern : public LogSelect,
		   public Destination, 
		   public LogExternSource
{
  // Destination Content.
  typedef map<symbol, type> type_map;
  typedef map<symbol, double> number_map;
  typedef map<symbol, symbol> name_map;
  typedef map<symbol, const vector<double>*> array_map;
  type_map types;
  number_map numbers;
  name_map names;
  array_map arrays;
  
  // Log.
  symbol tag;
  void done (const Time&);

  // No initial line.
  bool initial_match (const Daisy&, Treelog&)
  { return false; }

  // Select::Destination
  void error ();
  void missing ();
  void add (const vector<double>& value);
  void add (const double value);
  void add (const symbol value);

  // LogExternSource
  type lookup (symbol tag) const;
  double number (symbol tag) const;
  symbol name (symbol tag) const;
  const vector<double>& array (symbol tag) const;

  // Create and destroy.
  LogExtern (const AttributeList& al);
  ~LogExtern ();
};

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
LogExtern::add (const vector<double>& value)
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

LogExternSource::type 
LogExtern::lookup (symbol tag) const
{ 
  type_map::const_iterator i = types.find (tag);
  
  if (i == types.end ())
    return Error;

  return (*i).second;
}

double 
LogExtern::number (symbol tag) const
{
  number_map::const_iterator i = numbers.find (tag);
  
  daisy_assert (i != numbers.end ());
  return (*i).second;
}

symbol
LogExtern::name (symbol tag) const
{ 
  name_map::const_iterator i = names.find (tag);
  
  daisy_assert (i != names.end ());
  return (*i).second;
}

const vector<double>&
LogExtern::array (symbol tag) const
{ 
  array_map::const_iterator i = arrays.find (tag);
  
  daisy_assert (i != arrays.end ());
  return *(*i).second;
}

LogExtern::LogExtern (const AttributeList& al)
  : LogSelect (al),
    LogExternSource (al)
{ 
  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->add_dest (this);
}

LogExtern::~LogExtern ()
{ }

static struct LogExternSyntax
{
  static Log& make (const AttributeList& al)
    { return *new LogExtern (al); }

  LogExternSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      LogSelect::load_syntax (syntax, alist);

      syntax.add ("where", Syntax::String, Syntax::OptionalConst,
		  "Name of the extern log to use.\n\
By default, use the model name.");

      Librarian<Log>::add_type ("extern", alist, syntax, &make);
    }
} LogExtern_syntax;

