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

typedef map<string, const LogExternSource*, 
  less<string>/**/> log_extern_map_type; 
log_extern_map_type* log_extern_map = NULL;
int log_extern_count = 0;

const LogExternSource& 
LogExternSource::find (const string& name)
{ 
  daisy_assert (log_extern_map);
  daisy_assert (log_extern_count > 0);
  return *((*log_extern_map)[name]);
}

LogExternSource::LogExternSource (const AttributeList& al)
{ 
  const string name 
    = al.check ("where") ? al.name ("where") : al.name ("type");

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
		   public Select::Destination, 
		   public LogExternSource
{
  // Destination Content.
  typedef map<string, type, less<string>/**/> type_map;
  typedef map<string, double, less<string>/**/> number_map;
  typedef map<string, string, less<string>/**/> name_map;
  typedef map<string, const vector<double>*, less<string>/**/> array_map;
  type_map types;
  number_map numbers;
  name_map names;
  array_map arrays;
  
  // Log.
  void done ();

  // No initial line.
  bool initial_match (const Daisy&, Treelog&)
  { return false; }

  // Select::Destination
  void error (const string& tag);
  void missing (const string& tag);
  void add (const string& tag, const vector<double>& value);
  void add (const string& tag, double value);
  void add (const string& tag, const string& value);

  // LogExternSource
  type lookup (const string& tag) const;
  double number (const string& tag) const;
  const string& name (const string& tag) const;
  const vector<double>& array (const string& tag) const;

  // Create and destroy.
  LogExtern (const AttributeList& al);
  ~LogExtern ();
};

void 
LogExtern::done ()
{ 
  LogSelect::done ();

  if (!is_printing)
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->done (*this);
}

void 
LogExtern::error (const string& tag)
{ 
  types[tag] = Error;
}

void 
LogExtern::missing (const string& tag)
{ 
  types[tag] = Missing;
}

void 
LogExtern::add (const string& tag, const vector<double>& value)
{ 
  types[tag] = Array;
  arrays[tag] = &value;
}

void 
LogExtern::add (const string& tag, double value)
{ 
  types[tag] = Number;
  numbers[tag] = value;
}

void 
LogExtern::add (const string& tag, const string& value)
{ 
  types[tag] = Name;
  names[tag] = value;
}

LogExternSource::type 
LogExtern::lookup (const string& tag) const
{ 
  type_map::const_iterator i = types.find (tag);
  
  if (i == types.end ())
    return Error;

  return (*i).second;
}

double 
LogExtern::number (const string& tag) const
{
  number_map::const_iterator i = numbers.find (tag);
  
  daisy_assert (i != numbers.end ());
  return (*i).second;
}

const string& 
LogExtern::name (const string& tag) const
{ 
  name_map::const_iterator i = names.find (tag);
  
  daisy_assert (i != names.end ());
  return (*i).second;
}

const vector<double>&
LogExtern::array (const string& tag) const
{ 
  array_map::const_iterator i = arrays.find (tag);
  
  daisy_assert (i != arrays.end ());
  return *(*i).second;
}

LogExtern::LogExtern (const AttributeList& al)
  : LogSelect (al),
    LogExternSource (al)
{ }

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

