// log_select.C
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

bool 
LogSelect::check (const string& name) const
{ 
  if (!is_active)
    return false;

  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->valid (name))
      return true;

  return false;
}

bool 
LogSelect::check_derived (const string& field, const string& /* name */,
			  const Library& /* library */) const
{ 
  if (!check (field))
    return false;

#if 0
  open (field);
  bool ok = check_entry (name, library);
  close (field);
  return ok;
#else
  return true;
#endif
}

bool 
LogSelect::match (const Daisy& daisy)
{
  condition.tick (daisy);
  is_printing = condition.match (daisy);
  is_active = is_printing;

  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->match (daisy, is_printing))
      is_active = true;

  return is_active;
}

void 
LogSelect::open_maybe (const string& value)
{ 
  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->open_maybe (value);
}

void 
LogSelect::close_maybe ()
{ 
  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->close_maybe ();
}

void 
LogSelect::open (const string& name)
{ 
  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->open_group (name);
}

void 
LogSelect::close ()
{ 
  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->close ();
}

void 
LogSelect::open_unnamed ()
{ }

void 
LogSelect::close_unnamed ()
{ }

void 
LogSelect::open_derived (const string& field, const string& type)
{ open (field); open (type); }

void 
LogSelect::close_derived ()
{ close (); close (); }

void 
LogSelect::open_entry (const string& type, const AttributeList&)
{ open (type); }

void 
LogSelect::close_entry ()
{ close (); }

void 
LogSelect::output (const string& name, const Time& value)
{ 
  if (is_active)
    for (unsigned int i = 0; i < entries.size (); i++)
      entries[i]->output_time (name, value);
}

void 
LogSelect::output (const string&, const bool)
{ }

void 
LogSelect::output (const string& name, const double value)
{ 
  if (is_active)
    for (unsigned int i = 0; i < entries.size (); i++)
      entries[i]->output_number (name, value);
}

void 
LogSelect::output (const string& name, const int value)
{ 
  if (is_active)
    for (unsigned int i = 0; i < entries.size (); i++)
      entries[i]->output_integer (name, value);
}

void 
LogSelect::output (const string& name, const string& value)
{ 
  if (is_active)
    for (unsigned int i = 0; i < entries.size (); i++)
      entries[i]->output_name (name, value);
}

void 
LogSelect::output (const string& name, const vector<double>& value)
{ 
  if (is_active)
    for (unsigned int i = 0; i < entries.size (); i++)
      entries[i]->output_array (name, value, geometry ());
}

void 
LogSelect::output (const string&, const PLF&)
{ }

bool 
LogSelect::check (const Syntax&, Treelog&) const
{ return true; }

LogSelect::LogSelect (const AttributeList& al)
  : Log (al),
    description (al.name ("description")),
    condition (Librarian<Condition>::create (al.alist ("when"))),
    entries (map_create<Select> (al.alist_sequence ("entries")))
{
  // Create path convertion map.
  const vector<string>& conv_vector = al.name_sequence ("set");
  string_map conv_map;
  for (unsigned int i = 0; i < conv_vector.size (); i += 2)
    {
      assert (i+1 < conv_vector.size ());
      conv_map[conv_vector[i]] = conv_vector[i+1];
    }

  // Find default range.
  const double from  = al.number ("from");
  const double to = al.number ("to");

      // Initialize entries.
  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->initialize (conv_map, from, to, condition.timestep ());
}

  
LogSelect::~LogSelect ()
{
  delete &condition;
  sequence_delete (entries.begin (), entries.end ());
}

static bool check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  if ((al.size ("set") % 2) == 1)
    {
      err.entry ("'set' should contain an even number of arguments");
      ok = false;
    }

  return ok;
}

void 
LogSelect::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist);
  syntax.add ("description", Syntax::String, Syntax::Const,
	      "Description of this log file format.");
  alist.add ("description", "\
Each selected variable is represented by a column in the log.");
  syntax.add ("when", Librarian<Condition>::library (), 
	      "Add entries to the log file when this condition is true.");
  syntax.add ("entries", Librarian<Select>::library (), 
	      Syntax::Sequence,
	      "What to log in each column.");
  syntax.add ("set", Syntax::String, Syntax::Const, Syntax::Sequence, 
	      "Map path names in the entries.\n\
The first entry in the sequence is a symbol from the paths (e.g. $crop),\n\
and the second is the value to replace the symbol with (e.g. Grass).\n\
The third entry is another symbol to replace, and the fourth is another\n\
value to replace it with.  And so forth.");
  const vector<string> empty_string_vector;
  alist.add ("set", empty_string_vector);
  syntax.add ("from", "cm", Syntax::Const,
	      "Default 'from' value for all entries.");
  alist.add ("from", 0.0);
  syntax.add ("to", "cm", Syntax::Const,
	      "Default 'to' value for all entries.");
  alist.add ("to", 1.0);
}
