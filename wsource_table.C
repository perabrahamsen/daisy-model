// wsource_table.C -- Weather data that never changes.
// 
// Copyright 2010 KU
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

#include "wsource_base.h"
#include "weatherdata.h"
#include "time.h"
#include "lexer_table.h"
#include "units.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "assertion.h"
#include "mathlib.h"
#include <map>
#include <sstream>

struct WSourceTable : public WSourceBase
{
  typedef WSourceBase super;
  const Units& units;
  LexerTable lex;
  bool ok;
  FrameSubmodelValue keywords;

  std::map<symbol, size_t> columns;
  std::map<symbol, double> values;
  std::map<symbol, double> next_values;
  Time timestep_begin;
  Time timestep_end;

  // Scope.
  bool check (const symbol key) const;
  double number (const symbol key) const;
  symbol name (const symbol key) const;

  // WSource.
  bool end_check (const symbol key) const;
  double end_number (const symbol key) const;
  symbol end_name (const symbol key) const;
  const Time& begin () const;
  const Time& end () const;
  void read_line ();
  void tick ();
  bool done () const
  { return !ok; }

  void initialize (Treelog& msg) ;
  bool check (Treelog&) const
  { return ok; }
  WSourceTable (const BlockModel& al)
    : WSourceBase (al),
      units (al.units ()),
      lex (al),
      ok (false),
      keywords (*Librarian::submodel_frame (Weatherdata::load_syntax), 
                Frame::parent_link),
      timestep_begin (Time::null ()),
      timestep_end (Time::null ())
  { }
  ~WSourceTable ()
  { }
};

bool 
WSourceTable::check (const symbol key) const
{ 
  // Table.
  if (ok)
    {
      std::map<symbol, double>::const_iterator i = values.find (key);
      if (i != values.end () && std::isfinite (i->second))
        return true;
    }
  
  // Attribute.
  if (super::check (key))
    return true;

  // Keyword.
  return keywords.check (key);

}

double 
WSourceTable::number (const symbol key) const
{ 
  // Table.
  if (ok)
    {
      std::map<symbol, double>::const_iterator i = values.find (key);
      if (i != values.end () && std::isfinite (i->second))
        return i->second;
    }

  // Attribute.
  if (super::check (key))
    return super::number (key); 

  // Keyword.
  return keywords.number (key);
}

symbol
WSourceTable::name (const symbol key) const
{ 
  // Table not suported.

  // Attribute.
  if (super::check (key))
    return super::name (key); 

  // Keyword.
  return keywords.name (key);
}

bool 
WSourceTable::end_check (const symbol key) const
{ 
  // Table.
  if (ok)
    {
      std::map<symbol, double>::const_iterator i = next_values.find (key);
      if (i != next_values.end () && std::isfinite (i->second))
        return true;
    }
  
  // Attribute.
  if (super::check (key))
    return true;

  // Keyword.
  return keywords.check (key);

}

double 
WSourceTable::end_number (const symbol key) const
{
  // Table.
  if (ok)
    {
      std::map<symbol, double>::const_iterator i = next_values.find (key);
      if (i != next_values.end () && std::isfinite (i->second))
        return i->second;
    }

  // Attribute.
  if (super::check (key))
    return super::number (key); 

  // Keyword.
  return keywords.number (key);
}

symbol 
WSourceTable::end_name (const symbol key) const
{ return name (key); }

const Time& 
WSourceTable::begin () const
{ 
  if (ok)
    return timestep_begin;
  
  return super::begin ();
}

const Time& 
WSourceTable::end () const
{ 
  if (ok)
    return timestep_end;
  
  return super::end ();
}

void 
WSourceTable::read_line ()
{ 
  // Get entries.
  std::vector<std::string> entries;
  bool date_only;
  if (!lex.get_entries (entries)
      || !lex.get_time (entries, timestep_end, date_only))
    {
      lex.warning ("No more weather data.");
      ok = false;
      for (std::map<symbol, double>::iterator i = next_values.begin ();
           i != next_values.end ();
           i++)
        i->second = NAN;
      return;
    };
  if (date_only)                // End of day.
    timestep_end.tick_day (1);

  // Convert entries.
  for (std::map<symbol, size_t>::iterator i = columns.begin ();
       i != columns.end ();
       i++)
    {
      const symbol key = i->first;
      const size_t col = i->second;
      const std::string& entry = entries[col];
      if (lex.is_missing (entry))
        next_values[key] = NAN;
      else
        {
          double old_val = lex.convert_to_double (entry);
          if (units.can_convert (lex.dimension (col),
                                 dimension (key),
                                 old_val))
            {
              next_values[key] = units.convert (lex.dimension (col),
                                                dimension (key),
                                                old_val);
            }
          else
            {
              std::ostringstream tmp;
              tmp << "Can't convert '" << key << "' value of " << old_val
                  << " [" << lex.dimension (col) << "] to [" 
                  << dimension (key) << "]";
              lex.warning (tmp.str ());
            }
        }
    }
}

void 
WSourceTable::tick ()
{ 
  if (!ok)
    return;

  timestep_begin = timestep_end;
  values = next_values;
  read_line ();
}
  
void 
WSourceTable::initialize (Treelog& msg) 
{ 
  Treelog::Open nest (msg, __FUNCTION__);

  // Read header.
  ok = true;
  if (!lex.read_header_with_keywords (keywords, msg))
    {
      ok = false;
      return;
    }

  // Extract tags.
  const std::vector<symbol>& tags = lex.tag_names ();
  for (size_t i = 0; i < tags.size (); i++)
    {
      const symbol tag = tags[i];
      if (lex.is_time (tag))
        continue;
      if (Weatherdata::dimension (tag) == Attribute::Unknown ())
        {
          msg.warning ("Unknown tag '" + tag + "' ignored");
          continue;
        }
      columns[tag] = i;
    }
  
  // Read first data.
  read_line ();
  tick ();
}

static struct WSourceTableSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceTable (al); }
  WSourceTableSyntax ()
    : DeclareModel (WSource::component, "table", "base",
                    "Read weather data from a file.")
  { }
  void load_frame (Frame& frame) const
  { 
    LexerTable::load_syntax (frame);
  }
} WSourceTable_syntax;

// wsource_table.C ends here.
