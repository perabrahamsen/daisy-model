// wsource_table.C -- Weather data read from table.
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

#include "wsource_table.h"
#include "weatherdata.h"
#include "units.h"
#include "librarian.h"
#include "assertion.h"
#include "mathlib.h"
#include <sstream>

symbol 
WSourceTable::title () const
{ return lex.title (); }

double 
WSourceTable::lookup_month (const Time& time, const symbol key, 
                            const double default_value) const
{
  // Attribute.
  if (super::check (key))
    return lookup_month (time, super::number_sequence (key)); 

  // Keyword.
  if (keywords.check (key))
    return lookup_month (time, keywords.number_sequence (key));

  // No modification.
  return default_value;
}

double 
WSourceTable::lookup_month (const Time& time,
                            const std::vector<double>& numbers) const
{
  if (numbers.size () == 1)
    // One factor for all months.
    return numbers[0];
  
  // Monthly correction factors.
  daisy_assert (numbers.size () == 12);

  return numbers[time.month () - 1];
}

double 
WSourceTable::precip_correct (const Time& time) const
{
  return lookup_month (time,  Weatherdata::PrecipCorrect (), 1.0)
    * lookup_month (time,  Weatherdata::PrecipScale (), 1.0);
}

double 
WSourceTable::globrad_scale (const Time& time) const
{
  return lookup_month (time,  Weatherdata::GlobRadScale (), 1.0);
}

double 
WSourceTable::temp_offset (const Time& time) const
{ return lookup_month (time,  Weatherdata::TempOffset (), 0.0); }

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
WSourceTable::raw_number (const symbol key) const
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

double 
WSourceTable::number (const symbol key) const
{ 
  const double raw = raw_number (key);

  if (key == Weatherdata::Precip ())
    return raw * precip_correct (timestep_begin);
  else if (key == Weatherdata::AirTemp ()
           || key == Weatherdata::T_min ()
           || key == Weatherdata::T_max ())
    return raw + temp_offset (timestep_begin);
  else if (key == Weatherdata::GlobRad ())
    return raw * globrad_scale (timestep_begin);
  
  return raw;
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

double 
WSourceTable::timestep () const
{ return timestep_hours; }

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
WSourceTable::raw_end_number (const symbol key) const
{
  // Attribute.
  if (super::check (key))
    return super::number (key); 

  // Table.
  if (ok)
    {
      std::map<symbol, double>::const_iterator i = next_values.find (key);
      if (i != next_values.end () && std::isfinite (i->second))
        return i->second;
    }

  // Keyword.
  return keywords.number (key);
}

double 
WSourceTable::end_number (const symbol key) const
{ 
  const double raw = raw_end_number (key);

  // If timestep begin and ends in different months, we want the
  // correction factor for the timestep where it begins.
  if (key == Weatherdata::Precip ())
    return raw * precip_correct (timestep_begin);
  else if (key == Weatherdata::AirTemp ()
           || key == Weatherdata::T_min ()
           || key == Weatherdata::T_max ())
    return raw + temp_offset (timestep_begin);
  else if (key == Weatherdata::GlobRad ())
    return raw * globrad_scale (timestep_begin);

  return raw;
}

symbol 
WSourceTable::end_name (const symbol key) const
{ return name (key); }

const Time& 
WSourceTable::data_begin () const
{ return my_data_begin; }

const Time& 
WSourceTable::data_end () const
{ return my_data_end; }

const Time& 
WSourceTable::begin () const
{ 
  if (ok)
    return timestep_begin;
  
  return data_begin ();
}

const Time& 
WSourceTable::end () const
{ 
  if (ok)
    return timestep_end;
  
  return data_end ();
}

void 
WSourceTable::read_line ()
{ 
  // Get entries.
  std::vector<std::string> entries;
  bool date_only;
  if (!lex.get_entries (entries)
      || !lex.get_time_do (entries, timestep_end, date_only))
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
              const double value = units.convert (lex.dimension (col),
                                                  dimension (key),
                                                  old_val);
              next_values[key] = value;
              if (value < Weatherdata::min_value (key))
                {
                  std::ostringstream tmp;
                  tmp << "Value for '" << key << "' is " << value
                      << ", expected it to be more than "
                      << Weatherdata::min_value (key);
                  lex.warning (tmp.str ());
                }
              if (value > Weatherdata::max_value (key))
                {
                  std::ostringstream tmp;
                  tmp << "Value for '" << key << "' is " << value
                      << ", expected it to be less than "
                      << Weatherdata::max_value (key);
                  lex.warning (tmp.str ());
                }
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
WSourceTable::source_tick (Treelog& msg)
{ 
  Treelog::Open nest (msg, __FUNCTION__);

  if (!ok)
    return;

  values = next_values;
  timestep_begin = timestep_end; // Ensure we enter the loop.
  while (timestep_end <= timestep_begin)
    {
      read_line ();             // Overwrites timestep_end.
      if (!ok)
        return;
    }
  timestep_hours = Time::fraction_hours_between (timestep_begin, timestep_end);
  daisy_assert (timestep_hours > 0.0);

  if (done ())
    msg.message ("source done");
}
  
void 
WSourceTable::source_initialize (Treelog& msg) 
{ 
  Treelog::Open nest (msg, __FUNCTION__);

  // Read header.
  ok = true;
  if (!lex.read_header_with_keywords (keywords, msg))
    {
      msg.error ("Can't read weather file");
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

  // Data time.
  const Time& super_end = super::data_end ();
  if (super_end == Time::null () && keywords.check (Weatherdata::End ()))
    my_data_end = Time (keywords.submodel (Weatherdata::End ()));
  else
    my_data_end = super_end;

  const Time& super_begin = super::data_begin ();
  if (super_begin == Time::null () && keywords.check (Weatherdata::Begin ()))
    my_data_begin = Time (keywords.submodel (Weatherdata::Begin ()));
  else
    my_data_begin = super_begin;
  
  // Read first data.
  timestep_end = my_data_begin;
  read_line ();
  source_tick (msg);
}

bool 
WSourceTable::source_check (Treelog&) const
{ return ok; }

void 
WSourceTable::rewind (const Time& time, Treelog& msg)
{
  timestep_end = my_data_begin;
  read_line ();
  source_tick (msg);
  super::rewind (time, msg);
}

void
WSourceTable::skip_ahead (const Time& begin, Treelog& msg)
{
  while (timestep_end < begin && ok)
    read_line ();
}

WSourceTable::WSourceTable (const BlockModel& al)
  : WSourceBase (al),
    units (al.units ()),
    lex (al),
    ok (false),
    keywords (*Librarian::submodel_frame (Weatherdata::load_syntax), 
              Frame::parent_link),
    my_data_begin (Time::null ()),
    my_data_end (Time::null ()),
    timestep_begin (Time::null ()),
    timestep_end (Time::null ())
{ }

WSourceTable::~WSourceTable ()
{ }

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
