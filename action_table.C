// action_table.C
// 
// Copyright 2005 Per Abrahamsen and KVL.
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


#include "action.h"
#include "daisy.h"
#include "field.h"
#include "crop.h"
#include "am.h"
#include "im.h"
#include "lexer_table.h"
#include <set>
#include <map>
#include <memory>
#include <sstream>

struct ActionTable : public Action
{
  const std::auto_ptr<AttributeList> crop;
  const std::auto_ptr<AttributeList> am;

  std::set<Time> sow_dates;
  std::set<Time> harvest_dates;
  std::map<Time, double> fertilize_events;
  std::map<Time, double> irrigate_events;

  void read_event (const LexerTable&,
                   const std::vector<std::string>& entries, 
                   int tag_c, std::map<Time, double>& events);
  void read_date (const LexerTable&,
                  const std::vector<std::string>& entries, 
                  int rag_c, std::set<Time>& dates);

  void doIt (Daisy& daisy, Treelog& msg);
  bool check (const Daisy&, Treelog& err) const;
  ActionTable (Block& al);
};

void 
ActionTable::read_event (const LexerTable& lex,
                         const std::vector<std::string>& entries, 
                         int tag_c, std::map<Time, double>& events)
{
  if (tag_c < 0)
    return;

  const std::string val = entries[tag_c];

  if (lex.is_missing (val))
    return;

  Time time (9999, 1, 1, 0);
  if (!lex.get_time (entries, time))
    return;

  const double value = lex.convert_to_double (val);
  events[time] = value;
}

void 
ActionTable::read_date (const LexerTable& lex,
                        const std::vector<std::string>& entries, 
                        int tag_c, std::set<Time>& dates)
{
  if (tag_c < 0)
    return;

  const std::string val = entries[tag_c];

  if (lex.is_missing (val))
    return;

  Time time (9999, 1, 1, 0);
  if (!LexerTable::get_time (val, time))
    return;
  
  dates.insert (time);
}

void 
ActionTable::doIt (Daisy& daisy, Treelog& msg)
{ 
  const std::string name =  crop->name ("type");

  if (crop.get () && sow_dates.find (daisy.time) != sow_dates.end ())
    {
      msg.message (std::string ("Sowing ") + name);      
      daisy.field.sow (msg, *crop); 
    }
  if (crop.get () && harvest_dates.find (daisy.time) != harvest_dates.end ())
    {
      if (daisy.field.crop_ds (symbol (name)) < 0.0)
        {
          msg.warning ("Attempting to harvest " + name 
                       + " which has not emerged on the field");
        }
      else
        {
          daisy.field.harvest (daisy.time, symbol (name),
                               1.0, 1.0, 1.0, 1.0, false,
                               daisy.harvest, msg);

          if (daisy.field.crop_ds (symbol (name)) < 0.0)
            msg.message ("Harvesting " + name);
          else
            msg.message ("Cutting " + name);
        }
    }
  if (am.get () 
      && fertilize_events.find (daisy.time) != fertilize_events.end ())
    {
      AM::set_utilized_weight (*am, fertilize_events[daisy.time]);

      double water = 0.0;
      const std::string syntax = am->name ("syntax");
      std::ostringstream tmp;
      if (syntax == "mineral")
        tmp << "Fertilizing " << am->number ("weight") 
            << " kg "<< am->name ("type") << "-N/ha";
      else if (syntax == "organic")
        {
          tmp  << "Fertilizing " << am->number ("weight") 
               << " ton "<< am->name ("type") << " ww/ha";
          const double utilized_weight = AM::utilized_weight (*am);
          if (utilized_weight > 0.0)
            tmp << "; utilized " << utilized_weight << " kg N/ha";
          water = AM::get_water (*am);
          if (water > 0.0)
            tmp << "; water " << water << " mm";
        }
      else
        tmp << "Fertilizing " << am->name ("type");
      msg.message (tmp.str ());
      if (syntax != "mineral")
        {
          AttributeList new_time;
          new_time.add ("year", daisy.time.year ());
          new_time.add ("month", daisy.time.month ());
          new_time.add ("mday", daisy.time.mday ());
          new_time.add ("hour", daisy.time.hour ());
          am->add ("creation", new_time);
        }
      daisy.field.fertilize (*am);
      if (water > 0.0)
        daisy.field.irrigate_surface (water, IM ());
    }
  if (irrigate_events.find (daisy.time) != irrigate_events.end ())
    {
      const double value = irrigate_events[daisy.time];
      std::ostringstream tmp;
      IM im;
      daisy.field.irrigate_overhead (value, im); 
      tmp << "Irrigating " << value << " mm";
      msg.message (tmp.str ());
    }
}

bool 
ActionTable::check (const Daisy&, Treelog&) const
{
  bool ok = true;
  return ok;
}

ActionTable::ActionTable (Block& al)
  : Action (al),
    crop (al.check ("crop") ? new AttributeList (al.alist ("crop")) : NULL),
    am (al.check ("am") ? new AttributeList (al.alist ("am")) : NULL)
{ 
  LexerTable lex (al);
  if (!lex.read_header (al.msg ()))
    {
      al.error ("Read failed");
      return;
    }
  const int harvest_c = lex.find_tag ("Harvest");
  const int sow_c = lex.find_tag ("Sow");
  const int irrigate_c = lex.find_tag ("Irrigate");
  const int fertilize_c = lex.find_tag ("Fertilize");
  
  if (sow_c < 0 && harvest_c < 0 && irrigate_c < 0 && fertilize_c < 0)
    al.msg ().warning ("No applicable column found");

  if (sow_c < 0 && harvest_c < 0 && crop.get ())
    al.msg ().warning ("Specified crop not use");
  if (sow_c >= 0 && !crop.get ())
    al.msg ().warning ("No crop to sow");
  if (harvest_c >= 0 && !crop.get ())
    al.msg ().warning ("No crop to harvest");
  if (fertilize_c < 0 && am.get ())
    al.msg ().warning ("Specified fertilizer not used");
  if (fertilize_c >= 0 && !am.get ())
    al.msg ().warning ("No fertilizer to use");

  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      if (!lex.get_entries (entries))
        continue;

      read_event (lex, entries, irrigate_c, irrigate_events);
      read_event (lex, entries, fertilize_c, fertilize_events);
      read_date (lex, entries, sow_c, sow_dates);
      read_date (lex, entries, harvest_c, harvest_dates);
    }

  if (sow_dates.size () == 0 
      && harvest_dates.size () == 0
      && fertilize_events.size () == 0
      && irrigate_events.size () == 0)
    al.msg ().warning ("Nothing to do");
}

// Add the ActionTable syntax to the syntax table.
static struct ActionTableSyntax
{
  static Action& make (Block& al)
  { return *new ActionTable (al); }

  ActionTableSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    LexerTable::load_syntax (syntax, alist);
    alist.add ("description", "Sow a crop on the field.");
    syntax.add ("crop", Librarian<Crop>::library (), 
                Syntax::OptionalConst, Syntax::Singleton, 
                "Crop to sow.");
    syntax.add ("am", Librarian<AM>::library (),
                Syntax::OptionalConst, Syntax::Singleton, "\
The fertilizer you want to apply.");
    Librarian<Action>::add_type ("table", alist, syntax, &make);
  }
} ActionTable_syntax;

