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
  std::map<Time, const AttributeList*> fertilizers;

  static void read_alist (const LexerTable&,
                          const std::vector<std::string>& entries, 
                          int tag_c, const Library& library,
                          std::map<Time, const AttributeList*>& alists);
  static void read_event (const LexerTable&,
                          const std::vector<std::string>& entries, 
                          int tag_c, std::map<Time, double>& events);
  static void read_date (const LexerTable&,
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
ActionTable::read_alist (const LexerTable& lex,
                         const std::vector<std::string>& entries, 
                         int tag_c, const Library& library,
                         std::map<Time, const AttributeList*>& alists)
{
  if (tag_c < 0)
    return;

  const std::string val = entries[tag_c];

  if (lex.is_missing (val))
    return;

  if (!library.check (symbol (val)))
    {
      lex.error ("'" + val + "' undefined");
      return;
    }
  Time time (9999, 1, 1, 0);
  if (!lex.get_time (entries, time))
    return;

  alists[time] = &library.lookup (symbol (val));
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
  if (crop.get () && sow_dates.find (daisy.time) != sow_dates.end ())
    {
      const std::string name =  crop->name ("type");

      msg.message (std::string ("Sowing ") + name);      
      daisy.field.sow (msg, *crop); 
    }
  if (crop.get () && harvest_dates.find (daisy.time) != harvest_dates.end ())
    {
      const std::string name =  crop->name ("type");

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
  if ((am.get () 
       || fertilizers.find (daisy.time) != fertilizers.end ())
      && fertilize_events.find (daisy.time) != fertilize_events.end ())
    {
      AttributeList fert ((fertilizers.find (daisy.time) != fertilizers.end ())
                          ? *fertilizers[daisy.time] : *am);

      AM::set_utilized_weight (fert, fertilize_events[daisy.time]);

      double water = 0.0;
      const std::string syntax = fert.name ("syntax");
      std::ostringstream tmp;
      if (syntax == "mineral")
        tmp << "Fertilizing " << fert.number ("weight") 
            << " kg "<< fert.name ("type") << "-N/ha";
      else if (syntax == "organic")
        {
          tmp  << "Fertilizing " << fert.number ("weight") 
               << " ton "<< fert.name ("type") << " ww/ha";
          const double utilized_weight = AM::utilized_weight (fert);
          if (utilized_weight > 0.0)
            tmp << "; utilized " << utilized_weight << " kg N/ha";
          water = AM::get_water (fert);
          if (water > 0.0)
            tmp << "; water " << water << " mm";
        }
      else
        tmp << "Fertilizing " << fert.name ("type");
      msg.message (tmp.str ());
      if (syntax != "mineral")
        {
          AttributeList new_time;
          new_time.add ("year", daisy.time.year ());
          new_time.add ("month", daisy.time.month ());
          new_time.add ("mday", daisy.time.mday ());
          new_time.add ("hour", daisy.time.hour ());
          fert.add ("creation", new_time);
        }
      daisy.field.fertilize (fert);
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
    am (al.check ("fertilizer") ? new AttributeList (al.alist ("fertilizer")) : NULL)
{ 
  LexerTable lex (al);
  if (!lex.read_header (al.msg ()))
    {
      al.error ("Read failed");
      return;
    }
  const int harvest_c = crop.get () ? lex.find_tag ("Harvest") : -1;
  const int sow_c = crop.get () ? lex.find_tag ("Planting") : -1;
  const int irrigate_c = al.flag ("enable_irrigation") 
    ? lex.find_tag ("Irrigate") : -1;
  const int fertilizer_c = lex.find_tag ("Fertilizer");
  const int fertilize_c = (am.get () || fertilizer_c >= 0)
    ? lex.find_tag ("Fertilize") : -1;
  
  if (sow_c < 0 && harvest_c < 0 && irrigate_c < 0 && fertilize_c < 0)
    al.msg ().warning ("No applicable column found");

  if (sow_c < 0 && harvest_c < 0 && crop.get ())
    al.msg ().warning ("Specified crop not use");
  if (fertilize_c < 0 && am.get ())
    al.msg ().warning ("Specified fertilizer not used");
  if (fertilizer_c >= 0 && am.get ())
    al.msg ().warning ("Fertilizer specified twice");

  while (lex.good ())
    {
      // Read entries.
      std::vector<std::string> entries;
      if (!lex.get_entries (entries))
        continue;

      read_event (lex, entries, irrigate_c, irrigate_events);
      read_event (lex, entries, fertilize_c, fertilize_events);
      read_alist (lex, entries, fertilizer_c, Librarian<AM>::library (),
                  fertilizers);
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
    syntax.add ("fertilizer", Librarian<AM>::library (),
                Syntax::OptionalConst, Syntax::Singleton, "\
The fertilizer you want to apply.");
    syntax.add ("enable_irrigation", Syntax::Boolean, Syntax::Const, "\
Set this to false to ignore any irrigation information in the file.");
    alist.add ("enable_irrigation", true);
    Librarian<Action>::add_type ("table", alist, syntax, &make);
  }
} ActionTable_syntax;

