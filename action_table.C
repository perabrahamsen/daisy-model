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

#define BUILD_DLL

#include "action.h"
#include "metalib.h"
#include "library.h"
#include "daisy.h"
#include "field.h"
#include "am.h"
#include "im.h"
#include "lexer_table.h"
#include "mathlib.h"
#include "librarian.h"
#include <set>
#include <map>
#include <memory>
#include <sstream>

struct ActionTable : public Action
{
  const std::auto_ptr<Action> sow;
  const std::auto_ptr<Action> emerge;
  const std::auto_ptr<Action> harvest;
  const std::auto_ptr<AttributeList> am;

  std::set<Time> sow_dates;
  std::set<Time> emerge_dates;
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

  void doIt (Daisy& daisy, const Scope&, Treelog& msg);
  void tick (const Daisy&, const Scope&, Treelog&);
  void initialize (const Daisy&, const Scope&, Treelog&);
  bool check (const Daisy&, const Scope&, Treelog& err) const;
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
  if (!lex.get_time (entries, time, 8))
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
  if (!lex.get_time (entries, time, 8))
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
  if (!LexerTable::get_time (val, time, 8))
    return;
  
  dates.insert (time);
}

void 
ActionTable::doIt (Daisy& daisy, const Scope& scope, Treelog& msg)
{ 
  if (sow.get () && sow_dates.find (daisy.time) != sow_dates.end ())
    sow->doIt (daisy, scope, msg);
  if (emerge.get () && emerge_dates.find (daisy.time) != emerge_dates.end ())
    emerge->doIt (daisy, scope, msg);
  if (harvest.get () 
      && harvest_dates.find (daisy.time) != harvest_dates.end ())
    harvest->doIt (daisy, scope, msg);
  if ((am.get () 
       || fertilizers.find (daisy.time) != fertilizers.end ())
      && fertilize_events.find (daisy.time) != fertilize_events.end ())
    {
      AttributeList fert ((fertilizers.find (daisy.time) != fertilizers.end ())
                          ? *fertilizers[daisy.time] : *am);

      AM::set_utilized_weight (fert, fertilize_events[daisy.time]);
      if (irrigate_events.find (daisy.time) != irrigate_events.end ())
        {
          double value = irrigate_events[daisy.time];
          std::ostringstream tmp;
          if (iszero (value))
            {
              tmp << "Applying minimum of 0.1 mm\n";
              value = 0.1;
            }
          IM im (fert);
          const double conv = 100 * 100 * 1000; // [g/cm^2] -> [mg/m^2]
          im *= conv / value;   // [mg/l]
          daisy.field->irrigate_subsoil (value, im, -5.0, -25.0, daisy.dt); 
          tmp << "Fertigating " << value << " mm, " 
              << im.NO3 << " ppm NO3 and " << im.NH4 << " ppm NH4";
          msg.message (tmp.str ());
        }
      else
        {
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
              daisy.time.set_alist (new_time);
              fert.add ("creation", new_time);
            }
          daisy.field->fertilize (fert, daisy.dt);
          if (water > 0.0)
            daisy.field->irrigate_surface (water, IM (), daisy.dt);
        }
    }
  else if (irrigate_events.find (daisy.time) != irrigate_events.end ())
    {
      const double value = irrigate_events[daisy.time];
      std::ostringstream tmp;
      IM im;
      daisy.field->irrigate_overhead (value, im, daisy.dt); 
      tmp << "Irrigating " << value << " mm";
      msg.message (tmp.str ());
    }
}

void 
ActionTable::tick (const Daisy& daisy, const Scope& scope, Treelog& msg)
{ 
  if (sow.get ())
    sow->tick (daisy, scope, msg);
  if (emerge.get ())
    emerge->tick (daisy, scope, msg);
  if (harvest.get ())
    harvest->tick (daisy, scope, msg);
}

void 
ActionTable::initialize (const Daisy& daisy, const Scope& scope, Treelog& msg)
{ 
  if (sow.get ())
    sow->initialize (daisy, scope, msg);
  if (emerge.get ())
    emerge->initialize (daisy, scope, msg);
  if (harvest.get ())
    harvest->initialize (daisy, scope, msg);
}

bool 
ActionTable::check (const Daisy& daisy, const Scope& scope, Treelog& msg) const
{
  bool ok = true;
  if (sow.get () && ! sow->check (daisy, scope, msg))
    ok = false;
  if (emerge.get () && ! emerge->check (daisy, scope, msg))
    ok = false;
  if (harvest.get () && ! harvest->check (daisy, scope, msg))
    ok = false;
  return ok;
}

ActionTable::ActionTable (Block& al)
  : Action (al),
    sow (al.check ("sow") 
         ? Librarian::build_item<Action> (al, "sow")
         : NULL),
    emerge (al.check ("emerge") 
         ? Librarian::build_item<Action> (al, "emerge")
         : NULL),
    harvest (al.check ("harvest") 
             ? Librarian::build_item<Action> (al, "harvest")
             : NULL),
    am (al.check ("fertilizer") 
        ? new AttributeList (al.alist ("fertilizer")) 
        : NULL)
{ 
  LexerTable lex (al);
  if (!lex.read_header (al.msg ()))
    {
      al.error ("Read failed");
      return;
    }
  const int harvest_c = harvest.get () ? lex.find_tag ("Harvest") : -1;
  const int sow_c = sow.get () ? lex.find_tag ("Planting") : -1;
  const int emerge_c = emerge.get () ? lex.find_tag ("Emerging") : -1;
  const int irrigate_c = al.flag ("enable_irrigation") 
    ? lex.find_tag ("Irrigate") : -1;
  const int fertilizer_c = lex.find_tag ("Fertilizer");
  const int fertilize_c = (al.flag ("enable_fertilization") 
                           && (am.get () || fertilizer_c >= 0))
    ? lex.find_tag ("Fertilize") : -1;
  
  if (sow_c < 0 && emerge_c < 0 && harvest_c < 0 
      && irrigate_c < 0 && fertilize_c < 0)
    al.msg ().warning ("No applicable column found");

  if (sow_c < 0 && sow.get ())
    al.msg ().warning ("Specified planting operation not used");
  if (emerge_c < 0 && emerge.get ())
    al.msg ().warning ("Specified emerge operation not used");
  if (harvest_c < 0 && harvest.get ())
    al.msg ().warning ("Specified harvest operation not used");
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
      read_alist (lex, entries, fertilizer_c, 
                  al.metalib ().library (AM::component),
                  fertilizers);
      read_date (lex, entries, sow_c, sow_dates);
      read_date (lex, entries, emerge_c, emerge_dates);
      read_date (lex, entries, harvest_c, harvest_dates);
    }

  if (sow_dates.size () == 0 
      && emerge_dates.size () == 0 
      && harvest_dates.size () == 0
      && fertilize_events.size () == 0
      && irrigate_events.size () == 0)
    al.msg ().warning ("Nothing to do");
}

// Add the ActionTable syntax to the syntax table.
static struct ActionTableSyntax
{
  static Model& make (Block& al)
  { return *new ActionTable (al); }

  ActionTableSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    LexerTable::load_syntax (syntax, alist);
    alist.add ("description", "\
Read management actions from a Daisy data file.\n\
\n\
After the ddf header, the following column tags are recognized (with\n\
the dimension for the dimension linein square brackets).\n\
\n\
Date [date]: The date for fertilization or irrigation.\n\
\n\
Planting [date]: The content should be a date in yyyy-mm-dd format,\n\
where the operation specified by the 'sow' attribute will be perfomed.\n\
\n\
Emerging [date]: The content should be a date in yyyy-mm-dd format,\n\
where the operation specified by the 'emerge' attribute will be perfomed.\n\
\n\
Harvest [date]: The content should be a date in yyyy-mm-dd format,\n\
where the operation specified by the 'harvest' attribute will be\n\
perfomed.\n\
\n\
Irrigate [mm]: The content should be an irrigation amount, that will\n\
be applied as overhead irrigation for the date specified in the 'Date'\n\
field.  You can disable it with the 'enable_irrigation' attribute.\n\
\n\
Fertilize [kg N/ha]: The content should be an amount of nitrogen\n\
fertilizer to be applied on the date specified in the 'Date' field.\n\
The fertilizer type will be either the one specified in the\n\
'Fertilizer' column, or the 'fertilizer' attribute.  You can disable\n\
it with the 'enable_fertilization' attribute.\n\
\n\
Fertilizer [name]: The type of fertilizer to be applied.");
    syntax.add_object ("sow", Action::component, 
                       Syntax::OptionalConst, Syntax::Singleton, 
                       "Sow action.");
    syntax.add_object ("emerge", Action::component, 
                       Syntax::OptionalConst, Syntax::Singleton, 
                       "Emerge action.");
    syntax.add_object ("harvest", Action::component, 
                       Syntax::OptionalConst, Syntax::Singleton, 
                       "Harvest action.");
    syntax.add_object ("fertilizer", AM::component,
                       Syntax::OptionalConst, Syntax::Singleton, "\
The fertilizer you want to apply.");
    syntax.add ("enable_irrigation", Syntax::Boolean, Syntax::Const, "\
Set this to false to ignore any irrigation information in the file.");
    alist.add ("enable_irrigation", true);
    syntax.add ("enable_fertilization", Syntax::Boolean, Syntax::Const, "\
Set this to false to ignore any fertilization information in the file.");
    alist.add ("enable_fertilization", true);
    Librarian::add_type (Action::component, "table", alist, syntax, &make);
  }
} ActionTable_syntax;

