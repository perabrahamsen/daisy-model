// wsource_combine.C -- Combine multiple sources of weather data.
// 
// Copyright 2011 KU
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

#include "wsource_weather.h"
#include "weatherdata.h"
#include "time.h"
#include "memutils.h"
#include "librarian.h"
#include "submodeler.h"
#include "block_model.h"
#include "vcheck.h"
#include "assertion.h"
#include "mathlib.h"
#include <boost/scoped_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <sstream>

struct WSourceCombine : public WSourceWeather
{
  const boost::scoped_ptr<WSource> reserve;

  // Const.
  static symbol Any ();

  // Entries.
  struct Entry : private boost::noncopyable
  {
    boost::scoped_ptr<WSource> source;
    Time begin;
    Time end;
    const std::set<symbol> use;
    static bool check_alist (const Metalib&, const Frame& al, Treelog&);
    static void load_syntax (Frame&);
    static std::set<symbol> vector2set (const std::vector<symbol>& v);
    Entry (const BlockSubmodel&);
    ~Entry ()
    { }
  };
  auto_vector<Entry*> entry;
  const WSource& find_source (const symbol key) const;
  const WSource& find_end (const symbol key) const;
  const WSource& find_meta (const symbol key, const symbol meta) const;
  const WSource& find_meta_end (const symbol key, const symbol meta) const;

  // Scope.
  void entries (std::set<symbol>&) const;
  Attribute::type lookup (const symbol key) const
  { return find_source (key).lookup (key); }
  symbol dimension (const symbol key) const
  { return find_source (key).dimension (key); }
  symbol description (const symbol key) const
  { return find_source (key).description (key); }
  int type_size (const symbol key) const
  { return find_source (key).type_size (key); }
  bool check (const symbol key) const
  { return find_source (key).check (key); }
  double number (const symbol key) const
  { return find_source (key).number (key); }
  int value_size (const symbol key) const
  { return find_source (key).value_size (key); }
  symbol name (const symbol key) const
  { return find_source (key).name (key); }
  
  // Timestep.
  Time my_data_begin;
  Time my_data_end;
  Time my_begin;
  Time my_end;
  double my_timestep;
  const Time& data_begin () const // Start of first timestep.
  { return my_data_begin; }
  const Time& data_end () const   // End of last timestep.
  { return my_data_end; }
  const Time& begin () const         // Start of timestep.
  { return my_begin; }
  const Time& end () const           // End of timestep.
  { return my_end; }
  double timestep () const           // Length of timetstep [h]
  { return my_timestep; }

  // End of timstep.
  bool end_check (symbol key) const
  { return find_end (key).end_check (key); }
  double end_number (symbol key) const
  { return find_end (key).end_number (key); }
  symbol end_name (symbol key) const
  { return find_end (key).end_name (key); }
  
  // Number sequences.
  const std::vector<double>& number_sequence (const symbol key) const
  { return find_source (key).number_sequence (key); }
  const std::vector<double>& end_number_sequence (const symbol key) const
  { return find_end (key).end_number_sequence (key); }
  
  // Meta information.
  double meta_timestep (const symbol key) const
  { return find_source (key).timestep (); }
  bool meta_check (const symbol key, const symbol meta) const
  { return find_meta (key, meta).check (meta); }
  double meta_number (const symbol key, const symbol meta) const
  { return find_meta (key, meta).number (meta); }
  symbol meta_name (const symbol key, const symbol meta) const
  { return find_meta (key, meta).name (meta); }
  bool meta_end_check (const symbol key, const symbol meta) const
  { return find_meta_end (key, meta).end_check (meta); }
  double meta_end_number (const symbol key, const symbol meta) const
  { return find_meta_end (key, meta).end_number (meta); }
  symbol meta_end_name (const symbol key, const symbol meta) const
  { return find_meta_end (key, meta).end_name (meta); }
  
  // Simulation.
  mutable bool more_data_available;
  void source_tick (Treelog& msg);
  bool done () const
  { return !more_data_available; }
  
  // Create and Destroy.
  void source_initialize (Treelog&);
  bool source_check (Treelog&) const;
  WSourceCombine (const BlockModel& al)
    : WSourceWeather (al),
      reserve (Librarian::build_item<WSource> (al, "reserve")),
      entry (map_submodel<Entry> (al, "entry")),
      my_data_begin (9999,1,1,0),
      my_data_end (1, 1, 1, 1),
      my_begin (9999,1,1,0),
      my_end (1,1,1,0),
      my_timestep (NAN)
  { }
  ~WSourceCombine ()
  { }
};

symbol 
WSourceCombine::Any ()
{
  static const symbol name ("Any");
  return name;
}

bool 
WSourceCombine::Entry::check_alist (const Metalib&, 
                                    const Frame& al, Treelog& msg)
{ 
  bool ok = true;

  if (al.check ("begin") && al.check ("end"))
    {
      const Time begin (al.submodel ("begin"));
      const Time end (al.submodel ("end"));
      if (begin >= end)
        msg.warning ("'begin' later than 'end'");
    }
  
  return ok;
}

void
WSourceCombine::Entry::load_syntax (Frame& frame)
{
  frame.declare_object ("source", WSource::component, "\
Source of weather data.");
  frame.declare_submodule ("begin", Attribute::OptionalConst, "\
Use weather data from source after this time.\n\
By default, use data from start of source.", Time::load_syntax);
  frame.declare_submodule ("end", Attribute::OptionalConst, "\
Use weather data from source until this time.\n\
By default, use data until end of source.", Time::load_syntax);
  frame.declare_string ("use", Attribute::Const, Attribute::Variable, "\
List of weather data to use from source.\n\
Specify 'Any' to use all present weather data.");
  frame.set_strings ("use", Any ());
  static struct UseCheck : public VCheck
  {
    bool verify (const Metalib&, const Frame& frame, const symbol key, 
                 Treelog& msg) const
    {
      daisy_assert (frame.check (key));
      daisy_assert (frame.lookup (key) == Attribute::String);
      daisy_assert (!frame.is_log (key));
      daisy_assert (frame.type_size (key) == Attribute::Variable);
      const std::vector<symbol>& names = frame.name_sequence (key);
      if (names.size () == 1 && names[0] == Any ())
        return true;
      static const Frame& keywords 
        = *Librarian::submodel_frame (Weatherdata::load_syntax);
      bool ok = true;
      for (size_t i = 0; i < names.size (); i++)
        if (keywords.lookup (names[i]) == Attribute::Error)
          {
            msg.error ("'" + names[i] + "': Unknown weather data");
            ok = false;
          }
      return ok;
    }
  } use_check;
  static VCheck::All use_unique (use_check, VCheck::unique ());
  frame.set_check ("use", use_unique);
}

std::set<symbol>
WSourceCombine::Entry::vector2set (const std::vector<symbol>& v)
{
  std::set<symbol> result;

  for (std::vector<symbol>::const_iterator i = v.begin (); 
       i != v.end ();
       i++)
    result.insert (*i);
  return result;
}

WSourceCombine::Entry::Entry (const BlockSubmodel& al)
  : source (Librarian::build_item<WSource> (al, "source")),
    begin (al.check ("begin") ? *submodel<Time> (al, "begin") : Time::null ()),
    end (al.check ("end") ? *submodel<Time> (al, "end") : Time::null ()),
    use (vector2set (al.name_sequence ("use")))
{ 
  daisy_assert (source.get ());
}

const WSource& 
WSourceCombine::find_source (const symbol key) const
{ 
  for (size_t i = 0; i < entry.size (); i++)
    {
      Entry& e = *(entry[i]);
      
      // Within period?
      if (my_begin < e.begin || my_end > e.end)
        continue;
      
      // Do we handle this key?
      if (e.use.find (Any ()) == e.use.end ()
          && e.use.find (key) == e.use.end ())
        continue;

      // Do we currently have a value?
      if (!e.source->check (key))
        continue;

      // Found it!
      return *e.source;
    }

  // Nope.
#if 0
  Assertion::message (" source found for '" + key + "'");
#endif
  return *reserve;
}

const WSource& 
WSourceCombine::find_end (const symbol key) const
{ 
  for (size_t i = 0; i < entry.size (); i++)
    {
      Entry& e = *(entry[i]);
      
      // Within period?
      if (my_begin < e.begin || my_end > e.end)
        continue;
      
      // Do we handle this key?
      if (e.use.find (Any ()) == e.use.end ()
          && e.use.find (key) == e.use.end ())
        continue;

      // Do we currently have a value?
      if (!e.source->end_check (key))
        continue;

      // Found it!
      return *e.source;
    }

  // Nope.
#if 0
  Assertion::message ("No end source found for '" + key + "'");
#endif
  return *reserve;
}

const WSource& 
WSourceCombine::find_meta (const symbol key, const symbol meta) const
{ 
  const WSource& source = find_source (key);
  if (&source != reserve.get ())
    return source;
  return find_source (meta);
}

const WSource& 
WSourceCombine::find_meta_end (const symbol key, const symbol meta) const
{ 
  const WSource& source = find_end (key);
  if (&source != reserve.get ())
    return source;
  return find_end (meta);
}

void 
WSourceCombine::entries (std::set<symbol>& result) const
{ 
  for (size_t i = 0; i < entry.size (); i++)
    {
      Entry& e = *(entry[i]);

#if 0
      std::ostringstream tmp;
      tmp << "Hej ";
#endif

      // Within period?
      if (my_begin < e.begin || my_end > e.end)
        continue;

      daisy_assert (e.source.get ());
      WSource& source = *(e.source);

      if (e.use.find (Any ()) != e.use.end ())
        {
          source.entries (result);
#if 0
          for (std::set<symbol>::const_iterator i = result.begin ();
               i != result.end ();
               i++)
            tmp << " " << *i;
          Assertion::message (tmp.str ());
#endif
          continue;
        }

      // Find all entries.
      std::set<symbol> mine;
      source.entries (mine);
      
      // Find valid entries.
      std::set<symbol> valid; 
      std::set_intersection (mine.begin(), mine.end(), 
                             e.use.begin(), e.use.end(), 
                             std::inserter(valid, valid.begin()));
     
      // Find all entries.
      std::set<symbol> all;
      std::set_union (valid.begin(), valid.end(), 
                      result.begin(), result.end(), 
                      std::inserter(all, all.begin()));

      // Use them.
      result.swap (all);

#if 0
      tmp << "\nUse";
      for (std::set<symbol>::const_iterator i = e.use.begin ();
           i != e.use.end ();
           i++)
        tmp << " " << *i;
      tmp << "\nMine";
      for (std::set<symbol>::const_iterator i = mine.begin ();
           i != mine.end ();
           i++)
        tmp << " " << *i;
      tmp << "\nValid";
      for (std::set<symbol>::const_iterator i = valid.begin ();
           i != valid.end ();
           i++)
        tmp << " " << *i;
      tmp << "\nAll";
      for (std::set<symbol>::const_iterator i = all.begin ();
           i != all.end ();
           i++)
        tmp << " " << *i;
      tmp << "\nResult";
      for (std::set<symbol>::const_iterator i = result.begin ();
           i != result.end ();
           i++)
        tmp << " " << *i;
      Assertion::message (tmp.str ());
#endif
    }
}

void 
WSourceCombine::source_tick (Treelog& msg)
{
  if (!more_data_available)
    return;
  
  my_begin = my_end;
  my_end = my_data_end;

  bool all_done = true;
  for (size_t i = 0; i < entry.size (); i++)
    {
      Entry& e = *(entry[i]);

      // Within period?
      if (my_end < e.begin || my_begin > e.end)
        continue;

      daisy_assert (e.source.get ());
      WSource& source = *(e.source);

      Treelog::Open nest (msg, "source", i, entry[i]->source->objid);
      while(!source.done () && source.end () <= my_begin)
        source.source_tick (msg);
      if (source.done ())
        continue;
      
      if (source.end () < my_end)
        my_end = source.end ();
      
      all_done = false;
    }
  if (all_done)
    more_data_available = false;

  my_timestep = Time::fraction_hours_between (my_begin, my_end);
  daisy_assert (my_timestep > 0.0);
}

void 
WSourceCombine::source_initialize (Treelog& msg)
{
  TREELOG_MODEL (msg);

  more_data_available = true;

  for (size_t i = 0; i < entry.size (); i++)
    {
      Entry& e = *(entry[i]);
      daisy_assert (e.source.get ());
      WSource& source = *(e.source);
      Treelog::Open nest (msg, "source", i, source.objid);
      source.source_initialize (msg);
      if (e.begin == Time::null ())
        e.begin = source.data_begin ();
      if (e.end == Time::null ())
        e.end = source.data_end ();

      if (e.begin < my_data_begin)
        my_data_begin = e.begin;
      if (e.end > my_data_end)
        my_data_end = e.end;
      if (source.begin () < my_begin)
        my_begin = source.begin ();
    }
  my_end = my_data_end;
  for (size_t i = 0; i < entry.size (); i++)
    {
      Entry& e = *(entry[i]);
      daisy_assert (e.source.get ());
      WSource& source = *(e.source);
      if (source.end () > my_begin && source.end () < my_end)
        my_end = source.end ();
    }
  my_timestep = Time::fraction_hours_between (my_begin, my_end);
}

bool 
WSourceCombine::source_check (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  if (my_end <= my_begin)
    {
      msg.error ("Bad initial timestep from " + my_begin.print () 
                 + " to " + my_end.print ());
      more_data_available = false;
    }

  if (my_data_end <= my_data_begin)
    {
      msg.error ("Bad data interval from " + my_data_begin.print () 
                 + " to " + my_data_end.print ());
      more_data_available = false;
    }

  for (size_t i = 0; i < entry.size (); i++)
    {
      Treelog::Open nest (msg, "source", i, entry[i]->source->objid);
      if (!entry[i]->source->source_check (msg))
        more_data_available = false;
    }

  return more_data_available;
}

static struct WSourceCombineSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new WSourceCombine (al); }
  WSourceCombineSyntax ()
    : DeclareModel (WSource::component, "combine", "weather",
                    "Combine multiple weather sources.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_submodule_sequence ("entry", Attribute::Const, "\
List of weather sources.", WSourceCombine::Entry::load_syntax);
  frame.declare_object ("reserve", WSource::component,
                        Attribute::State, Attribute::Singleton, "\
Reserve weather model to use when no source match.");
  frame.set ("reserve", "null");
  }
} WSourceCombine_syntax;

// wsource_combine.C ends here.

