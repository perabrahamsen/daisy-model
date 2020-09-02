// log_select.C
// 
// Copyright 1996-2001, 2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2005 KVL.
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

#include "log_select.h"
#include "select.h"
#include "condition.h"
#include "metalib.h"
#include "library.h"
#include "block_top.h"
#include "block_model.h"
#include "field.h"
#include "format.h"
#include "volume.h"
#include "scope.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "treelog.h"
#include "frame_model.h"
#include "daisy.h"
#include "summary.h"
#include <sstream>

void 
LogSelect::find_scopes (std::vector<const Scope*>& scopes) const
{
  for (size_t i = 0; i < summary.size (); i++)
    summary[i]->find_scopes (scopes);
}

bool 
LogSelect::check_leaf (symbol) const
{ daisy_notreached (); }

bool 
LogSelect::check_interior (symbol) const
{ daisy_notreached (); }

bool 
LogSelect::check_derived (symbol field, const symbol /* name */,
			  const symbol /* component */) const
{ return check_interior (field); }

bool 
LogSelect::match (const Daisy& daisy, Treelog& out)
{
  active->tick (daisy, Scope::null (), out);
  const bool log_active = active->match (daisy, Scope::null (), out);
      
  condition->tick (daisy, Scope::null (), out);
  is_printing = condition->match (daisy, Scope::null (), out);
  is_active = is_printing;

  if (log_active)
    {
      for (std::vector<Select*>::const_iterator i = entries.begin (); 
           i < entries.end (); 
           i++)
        if ((*i)->match (is_printing))
          is_active = true;
    }
  else
    for (std::vector<Select*>::const_iterator i = entries.begin (); 
         i < entries.end (); 
         i++)
      (*i)->is_active = is_printing;

  return is_active;
}

void
LogSelect::done (const std::vector<Time::component_t>& time_columns,
		 const Time& time, const double dt, Treelog&)
{ 
  for (std::vector<Select*>::const_iterator i = entries.begin (); 
       i < entries.end (); 
       i++)
    (*i)->done_small (dt);
  end = time;

  if (is_printing)
    {
      for (size_t i = 0; i < summary.size (); i++)
        summary[i]->tick (time);
      done_print (time_columns, time);
    }
}

bool
LogSelect::initial_match (const Daisy& daisy, const Time& previous, Treelog&)
{
  for (size_t i = 0; i < summary.size (); i++)
    summary[i]->clear ();

  begin = daisy.time ();

  active->initiate_log (daisy, previous);
  condition->initiate_log (daisy, previous);

  if (print_initial)
    {
      for (std::vector<Select*>::const_iterator i = entries.begin (); 
           i < entries.end (); 
           i++)
        if ((*i)->match (true))
          is_active = true;
    }
  else
    is_active = false;

  is_printing = is_active;

  return is_active;
}

void
LogSelect::initial_done (const std::vector<Time::component_t>& time_columns,
			 const Time& time, Treelog&)
{
  for (std::vector<Select*>::const_iterator i = entries.begin (); 
       i < entries.end (); 
       i++)
    (*i)->done_initial ();
  begin = time;

  if (is_printing)
    {
      for (size_t i = 0; i < summary.size (); i++)
        summary[i]->tick (time);
      done_print (time_columns, time);
    }
}

void 
LogSelect::open_derived_type (const symbol type, const symbol)
{ open (type); }

void 
LogSelect::open (symbol)
{ daisy_notreached (); }

void 
LogSelect::close ()
{ daisy_notreached (); }

void 
LogSelect::open_unnamed ()
{ }

void 
LogSelect::close_unnamed ()
{ }

void 
LogSelect::open_named (const symbol name)
{ open (name); }

void 
LogSelect::close_named ()
{ close (); }

void 
LogSelect::open_ordered (const int index)
{ 
  open (symbol (index));
}

void 
LogSelect::close_ordered ()
{ close (); }

void 
LogSelect::open_derived (symbol field, symbol type, const symbol library)
{ open (field); open_derived_type (type, library); }

void 
LogSelect::close_derived ()
{ close (); close (); }

void 
LogSelect::open_object (const symbol field, const symbol type, const Frame&,
			const symbol library)
{ open_derived (field, type, library); }

void 
LogSelect::close_object ()
{ close_derived (); }

void 
LogSelect::open_entry (const symbol type, const Frame&, 
		       const symbol library)
{ open_derived_type (type, library); }

void 
LogSelect::close_entry ()
{ close (); }

void 
LogSelect::open_named_entry (symbol name, symbol, 
			     const Frame&)
{ open (name); }

void 
LogSelect::close_named_entry ()
{ close (); }

void 
LogSelect::open_shallow (symbol type, const symbol library)
{ open_derived_type (type, library); }

void 
LogSelect::close_shallow ()
{ close (); }

void 
LogSelect::output_entry (symbol, const bool)
{ daisy_notreached (); }

void 
LogSelect::output_entry (symbol, const double)
{ daisy_notreached (); }

void 
LogSelect::output_entry (symbol, const int)
{ daisy_notreached (); }

void 
LogSelect::output_entry (symbol, const symbol)
{ daisy_notreached (); }

void 
LogSelect::output_entry (symbol, const std::vector<double>&)
{ daisy_notreached (); }

void 
LogSelect::output_entry (symbol, const PLF&)
{ daisy_notreached (); }

void
LogSelect::initialize (const symbol, const symbol, Treelog& msg)
{
  for (size_t i = 0; i < summary.size (); i++)
    summary[i]->initialize (entries, msg);
}

bool 
LogSelect::check (const Border& border, Treelog& err) const
{
  bool ok = true;

  if (!volume->check_border (border, err))
    {  ok = false; }

  for (size_t i = 0; i < entries.size (); i++)
    {
      std::ostringstream tmp;
      tmp << "entries [" << i << "]: " << entries[i]->tag ();
      Treelog::Open nest (err, tmp.str ());
      if (!entries[i]->check (err))
        ok = false;
      if (!entries[i]->check_border (border, *volume, err))
        { ok = false; }
    }

  for (size_t i = 0; i < summary.size (); i++)
    if (!summary[i]->check (err))
      ok = false;

  return ok; 
}

std::vector<std::pair<symbol, symbol>/**/>
LogSelect::build_parameters (const Block& al)
{
  daisy_assert (al.check ("parameter_names"));
  const std::vector<symbol> pars = al.name_sequence ("parameter_names");
  std::vector<std::pair<symbol, symbol>/**/> result;
  for (size_t i = 0; i < pars.size (); i++)
    {
      const symbol key = pars[i];
      if (al.can_extract_as (key, Attribute::String))
        result.push_back (std::pair<symbol, symbol> (key, al.name (key)));
      else if (al.can_extract_as (key, Attribute::Integer))
        {
          std::ostringstream tmp;
          tmp << al.integer (key);
          result.push_back (std::pair<symbol, symbol> (key, tmp.str ()));
        }
      else if (al.can_extract_as (key, Attribute::Number))
        {
          std::ostringstream tmp;
          tmp << al.number  (key) << "[" << al.dimension (key) << "]";
          result.push_back (std::pair<symbol, symbol> (key, tmp.str ()));
        }
      else
        al.msg ().warning ("Parameter name '" + key + "' not found"); 
    }
  return result;
}

bool
LogSelect::default_print_initial (const std::vector<Select*>& entries)
{
  // Any in favour?
  for (size_t i = 0; i < entries.size (); i++)
    if (entries[i]->print_initial ())
      return true;

  // Noone cares.
  return false;
}

LogSelect::LogSelect (const BlockModel& al)
  : Log (al),
    description (al.name ("description", "")),
    file (al.name ("where")),
    parameters (build_parameters (al)),
    condition (Librarian::build_item<Condition> (al, "when")),
    active (Librarian::build_item<Condition> (al, "active")),
    entries (Librarian::build_vector<Select> (al, "entries")),
    volume (Volume::build_obsolete (al)),
    print_initial (al.flag ("print_initial", default_print_initial (entries))),
    begin (1, 1, 1, 1),
    end (1, 1, 1, 1),
    summary (Librarian::build_vector<Summary> (al, "summary"))
{
  if (!al.ok ())
    return;

  // Initialize entries.
  for (size_t i = 0; i < entries.size (); i++)
    if (!entries[i]->initialize (al.units (), *volume, condition->timestep (),
                                 al.msg ()))
      al.set_error ();
}

void
LogSelect::summarize (Treelog& msg)
{
  if (summary.size () > 0)
    {
      TREELOG_MODEL (msg);
      std::ostringstream tmp;

      tmp << "LOGFILE: " << file  << "\n";
      tmp << "VOLUME: " << volume->one_line_description () << "\n";
      tmp << "TIME: " << begin.print () << " to " << end.print ();
      for (size_t i = 0; i < parameters.size (); i++)
        if (parameters[i].second != "*")
          {
            std::string id = parameters[i].first.name ();
            std::transform (id.begin (), id.end (), id.begin (), ::toupper);
            
            tmp << "\n" << id << ": " << parameters[i].second;
          }

      msg.message (tmp.str ());
      for (size_t i = 0; i < summary.size (); i++)
        summary[i]->summarize (msg);
    }
}

LogSelect::LogSelect (const char *const id)
  : Log (id),
    description ("Build in log select use."),
    file (id),
    condition (Condition::create_true ()),
    active (Condition::create_true ()),
    entries (std::vector<Select*> ()),
    volume (Volume::build_none ()),
    print_initial (false)
{ }

LogSelect::~LogSelect ()
{ }

void
LogSelect::document_entries (Format& format, const Metalib& metalib, 
                             Treelog& msg, const symbol name)
{
  const Library& log_lib = metalib.library (Log::component);
  const FrameModel& frame = log_lib.model (name);

  // We need a type.
  const symbol parent = frame.base_name ();
  if (parent == Attribute::None ())
    {
      msg.warning ("bug: Orphan log parameterisation.");
      return;
    }

  // Check if this log parameterizations adds something compared to
  // its parent. 
  daisy_assert (parent != name);
  if (log_lib.check (parent)
      && frame.subset (metalib, log_lib.model (parent), "entries"))
    // If not, don't document the entries.
    return;

  // Incomplete log.
  if (!frame.check (metalib, Treelog::null ()))
    {
      if (!frame.check ("entries"))
	return;

      const std::vector<boost::shared_ptr<const FrameModel>/**/>& entries 
        = frame.model_sequence ("entries");
      if (entries.size () < 1)
	return;

      // At least one interesting description required.
      const Library& library = metalib.library (Select::component);
      int interesting = 0;
      for (size_t i = 0; i < entries.size (); i++)
	if (library.has_interesting_description (*entries[i]))
	  interesting++;
      if (interesting < 1)
	return;

      format.bold ("Table columns include:");
      Format::List dummy (format);

      for (size_t i = 0; i < entries.size (); i++)
	{
	  const Frame& entry = *entries[i];
	  Format::Item d2 (format, Select::select_get_tag (entry).name ());
	  if (library.has_interesting_description (entry))
	    format.text (entry.description ());
	  format.soft_linebreak ();
	}
      return;
    }

  // Complete log.
  BlockTop block (metalib, msg, frame);
  auto_vector<Select*> entries (Librarian::build_vector<Select> (block,
                                                                 "entries")); 
  daisy_assert (block.ok ());

  format.bold ("Table columns:");
  Format::List dummy (format);

  for (size_t i = 0; i < entries.size (); i++)
    entries[i]->document (format);
}

static struct LogSelectSyntax : public DeclareBase
{
  LogSelectSyntax ()
    : DeclareBase (Log::component, "select", "Select variables to log.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
    frame.declare_string ("parameter_names", 
                          Attribute::Const, Attribute::Variable, "\
List of string parameters to print to the table header.\n\
\n\
For example, if you have defined 'column' and 'crop' parameters for\n\
this table log parameterization, you can print them to the log file\n\
header by specifying '(parameter_names column crop)'.");
    frame.set_empty ("parameter_names");
    frame.declare_object ("when", Condition::component, "\
Add entries to the log file when this condition is true.");
    frame.declare_object ("active", Condition::component, "\
Add data when this condition is true.\n\
E.g. count percolation only when there is no crop.");
    frame.set ("active", "true");
    frame.declare_object ("entries", Select::component, 
                          Attribute::State, Attribute::Variable,
                          "What to log in each column.");
    frame.declare_boolean ("time_columns", Attribute::OptionalConst, "\
Iff true, add columns for year, month, mday and hour in the begining of\n\
the lines.  By default, this will be true of you have not specified any\n\
time entries yourself.");
    frame.declare_object ("volume", Volume::component, 
                          Attribute::Const, Attribute::Singleton,
                          "Soil volume to log.");
    frame.set ("volume", "box");
    frame.declare ("from", "cm", Attribute::OptionalConst,
                   "Default 'from' value for all entries.\n\
By default, use the top of the soil.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
    frame.declare ("to", "cm", Attribute::OptionalConst,
                   "Default 'to' value for all entries.\n\
By default, use the bottom of the soil.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
    frame.declare_boolean ("print_initial", Attribute::OptionalConst, "\
Print a line with initial values when logging starts.\n\
By default, an initial line will be printed if any entry has 'handle'\n\
set to 'current'.");
    frame.declare_object ("summary", Summary::component,
                          Attribute::Const, Attribute::Variable,
                          "Summaries for this log file.");
    frame.set_empty ("summary");
  }
} LogSelect_syntax;

// log_select.C ends here.
