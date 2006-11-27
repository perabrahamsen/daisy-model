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


#include "log_select.h"
#include "library.h"
#include "block.h"
#include "field.h"
#include "format.h"
#include "volume.h"
#include "memutils.h"
#include <sstream>

bool 
LogSelect::check_leaf (symbol) const
{ daisy_assert (false); }

bool 
LogSelect::check_interior (symbol) const
{ daisy_assert (false); }

bool 
LogSelect::check_derived (symbol field, symbol /* name */,
			  const Library& /* library */) const
{ return check_interior (field); }

bool 
LogSelect::match (const Daisy& daisy, Treelog& out)
{
  condition->tick (daisy, out);
  is_printing = condition->match (daisy, out);
  is_active = is_printing;

  for (std::vector<Select*>::const_iterator i = entries.begin (); 
       i < entries.end (); 
       i++)
    if ((*i)->match (is_printing))
      is_active = true;

  return is_active;
}

void
LogSelect::done (const Time&)
{ }

bool
LogSelect::initial_match (const Daisy&, Treelog&)
{
  is_active = false;

  for (std::vector<Select*>::const_iterator i = entries.begin (); 
       i < entries.end (); 
       i++)
    if ((*i)->initial_match ())
      is_active = true;

  is_printing = is_active;
  return is_active;
}

void
LogSelect::initial_done (const Time&)
{ }

void 
LogSelect::open (symbol)
{ daisy_assert (false); }

void 
LogSelect::close ()
{ daisy_assert (false); }

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
LogSelect::open_ordered (int index)
{ 
  open (symbol (index));
}

void 
LogSelect::close_ordered ()
{ close (); }

void 
LogSelect::open_derived (symbol field, symbol type)
{ open (field); open (type); }

void 
LogSelect::close_derived ()
{ close (); close (); }

void 
LogSelect::open_object (symbol field, symbol type, const AttributeList&)
{ open_derived (field, type); }

void 
LogSelect::close_object ()
{ close_derived (); }

void 
LogSelect::open_entry (symbol type, const AttributeList&)
{ open (type); }

void 
LogSelect::close_entry ()
{ close (); }

void 
LogSelect::open_named_entry (symbol name, symbol, 
			     const AttributeList&)
{ open (name); }

void 
LogSelect::close_named_entry ()
{ close (); }

void 
LogSelect::output (symbol, const bool)
{ daisy_assert (false); }

void 
LogSelect::output (symbol, const double)
{ daisy_assert (false); }

void 
LogSelect::output (symbol, const int)
{ daisy_assert (false); }

void 
LogSelect::output (symbol, const symbol)
{ daisy_assert (false); }

void 
LogSelect::output (symbol, const std::vector<double>&)
{ daisy_assert (false); }

void 
LogSelect::output (symbol, const PLF&)
{ daisy_assert (false); }

bool 
LogSelect::check (const Border& border, Treelog& err) const
{
  bool ok = true;

  if (!volume->check_border (border, err))
    /* ok = false */;

  for (unsigned int i = 0; i < entries.size (); i++)
    {
      std::ostringstream tmp;
      tmp << "entries [" << i << "]: " << entries[i]->tag ();
      Treelog::Open nest (err, tmp.str ());
      if (!entries[i]->check (err))
        ok = false;
      if (!entries[i]->check_border (border, *volume, err))
        /* ok = false */;
    }
  return ok; 
}

LogSelect::LogSelect (Block& al)
  : Log (al),
    description (al.name ("description")),
    condition (Librarian<Condition>::build_item (al, "when")),
    entries (Librarian<Select>::build_vector (al, "entries")),
    volume (Volume::build_obsolete (al))
{
  if (!al.ok ())
    return;

  // Initialize entries.
  for (unsigned int i = 0; i < entries.size (); i++)
    if (!entries[i]->initialize (*volume, condition->timestep (),
                                 al.msg ()))
      al.set_error ();
}

  
LogSelect::~LogSelect ()
{ sequence_delete (entries.begin (), entries.end ()); }

static bool check_alist (const AttributeList&, Treelog&)
{
  bool ok = true;
  return ok;
}

// GCC 2.95 doesn't allow classes nested in functions.
struct DocSelect : public LogSelect 
{
  void initialize (Treelog&)
  { }
  DocSelect (Block& al)
    : LogSelect (al)
  { }
};

void
LogSelect::document_entries (Format& format, const AttributeList& alist)
{
  Syntax syntax;
  AttributeList dummy_alist;
  LogSelect::load_syntax (syntax, dummy_alist);

  const Library& log_lib = Librarian<Log>::library ();
  if (alist.check ("type"))
    {
      const symbol parent = alist.identifier ("type");
      if (log_lib.check (parent)
          && alist.subset (log_lib.lookup (parent), log_lib.syntax (parent),
                           "entries"))
        return;
    }

  if (!syntax.check (alist, Treelog::null ()))
    {
      // Incomplete log.
      if (!alist.check ("entries"))
	return;

      const std::vector<AttributeList*>& entries 
        = alist.alist_sequence ("entries");
      if (entries.size () < 1)
	return;

      // At least one interesting description required.
      const Library& library = Librarian<Select>::library ();
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
	  const AttributeList& entry = *entries[i];
	  Format::Item d2 (format, Select::select_get_tag (entry).name ());
	  if (library.has_interesting_description (entry))
	    format.text (entry.name ("description"));
	  format.soft_linebreak ();
	}
      return;
    }
  // Complete log.
  Block block (syntax, alist, Treelog::null (), "docselect");
  DocSelect select (block);
  daisy_assert (block.ok ());

  format.bold ("Table columns:");
  Format::List dummy (format);

  for (size_t i = 0; i < select.entries.size (); i++)
    select.entries[i]->document (format);
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
	      Syntax::State, Syntax::Sequence,
	      "What to log in each column.");
  syntax.add ("time_columns", Syntax::Boolean, Syntax::OptionalConst, "\
Iff true, add columns for year, month, mday and hour in the begining of\n\
the lines.  By default, this will be true of you have not specified any\n\
time entries yourself.");
  syntax.add ("volume", Librarian<Volume>::library (), 
              Syntax::Const, Syntax::Singleton,
              "Soil volume to log.");
  alist.add ("volume", Volume::infinite_box ());
  syntax.add ("from", "cm", Syntax::OptionalConst,
	      "Default 'from' value for all entries.\n\
By default, use the top of the soil.\n\
OBSOLETE: Use (volume box (top FROM)) instead.");
  syntax.add ("to", "cm", Syntax::OptionalConst,
	      "Default 'to' value for all entries.\n\
By default, use the bottom of the soil.\n\
OBSOLETE: Use (volume box (bottom TO)) instead.");
}
