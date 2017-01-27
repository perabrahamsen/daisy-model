// format.C --- Text formatting component.
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

#include "format.h"
#include "assertion.h"
#include "block_model.h"
#include "librarian.h"
#include "frame.h"

const char *const Format::component = "format";

symbol
Format::library_id () const
{
  static const symbol id (component);
  return id;
}

symbol
Format::format_type () const
{ 
  static const symbol type ("unknown");
  return type; 
}

std::ostream&
Format::out ()
{ 
  daisy_assert (output);
  return *output;
}

void
Format::push (const symbol name)
{ nest.push (name); }

void
Format::pop (const symbol name)
{
  // May be called from destructor, don't use 'daisy_assert'!
  daisy_safe_assert (!nest.empty ());
  daisy_safe_assert (nest.top () == name);
  if (!nest.empty ())
    nest.pop ();
}

Format::List::List (Format& f)
  : format (f)
{ 
  format.push ("list");
  format.list_open ();
}

Format::List::~List ()
{ 
  format.pop ("list");
  format.list_close (); 
}

Format::Item::Item (Format& f, const symbol name)
  : format (f)
{ 
  daisy_assert (!format.nest.empty ());
  daisy_assert (format.nest.top () == "list");

  format.push ("item");
  format.item_open (name); 
}

Format::Item::~Item ()
{ 
  format.pop ("item");
  format.item_close (); 
}

Format::Table::Table (Format& f, const symbol name)
  : format (f)
{ 
  format.push ("table");
  format.table_open (name); 
}

Format::Table::~Table ()
{ 
  format.pop ("table");
  format.table_close (); 
}

Format::TableRow::TableRow (Format& f)
  : format (f)
{ 
  format.push ("table_row");
  format.table_row_open ();
}

Format::TableRow::~TableRow ()
{ 
  format.pop ("table_row");
  format.table_row_close (); 
}

Format::TableCell::TableCell (Format& f)
  : format (f)
{ 
  format.push ("table_cell");
  format.table_cell_open ();
}

Format::TableCell::~TableCell ()
{ 
  format.pop ("table_cell");
  format.table_cell_close (); 
}

Format::TableMultiCell::TableMultiCell (Format& f,
					int cells, const symbol form)
  : format (f)
{ 
  format.push ("table_cell");
  format.table_multi_cell_open (cells, form);
}

Format::TableMultiCell::~TableMultiCell ()
{ 
  format.pop ("table_cell");
  format.table_multi_cell_close (); 
}

Format::Typewriter::Typewriter (Format& f)
  : format (f)
{ 
  format.push ("typewriter");
  format.typewriter_open ();
}

Format::Typewriter::~Typewriter ()
{ 
  format.pop ("typewriter");
  format.typewriter_close (); 
}

Format::Section::Section (Format& f, 
			  const symbol type, const symbol title,
			  const symbol scope, const symbol label)
  : format (f)
{ 
  daisy_assert (!format.nest.empty ());
  daisy_assert (format.nest.top () == "document"
		|| format.nest.top () == "section");

  format.push ("section");
  format.section_open (type, title, scope, label); 
}

Format::Section::~Section ()
{ 
  format.pop ("section");
  format.section_close (); 
}

Format::Document::Document (Format& f, const symbol w, const symbol desc)
  : format (f),
    where (w)
{ 
  daisy_assert (format.nest.empty ());
  format.push ("document");
  format.document_open (where, desc); 
}

Format::Document::~Document ()
{ 
  format.pop ("document");
  format.document_close (where); 
  daisy_safe_assert (format.nest.empty ());
}

void
Format::frame_description (const Frame& frame)
{
  const std::string native_description = "description_" + format_type ();
  if (frame.check (native_description))
    {
      soft_linebreak ();
      raw (format_type (), frame.name (native_description));
      soft_linebreak ();
      return;
    }
  const symbol d = frame.description ();
  if (d != Attribute::None ())
    {
      soft_linebreak ();
      text (d);
      soft_linebreak ();
    }
}

void
Format::initialize (std::ostream& o)
{ 
  daisy_assert (output == NULL);
  output = &o;
}

Format::Format (const BlockModel& al)
  : output (NULL)
{ }

Format::~Format ()
{ daisy_safe_assert (nest.empty ()); }

static struct FormatInit : public DeclareComponent 
{
  FormatInit ()
    : DeclareComponent (Format::component, "\
Text formatting component.")
  { }
} Format_init;

// format.C ends here.
