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


#include "format.h"

EMPTY_TEMPLATE
Librarian<Format>::Content* Librarian<Format>::content = NULL;

const char *const Format::description = "\
Text formatting component.";

std::ostream&
Format::out ()
{ 
  daisy_assert (output);
  return *output;
}

void
Format::push (const std::string& name)
{ nest.push (name); }

void
Format::pop (const std::string& name)
{
  daisy_assert (!nest.empty ());
  daisy_assert (nest.top () == name);
  nest.pop ();
}

Format::Item::Item (Format& f, const std::string& name)
  : format (f)
{ 
  format.push ("item");
  format.item_open (name); 
}

Format::Item::~Item ()
{ 
  format.pop ("item");
  format.item_close (); 
}

Format::Section::Section (Format& f, 
			  const std::string& type, const std::string& title,
			  const std::string& scope, const std::string& label)
  : format (f)
{ 
  format.push ("section");
  format.section_open (type, title, scope, label); 
}

Format::Section::~Section ()
{ 
  format.pop ("section");
  format.section_close (); 
}

Format::Document::Document (Format& f)
  : format (f)
{ 
  daisy_assert (format.nest.empty ());
  format.push ("document");
  format.document_open (); 
}

Format::Document::~Document ()
{ 
  format.pop ("document");
  format.document_close (); 
  daisy_assert (format.nest.empty ());
}

void
Format::initialize (std::ostream& o)
{ 
  daisy_assert (output == NULL);
  output = &o;
}

Format::Format (const AttributeList& al)
  : name (al.identifier ("type")),
    output (NULL)
{ }

Format::~Format ()
{ daisy_assert (nest.empty ()); }
