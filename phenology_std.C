// phenology_std.C -- Default crop phenology model.
// 
// Copyright 2003 Per Abrahamsen and Søren Hansen
// Copyright 2003 KVL.
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


#include "phenology.h"

class PhenologyStandard : public Phenology
{
  // Create.
public:
  PhenologyStandard (const AttributeList&);
};
PhenologyStandard::PhenologyStandard (const AttributeList& al)
  : Phenology (al)
{ }

static struct PhenologyStandardSyntax
{
  static Phenology&
  make (const AttributeList& al)
  { return *new PhenologyStandard (al); }

  PhenologyStandardSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Phenology::load_syntax (syntax, alist);
    alist.add ("description", 
	       "Default crop phenology model.");
    Librarian<Phenology>::add_type ("default", alist, syntax, &make);
  }
} PhenologyStandard_syntax;
