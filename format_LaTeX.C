// format_LaTeX.C --- LaTeX text formatting.
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

struct FormatLaTeX : public Format
{
  // Create and Destroy.
  explicit FormatLaTeX (const AttributeList& al)
    : Format (al)
  { }
};

static struct FormatLaTeXSyntax
{
  static Format& make (const AttributeList& al)
  { return *new FormatLaTeX (al); }
  FormatLaTeXSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Format text as LaTeX.");
    Librarian<Format>::add_type ("LaTeX", alist, syntax, &make);
  }
} FormatLaTeX_syntax;
