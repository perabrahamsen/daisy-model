// program_refdoc.C -- Create reference documentation for Daisy..
// 
// Copyright 2004 Per Abrahamsen and KVL.
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


#include "program.h"
#include "document.h"
#include "treelog.h"
#include <iostream>
#include <memory>

struct ProgramRefdoc : public Program
{
  // Use.
  void run (Treelog& msg)
  { 
    const Library& library = Librarian<Document>::library ();
    static const symbol name ("LaTeX");
    daisy_assert (library.check (name));
    const Syntax& syntax = library.syntax (name);
    AttributeList alist (library.lookup (name));
    alist.add ("type", name);
    if (syntax.check (alist, msg))
      {
        std::auto_ptr<Document> document (Librarian<Document>::create (alist));
        document->print_document (std::cout);
      }
  }

  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { };
  bool check (Treelog&)
  { return true; }
  ProgramRefdoc (const AttributeList& al)
    : Program (al)
  { }
  ~ProgramRefdoc ()
  { }
};

static struct ProgramRefdocSyntax
{
  static Program&
  make (const AttributeList& al)
  { return *new ProgramRefdoc (al); }
  ProgramRefdocSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Generate the components part of the reference manual.");
    Librarian<Program>::add_type ("LaTeX", alist, syntax, &make);
  }
} ProgramRefdoc_syntax;
