// program_file.C -- File operations from within a batch program.
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
#include "path.h"
#include "treelog.h"
#include <string>

struct ProgramCD : public Program
{
  // Content.
  const std::string dir;

  // Use.
  void run (Treelog& msg)
  { 
    if (!Path::set_directory (dir))
      msg.error (std::string ("Could not change to directory '") + dir + "'");
  }

  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { }
  bool check (Treelog&)
  { return true; }

  ProgramCD (const AttributeList& al)
    : Program (al),
      dir (al.name ("directory"))
  { }
  ~ProgramCD ()
  { }
};

static struct ProgramCDSyntax
{
  static Program&
  make (const AttributeList& al)
  { return *new ProgramCD (al); }
  ProgramCDSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Change working directory."); 
    syntax.add ("directory", Syntax::String, Syntax::Const, "\
Name of directory to change into.");
    syntax.order ("directory");
    Librarian<Program>::add_type ("cd", alist, syntax, &make);
  }
} ProgramCD_syntax;
