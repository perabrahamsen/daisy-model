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
#include "block.h"
#include "alist.h"
#include "path.h"
#include "treelog.h"
#include "librarian.h"
#include <string>
#include <fstream>

struct ProgramCD : public Program
{
  // Content.
  const std::string dir;

  // Use.
  bool run (Treelog& msg)
  { 
    if (Path::set_directory (dir))
      return true;
    msg.error (std::string ("Could not change to directory '") + dir + "'");
    return false;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog&)
  { return true; }

  ProgramCD (Block& al)
    : Program (al),
      dir (al.name ("directory"))
  { }
  ~ProgramCD ()
  { }
};

static struct ProgramCDSyntax
{
  static Model& make (Block& al)
  { return *new ProgramCD (al); }
  ProgramCDSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Change working directory."); 
    syntax.add ("directory", Syntax::String, Syntax::Const, "\
Name of directory to change into.");
    syntax.order ("directory");
    Librarian::add_type (Program::component, "cd", alist, syntax, &make);
  }
} ProgramCD_syntax;

struct ProgramWrite : public Program
{
  // Content.
  const std::string what;
  const std::string where;

  // Use.
  bool run (Treelog& msg)
  { 
    if (where == "screen")
      {
        msg.message (what);
        return true;
      }
    std::ofstream out (where.c_str ());
    out << what;
    if (out.good ())
      return true;

    msg.error ("Could not write to '" + where + "'");
    return false;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog&)
  { return true; }

  ProgramWrite (Block& al)
    : Program (al),
      what (al.name ("what")),
      where (al.name ("where"))
  { }
  ~ProgramWrite ()
  { }
};

static struct ProgramWriteSyntax
{
  static Model& make (Block& al)
  { return *new ProgramWrite (al); }
  ProgramWriteSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Write string to file."); 
    syntax.add ("what", Syntax::String, Syntax::Const, "\
String to write.");
    syntax.add ("where", Syntax::String, Syntax::Const, "\
File to write it in.\n\
If the value is 'screen', write the string to the screen.");
    alist.add ("where", "screen");
    Librarian::add_type (Program::component, "write", alist, syntax, &make);
  }
} ProgramWrite_syntax;
