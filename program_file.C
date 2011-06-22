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

#define BUILD_DLL

#include "program.h"
#include "block_model.h"
#include "path.h"
#include "treelog.h"
#include "librarian.h"
#include "frame.h"
#include <string>
#include <fstream>

struct ProgramCD : public Program
{
  // Content.
  Path& path;
  const symbol dir;

  // Use.
  bool run (Treelog& msg)
  { 
    if (path.set_directory (dir.name ()))
      return true;
    msg.error (std::string ("Could not change to directory '") + dir + "'");
    return false;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog&)
  { return true; }

  ProgramCD (const BlockModel& al)
    : Program (al),
      path (al.path ()),
      dir (al.name ("directory"))
  { }
  ~ProgramCD ()
  { }
};

static struct ProgramCDSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramCD (al); }
  ProgramCDSyntax ()
    : DeclareModel (Program::component, "cd", "Change working directory.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("directory", Attribute::Const, "\
Name of directory to change into.");
    frame.order ("directory");
  }
} ProgramCD_syntax;

struct ProgramWrite : public Program
{
  // Content.
  const symbol what;
  const symbol where;

  // Use.
  bool run (Treelog& msg)
  { 
    if (where == "screen")
      {
        msg.message (what.name ());
        return true;
      }
    std::ofstream out (where.name ().c_str ());
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

  ProgramWrite (const BlockModel& al)
    : Program (al),
      what (al.name ("what")),
      where (al.name ("where"))
  { }
  ~ProgramWrite ()
  { }
};

static struct ProgramWriteSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramWrite (al); }
  ProgramWriteSyntax ()
    : DeclareModel (Program::component, "write", "Write string to file.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("what", Attribute::Const, "\
String to write.");
    frame.declare_string ("where", Attribute::Const, "\
File to write it in.\n\
If the value is 'screen', write the string to the screen.");
    frame.set ("where", "screen");
  }
} ProgramWrite_syntax;
