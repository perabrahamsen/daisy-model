// program_batch.C -- Run a number of programs.
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
#include "treelog.h"
#include "path.h"
#include "memutils.h"
#include <vector>

struct ProgramBatch : public Program
{
  // Content.
  const std::string directory;
  std::vector<Program*> program;

  // Use.
  void run (Treelog& msg)
  { 
    Path::InDirectory cwd (directory);

    for (size_t i = 0; i < program.size (); i++)
      {
        Treelog::Open nest (msg, program[i]->name);
        program[i]->run (msg);
      }
  }

  // Create and Destroy.
  void initialize (const Syntax *const gs, const AttributeList *const gal,
                   Treelog& msg)
  { 
    Path::InDirectory cwd (directory);

    for (size_t i = 0; i < program.size (); i++)
      {
        Treelog::Open nest (msg, program[i]->name);
        program[i]->initialize (gs, gal, msg);
      }
  }
  bool check (Treelog& msg)
  { 
    bool ok = true;

    Path::InDirectory cwd (directory);
    if (!cwd.check ())
      {
        msg.error ("Could not change to directory '" + directory + "'");
        ok = false;
      }

    for (size_t i = 0; i < program.size (); i++)
      {
        Treelog::Open nest (msg, program[i]->name);
        if (!program[i]->check (msg))
          ok = false;
      }
    return ok;
  }

  ProgramBatch (const Block& al)
    : Program (al),
      directory (al.name ("directory")),
      program (map_build<Program> (al, "run"))
  { }

  ~ProgramBatch ()
  { sequence_delete (program.begin (), program.end ()); }
};

static struct ProgramBatchSyntax
{
  static Program&
  make (const Block& al)
  { return *new ProgramBatch (al); }
  ProgramBatchSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("description", Syntax::String, Syntax::Const, 
                "Description of this batch program.");
    alist.add ("description", "Run a sequence of programs."); 
    syntax.add ("directory", Syntax::String, Syntax::Const, "\
Directory in which to initialize, check and run the programs.");
    alist.add ("directory", ".");
    syntax.add ("run", Librarian<Program>::library (), 
                Syntax::State, Syntax::Sequence, "\
List of programs to run.  The programs will be run in the sequence listed.");
   
    Librarian<Program>::add_type ("batch", alist, syntax, &make);
  }
} ProgramBatch_syntax;
