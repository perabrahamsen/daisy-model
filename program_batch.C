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

#define BUILD_DLL

#include "program.h"
#include "block.h"
#include "alist.h"
#include "treelog.h"
#include "path.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include <vector>

struct ProgramBatch : public Program
{
  // Content.
  Metalib& metalib;
  Path& path;
  const std::string directory;
  std::vector<Program*> program;
  
  // Use.
  bool run (Treelog& msg)
  { 

    Path::InDirectory cwd (path, directory);
    if (!cwd.check ())
      {
        msg.error ("Could not change to directory '" + directory + "'");
        return false;
      }

    for (size_t i = 0; program.size () > 0; i++)
      {
        if (!ui_running ())
          return false;

        Treelog::Open nest (msg, name.name (), i, program[i]->name);
        msg.touch ();
        {
          Block block (metalib, msg, "Initializing");
          program[0]->initialize (block);
          if (!block.ok ())
            throw EXIT_FAILURE;
        }
        if (!program[0]->check (msg))
          return false;
        propagate_ui (program[0]);
        if (!program[0]->run (msg))
          return false;
        delete *program.begin ();
        program.erase (program.begin ());
      }
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog&)
  { return true; }

  ProgramBatch (Block& al)
    : Program (al),
      metalib (al.metalib ()),
      path (al.path ()),
      directory (al.name ("directory")),
      program (Librarian::build_vector<Program> (al, "run"))
  { }

  ~ProgramBatch ()
  { sequence_delete (program.begin (), program.end ()); }
};

static struct ProgramBatchSyntax
{
  static Model& make (Block& al)
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
    syntax.add_object ("run", Program::component, 
                       Syntax::State, Syntax::Sequence, "\
List of programs to run.  The programs will be run in the sequence listed.");
   
    Librarian::add_type (Program::component, "batch", alist, syntax, &make);
  }
} ProgramBatch_syntax;
