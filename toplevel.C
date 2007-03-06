// toplevel.h -- The top level syntax for .dai files.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#include "toplevel.h"
#include "daisy.h"
#include "library.h"
#include "parser.h"
#include "submodel.h"
#include "block.h"
#include "program.h"
#include "syntax.h"
#include "alist.h"

void
Toplevel::run (Treelog& msg)
{
  program->run (msg);
}

void
Toplevel::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Top level Daisy syntax.
  Daisy::load_syntax (syntax, alist);
  alist.add ("type", "Daisy");
  Library::load_syntax (syntax, alist);
      
  syntax.add ("directory", Syntax::String, Syntax::OptionalConst,
              "Run program in this directory.\n\
This can affect both where input files are found and where log files\n\
are generated.");
  syntax.add ("path", Syntax::String,
              Syntax::OptionalConst, Syntax::Sequence,
              "List of directories to search for input files in.\n\
The special value \".\" means the current directory.");
  syntax.add ("input", Librarian<Parser>::library (),
              Syntax::OptionalConst, Syntax::Singleton,
              "Command to add more information about the simulation.");
  syntax.add ("run", Librarian<Program>::library (), 
              Syntax::OptionalState, Syntax::Singleton, 
              "Program to run.\n\
\n\
If this option is specified, all the 'Daisy' specific top-level attributes\n\
will be ignored.  If unspecified, run 'Daisy' on the current top-level\n\
attributes.");
}

std::auto_ptr<Program>
Toplevel::build_program (const Syntax& top_syntax, 
                         const AttributeList& top_alist,
                         Treelog& msg)
{
  // Explicit or implicit program?
  const Library& library = Librarian<Program>::library ();
  const Syntax* run_syntax;
  const AttributeList* run_alist;

  if (top_alist.check ("run"))
    {
      run_alist = &top_alist.alist ("run");
      daisy_assert (run_alist->check ("type"));
      run_syntax = &library.syntax (run_alist->identifier ("type"));
    }
  else
    {
      run_alist = &top_alist;
      run_syntax = &top_syntax;
    }

  // Create, check and run the program.
  if (!run_syntax->check (*run_alist, msg))
    throw EXIT_FAILURE;

  std::auto_ptr<Program> program;
  Block block (top_syntax, top_alist, msg, "Building");
  program.reset (Librarian<Program>::build_alist (block, *run_alist, "run"));
  if (!block.ok ())
    throw EXIT_FAILURE;

  return program;
}

Toplevel::Toplevel (const Syntax& top_syntax, const AttributeList& top_alist,
                    Treelog& msg)
  : program (build_program (top_syntax, top_alist, msg))
{ 
  program->initialize (&top_syntax, &top_alist, msg);
  if (!program->check (msg))
    throw EXIT_FAILURE;
}

static Submodel::Register 
toplevel_submodel ("Toplevel", Toplevel::load_syntax);

// toplevel.C ends here.
