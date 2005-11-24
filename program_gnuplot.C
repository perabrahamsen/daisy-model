// program_gnuplot.C -- gnuplot interface 
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

#include "program.h"
#include "gnuplot.h"
#include "treelog.h"
#include "path.h"
#include "memutils.h"
#include <string>
#include <set>
#include <fstream>
#include <sstream>

struct ProgramGnuplot : public Program
{
  // Content.
  const std::string command_file;
  const bool do_cd;
  const std::vector<symbol> extra;

  // Graphs.
  const std::vector<Gnuplot*> graph;

  // Use.
  bool run (Treelog& msg);
  
  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { }
  bool check (Treelog&)
  { return true; }
  explicit ProgramGnuplot (Block& al);
  ~ProgramGnuplot ();
};

bool
ProgramGnuplot::run (Treelog& msg)
{ 
  // Initialize.
  {
    Treelog::Open nest (msg, "Reading");
    bool ok = true;
    for (size_t i = 0; i < graph.size(); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]: " << graph[i]->name;
        Treelog::Open nest (msg, tmp.str ());
        msg.touch ();
        if (!graph[i]->initialize (msg))
          ok = false;
      }
    if (!ok)
      return false;
  }

  // Open file, and change directory.
  const std::string dir = Path::get_directory ();
  std::ofstream out (command_file.c_str ());
  if (do_cd)
    out << "cd " << Gnuplot::quote (dir) << "\n";

  // Extra.
  for (size_t i = 0; i < extra.size (); i++)
    out << extra[i].name () << "\n";

  
  // Plot.
  {
    Treelog::Open nest (msg, "Writing");
    msg.touch ();
    bool ok = true;
    for (size_t i = 0; i < graph.size(); i++)
      {
        std::ostringstream tmp;
        tmp << name << "[" << i << "]: " << graph[i]->name;
        Treelog::Open nest (msg, tmp.str ());
        if (!graph[i]->plot (out, msg))
          ok = false;
      }
    if (!ok)
      return false;
  }

  // Done.
  if (!out.good ())
    {
      msg.error ("Problems writing to temporary file '" + command_file + "'");
      return false;
    }
  return true;
}

ProgramGnuplot::ProgramGnuplot (Block& al)
  : Program (al),
    command_file (al.name ("command_file")),
    do_cd (al.flag ("cd")),
    extra (al.identifier_sequence ("extra")),
    graph (Librarian<Gnuplot>::build_vector (al, "graph"))
{ }

ProgramGnuplot::~ProgramGnuplot ()
{ sequence_delete (graph.begin (), graph.end ()); }

static struct ProgramGnuplotSyntax
{
  static Program&
  make (Block& al)
  { return *new ProgramGnuplot (al); }
  ProgramGnuplotSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description",
               "Generate a gnuplot command file."); 
    syntax.add ("command_file", Syntax::String, Syntax::Const, "\
File name for gnuplot commands.");
    alist.add ("command_file", "daisy.gnuplot");
    syntax.add ("cd", Syntax::Boolean, Syntax::Const, "\
Set this flag to add a 'cd' command to the current working directory.\n\
This is useful under MS Windows when dragging the file to a gnuplot icon.");
#if defined(__unix)
    alist.add ("cd", false);
#else
    alist.add ("cd", true);
#endif
    syntax.add ("extra", Syntax::String, Syntax::Const, 
                Syntax::Sequence, "List of extra gnuplot commands.\n\
The commands will be inserted right before the list of graphs.");
    alist.add ("extra", std::vector<symbol> ());
                
    syntax.add ("graph", Librarian<Gnuplot>::library (), Syntax::State, 
		Syntax::Sequence, "Graphs to plot.");
    Librarian<Program>::add_type ("gnuplot", alist, syntax, &make);
  }
} ProgramGnuplot_syntax;
