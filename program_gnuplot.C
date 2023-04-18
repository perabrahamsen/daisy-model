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

#define BUILD_DLL
#include "program.h"
#include "block_model.h"
#include "gnuplot.h"
#include "treelog.h"
#include "path.h"
#include "memutils.h"
#include "librarian.h"
#include "frame.h"
#include <string>
#include <set>
#include <fstream>

struct ProgramGnuplot : public Program
{
  // Content.
  const Units& units;
  Path& path;
  const symbol command_file;
  const bool do_cd;
  const std::vector<symbol> extra;

  // Graphs.
  auto_vector<Gnuplot*> graph;

  // Use.
  bool run (Treelog& msg);
  
  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog&)
  { return true; }
  explicit ProgramGnuplot (const BlockModel& al);
  ~ProgramGnuplot ();
};

bool
ProgramGnuplot::run (Treelog& msg)
{ 
  TREELOG_MODEL (msg);
  bool ok = true;

  // Open file, and change directory.
  const symbol dir = path.get_output_directory ();
  std::ofstream out (command_file.name ().c_str ());
  if (do_cd)
    out << "cd " << Gnuplot::quote (dir) << "\n";

  // Process graphs.
  while (graph.size() > 0)
    {
      Gnuplot* gg = graph[0];
      Treelog::Open nest (msg, gg->objid);
      msg.touch ();
      
      // Initialize
      if (!gg->initialize (units, msg))
	{
	  ok = false;
	  break;
	}

      // Extra.
      for (size_t i = 0; i < extra.size (); i++)
	out << extra[i].name () << "\n";

      // Plot.
      if (!gg->plot (out, msg))
	{
	  ok = false;
	  break;
	}
      
      // Cleanup.
      graph.erase (graph.begin ());
      delete gg;
    }

  // Done.
  if (!out.good ())
    {
      msg.error ("Problems writing to temporary file '" + command_file + "'");
      return false;
    }
  return ok;
}

ProgramGnuplot::ProgramGnuplot (const BlockModel& al)
  : Program (al),
    units (al.units ()),
    path (al.path ()),
    command_file (al.name ("command_file")),
    do_cd (al.flag ("cd")),
    extra (al.name_sequence ("extra")),
    graph (Librarian::build_vector<Gnuplot> (al, "graph"))
{ }

ProgramGnuplot::~ProgramGnuplot ()
{ }

static struct ProgramGnuplotSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramGnuplot (al); }
  ProgramGnuplotSyntax ()
    : DeclareModel (Program::component, "gnuplot",
               "Generate a gnuplot command file.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("command_file", Attribute::Const, "\
File name for gnuplot commands.");
    frame.set ("command_file", "daisy.gnuplot");
    frame.declare_boolean ("cd", Attribute::Const, "\
Set this flag to add a 'cd' command to the current working directory.\n\
This is useful under MS Windows when dragging the file to a gnuplot icon.");
#if defined(__unix)
    frame.set ("cd", false);
#else
    frame.set ("cd", true);
#endif
    frame.declare_string ("extra", Attribute::Const, 
                Attribute::Variable, "List of extra gnuplot commands.\n\
The commands will be inserted right before the list of graphs.");
    frame.set_empty ("extra");
                
    frame.declare_object ("graph", Gnuplot::component, Attribute::State, 
                       Attribute::Variable, "Graphs to plot.");
  }
} ProgramGnuplot_syntax;
