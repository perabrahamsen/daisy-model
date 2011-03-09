// gnuplot_multi.C -- Multiple graphs in one plot.
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
#include "gnuplot.h"
#include "block_model.h"
#include "source.h"
#include "treelog.h"
#include "memutils.h"
#include "librarian.h"
#include "frame.h"

struct GnuplotMulti : public Gnuplot
{
  // Content.
  const std::vector<symbol> before;
  const std::vector<symbol> after;
  const std::vector<Gnuplot*> graph;

  // Use.
  bool initialize (const Units& units, Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  explicit GnuplotMulti (const BlockModel& al);
  ~GnuplotMulti ();
};

bool
GnuplotMulti::initialize (const Units& units, Treelog& msg)
{ 
  bool ok = true;
  for (size_t i = 0; i < graph.size(); i++)
    {
      Treelog::Open nest (msg, objid, i, graph[i]->objid);
      msg.touch ();
      if (!graph[i]->initialize (units, msg))
        ok = false;
    }
  return ok;
}

bool
GnuplotMulti::plot (std::ostream& out, Treelog& msg)
{ 
  bool ok = true;

  for (size_t i = 0; i < before.size (); i++)
    out << before[i] << "\n";
 
  for (size_t i = 0; i < graph.size(); i++)
    {
      Treelog::Open nest (msg, objid, i, graph[i]->objid);
      if (!graph[i]->plot (out, msg))
        ok = false;
    }

  for (size_t i = 0; i < after.size (); i++)
    out << after[i] << "\n";

  return ok;
}

GnuplotMulti::GnuplotMulti (const BlockModel& al)
  : Gnuplot (al),
    before (al.name_sequence ("before")),
    after (al.name_sequence ("after")),
    graph (Librarian::build_vector<Gnuplot> (al, "graph"))
{ }

GnuplotMulti::~GnuplotMulti ()
{ sequence_delete (graph.begin (), graph.end ()); }

static struct GnuplotMultiSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new GnuplotMulti (al); }
  GnuplotMultiSyntax ()
    : DeclareModel (Gnuplot::component, "multi",
               "Generate multiple graphs for the gnuplot command file.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("before", Attribute::Const, 
                Attribute::Variable, "List of extra gnuplot commands.\n\
The commands will be inserted right before the first graph.");
    frame.set_empty ("before");
    frame.declare_string ("after", Attribute::Const, 
                Attribute::Variable, "List of extra gnuplot commands.\n\
The commands will be inserted right after the last graph.");
    frame.set_empty ("after");
    frame.declare_object ("graph", Gnuplot::component, Attribute::State, 
                       Attribute::Variable, "Graphs to plot.");

  }
} GnuplotMulti_syntax;
