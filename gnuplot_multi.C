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
#include "block.h"
#include "alist.h"
#include "source.h"
#include "treelog.h"
#include "memutils.h"
#include "librarian.h"
#include <sstream>

struct GnuplotMulti : public Gnuplot
{
  // Content.
  const std::vector<symbol> before;
  const std::vector<symbol> after;
  const std::vector<Gnuplot*> graph;

  // Use.
  bool initialize (Treelog& msg);
  bool plot (std::ostream& out, Treelog& msg);
  
  // Create and Destroy.
  explicit GnuplotMulti (Block& al);
  ~GnuplotMulti ();
};

bool
GnuplotMulti::initialize (Treelog& msg)
{ 
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
  return ok;
}

bool
GnuplotMulti::plot (std::ostream& out, Treelog& msg)
{ 
  bool ok = true;

  for (size_t i = 0; i < before.size (); i++)
    out << before[i].name () << "\n";
 
  for (size_t i = 0; i < graph.size(); i++)
    {
      std::ostringstream tmp;
      tmp << name << "[" << i << "]: " << graph[i]->name;
      Treelog::Open nest (msg, tmp.str ());
      if (!graph[i]->plot (out, msg))
        ok = false;
    }

  for (size_t i = 0; i < after.size (); i++)
    out << after[i].name () << "\n";

  return ok;
}

GnuplotMulti::GnuplotMulti (Block& al)
  : Gnuplot (al),
    before (al.identifier_sequence ("before")),
    after (al.identifier_sequence ("after")),
    graph (Librarian::build_vector<Gnuplot> (al, "graph"))
{ }

GnuplotMulti::~GnuplotMulti ()
{ sequence_delete (graph.begin (), graph.end ()); }

static struct GnuplotMultiSyntax
{
  static Model& make (Block& al)
  { return *new GnuplotMulti (al); }
  GnuplotMultiSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description",
               "Generate multiple graphs for the gnuplot command file."); 
    syntax.add ("before", Syntax::String, Syntax::Const, 
                Syntax::Sequence, "List of extra gnuplot commands.\n\
The commands will be inserted right before the first graph.");
    alist.add ("before", std::vector<symbol> ());
    syntax.add ("after", Syntax::String, Syntax::Const, 
                Syntax::Sequence, "List of extra gnuplot commands.\n\
The commands will be inserted right after the last graph.");
    alist.add ("after", std::vector<symbol> ());
    syntax.add_object ("graph", Gnuplot::component, Syntax::State, 
                       Syntax::Sequence, "Graphs to plot.");

    Librarian::add_type (Gnuplot::component, "multi", alist, syntax, &make);
  }
} GnuplotMulti_syntax;
