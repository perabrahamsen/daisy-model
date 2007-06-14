// gnuplot.C --- Plot a graph with gnuplot.
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
#include "librarian.h"

const char *const Gnuplot::component = "gnuplot";

std::string 
Gnuplot::quote (const std::string& value)
{ return "'" + value + "'"; }

std::string 
Gnuplot::quote (const symbol value)
{ return quote (value.name ()); }

Gnuplot::Gnuplot (Block& al)
  : name (al.identifier ("type"))
{ }

Gnuplot::~Gnuplot ()
{ }

static Librarian Gnuplot_init (Gnuplot::component, "\
Plot a graph with gnuplot.");
