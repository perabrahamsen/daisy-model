// gnuplot.h --- A single gnuplot graph.
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

#ifndef GNUPLOT_H
#define GNUPLOT_H

#include "model.h"
#include "symbol.h"
#include <ostream>

class Treelog;
class Block;

class Gnuplot : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const description;
  static const char *const component;

  // Utilities.
public:
  static std::string quote (const std::string& value);
  static std::string quote (symbol);

  // Simulation.
public:
  virtual bool initialize (Treelog& err) = 0;
  virtual bool plot (std::ostream& out, Treelog&) = 0;

  // Create and Destroy.
protected:
  explicit Gnuplot (Block&);
private:
  explicit Gnuplot ();
  explicit Gnuplot (const Gnuplot&);
public:
  ~Gnuplot ();
};

#endif // GNUPLOT_H
