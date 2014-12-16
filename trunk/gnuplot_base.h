// gnuplot_base.h -- Shared gnuplot interface code.
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

#ifndef GNUPLOT_BASE_H
#define GNUPLOT_BASE_H

#include "gnuplot.h"
#include <vector>
#include <map>

class Frame;
class FrameSubmodel;

class GnuplotBase : public Gnuplot
{
  // Content.
private:
  const symbol file;
protected:
  const symbol device;
  const symbol canvas;
  const std::vector<symbol> extra;
private:
  const symbol title;

public:
  struct Size 
  {
    const double x;
    const double y;
    static void load_syntax (Frame&);
    explicit Size (const FrameSubmodel* al);
  };
private:
  Size size;
  
  // Legend placement.
public:
  static struct LegendTable : public std::map<symbol, symbol>
  {
    explicit LegendTable ();
  } legend_table;
  symbol legend;

  // Use.
protected:
  void plot_header (std::ostream& out) const;
  bool interactive () const;

  // Create and Destroy.
public:
  static symbol file2device (symbol file);
protected:
  explicit GnuplotBase (const BlockModel& al);
  ~GnuplotBase ();
};

#endif // GNUPLOT_BASE_H
