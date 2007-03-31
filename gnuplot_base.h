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
#include <string>
#include <map>

class Syntax;
class AttributeList;

class GnuplotBase : public Gnuplot
{
  // Content.
private:
  const std::string file;
protected:
  const std::string device;
  const std::vector<symbol> extra;
private:
  const std::string title;

private:
  struct Size 
  {
    const double x;
    const double y;
    static void load_syntax (Syntax& syntax, AttributeList&);
    static const AttributeList& unset ();
    explicit Size (const AttributeList& al);
  };
  Size size;
  
  // Legend placement.
protected:
  static struct LegendTable : public std::map<std::string,std::string>
  {
    explicit LegendTable ();
  } legend_table;
  std::string legend;

  // Use.
protected:
  void plot_header (std::ostream& out) const;
  
  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList&);
private:
  static std::string file2device (const std::string& file);
protected:
  explicit GnuplotBase (Block& al);
  ~GnuplotBase ();
};

#endif // GNUPLOT_BASE_H
