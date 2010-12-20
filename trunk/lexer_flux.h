// lexer_flux.h --- Reading edge content for Daisy table files.
// 
// Copyright 2005 Per Abrahamsen and KVL.
// Copyright 2010 KU.
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


#ifndef LEXER_FLUX_H
#define LEXER_FLUX_H

#include "lexer_table.h"
#include <iosfwd>

class LexerFlux : public LexerTable
{
private:
  symbol array_name;
  symbol array_dimension;
  std::vector<int> array_c;
  std::vector<double> center_z;
  std::vector<double> center_x;
  std::vector<int> flux_from;
  std::vector<int> flux_to;
  std::vector<double> flux_z;
  int read_cell (std::istream& in);
public:
  bool read_flux (Treelog& msg);
  symbol flux_tag () const
  { return array_name; }
  symbol flux_dimension () const
  { return array_dimension; }
  const std::vector<int>& edge_from () const
  { return flux_from; }
  const std::vector<int>& edge_to () const
  { return flux_to; }
  const std::vector<double>& edge_z () const
  { return flux_z; }
  const std::vector<double>& cell_z () const
  { return center_z; }
  const std::vector<double>& cell_x () const
  { return center_x; }
  bool flux_edges (const std::vector<std::string>& entries,
                   std::vector<double>& values,
                   Treelog& msg) const;

  // Create and Destroy.
public:
  static void load_syntax (Frame&);
  explicit LexerFlux (const BlockModel&);
  ~LexerFlux ();
};

#endif // LEXER_FLUX_H
