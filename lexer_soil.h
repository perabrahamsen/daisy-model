// lexer_soil.h --- Reading cell content for Daisy table files.
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


#ifndef LEXER_SOIL_H
#define LEXER_SOIL_H

#include "lexer_table.h"

class LexerSoil : public LexerTable
{
private:
  symbol array_name;
  symbol array_dimension;
  std::vector<int> array_c;
  std::vector<double> array_z;
  std::vector<double> array_x;
  std::vector<double> array_dz;
  std::vector<double> array_dx;
  std::vector<double> matrix_zplus;
  std::vector<double> matrix_xplus;
public:
  bool read_soil (Treelog& msg);
  symbol soil_tag () const
  { return array_name; }
  symbol soil_dimension () const
  { return array_dimension; }
  const std::vector<double>& cell_z () const // index: cell
  { return array_z; }
  const std::vector<double>& cell_x () const // index: cell
  { return array_x; }
  const std::vector<double>& cell_dz () const // index: cell
  { return array_dz; }
  const std::vector<double>& cell_dx () const // index: cell
  { return array_dx; }
  const std::vector<double>& soil_zplus () const // index: row
  { return matrix_zplus; }
  const std::vector<double>& soil_xplus () const // index: column
  { return matrix_xplus; }
  bool soil_cells (const std::vector<std::string>& entries,
                   std::vector<double>& values,
                   Treelog& msg) const;

  // Create and Destroy.
public:
  static void load_syntax (Frame&);
  explicit LexerSoil (const BlockModel&);
  ~LexerSoil ();
};

#endif // LEXER_SOIL_H
