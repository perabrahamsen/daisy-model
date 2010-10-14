// format_LaTeX.h --- LaTeX text formatting.
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


#include "format.h"

struct FormatLaTeX : public Format
{
  // Content.
  symbol format_type () const;

  // Nesting.
  int list_level;
  void list_open ();
  void list_close ();
  void item_open (const symbol name);
  void item_close ();
  void table_open (const symbol format);
  void table_close ();
  std::stack<bool> table_first_row;
  void table_row_open ();
  void table_row_close ();
  std::stack<bool> table_first_column;
  void table_cell_open ();
  void table_cell_close ();
  void table_multi_cell_open (const int cells, const symbol format);
  void table_multi_cell_close ();
  void typewriter_open ();
  void typewriter_close ();
  void section_open (const symbol type, const symbol title,
		     const symbol scope, 
		     const symbol label);
  void section_close ();
  void document_open (const symbol where, const symbol desc);
  void document_close (const symbol where);
  void version ();

  // Use.
  void text (const symbol text);
  void bold (const symbol text);
  void italic (const symbol text);
  void verbatim (const symbol text);
  void raw (const symbol format, const symbol text);
  bool formatp (const symbol format);
  void special (const symbol name);
  void soft_linebreak ();
  void hard_linebreak ();
  void new_paragraph ();
  void index (const symbol name);
  void quote_id (const symbol name);
  void label (const symbol scope, const symbol id);
  void pageref (const symbol scope, const symbol id);
  void ref (const symbol scope, const symbol id);
  void see (const symbol type,
	    const symbol scope, const symbol id);
  void see_page (const symbol scope, const symbol id);
  void cite (const std::vector<symbol>& cite);
  void frame_description (const Frame& frame);

  // Create and Destroy.
  explicit FormatLaTeX (const BlockModel&);
};

// format_LaTeX.h ends here.
