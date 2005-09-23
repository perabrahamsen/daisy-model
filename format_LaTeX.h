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
  // Nesting.
  int list_level;
  void list_open ();
  void list_close ();
  void item_open (const std::string& name);
  void item_close ();
  void table_open (const std::string& format);
  void table_close ();
  std::stack<bool> table_first_row;
  void table_row_open ();
  void table_row_close ();
  std::stack<bool> table_first_column;
  void table_cell_open ();
  void table_cell_close ();
  void table_multi_cell_open (const int cells, const std::string& format);
  void table_multi_cell_close ();
  void typewriter_open ();
  void typewriter_close ();
  void section_open (const std::string& type, const std::string& title,
		     const std::string& scope, 
		     const std::string& label);
  void section_close ();
  void document_open ();
  void document_close ();

  // Use.
  void text (const std::string& text);
  void bold (const std::string& text);
  void italic (const std::string& text);
  void verbatim (const std::string& text);
  void raw (const std::string& format, const std::string& text);
  bool formatp (const std::string& format);
  void special (const std::string& name);
  void soft_linebreak ();
  void hard_linebreak ();
  void new_paragraph ();
  void index (const std::string& name);
  void label (const std::string& scope, const std::string& id);
  void pageref (const std::string& scope, const std::string& id);
  void ref (const std::string& scope, const std::string& id);
  void see (const std::string& type,
	    const std::string& scope, const std::string& id);
  void see_page (const std::string& scope, const std::string& id);

  // Create and Destroy.
  explicit FormatLaTeX (const AttributeList&);
};

// format_LaTeX.h ends here.
