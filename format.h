// format.h -- Text formatting component.
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


#ifndef FORMAT_H
#define FORMAT_H

#include "model.h"
#include "symbol.h"
#include <iosfwd>
#include <stack>

class Block;

class Format : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const component;
private:
  std::ostream* output;
protected:
  std::ostream& out ();
private:
  std::stack<std::string> nest;
  void push (const std::string& name);
  void pop (const std::string& name);
  
  // List.
private:
  virtual void list_open () = 0;
  virtual void list_close () = 0;
public:
  class List
  {
  private:
    Format& format;
    List (const List&);
  public:
    List (Format&);
    ~List ();  
  };
  friend class List;

  // Item.
private:
  virtual void item_open (const std::string& name) = 0;
  virtual void item_close () = 0;
public:
  class Item
  {
  private:
    Format& format;
    Item (const Item&);
  public:
    Item (Format&, const std::string& name);
    ~Item ();  
  };
  friend class Item;

  // Table.
private:
  virtual void table_open (const std::string& format) = 0;
  virtual void table_close () = 0;
public:
  class Table
  {
  private:
    Format& format;
    Table (const Table&);
  public:
    Table (Format&, const std::string& format);
    ~Table ();  
  };
  friend class Table;

  // TableRow.
private:
  virtual void table_row_open () = 0;
  virtual void table_row_close () = 0;
public:
  class TableRow
  {
  private:
    Format& format;
    TableRow (const TableRow&);
  public:
    TableRow (Format&);
    ~TableRow ();  
  };
  friend class TableRow;

  // TableCell.
private:
  virtual void table_cell_open () = 0;
  virtual void table_cell_close () = 0;
public:
  class TableCell
  {
  private:
    Format& format;
    TableCell (const TableCell&);
  public:
    TableCell (Format&);
    ~TableCell ();  
  };
  friend class TableCell;

  // MultiCell
private:
  virtual void table_multi_cell_open (int cells, 
				      const std::string& format) = 0;
  virtual void table_multi_cell_close () = 0;
public:
  class TableMultiCell
  {
  private:
    Format& format;
    TableMultiCell (const TableMultiCell&);
  public:
    TableMultiCell (Format&, int cells, const std::string& format);
    ~TableMultiCell ();  
  };
  friend class TableMultiCell;

  // Typewriter.
private:
  virtual void typewriter_open () = 0;
  virtual void typewriter_close () = 0;
public:
  class Typewriter
  {
  private:
    Format& format;
    Typewriter (const Typewriter&);
  public:
    Typewriter (Format&);
    ~Typewriter ();  
  };
  friend class Typewriter;

  // Section.
private:
  virtual void section_open (const std::string& type, const std::string& title,
			     const std::string& scope, 
			     const std::string& label) = 0;
  virtual void section_close () = 0;
public:
  class Section
  {
  private:
    Format& format;
    Section (const Section&);
  public:
    Section (Format&, const std::string& type, const std::string& title,
	     const std::string& scope, const std::string& label);
    ~Section ();  
  };
  friend class Section;

  // Document.
private:
  virtual void document_open () = 0;
  virtual void document_close () = 0;
public:
  class Document
  {
  private:
    Format& format;
    Document (const Document&);
  public:
    Document (Format&);
    ~Document ();  
  };
  friend class Document;

  // Use.
public:
  virtual void text (const std::string& text) = 0;
  virtual void bold (const std::string& text) = 0;
  virtual void italic (const std::string& text) = 0;
  virtual void verbatim (const std::string& text) = 0;
  virtual void raw (const std::string& format, const std::string& text) = 0;
  virtual bool formatp (const std::string& format) = 0;
  virtual void special (const std::string& name) = 0;
  virtual void soft_linebreak () = 0;
  virtual void hard_linebreak () = 0;
  virtual void new_paragraph () = 0;
  virtual void index (const std::string& name) = 0;
  virtual void label (const std::string& scope, const std::string& id) = 0;
  virtual void pageref (const std::string& scope, const std::string& id) = 0;
  virtual void ref (const std::string& scope, const std::string& id) = 0;
  virtual void see (const std::string& type,
		    const std::string& scope, const std::string& id) = 0;
  virtual void see_page (const std::string& scope, const std::string& id) = 0;

  // Create and Destroy.
public:
  void initialize (std::ostream&);
protected:
  Format (Block& al);
public:
  ~Format ();
};

#endif // FORMAT_H
