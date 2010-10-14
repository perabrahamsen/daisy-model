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
#include <vector>

class BlockModel;

class Format : public Model
{
  // Content.
public:
  static const char *const component;
  symbol library_id () const;
  virtual symbol format_type () const;
private:
  std::ostream* output;
protected:
  std::ostream& out ();
private:
  std::stack<symbol> nest;
  void push (const symbol name);
  void pop (const symbol name);
  
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
  virtual void item_open (const symbol name) = 0;
  virtual void item_close () = 0;
public:
  class Item
  {
  private:
    Format& format;
    Item (const Item&);
  public:
    Item (Format&, const symbol name);
    ~Item ();  
  };
  friend class Item;

  // Table.
private:
  virtual void table_open (const symbol format) = 0;
  virtual void table_close () = 0;
public:
  class Table
  {
  private:
    Format& format;
    Table (const Table&);
  public:
    Table (Format&, const symbol format);
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
				      const symbol format) = 0;
  virtual void table_multi_cell_close () = 0;
public:
  class TableMultiCell
  {
  private:
    Format& format;
    TableMultiCell (const TableMultiCell&);
  public:
    TableMultiCell (Format&, int cells, const symbol format);
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
  virtual void section_open (const symbol type, const symbol title,
			     const symbol scope, 
			     const symbol label) = 0;
  virtual void section_close () = 0;
public:
  class Section
  {
  private:
    Format& format;
    Section (const Section&);
  public:
    Section (Format&, const symbol type, const symbol title,
	     const symbol scope, const symbol label);
    ~Section ();  
  };
  friend class Section;

  // Document.
private:
  virtual void document_open (const symbol where, const symbol desc) = 0;
  virtual void document_close (const symbol where) = 0;
public:
  class Document : private boost::noncopyable
  {
  private:
    Format& format;
    const symbol where;
  public:
    Document (Format&, const symbol where, const symbol desc);
    ~Document ();  
  };
  friend class Document;

  // Use.
public:
  virtual void version () = 0;
  virtual void text (const symbol text) = 0;
  virtual void bold (const symbol text) = 0;
  virtual void italic (const symbol text) = 0;
  virtual void verbatim (const symbol text) = 0;
  virtual void raw (const symbol format, const symbol text) = 0;
  virtual bool formatp (const symbol format) = 0;
  virtual void special (const symbol name) = 0;
  virtual void soft_linebreak () = 0;
  virtual void hard_linebreak () = 0;
  virtual void new_paragraph () = 0;
  virtual void index (const symbol name) = 0;
  virtual void label (const symbol scope, const symbol id) = 0;
  virtual void pageref (const symbol scope, const symbol id) = 0;
  virtual void ref (const symbol scope, const symbol id) = 0;
  virtual void see (const symbol type,
		    const symbol scope, const symbol id) = 0;
  virtual void see_page (const symbol scope, const symbol id) = 0;
  virtual void cite (const std::vector<symbol>& cite) = 0;
  virtual void frame_description (const Frame&);

  // Create and Destroy.
public:
  void initialize (std::ostream&);
protected:
  Format (const BlockModel& al);
public:
  ~Format ();
};

#endif // FORMAT_H
