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

#include "librarian.h"
#include <iosfwd>
#include <stack>

class Format
{
  // Content.
public:
  const symbol name;
  static const char *const description;
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

  // Use.
public:
  virtual void text (const std::string& text) = 0;
  virtual void bold (const std::string& text) = 0;
  virtual void italic (const std::string& text) = 0;
  virtual void verbatim (const std::string& text) = 0;
  virtual void raw (const std::string& format, const std::string& text) = 0;
  virtual void special (const std::string& name) = 0;
  virtual void soft_linebreak () = 0;
  virtual void hard_linebreak () = 0;
  virtual void new_paragraph () = 0;
  virtual void index (const std::string& name) = 0;
  virtual void see (const std::string& type,
		    const std::string& scope, const std::string& id) = 0;
  virtual void see_page (const std::string& scope, const std::string& id) = 0;

  // Create and Destroy.
public:
  void initialize (std::ostream&);
protected:
  Format (const AttributeList& al);
public:
  virtual ~Format ();
};

#ifdef FORWARD_TEMPLATES
EMPTY_TEMPLATE
Librarian<Format>::Content* Librarian<Format>::content;
#endif

static Librarian<Format> Format_init ("format");

#endif // FORMAT_H
