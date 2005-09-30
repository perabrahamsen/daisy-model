// printer.h --- Support for printing alist structures.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#ifndef PRINTER_H
#define PRINTER_H

#include "librarian.h"

class Printer
{
  // Content.
public:
  const symbol name;
  static const char *const description;

  // Interface.
public:
  // Print comment.
  virtual void print_comment (const std::string& comment) = 0;
  // Print content of alist.
  virtual void print_alist (const AttributeList& alist, const Syntax&,
			    const AttributeList& super) = 0;
  // Print entry in alist.
  virtual void print_entry (const AttributeList&, const Syntax&,
			    const std::string& key) = 0;
  // Print all elements in all libraries associated with 'filename'.
  virtual void print_library_file (const std::string& filename) = 0;
  // Print a parser input.
  virtual void print_input (const AttributeList& alist) = 0;

  // True iff no errors have occured.
  virtual bool good () = 0;

  // Create and Destroy.
protected:
  Printer (const AttributeList& name);
public:
  virtual ~Printer ();
};

#ifdef FORWARD_TEMPLATES
template<>
Librarian<Printer>::Content* Librarian<Printer>::content;
#endif

static Librarian<Printer> Printer_init ("printer");

#endif // PRINTER_H
