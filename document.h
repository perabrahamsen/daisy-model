// document.h --- Create documentation.
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


#ifndef DOCUMENT_H
#define DOCUMENT_H

#include "librarian.h"
#include "xref.h"

class Document
{
  // Content.
public:
  static const char *const description;
private:
  XRef xref;

  // Typeset parts of it.
protected:
  virtual void print_users (std::ostream&, const XRef::Users&) = 0;
  virtual void print_submodel_entry (std::ostream&, const std::string&, int level,
				     const Syntax& syntax,
				     const AttributeList& alist) = 0;
  virtual void print_submodel_empty (std::ostream&, const std::string&,
                                     int level) = 0;
  virtual void print_submodel_header (std::ostream&, const std::string&,
                                      int level) = 0;
  virtual void print_submodel_trailer (std::ostream&, const std::string&,
                                       int level) = 0;
  virtual void print_log_header (std::ostream&, const std::string&, int level) = 0;
  virtual void print_log_trailer (std::ostream&, const std::string&, int level) = 0;
  virtual void print_sample_ordered (std::ostream&, const std::string&,
                                     bool seq) = 0;
  virtual void print_sample_entry (std::ostream&, const std::string& name, 
				   const Syntax& syntax,
				   const AttributeList& alist) = 0;
  virtual void print_sample_header (std::ostream& out, const std::string& name) = 0;
  virtual void print_sample_trailer (std::ostream& out, const std::string&) = 0;
  virtual void print_model_header (std::ostream&, symbol name) = 0;
  virtual void print_model_description (std::ostream&, const std::string&) = 0;
  virtual void print_model_trailer (std::ostream&, symbol name) = 0;
  virtual void print_parameterization_header (std::ostream& out, 
					      symbol name, symbol type) = 0;
  virtual void print_parameterization_file (std::ostream& out, 
					    const std::string& name) = 0;
  virtual void print_parameterization_no_file (std::ostream& out) = 0;
  virtual void print_parameterization_description (std::ostream& out, 
						   const std::string&
						   description) = 0;
  virtual void print_parameterization_trailer (std::ostream& out, 
					       symbol name) = 0;
  virtual void print_fixed_header (std::ostream&, const std::string& name) = 0;
  virtual void print_fixed_trailer (std::ostream&, const std::string& name) = 0;
  virtual void print_component_header (std::ostream&, symbol name) = 0;
  virtual void print_component_description (std::ostream&, const std::string&) = 0;
  virtual void print_component_trailer (std::ostream&, symbol name) = 0;
  virtual void print_fixed_all_header (std::ostream&) = 0;
  virtual void print_fixed_all_trailer (std::ostream&) = 0;
  virtual void print_document_header (std::ostream&) = 0;
  virtual void print_document_trailer (std::ostream&) = 0;

  // Print parts of it.
protected:
  void print_submodel (std::ostream& out, const std::string& name, int level,
		       const Syntax& syntax,
		       const AttributeList& alist);
  void print_sample (std::ostream& out, const std::string& name,
		     const Syntax& syntax,
		     const AttributeList& alist);
private:
  void print_model (std::ostream& out, symbol name, 
		    const Library& library);
  void print_fixed (std::ostream& out, const std::string& name, 
		    const Syntax& syntax,
		    const AttributeList& alist);
  void print_component (std::ostream& out, const Library& library);

  // Print it.
public:
  void print_document (std::ostream& out);

  // Print syntactical sugar.

  // Create and destroy.
public: 
  Document (const AttributeList&);
  virtual ~Document ();
};

#ifdef FORWARD_TEMPLATES
EMPTY_TEMPLATE
Librarian<Document>::Content* Librarian<Document>::content;
#endif

static Librarian<Document> Document_init ("document");

#endif // DOCUMENT_H
