// document.h
// 
// Create documentation for Daisy.

#ifndef DOCUMENT_H
#define DOCUMENT_H

#include "librarian.h"

struct ostream;

class Document
{
  // Typeset parts of it.
protected:
  virtual void print_submodel_entry (ostream&, const string&, int level,
				     const Syntax& syntax,
				     const AttributeList& alist) = 0;
  virtual void print_submodel_empty (ostream&, const string&, int level) = 0;
  virtual void print_submodel_header (ostream&, const string&, int level) = 0;
  virtual void print_submodel_trailer (ostream&, const string&, int level) = 0;
  virtual void print_sample_ordered (ostream& out, const string& name) = 0;
  virtual void print_sample_entry (ostream& out, const string& name) = 0;
  virtual void print_sample_header (ostream& out, const string& name) = 0;
  virtual void print_sample_trailer (ostream& out, const string&) = 0;
  virtual void print_model_header (ostream&, const string& name) = 0;
  virtual void print_model_description (ostream&, const string&) = 0;
  virtual void print_model_trailer (ostream&, const string& name) = 0;
  virtual void print_component_header (ostream&, const string& name) = 0;
  virtual void print_component_description (ostream&, const string&) = 0;
  virtual void print_component_trailer (ostream&, const string& name) = 0;
  virtual void print_document_header (ostream&) = 0;
  virtual void print_document_trailer (ostream&) = 0;

  // Print parts of it.
protected:
  void print_submodel (ostream& out, const string& name, int level,
		       const Syntax& syntax,
		       const AttributeList& alist);
private:
  void print_sample (ostream& out, const string& name,
		     const Syntax& syntax,
		     const AttributeList& alist);
  void print_model (ostream& out, const string& name, 
		    const Syntax& syntax,
		    const AttributeList& alist);
  void print_component (ostream& out, const Library& library);

  // Print it.
public:
  void print_document (ostream& out);

  // Print syntactical sugar.

  // Create and destroy.
public: 
  Document (const AttributeList&);
  virtual ~Document ();
};

static Librarian<Document> Document_init ("document");

#endif DOCUMENT_H
