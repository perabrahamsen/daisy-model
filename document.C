// document.C
// 
// Create documentation for Daisy.

#include "document.h"
#include "library.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"

Librarian<Document>::Content* Librarian<Document>::content = NULL;

void 
Document::print_submodel (ostream& out, const string& name, int level,
			  const Syntax& syntax,
			  const AttributeList& alist)
{
  const vector<string>& order = syntax.order ();
  vector<string> entries;
  syntax.entries (entries);

  if (entries.size () == 0)
    print_submodel_empty (out, name, level);
  else
    {
      print_submodel_header (out, name, level);

      // Ordered members first.
      for (unsigned int i = 0; i < order.size (); i++)
	print_submodel_entry (out, order[i], level, syntax, alist);
      
      // Then the remaining members.
      for (unsigned int i = 0; i < entries.size (); i++)
	if (syntax.order (entries[i]) < 0)
	  print_submodel_entry (out, entries[i], level, syntax, alist);

      print_submodel_trailer (out, name, level);
    }
}

void 
Document::print_sample (ostream& out, const string& name,
			const Syntax& syntax,
			const AttributeList& /* alist */)
{
  print_sample_header (out, name);

  // Ordered members first.
  const vector<string>& order = syntax.order ();
  for (unsigned int i = 0; i < order.size (); i++)
    print_sample_ordered (out, order[i]);
      
  // Then the remaining members.
  vector<string> entries;
  syntax.entries (entries);
  for (unsigned int i = 0; i < entries.size (); i++)
    if (syntax.order (entries[i]) < 0 && !syntax.is_log (entries[i]))
      print_sample_entry (out, entries[i]);

  print_sample_trailer (out, name);
}

void
Document::print_model (ostream& out, const string& name, 
		       const Syntax& syntax,
		       const AttributeList& alist)
{
  print_model_header (out, name);

  // Print description, if any.
  if (alist.check ("description"))
    print_model_description (out, alist.name ("description"));

  print_sample (out, name, syntax, alist);
  print_submodel (out, name, 0, syntax, alist);
  print_model_trailer (out, name);
}

void
Document::print_component (ostream& out, const Library& library)
{
  const string& name = library.name ();
  print_component_header (out, name);

  const char *const description = library.description ();
  if (description)
    print_component_description (out, description);

  // For all members...
  vector<string> entries;
  library.entries (entries);
  for (unsigned int i = 0; i < entries.size (); i++)
    print_model (out, entries[i], 
		   library.syntax (entries[i]),
		   library.lookup (entries[i]));

  print_component_trailer (out, name);
}

void
Document::print_document (ostream& out)
{
  print_document_header (out);

  // For all components...
  vector<string> entries;
  Library::all (entries);
  for (unsigned int i = 0; i < entries.size (); i++)
    print_component (out, Library::find (entries[i]));

  print_document_trailer (out);
}

Document::Document (const AttributeList&)
{ }

Document::~Document ()
{ }
