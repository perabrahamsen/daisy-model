// document.C -- Create documentation.
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

#include "document.h"
#include "submodel.h"
#include "common.h"

#include <algorithm>

EMPTY_TEMPLATE
Librarian<Document>::Content* Librarian<Document>::content = NULL;

const char *const Document::description = "\
This component is responsible for printing documentation for all the\n\
models and components.  Each 'document' model outputs the\n\
documentation in a form suitable for a specific presentation system.";

void 
Document::print_submodel (ostream& out, const string& name, int level,
			  const Syntax& syntax,
			  const AttributeList& alist)
{
  const vector<string>& order = syntax.order ();
  vector<string> entries;
  syntax.entries (entries);
  int log_count = 0;
  for (unsigned int i = 0; i < entries.size (); i++)
    if (syntax.is_log (entries[i]))
      log_count++;

  if (entries.size () == 0)
    print_submodel_empty (out, name, level);
  else
    {
      // Print normal attributes.
      if (log_count < entries.size ())
	{
	  print_submodel_header (out, name, level);

	  // Ordered members first.
	  for (unsigned int i = 0; i < order.size (); i++)
	    print_submodel_entry (out, order[i], level, syntax, alist);
      
	  // Then the remaining members, except log variables.
	  for (unsigned int i = 0; i < entries.size (); i++)
	    if (syntax.order (entries[i]) < 0 && !syntax.is_log (entries[i]))
	      print_submodel_entry (out, entries[i], level, syntax, alist);

	  print_submodel_trailer (out, name, level);
	}

      // Print log variables.
      if (log_count > 0)
	{
	  print_log_header (out, name, level);

	  for (unsigned int i = 0; i < entries.size (); i++)
	    if (syntax.is_log (entries[i]))
	      print_submodel_entry (out, entries[i], level, syntax, alist);

	  print_log_trailer (out, name, level);
	}
    }
}

void 
Document::print_sample (ostream& out, const string& name,
			const Syntax& syntax,
			const AttributeList& alist)
{
  print_sample_header (out, name);

  // Ordered members first.
  const vector<string>& order = syntax.order ();
  for (unsigned int i = 0; i < order.size (); i++)
    print_sample_ordered (out, order[i], 
			  syntax.size (order[i]) == Syntax::Sequence);
      
  // Then the remaining members.
  vector<string> entries;
  syntax.entries (entries);
  for (unsigned int i = 0; i < entries.size (); i++)
    if (syntax.order (entries[i]) < 0
	&& !syntax.is_log (entries[i])
	&& syntax.lookup (entries[i]) != Syntax::Library)
      print_sample_entry (out, entries[i], syntax, alist);

  print_sample_trailer (out, name);
}

void
Document::print_model (ostream& out, const symbol name, 
		       const Library& library)
{
  
  const Syntax& syntax = library.syntax (name);
  const AttributeList& alist = library.lookup (name);  

  const XRef::ModelUsed used (library.name (), name);
  if (alist.check ("type"))
    {
      const symbol type = alist.identifier ("type");
      print_parameterization_header (out, name, type);
      
      if (alist.check ("parsed_from_file"))
	print_parameterization_file (out, alist.name ("parsed_from_file"));
      else
	print_parameterization_no_file (out);

      if (alist.check ("description"))
	{
	  daisy_assert (library.check (type));
	  const AttributeList& super = library.lookup (type);
	  const string description = alist.name ("description");
	  
	  if (!super.check ("description") 
	      || super.name ("description") != description)
	    print_parameterization_description (out, description);
	}
      print_users (out, xref.models[used]);
      print_parameterization_trailer (out, name);
    }
  else
    {
      print_model_header (out, name);

      // Print description, if any.
      if (alist.check ("description"))
	print_model_description (out, alist.name ("description"));

      print_users (out, xref.models[used]);
      print_sample (out, name.name (), syntax, alist);
      print_submodel (out, name.name (), 0, syntax, alist);
      print_model_trailer (out, name);
    }
}

void
Document::print_fixed (ostream& out, const string& name, 
		       const Syntax& syntax,
		       const AttributeList& alist)
{
  print_fixed_header (out, name);

  // Print description, if any.
  if (alist.check ("description"))
    print_model_description (out, alist.name ("description"));

  print_users (out, xref.submodels[name]);

  print_sample (out, name, syntax, alist);
  print_submodel (out, name, 0, syntax, alist);
  print_fixed_trailer (out, name);
}

class ModelCompare
{ 
  const Library& library;

  const symbol find_next_in_line (const symbol root, const symbol leaf) const
  {
    // Find the child of root that leaf is descended from.
    daisy_assert (root != leaf);
    const AttributeList& al = library.lookup (leaf);
    daisy_assert (al.check ("type"));
    const symbol type = al.identifier ("type");
    if (type == root)
      return leaf;
    
    return find_next_in_line (root, type);
  }

public:
  bool operator() (const symbol a, const symbol b) const
  { 
    // They may be the same.
    if (a == b)
      return false;

    // One may be a derivative of the other.  Sort base first.
    if (library.is_derived_from (a, b))
      return false;
    if (library.is_derived_from (b, a))
      return true;

    symbol base_a = library.base_model (a);
    symbol base_b = library.base_model (b);
    
    // They may be otherwise related.
    while (base_a == base_b)
      {
	// Find place where tree branches,
	base_a = find_next_in_line (base_a, a);
	base_b = find_next_in_line (base_b, b);
      }
    // Unrelated, sort according to their base classes.
    return  base_a < base_b;
  }
  ModelCompare (const Library& lib)
    : library (lib)
  { }
};

void
Document::print_component (ostream& out, const Library& library)
{

  const symbol name = library.name ();
  print_component_header (out, name);

  const char *const description = library.description ();
  if (description)
    print_component_description (out, description);

  print_users (out, xref.components[name]);

  // For all members...
  vector<symbol> entries;
  library.entries (entries);
  ModelCompare model_compare (library);
  sort (entries.begin (), entries.end (), model_compare);
  for (unsigned int i = 0; i < entries.size (); i++)
    print_model (out, entries[i], library);

  print_component_trailer (out, name);
}

void
Document::print_document (ostream& out)
{
  print_document_header (out);

  // For all components...
  vector<symbol> entries;
  Library::all (entries);
  for (unsigned int i = 0; i < entries.size (); i++)
    print_component (out, Library::find (entries[i]));

  print_fixed_all_header (out);

  // Fixed components.
  vector<string> fixed;
  Submodel::all (fixed);
  for (unsigned int i = 0; i < fixed.size (); i++)
    {
      const string& name = fixed[i];
      Syntax syntax;
      AttributeList alist;
      Submodel::load_syntax (name, syntax, alist);
      print_fixed (out, name, syntax, alist);
  }
  print_fixed_all_trailer (out);

  print_document_trailer (out);
}

Document::Document (const AttributeList&)
{ }

Document::~Document ()
{ }
