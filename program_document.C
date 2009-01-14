// program_document.C -- Create reference documentation for Daisy.
// 
// Copyright 2002, 2005 Per Abrahamsen and KVL.
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL
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

#define BUILD_DLL

#include "program.h"
#include "library.h"
#include "metalib.h"
#include "block.h"
#include "alist.h"
#include "submodel.h"
#include "printer_file.h"
#include "xref.h"
#include "plf.h"
#include "format.h"
#include "treelog.h"
#include "assertion.h"
#include "librarian.h"
#include "frame.h"
#include <sstream>
#include <fstream>
#include <memory>
#include <algorithm>

struct ProgramDocument : public Program
{
  // Content.
  Metalib& metalib;
  XRef xref;
  std::ofstream out;
  std::auto_ptr<Format> format;
  const bool print_parameterizations;
  // remember this for models.
  symbol current_component;

  // LaTeX functions.
  void print_description (const symbol description);

  // Private functions.
  void print_string (const symbol);

  // Document functions.
  void print_entry_type (const symbol name,
			 const Syntax& syntax,
			 const AttributeList& alist);
  void print_entry_submodel (const symbol name, 
			     int level,
			     const Syntax& syntax,
			     const AttributeList& alist,
			     const symbol aref);
  void print_entry_category (const symbol name, 
			     const Syntax& syntax,
			     const AttributeList& alist);
  void print_entry_value (const symbol name, 
			  const Syntax& syntax,
			  const AttributeList& alist);

  void print_users (const XRef::Users&);
  void print_sample_entry (const symbol name, 
			   const Syntax& syntax,
			   const AttributeList& alist,
			   bool last);

  // Print parts of it.
  static void own_entries (const Metalib&,
                           const Library& library, const symbol name, 
                           std::vector<symbol>& entries, 
                           bool new_only = false);
  static void inherited_entries (const Metalib&, const Library& library,
                                 const symbol name, 
                                 std::vector<symbol>& entries);
  void print_sample (const symbol name,
		     const Syntax& syntax, const AttributeList& alist,
		     bool top_level);
  void print_sample (const symbol name, const Library&);
  void print_sample_name (const symbol name, bool top_level);
  void print_sample_end ();
  void print_sample_entries (const symbol name,
                             const Syntax& syntax,
                             const AttributeList& alist,
                             const std::vector<symbol>& order,
                             const std::vector<symbol>& own_entries,
                             const symbol lib_name,
                             const std::vector<symbol>& base_entries,
			     bool top_level);
  void print_submodel (const symbol name, int level,
		       const Syntax& syntax,
		       const AttributeList& alist, 
		       const symbol aref);
  void print_submodel_entries (const symbol name, int level,
                               const Syntax& syntax, 
                               const AttributeList& alist,
                               const std::vector<symbol>& entries, 
			       const symbol aref);
  void print_submodel_entry (const symbol, int level,
                             const Syntax& syntax,
                             const AttributeList& alist, bool& first, 
			     const symbol aref);
  void print_model (symbol name, const Library& library, Treelog&);
  void print_fixed (const symbol name, 
		    const Syntax& syntax,
		    const AttributeList& alist);
  void print_component (const Library& library, Treelog& msg);

  // Print it.
  void print_document (Treelog&);

  // Program.
  bool run (Treelog& msg)
  {
    format->initialize (out);
    print_document (msg); 
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { };
  bool check (Treelog&)
  { return true; }
  ProgramDocument (Block& al)
    : Program (al),
      metalib (al.metalib ()),
      xref (metalib),
      out (al.name ("where").name ().c_str ()),
      format (Librarian::build_item<Format> (al, "format")),
      print_parameterizations (al.flag ("print_parameterizations"))
  { }
  ~ProgramDocument ()
  { }
};

void
ProgramDocument::print_string (const symbol name)
{
  std::ostringstream tmp;
  PrinterFile::print_string (tmp, name.name ());
  format->text (tmp.str ());
}

void
ProgramDocument::print_description (const symbol description)
{ 
  format->soft_linebreak ();
  format->text (description);
  format->soft_linebreak ();
}

void 
ProgramDocument::print_entry_type (const symbol name,
				   const Syntax& syntax,
				   const AttributeList& alist)
{
  const Value::type type = syntax.lookup (name);

  switch (type)
    {
    case Value::Number:
      {
	format->text ("number ");
	const symbol dimension = syntax.dimension (name);
	if (dimension == Value::None ())
	  format->text ("(dimensionless)");
	else if (dimension == Value::Unknown ())
	  format->text ("(dimension not specified)");
	else
	  format->bold ("[" + dimension + "]");
      }
      break;
    case Value::AList:
      {
	if (Submodel::is_submodel (syntax, alist, name))
	  {
	    format->bold (Submodel::find_submodel (syntax, alist, name));
	    format->text (" fixed component ");
	    format->see ("section", "fixed", 
			 Submodel::find_submodel (syntax, alist, name));
	  }
	else
	  {
	    format->text ("submodel ");
	    format->see ("section", "type", "alist");
	  }
      }
      break;
    case Value::PLF:
      {
	format->text ("plf ");
	const symbol domain = syntax.domain (name);
	const symbol range = syntax.range (name);
	format->bold ("[" + domain);
        format->text (" ");
	// format->special("nbsp");
	format->special ("->");
        format->text (" ");
	// format->special ("nbsp");
	format->bold (range + "]");
      }
      break;
    case Value::Boolean:
      format->text ("boolean ");
      format->see ("section", "type", "boolean");
      break;
    case Value::String:
      format->text ("string ");
      format->see ("section", "type", "string");
      break;
    case Value::Integer:
      format->text ("integer");
      break;
    case Value::Object:
      {
	const symbol component = syntax.library (metalib, name).name ();
	format->bold (component.name ());
	format->text (" component ");
	format->see ("chapter", "component",  component.name ());
      }
      break;
    case Value::Library:
    case Value::Error:
    default:
      daisy_notreached ();
    };
}

void 
ProgramDocument::print_entry_submodel (const symbol name, 
				       const int level,
				       const Syntax& syntax,
				       const AttributeList& alist,
				       const symbol aref)
{
  const Value::type type = syntax.lookup (name);
  const int size = syntax.size (name);

  if (type == Value::AList)
    {
      const Syntax& child = syntax.syntax (name);
      const AttributeList& nested 
	= (size != Value::Singleton || !alist.check (name))
	? syntax.default_alist (name)
	: alist.alist (name);
      if (!nested.check ("submodel"))
	{
	  print_sample (name, child, nested, false);
	  print_submodel (name, level, child, nested, aref);
	}
    }
}
    
void 
ProgramDocument::print_entry_category (const symbol name, 
				       const Syntax& syntax,
				       const AttributeList& alist)
{
  const Value::type type = syntax.lookup (name);

  if (type == Value::Object)	// Objects and ALists don't have categories.
    {
      if (syntax.is_optional (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional component");
	}
      else if (alist.check (name))
	{
	  format->hard_linebreak ();
	  format->text ("Component");
	}
    }
  else if (type == Value::AList)
    {
      if (syntax.is_optional (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional submodel");
	}
      else if (alist.check (name))
	{
	  format->hard_linebreak ();
	  format->text ("Submodel");
	}
    }
  else if (syntax.is_optional (name))
    {
      if (syntax.is_const (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional parameter");
	}
      else if (syntax.is_state (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional state variable");
	}
      else if (syntax.is_log (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional log variable");
	}
      else 
	daisy_notreached ();
    }
  else
    {
      if (syntax.is_const (name))
	{
	  format->hard_linebreak ();
	  format->text ("Parameter");
	}
      else if (syntax.is_state (name))
	{
	  format->hard_linebreak ();
	  format->text ("State variable");
	}
      else if (syntax.is_log (name))
	{
	  format->hard_linebreak ();
	  format->text ("Log variable");
	}
      else 
	daisy_notreached ();
    }
}

void 
ProgramDocument::print_entry_value (const symbol name, 
				    const Syntax& syntax,
				    const AttributeList& alist)
{
  if (alist.check (name))
    {
      const Value::type type = syntax.lookup (name);
      const int size = syntax.size (name);

      bool print_default_value = false;
      
      if (size == Value::Singleton)
	switch (type)
	  {
	  case Value::Number:
	    {
	      std::ostringstream tmp;
	      tmp << " (default " << alist.number (name) << ")";
	      format->text (tmp.str ());
	    }
	    break;
	  case Value::AList:
	    {
	      const bool has_errors
		= !syntax.syntax (name).check (metalib, alist.alist (name), 
					       Treelog::null ());
	      if (has_errors)
		format->text (" (has partially specified default value)");
	      else 
		format->text (" (has fully specified default value)");
	      if (Submodel::is_submodel (syntax, alist, name))
		{
		  const AttributeList& nested = alist.alist (name);
		  const symbol submodel
		    = Submodel::find_submodel (syntax, alist, name);
		  Syntax nested_syntax;
		  AttributeList default_alist;
		  Submodel::load_syntax (submodel, 
					 nested_syntax, default_alist);
		  
		  if (!nested.subset (metalib, default_alist, nested_syntax))
		    print_default_value = true;
		}
	      else
		print_default_value = true;
	    }
	    break;
	  case Value::PLF:
	    {
	      std::ostringstream tmp;
	      tmp << " (has default value with " << alist.plf (name).size ()
		     << " points)";
	      format->text (tmp.str ());
	      if (alist.plf (name).size () > 0)
		print_default_value = true;
	    }
	    break;
	  case Value::Boolean:
	    format->text (" (default ");
	    if (alist.flag (name))
	      format->text ("true");
	    else 
	      format->text ("false");
	    format->text (")");
	    break;
	  case Value::String:
	    {
	      const std::string value = alist.name (name).name ();
	      if (value.length () < 30)
		format->text (" (default `" + value + "')");
	      else
		{
		  std::ostringstream tmp;
		  tmp << " (has default value with length "
			 << value.length () << ")";
		  format->text (tmp.str ());
		  print_default_value = true;
		}
	    }
	    break;
	  case Value::Integer:
	    {
	      std::ostringstream tmp;
	      tmp << " (default " << alist.integer (name) << ")";
	      format->text (tmp.str ());
	    }
	    break;
	  case Value::Object:
	    {
	      const AttributeList& object = alist.alist (name);
	      daisy_assert (object.check ("type"));
	      const symbol type = object.name ("type");
	      format->text (" (default `" + type.name () + "')");
	      const Library& library = syntax.library (metalib, name);
	      const AttributeList& super = library.lookup (type);
	      if (!object.subset (metalib, super, library.syntax (type)))
		print_default_value = true;
	    }
	    break;
	  case Value::Library:
	  case Value::Error:
	    daisy_notreached ();
	  }
      else
	switch (type)
	  {
	  case Value::Number:
	  case Value::AList:
	  case Value::PLF:
	  case Value::Boolean:
	  case Value::String:
	  case Value::Integer:
	  case Value::Object:
	    if (alist.size (name) == 0)
	      format->text (" (default: an empty sequence)");
	    else
	      {
		std::ostringstream tmp;
		tmp << " (has default value with length " 
		       << alist.size (name) << ")";
		format->text (tmp.str ());
		print_default_value = true;
	      }
	    break;
	  case Value::Library:
	  case Value::Error:
	    daisy_notreached ();
	  }

      if (print_default_value)
	{
	  std::ostringstream tmp;
	  PrinterFile printer (metalib, tmp);
	  printer.print_entry (alist, syntax, name);
	  format->soft_linebreak ();
	  format->verbatim (tmp.str ());
	  format->text ("Description:");
	}
    }
}

void 
ProgramDocument::print_users (const XRef::Users& users)
{
  if (users.models.empty () && users.submodels.empty ())  
    return;

  format->soft_linebreak ();
  format->text ("Used by ");

  for (std::set<XRef::ModelUser>::const_iterator i = users.models.begin ();
       i != users.models.end ();
       i++)
    {
      std::set<XRef::ModelUser>::const_iterator next = i;
      next++;

      if (i != users.models.begin ())
	{
	  if (next == users.models.end () && users.submodels.empty ())
	    format->text (", and ");
	  else 
	    format->text (",");
	  format->soft_linebreak ();
	}
      const symbol component = (*i).component;
      const symbol model = (*i).model;
      const std::vector<symbol>& path = (*i).path;
      format->text (component.name () + " " + model.name () + " ");
      for (unsigned int j = 0; j < path.size (); j++)
	format->text (" " + path[j]);
      format->text (" ");
      format->see_page ("model", component.name () + "-" + model.name ());
    }

  for (std::set<XRef::SubmodelUser>::const_iterator i 
         = users.submodels.begin ();
       i != users.submodels.end (); 
       i++)
    {
      std::set<XRef::SubmodelUser>::const_iterator next = i;
      next++;
      if (i == users.submodels.begin () && users.models.empty ())
	;
      else if (next == users.submodels.end ())
	{
	  format->text (", and ");
	  format->soft_linebreak ();
	}
      else 
	{
	  format->text (",");
	  format->soft_linebreak ();
	}
      const symbol submodel = (*i).submodel;
      const std::vector<symbol>& path = (*i).path;
      format->text (submodel + " @");
      for (unsigned int j = 0; j < path.size (); j++)
	format->text (" " + path[j]);
      format->text (" ");
      format->see_page ("fixed",  submodel);
    }
  format->text (".");
  format->soft_linebreak ();
}

void
ProgramDocument::print_sample_entry (const symbol name, 
				     const Syntax& syntax,
				     const AttributeList& alist,
				     const bool last)
{ 
  std::string comment;
  {
    Format::TableCell dummy (*format);
    format->text ("(");
    print_string (name);

    if (alist.check (name))
      {
	const Value::type type = syntax.lookup (name);
	const int size = syntax.size (name);

	bool print_name = true;
	comment = "Has default value.";

	if (size == Value::Singleton)
	  switch (type)
	    {
	    case Value::Number:
	      {
		format->special ("nbsp");
		std::ostringstream tmp;
		tmp << alist.number (name);
                const symbol dimension = syntax.dimension (name);
                if (dimension == Value::None ())
                  tmp << " []";
                else if (dimension == Value::Unknown ())
                  tmp << " [?]";
                else
                  tmp << " [" << dimension << "]";
                tmp << ")";
		format->text (tmp.str ());
		print_name = false;
	      }
	      break;
	    case Value::AList:
	      {
		const bool has_errors
		  = !syntax.syntax (name).check (metalib,
                                                 alist.alist (name), 
						 Treelog::null ());
		if (has_errors)
		  comment = "Has partial value.";
	      }
	      break;
	    case Value::PLF:
	      break;
	    case Value::Boolean:
	      format->special ("nbsp");
	      format->text (alist.flag (name) ? "true" : "false");
	      format->text (")");
	      print_name = false;
	      break;
	    case Value::String:
	      {
		const std::string value = alist.name (name).name ();
		if (value.length () < 20)
		  {
		    format->special ("nbsp");
		    print_string (value);
		    format->text (")");
		    print_name = false;
		  }
	      }
	      break;
	    case Value::Integer:
	      {
		format->special ("nbsp");
		std::ostringstream tmp;
		tmp << alist.integer (name) << ")";
		format->text (tmp.str ());
		print_name = false;
	      }
	      break;
	    case Value::Object:
	      {
		const AttributeList& object = alist.alist (name);
		daisy_assert (object.check ("type"));
		const symbol type = object.name ("type");
		comment = "Default " + type + " value.";
	      }
	      break;
	    case Value::Library:
	    case Value::Error:
	      daisy_notreached ();
	    }
	else if (alist.size (name) == 0)
	  {
	    format->text (")");
	    print_name = false;
	  }
	else
	  switch (type)
	    {
	    case Value::Number:
	      if (alist.size (name) < 5)
		{
		  const std::vector<double>& numbers
		    = alist.number_sequence (name);
		  for (int i = 0; i < numbers.size (); i++)
		    {
		      format->special ("nbsp");
		      std::ostringstream tmp;
		      tmp << numbers[i];
		      format->text (tmp.str ());
		    }
		  format->text (")");
		  print_name = false;
		}
	      break;
	    case Value::AList:
	    case Value::PLF:
	    case Value::Boolean:
	    case Value::String:
	    case Value::Integer:
	    case Value::Object:
	      break;
	    case Value::Library:
	    case Value::Error:
	      daisy_notreached ();
	    }
	if (print_name)
	  {
	    format->special ("nbsp");
	    format->italic (name);
	    if (syntax.size (name) != Value::Singleton)
	      {
		format->special ("nbsp");
		format->special ("...");
	      }
	    format->text (")");
	  }
	else
	  comment = "";
      }
    else
      {
	format->special ("nbsp");
	format->italic (name);
	if (syntax.size (name) != Value::Singleton)
	  {
	    format->special ("nbsp");
	    format->special ("...");
	  }
	format->text (")");

      }
    if (last && comment.size () < 1)
      print_sample_end ();
  }
  if (comment.size () > 0)
    {
      Format::TableCell dummy (*format);
      format->text (";");
      format->special ("nbsp");
      format->text (comment);
      if (last)
	print_sample_end ();
    }
}

void
ProgramDocument::own_entries (const Metalib& metalib,
                              const Library& library, const symbol name, 
			      std::vector<symbol>& entries,
                              const bool new_only)
{
  const Syntax& syntax = library.syntax (name);
  const AttributeList& alist = library.lookup (name);

  syntax.entries (entries);

  // Remove base entries.
  if (alist.check ("base_model") || alist.check ("type"))
    {
      const symbol base_model = alist.check ("base_model")
        ? alist.name ("base_model")
        : alist.name ("type");
          
      if (base_model != name)
        {
          const Syntax& base_syntax = library.syntax (base_model);
          const AttributeList& base_alist = library.lookup (base_model);
          std::vector<symbol> base_entries;
          base_syntax.entries (base_entries);
          for (size_t i = 0; i < base_entries.size (); i++)
            {
              const symbol key = base_entries[i];
              if (new_only
                  || key == "description"
                  || alist.subset (metalib, base_alist, base_syntax, key))
                entries.erase (find (entries.begin (), entries.end (), key));
            }
        }
    }
}

void
ProgramDocument::inherited_entries (const Metalib& metalib,
                                    const Library& library, const symbol name, 
				    std::vector<symbol>& entries)
{
  const AttributeList& alist = library.lookup (name);

  if (alist.check ("base_model"))
    {
      const symbol base_model = alist.name ("base_model");
          
      if (base_model != name)
        {
          const Syntax& base_syntax = library.syntax (base_model);
          const AttributeList& base_alist = library.lookup (base_model);
          base_syntax.entries (entries);
          for (size_t i = 0; i < entries.size (); i++)
            {
              const symbol key = entries[i];
              if (key != "description" 
                  && !alist.subset (metalib, base_alist, base_syntax, key))
                entries.erase (find (entries.begin (), entries.end (), key));
            }
        }
    }
}

void 
ProgramDocument::print_sample (const symbol name,
			       const Syntax& syntax,
			       const AttributeList& alist,
			       const bool top_level)
{
  const std::vector<symbol>& order = syntax.order ();
  std::vector<symbol> own;
  syntax.entries (own);
  const std::vector<symbol> base;
  print_sample_entries (name, syntax, alist, order, own, "dummy", base, 
			top_level);
}

void 
ProgramDocument::print_sample (const symbol name, const Library& library)
{
  const Syntax& syntax = library.syntax (name);
  const AttributeList& alist = library.lookup (name);

  const std::vector<symbol>& order = syntax.order ();
  std::vector<symbol> own;
  own_entries (metalib, library, name, own);
  std::vector<symbol> base;
  inherited_entries (metalib, library, name, base);

  print_sample_entries (name.name (), syntax, alist, order, own, 
                        library.name ().name (), base, true);
}

void 
ProgramDocument::print_sample_name (const symbol name, 
				    const bool top_level)
{
  Format::TableCell dummy (*format);
  format->text ("<");
  format->special ("nbsp");
  if (top_level)
    {
      print_string (name);
      format->special ("nbsp");
    }
}

void 
ProgramDocument::print_sample_end ()
{
  format->special ("nbsp");
  format->text (">");
}

void 
ProgramDocument::print_sample_entries (const symbol name,
                                       const Syntax& syntax,
                                       const AttributeList& alist,
                                       const std::vector<symbol>& 
                                       /**/ order,
                                       const std::vector<symbol>&
                                       /**/ own_entries,
                                       const symbol lib_name,
                                       const std::vector<symbol>& 
                                       /**/ base_entries, 
				       const bool top_level)
{
  // Remove uninteresting entries
  std::vector<symbol> own; 
  for (size_t i = 0; i < own_entries.size (); i++)
    if (syntax.order_index (own_entries[i]) < 0
	&& !syntax.is_log (own_entries[i])
	&& syntax.lookup (own_entries[i]) != Value::Library)
      own.push_back (own_entries[i]);
  std::vector<symbol> base;
  for (size_t i = 0; i < base_entries.size (); i++)
    if (syntax.order_index (base_entries[i]) < 0
	&& !syntax.is_log (base_entries[i])
	&& syntax.lookup (base_entries[i]) != Value::Library)
      base.push_back (base_entries[i]);

  // Count entries.
  const size_t count = order.size () + own.size () + base.size ();
  size_t left = count;

  format->soft_linebreak ();
  format->raw ("LaTeX", "\\noindent\n");
  Format::Typewriter dummy (*format);
  Format::Table d2 (*format, "lll");

  // Empty models.
  if (count == 0)
    {
      Format::TableRow d3 (*format);
      print_sample_name (name, top_level);
      Format::TableCell d4 (*format);
      print_sample_end ();
    }

  // Ordered members first.
  if (order.size () > 0)
    {
      Format::TableRow d3 (*format);
      print_sample_name (name, top_level);
      Format::TableCell d4 (*format);
      for (unsigned int i = 0; i < order.size (); i++)
	{ 
	  format->italic (order[i]);
	  if (syntax.size (order[i]) == Value::Sequence)
	    format->special ("...");
	  format->special ("nbsp");
	  left--;
	  if (left == 0)
	    print_sample_end ();
	}
    }
  
  // Then own members.
  for (unsigned int i = 0; i < own.size (); i++)
    {
      Format::TableRow row (*format);
      if (left == count)
	print_sample_name (name, top_level);
      else
	Format::TableCell empty (*format);
	  
      print_sample_entry (own[i], syntax, alist, left == 1);
      left--;
    }
 
  // Finally inherited members.
  if (base.size () > 0)
    {
      {
	Format::TableRow row (*format);
	
	if (left == count)
	  print_sample_name (name, top_level);
	else
	  { Format::TableCell empty (*format); }
	
	{
	  Format::TableMultiCell dummy (*format, 2, "l");
	  format->text (";; Shared parameters are described in section");
	  format->special ("nbsp");
	  format->ref ("model", 
		       lib_name + "-" + alist.name ("base_model"));
	}
      }
      for (unsigned int i = 0; i < base.size (); i++)
	{
	  Format::TableRow row (*format);
	  { Format::TableCell empty (*format); }
	  print_sample_entry (base[i], syntax, alist, left == 1);
	  left--;
	}
    }
  daisy_assert (left == 0);
}

void 
ProgramDocument::print_submodel (const symbol name, int level,
				 const Syntax& syntax,
				 const AttributeList& alist, 
				 const symbol aref)
{
  std::vector<symbol> entries;
  syntax.entries (entries);
  print_submodel_entries (name, level, syntax, alist, entries, aref);
}

void 
ProgramDocument::print_submodel_entries (const symbol name, int level,
					 const Syntax& syntax,
					 const AttributeList& alist,
					 const std::vector<symbol>&
					 /**/ entries, 
					 const symbol aref)
{
  const std::string bref = aref + "-" + name;
  const std::vector<symbol>& order = syntax.order ();
  int log_count = 0;
  for (unsigned int i = 0; i < entries.size (); i++)
    if (syntax.is_log (entries[i]))
      log_count++;

  if (entries.size () == 0)
    { 
      format->soft_linebreak ();
      format->text (name + " has no members");
      format->soft_linebreak ();
    }
  else
    {
      // Print normal attributes.
      if (log_count < entries.size ())
	{
	  Format::List dummy (*format);
	  bool first = true;
	  // Ordered members first.
	  for (unsigned int i = 0; i < order.size (); i++)
	    print_submodel_entry (order[i], level, syntax, alist, first, bref);
      
	  // Then the remaining members, except log variables.
	  for (unsigned int i = 0; i < entries.size (); i++)
	    if (syntax.order_index (entries[i]) < 0 
                && !syntax.is_log (entries[i]))
	      print_submodel_entry (entries[i], level, syntax, alist, first, 
				    bref);
	}

      if (log_count < entries.size () && log_count > 0)
	format->soft_linebreak ();

      // Print log variables.
      if (log_count > 0)
	{
	  if (format->formatp ("LaTeX") && level == 0)
	    format->raw ("LaTeX", "\\subsection*{Log Variables}");
	  else 
	    format->bold ("Log Variables");
	  format->soft_linebreak ();
	  format->soft_linebreak ();
	  
	  Format::List dummy (*format);
	  bool first = true;
	  for (unsigned int i = 0; i < entries.size (); i++)
	    if (syntax.is_log (entries[i]))
	      print_submodel_entry (entries[i], level, syntax, alist, first,
				    bref);
	}
    }
}

void 
ProgramDocument::print_submodel_entry (const symbol name, int level,
				       const Syntax& syntax,
				       const AttributeList& alist, bool& first,
				       const symbol aref)
{
  if (first)
    first = false;
  else
    format->soft_linebreak ();

  const Value::type type = syntax.lookup (name);

  // We ignore libraries.
  if (type == Value::Library)
    return;

  const int size = syntax.size (name);

  // Print name.
  Format::Item dummy (*format, name);
  format->label ("parameter", aref + "-" + name);
  format->index (name);
  
  // Print type.
  print_entry_type (name, syntax, alist);

  // Print size.
  if (size == Value::Singleton)
    /* do nothing */;
  else if (size == Value::Sequence)
    format->text (" sequence");
  else
    {
      std::ostringstream tmp;
      tmp << " array of length " << size;
      format->text (tmp.str ());
    }

  if (!syntax.is_log (name))
    {
      // Print category.
      print_entry_category (name, syntax, alist);

      // Print value.
      if (name != "description")
        print_entry_value (name, syntax, alist);
    }

  // Print description line.
  const symbol description = syntax.description (name);
  if (description != Value::Unknown ())
    {
      format->hard_linebreak ();
      format->text (description);
      format->soft_linebreak ();
    }

  // print submodel entries, if applicable
  print_entry_submodel (name, level + 1, syntax, alist, aref);
}


void
ProgramDocument::print_model (const symbol name, const Library& library,
                              Treelog& msg)
{
  const Syntax& syntax = library.syntax (name);
  const AttributeList& alist = library.lookup (name);  

  const XRef::ModelUsed used (library.name (), name);
  if (alist.check ("type"))
    {
      // This is a parameterization.
      const symbol type = alist.name ("type");
      format->soft_linebreak ();
      Format::Section dummy (*format, "section", name.name (), "model", 
			     current_component + "-" + name);
      format->index (name.name ());
      format->text ("A `" + type + "' parameterization ");
      
      if (alist.check ("parsed_from_file"))
	format->text ("defined in `" + alist.name ("parsed_from_file")
		      + "'.\n");
      else
	{
	  format->text ("build into ");
	  format->special ("daisy");
	  format->text (".\n");
	}

      if (library.has_interesting_description (alist))
        format->alist_description (alist);

      print_users (xref.models[used]);
      std::vector<symbol> entries;
      own_entries (metalib, library, name, entries, true);
      if (entries.size () > 0)
        print_submodel_entries (name.name (), 0, syntax, alist, entries, 
                                library.name ().name ());

      const std::vector<Library::doc_fun>& doc_funs 
	= library.doc_funs ();
      for (size_t i = 0; i < doc_funs.size ();i++)
	{
	  format->soft_linebreak ();
	  doc_funs[i](*format, metalib, msg, name);
	}
      if (print_parameterizations)
	{
	  std::ostringstream tmp;
	  PrinterFile printer (metalib, tmp);
	  printer.print_parameterization (library.name (), name, false);
	  format->soft_linebreak ();
	  format->verbatim (tmp.str ());
	}
    }
  else
    {
      format->soft_linebreak ();
      Format::Section dummy (*format, "section", name.name (), "model",
			     current_component + "-" + name);
      format->index (name.name ());

      // Print description, if any.
      format->alist_description (alist);

      print_users (xref.models[used]);
      print_sample (name, library);
      
      // Print own entries.
      std::vector<symbol> entries;
      own_entries (metalib, library, name, entries);
      print_submodel_entries (name.name (), 0, syntax, alist, entries, 
			      library.name ().name ());
    }
}

void
ProgramDocument::print_fixed (const symbol name, 
			      const Syntax& syntax,
			      const AttributeList& alist)
{
  format->soft_linebreak ();
  Format::Section dummy (*format, "section", name, "fixed", name);
  format->index (name);

  // Print description, if any.
  format->alist_description (alist);

  print_users (xref.submodels[name]);

  print_sample (name, syntax, alist, true);
  print_submodel (name, 0, syntax, alist, "fixed");
}

class ModelCompare
{ 
  const Library& library;

  const symbol find_next_in_line (const symbol root, const symbol leaf) const
  {
    // Find the child of root that leaf is descended from.
    daisy_assert (root != leaf);
    const AttributeList& al = library.lookup (leaf);
    const symbol type = al.check ("type")
      ? al.name ("type") 
      : al.name ("base_model");
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
ProgramDocument::print_component (const Library& library, Treelog& msg)
{

  const symbol name = library.name ();
  current_component = name;
  format->soft_linebreak ();
  Format::Section dummy (*format, "chapter", name.name (), 
			 "component", name.name ());
  format->index (name.name ());

  const symbol description = library.description ();
  if (description != symbol ())
    print_description (description);

  print_users (xref.components[name]);

  // For all members...
  std::vector<symbol> entries;
  library.entries (entries);
  ModelCompare model_compare (library);
  sort (entries.begin (), entries.end (), model_compare);
  for (unsigned int i = 0; i < entries.size (); i++)
    print_model (entries[i], library, msg);

  static const symbol Daisy_symbol ("Daisy");
  current_component = Daisy_symbol;
}

void
ProgramDocument::print_document (Treelog& msg)
{
  Format::Document dummy (*format);

  // For all components...
  std::vector<symbol> entries;
  metalib.all (entries);
  sort (entries.begin (), entries.end (), symbol::alphabetical);
  for (unsigned int i = 0; i < entries.size (); i++)
    print_component (metalib.library (entries[i]), msg);

  // Fixed components.
  Format::Section d2 (*format, "chapter", "Fixed Components", "cha", "fixed");
  print_description ("\
Fixed components are similar to ordinary component, with the exceptions\n\
that there can only be one model, that is, only a single implementation\n\
of the component, and that it is not possible to define libraries of\n\
standard parameterizations for the model."); 
  std::vector<symbol> fixed;
  Submodel::all (fixed);
  for (unsigned int i = 0; i < fixed.size (); i++)
    {
      const symbol name = fixed[i];
      Syntax syntax;
      AttributeList alist;
      Submodel::load_syntax (name, syntax, alist);
      print_fixed (name, syntax, alist);
  }
}

static struct ProgramDocumentSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ProgramDocument (al); }
  ProgramDocumentSyntax ()
    : DeclareModel (Program::component, "document", "\
Generate the components part of the reference manual.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.add ("where", Value::String, Value::Const, 
                "Name of file to store results in.");
    frame.add ("where", "components.tex");
    frame.add_object ("format", Format::component, 
                       Value::Const, Value::Singleton,
                       "Text format used for the document.");
    frame.add ("format", "LaTeX");
    frame.add ("print_parameterizations", Value::Boolean, Value::Const,
		"Include a copy of all loaded parameterizations in document.");
    frame.add ("print_parameterizations", false);
  }
} ProgramDocument_syntax;
