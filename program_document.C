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


#include "program.h"
#include "submodel.h"
#include "printer_file.h"
#include "xref.h"
#include "plf.h"
#include "format.h"
#include "treelog.h"
#include "tmpstream.h"
#include <iostream>
#include <memory>

struct ProgramDocument : public Program
{
  

  // Content.
  XRef xref;
  std::auto_ptr<Format> format;
  const bool print_parameterizations;
  // remember this for models.
  symbol current_component;
  bool submodel;

  // LaTeX functions.
  void print_description (const std::string& description);

  // Private functions.
  void print_string (const std::string&);
  bool is_submodel (const Syntax&, const AttributeList&, const std::string&);
  std::string find_submodel (const Syntax&, const AttributeList&, 
			     const std::string&);

  // Document functions.
  void print_entry_type (const std::string& name,
			 const Syntax& syntax,
			 const AttributeList& alist);
  void print_entry_submodel (const std::string& name, 
			     int level,
			     const Syntax& syntax,
			     const AttributeList& alist);
  void print_entry_category (const std::string& name, 
			     const Syntax& syntax,
			     const AttributeList& alist);
  void print_entry_value (const std::string& name, 
			  const Syntax& syntax,
			  const AttributeList& alist);

  void print_users (const XRef::Users&);
  void print_sample_entry (const std::string& name, 
			   const Syntax& syntax,
			   const AttributeList& alist,
			   bool last);

  // Print parts of it.
  static void own_entries (const Library& library, const symbol name, 
                           std::vector<std::string>& entries);
  static void inherited_entries (const Library& library,
                                 const symbol name, 
                                 std::vector<std::string>& entries);
  void print_sample (const std::string& name,
		     const Syntax& syntax, const AttributeList& alist);
  void print_sample (const symbol name, const Library&);
  void print_sample_name (const std::string& name);
  void print_sample_end ();
  void print_sample_entries (const std::string& name,
                             const Syntax& syntax,
                             const AttributeList& alist,
                             const std::vector<std::string>& order,
                             const std::vector<std::string>& own_entries,
                             const std::string& lib_name,
                             const std::vector<std::string>& base_entries);
  void print_submodel (const std::string& name, int level,
		       const Syntax& syntax,
		       const AttributeList& alist);
  void print_submodel_entries (const std::string& name, int level,
                               const Syntax& syntax, 
                               const AttributeList& alist,
                               const std::vector<std::string>& entries);
  void print_submodel_entry (const std::string&, int level,
                             const Syntax& syntax,
                             const AttributeList& alist, bool& first);
  void print_model (symbol name, const Library& library);
  void print_fixed (const std::string& name, 
		    const Syntax& syntax,
		    const AttributeList& alist);
  void print_component (const Library& library);

  // Print it.
  void print_document ();

  // Program.
  void run (Treelog&)
  {
    format->initialize (std::cout);
    print_document (); 
  }

  // Create and Destroy.
  void initialize (const Syntax*, const AttributeList*, Treelog&)
  { };
  bool check (Treelog&)
  { return true; }
  ProgramDocument (const AttributeList& al)
    : Program (al),
       format (Librarian<Format>::create (al.alist ("format"))),
       print_parameterizations (al.flag ("print_parameterizations"))
  { }
  ~ProgramDocument ()
  { }
};

void
ProgramDocument::print_string (const std::string& name)
{
  TmpStream tmp;
  PrinterFile::print_string (tmp (), name);
  format->text (tmp.str ());
}

void
ProgramDocument::print_description (const std::string& description)
{ 
  format->soft_linebreak ();
  format->text (description);
  format->soft_linebreak ();
}

bool 
ProgramDocument::is_submodel (const Syntax& syntax, const AttributeList& alist,
			      const std::string& name)
{
  if (syntax.size (name) != Syntax::Singleton || !alist.check (name))
    {
      const AttributeList& nested = syntax.default_alist (name);
      if (nested.check ("submodel"))
	return true;
    }
  else
    {
      const AttributeList& nested = alist.alist (name);
      if (nested.check ("submodel"))
	return true;
    }
  return false;
}

std::string
ProgramDocument::find_submodel (const Syntax& syntax, 
				const AttributeList& alist,
				const std::string& name)
{
  if (syntax.size (name) != Syntax::Singleton || !alist.check (name))
    {
      const AttributeList& nested = syntax.default_alist (name);
      if (nested.check ("submodel"))
	return nested.name ("submodel");
    }
  else
    {
      const AttributeList& nested = alist.alist (name);
      if (nested.check ("submodel"))
	return nested.name ("submodel");
    }
  daisy_assert (false);
}

void 
ProgramDocument::print_entry_type (const std::string& name,
				   const Syntax& syntax,
				   const AttributeList& alist)
{
  const Syntax::type type = syntax.lookup (name);

  switch (type)
    {
    case Syntax::Number:
      {
	format->text ("number ");
	const std::string& dimension = syntax.dimension (name);
	if (dimension == Syntax::None ())
	  format->text ("(dimensionless)");
	else if (dimension == Syntax::Unknown ())
	  format->text ("(dimension not specified)");
	else
	  format->bold ("[" + dimension + "]");
      }
      break;
    case Syntax::AList:
      {
	if (is_submodel (syntax, alist, name))
	  {
	    format->bold (find_submodel (syntax, alist, name));
	    format->text (" fixed component ");
	    format->see ("section", "fixed", 
			 find_submodel (syntax, alist, name));
	  }
	else
	  {
	    format->text ("submodel ");
	    format->see ("section", "type", "alist");
	  }
      }
      break;
    case Syntax::PLF:
      {
	format->text ("plf ");
	const std::string& domain = syntax.domain (name);
	const std::string& range = syntax.range (name);
	format->bold ("[" + domain);
        format->text (" ");
	// format->special("nbsp");
	format->special ("->");
        format->text (" ");
	// format->special ("nbsp");
	format->bold (range + "]");
      }
      break;
    case Syntax::Boolean:
      format->text ("boolean ");
      format->see ("section", "type", "boolean");
      break;
    case Syntax::String:
      format->text ("string ");
      format->see ("section", "type", "string");
      break;
    case Syntax::Integer:
      format->text ("integer");
      break;
    case Syntax::Object:
      {
	const symbol component = syntax.library (name).name ();
	format->bold (component.name ());
	format->text (" component ");
	format->see ("chapter", "component",  component.name ());
      }
      break;
    case Syntax::Library:
    case Syntax::Error:
    default:
      daisy_assert (false);
    };
}

void 
ProgramDocument::print_entry_submodel (const std::string& name, 
				       const int level,
				       const Syntax& syntax,
				       const AttributeList& alist)
{
  const Syntax::type type = syntax.lookup (name);
  const int size = syntax.size (name);

  if (type == Syntax::AList)
    {
      submodel = true;		// Affects how the sample header looks.
      const Syntax& child = syntax.syntax (name);
      const AttributeList& nested 
	= (size != Syntax::Singleton || !alist.check (name))
	? syntax.default_alist (name)
	: alist.alist (name);
      if (!nested.check ("submodel"))
	{
	  print_sample (name, child, nested);
	  print_submodel (name, level, child, nested);
	}
      if (level == 1)
	submodel = false;
    }
}
    
void 
ProgramDocument::print_entry_category (const std::string& name, 
				       const Syntax& syntax,
				       const AttributeList& alist)
{
  const Syntax::type type = syntax.lookup (name);

  if (type == Syntax::Object)	// Objects and ALists don't have categories.
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
  else if (type == Syntax::AList)
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
	daisy_assert (false);
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
	daisy_assert (false);
    }
}

void 
ProgramDocument::print_entry_value (const std::string& name, 
				    const Syntax& syntax,
				    const AttributeList& alist)
{
  if (alist.check (name))
    {
      const Syntax::type type = syntax.lookup (name);
      const int size = syntax.size (name);

      bool print_default_value = false;
      
      if (size == Syntax::Singleton)
	switch (type)
	  {
	  case Syntax::Number:
	    {
	      TmpStream tmp;
	      tmp () << " (default " << alist.number (name) << ")";
	      format->text (tmp.str ());
	    }
	    break;
	  case Syntax::AList:
	    {
	      const bool has_errors
		= !syntax.syntax (name).check (alist.alist (name), 
					       Treelog::null ());
	      if (has_errors)
		format->text (" (has partially specified default value)");
	      else 
		format->text (" (has fully specified default value)");
	      if (is_submodel (syntax, alist, name))
		{
		  const AttributeList& nested = alist.alist (name);
		  const std::string submodel = find_submodel (syntax, alist,
							      name);
		  Syntax nested_syntax;
		  AttributeList default_alist;
		  Submodel::load_syntax (submodel, 
					 nested_syntax, default_alist);
		  
		  if (!nested.subset (default_alist, nested_syntax))
		    print_default_value = true;
		}
	      else
		print_default_value = true;
	    }
	    break;
	  case Syntax::PLF:
	    {
	      TmpStream tmp;
	      tmp () << " (has default value with " << alist.plf (name).size ()
		     << " points)";
	      format->text (tmp.str ());
	      if (alist.plf (name).size () > 0)
		print_default_value = true;
	    }
	    break;
	  case Syntax::Boolean:
	    format->text (" (default ");
	    if (alist.flag (name))
	      format->text ("true");
	    else 
	      format->text ("false");
	    format->text (")");
	    break;
	  case Syntax::String:
	    {
	      const std::string& value = alist.name (name);
	      if (value.length () < 30)
		format->text (" (default `" + value + "')");
	      else
		{
		  TmpStream tmp;
		  tmp () << " (has default value with length "
			 << value.length () << ")";
		  format->text (tmp.str ());
		  print_default_value = true;
		}
	    }
	    break;
	  case Syntax::Integer:
	    {
	      TmpStream tmp;
	      tmp () << " (default " << alist.integer (name) << ")";
	      format->text (tmp.str ());
	    }
	    break;
	  case Syntax::Object:
	    {
	      const AttributeList& object = alist.alist (name);
	      daisy_assert (object.check ("type"));
	      const symbol type = object.identifier ("type");
	      format->text (" (default `" + type.name () + "')");
	      const Library& library = syntax.library (name);
	      const AttributeList& super = library.lookup (type);
	      if (!object.subset (super, library.syntax (type)))
		print_default_value = true;
	    }
	    break;
	  case Syntax::Library:
	  case Syntax::Error:
	    daisy_assert (false);
	  }
      else
	switch (type)
	  {
	  case Syntax::Number:
	  case Syntax::AList:
	  case Syntax::PLF:
	  case Syntax::Boolean:
	  case Syntax::String:
	  case Syntax::Integer:
	  case Syntax::Object:
	    if (alist.size (name) == 0)
	      format->text (" (default: an empty sequence)");
	    else
	      {
		TmpStream tmp;
		tmp () << " (has default value with length " 
		       << alist.size (name) << ")";
		format->text (tmp.str ());
		print_default_value = true;
	      }
	    break;
	  case Syntax::Library:
	  case Syntax::Error:
	    daisy_assert (false);
	  }

      if (print_default_value)
	{
	  TmpStream tmp;
	  PrinterFile printer (tmp ());
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
      const std::vector<std::string>& path = (*i).path;
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
      const std::string submodel = (*i).submodel;
      const std::vector<std::string>& path = (*i).path;
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
ProgramDocument::print_sample_entry (const std::string& name, 
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
	const Syntax::type type = syntax.lookup (name);
	const int size = syntax.size (name);

	bool print_name = true;
	comment = "Has default value.";

	if (size == Syntax::Singleton)
	  switch (type)
	    {
	    case Syntax::Number:
	      {
		format->special ("nbsp");
		TmpStream tmp;
		tmp () << alist.number (name) << ")";
		format->text (tmp.str ());
		print_name = false;
	      }
	      break;
	    case Syntax::AList:
	      {
		const bool has_errors
		  = !syntax.syntax (name).check (alist.alist (name), 
						 Treelog::null ());
		if (has_errors)
		  comment = "Has partial value.";
	      }
	      break;
	    case Syntax::PLF:
	      break;
	    case Syntax::Boolean:
	      format->special ("nbsp");
	      format->text (alist.flag (name) ? "true" : "false");
	      format->text (")");
	      print_name = false;
	      break;
	    case Syntax::String:
	      {
		const std::string& value = alist.name (name);
		if (value.length () < 20)
		  {
		    format->special ("nbsp");
		    print_string (value);
		    format->text (")");
		    print_name = false;
		  }
	      }
	      break;
	    case Syntax::Integer:
	      {
		format->special ("nbsp");
		TmpStream tmp;
		tmp () << alist.integer (name) << ")";
		format->text (tmp.str ());
		print_name = false;
	      }
	      break;
	    case Syntax::Object:
	      {
		const AttributeList& object = alist.alist (name);
		daisy_assert (object.check ("type"));
		const std::string& type = object.name ("type");
		comment = "Default " + type + " value.";
	      }
	      break;
	    case Syntax::Library:
	    case Syntax::Error:
	      daisy_assert (false);
	    }
	else if (alist.size (name) == 0)
	  {
	    format->text (")");
	    print_name = false;
	  }
	else
	  switch (type)
	    {
	    case Syntax::Number:
	      if (alist.size (name) < 5)
		{
		  const std::vector<double>& numbers
		    = alist.number_sequence (name);
		  for (int i = 0; i < numbers.size (); i++)
		    {
		      format->special ("nbsp");
		      TmpStream tmp;
		      tmp () << numbers[i];
		      format->text (tmp.str ());
		    }
		  format->text (")");
		  print_name = false;
		}
	      break;
	    case Syntax::AList:
	    case Syntax::PLF:
	    case Syntax::Boolean:
	    case Syntax::String:
	    case Syntax::Integer:
	    case Syntax::Object:
	      break;
	    case Syntax::Library:
	    case Syntax::Error:
	      daisy_assert (false);
	    }
	if (print_name)
	  {
	    format->special ("nbsp");
	    format->italic (name);
	    if (syntax.size (name) != Syntax::Singleton)
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
	if (syntax.size (name) != Syntax::Singleton)
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
ProgramDocument::own_entries (const Library& library, const symbol name, 
			      std::vector<std::string>& entries)
{
  const Syntax& syntax = library.syntax (name);
  const AttributeList& alist = library.lookup (name);

  syntax.entries (entries);

  // Remove base entries.
  if (alist.check ("base_model"))
    {
      const symbol base_model = alist.identifier ("base_model");
          
      if (base_model != name)
        {
          const Syntax& base_syntax = library.syntax (base_model);
          const AttributeList& base_alist = library.lookup (base_model);
          std::vector<std::string> base_entries;
          base_syntax.entries (base_entries);
          for (size_t i = 0; i < base_entries.size (); i++)
            {
              const std::string& key = base_entries[i];
              if (key == "description"
                  || alist.subset (base_alist, base_syntax, key))
                entries.erase (find (entries.begin (), entries.end (), key));
            }
        }
    }
}

void
ProgramDocument::inherited_entries (const Library& library, const symbol name, 
				    std::vector<std::string>& entries)
{
  const AttributeList& alist = library.lookup (name);

  if (alist.check ("base_model"))
    {
      const symbol base_model = alist.identifier ("base_model");
          
      if (base_model != name)
        {
          const Syntax& base_syntax = library.syntax (base_model);
          const AttributeList& base_alist = library.lookup (base_model);
          base_syntax.entries (entries);
          for (size_t i = 0; i < entries.size (); i++)
            {
              const std::string& key = entries[i];
              if (key != "description" 
                  && !alist.subset (base_alist, base_syntax, key))
                entries.erase (find (entries.begin (), entries.end (), key));
            }
        }
    }
}

void 
ProgramDocument::print_sample (const std::string& name,
			       const Syntax& syntax,
			       const AttributeList& alist)
{
  const std::vector<std::string>& order = syntax.order ();
  std::vector<std::string> own;
  syntax.entries (own);
  const std::vector<std::string> base;
  print_sample_entries (name, syntax, alist, order, own, "dummy", base);
}

void 
ProgramDocument::print_sample (const symbol name, const Library& library)
{
  const Syntax& syntax = library.syntax (name);
  const AttributeList& alist = library.lookup (name);

  const std::vector<std::string>& order = syntax.order ();
  std::vector<std::string> own;
  own_entries (library, name, own);
  std::vector<std::string> base;
  inherited_entries (library, name, base);

  print_sample_entries (name.name (), syntax, alist, order, own, 
                        library.name ().name (), base);
}

void 
ProgramDocument::print_sample_name (const std::string& name)
{
  Format::TableCell dummy (*format);
  format->text ("<");
  format->special ("nbsp");
  if (!submodel)
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
ProgramDocument::print_sample_entries (const std::string& name,
                                       const Syntax& syntax,
                                       const AttributeList& alist,
                                       const std::vector<std::string>& 
                                       /**/ order,
                                       const std::vector<std::string>&
                                       /**/ own_entries,
                                       const std::string& lib_name,
                                       const std::vector<std::string>& 
                                       /**/ base_entries)
{
  // Remove uninteresting entries
  std::vector<std::string> own; 
  for (size_t i = 0; i < own_entries.size (); i++)
    if (syntax.order (own_entries[i]) < 0
	&& !syntax.is_log (own_entries[i])
	&& syntax.lookup (own_entries[i]) != Syntax::Library)
      own.push_back (own_entries[i]);
  std::vector<std::string> base;
  for (size_t i = 0; i < base_entries.size (); i++)
    if (syntax.order (base_entries[i]) < 0
	&& !syntax.is_log (base_entries[i])
	&& syntax.lookup (base_entries[i]) != Syntax::Library)
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
      print_sample_name (name);
      Format::TableCell d4 (*format);
      print_sample_end ();
    }

  // Ordered members first.
  if (order.size () > 0)
    {
      Format::TableRow d3 (*format);
      print_sample_name (name);
      Format::TableCell d4 (*format);
      for (unsigned int i = 0; i < order.size (); i++)
	{ 
	  format->italic (order[i]);
	  if (syntax.size (order[i]) == Syntax::Sequence)
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
	print_sample_name (name);
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
	  print_sample_name (name);
	else
	  { Format::TableCell empty (*format); }
	
	{
	  Format::TableMultiCell dummy (*format, 2, "l");
	  format->text (";; Shared parameters are described in section");
	  format->special ("nbsp");
	  format->ref ("model", 
		       lib_name + "-" + alist.identifier ("base_model"));
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
ProgramDocument::print_submodel (const std::string& name, int level,
				 const Syntax& syntax,
				 const AttributeList& alist)
{
  std::vector<std::string> entries;
  syntax.entries (entries);
  print_submodel_entries (name, level, syntax, alist, entries);
}

void 
ProgramDocument::print_submodel_entries (const std::string& name, int level,
					 const Syntax& syntax,
					 const AttributeList& alist,
					 const std::vector<std::string>&
					 /**/ entries)
{
  const std::vector<std::string>& order = syntax.order ();
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
	    {
	      print_submodel_entry (order[i], level, syntax, alist, first);
	    }
      
	  // Then the remaining members, except log variables.
	  for (unsigned int i = 0; i < entries.size (); i++)
	    if (syntax.order (entries[i]) < 0 && !syntax.is_log (entries[i]))
	      print_submodel_entry (entries[i], level, syntax, alist, first);
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
	      print_submodel_entry (entries[i], level, syntax, alist, first);
	}
    }
}

void 
ProgramDocument::print_submodel_entry (const std::string& name, int level,
				       const Syntax& syntax,
				       const AttributeList& alist, bool& first)
{
  if (first)
    first = false;
  else
    format->soft_linebreak ();

  const Syntax::type type = syntax.lookup (name);

  // We ignore libraries.
  if (type == Syntax::Library)
    return;

  const int size = syntax.size (name);

  // Print name.
  Format::Item dummy (*format, name);
  format->index (name);

  // Print type.
  print_entry_type (name, syntax, alist);

  // Print size.
  if (size == Syntax::Singleton)
    /* do nothing */;
  else if (size == Syntax::Sequence)
    format->text (" sequence");
  else
    {
      TmpStream tmp;
      tmp () << " array of length " << size;
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
  const std::string& description = syntax.description (name);
  if (description != Syntax::Unknown ())
    {
      format->hard_linebreak ();
      format->text (description);
      format->soft_linebreak ();
    }

  // print submodel entries, if applicable
  print_entry_submodel (name, level + 1, syntax, alist);
}


void
ProgramDocument::print_model (const symbol name, const Library& library)
{
  
  const Syntax& syntax = library.syntax (name);
  const AttributeList& alist = library.lookup (name);  

  const XRef::ModelUsed used (library.name (), name);
  if (alist.check ("type"))
    {
      // This is a parameterization.
      const symbol type = alist.identifier ("type");
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

      if (alist.check ("description"))
	{
	  daisy_assert (library.check (type));
	  const AttributeList& super = library.lookup (type);
	  const std::string description = alist.name ("description");
	  
	  if (!super.check ("description") 
	      || super.name ("description") != description)
	    print_description (description);
	}
      print_users (xref.models[used]);
      const std::vector<doc_fun>& doc_funs 
	= library.doc_funs ();
      for (size_t i = 0; i < doc.funs.size ();i++)
	doc_funs[i](format, alist);
      if (print_parameterizations)
	{
	  TmpStream tmp;
	  PrinterFile printer (tmp ());
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
      if (alist.check ("description"))
	print_description (alist.name ("description"));

      print_users (xref.models[used]);
      print_sample (name, library);
      
      // Print own entries.
      std::vector<std::string> entries;
      own_entries (library, name, entries);
      print_submodel_entries (name.name (), 0, syntax, alist, entries);
    }
}

void
ProgramDocument::print_fixed (const std::string& name, 
			      const Syntax& syntax,
			      const AttributeList& alist)
{
  format->soft_linebreak ();
  Format::Section dummy (*format, "section", name, "fixed", name);
  format->index (name);

  // Print description, if any.
  if (alist.check ("description"))
    print_description (alist.name ("description"));

  print_users (xref.submodels[name]);

  print_sample (name, syntax, alist);
  print_submodel (name, 0, syntax, alist);
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
      ? al.identifier ("type") 
      : al.identifier ("base_model");
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
ProgramDocument::print_component (const Library& library)
{

  const symbol name = library.name ();
  current_component = name;
  format->soft_linebreak ();
  Format::Section dummy (*format, "chapter", name.name (), 
			 "component", name.name ());
  format->index (name.name ());

  const char *const description = library.description ();
  if (description)
    print_description (description);

  print_users (xref.components[name]);

  // For all members...
  std::vector<symbol> entries;
  library.entries (entries);
  ModelCompare model_compare (library);
  sort (entries.begin (), entries.end (), model_compare);
  for (unsigned int i = 0; i < entries.size (); i++)
    print_model (entries[i], library);

  static const symbol Daisy_symbol ("Daisy");
  current_component = Daisy_symbol;
}

void
ProgramDocument::print_document ()
{
  Format::Document dummy (*format);

  // For all components...
  std::vector<symbol> entries;
  Library::all (entries);
  sort (entries.begin (), entries.end (), symbol::alphabetical);
  for (unsigned int i = 0; i < entries.size (); i++)
    print_component (Library::find (entries[i]));

  // Fixed components.
  Format::Section d2 (*format, "chapter", "Fixed Components", "cha", "fixed");
  print_description ("\
Fixed components are similar to ordinary component, with the exceptions\n\
that there can only be one model, that is, only a single implementation\n\
of the component, and that it is not possible to define libraries of\n\
standard parameterizations for the model."); 
  std::vector<std::string> fixed;
  Submodel::all (fixed);
  for (unsigned int i = 0; i < fixed.size (); i++)
    {
      const std::string& name = fixed[i];
      Syntax syntax;
      AttributeList alist;
      Submodel::load_syntax (name, syntax, alist);
      print_fixed (name, syntax, alist);
  }
}

static struct ProgramDocumentSyntax
{
  static Program&
  make (const AttributeList& al)
  { return *new ProgramDocument (al); }
  ProgramDocumentSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Generate the components part of the reference manual.");
    syntax.add ("format", Librarian<Format>::library (), 
		Syntax::Const, Syntax::Singleton,
		"Text format used for the document.");
    AttributeList LaTeX_alist;
    LaTeX_alist.add ("type", "LaTeX");
    alist.add ("format", LaTeX_alist);
    syntax.add ("print_parameterizations", Syntax::Boolean, Syntax::Const,
		"Include a copy of all loaded parameterizations in document.");
    alist.add ("print_parameterizations", false);

    Librarian<Program>::add_type ("document", alist, syntax, &make);
  }
} ProgramDocument_syntax;
