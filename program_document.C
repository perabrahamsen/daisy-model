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
#include "printer_file.h"
#include "xref.h"
#include "plf.h"
#include "format.h"
#include "treelog.h"
#include "assertion.h"
#include "librarian.h"
#include "frame_submodel.h"
#include "frame_model.h"
#include "filepos.h"
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
			 const Frame& frame);
  void print_entry_submodel (const symbol name, 
			     int level,
			     const Frame& frame,
                             const symbol aref);
  void print_entry_category (const symbol name, 
			     const Frame& frame);
  void print_entry_value (const symbol name, 
			  const Frame& frame);

  void print_users (const XRef::Users&);
  void print_sample_entry (const symbol name, 
			   const Frame& frame,
                           bool last);

  // Print parts of it.
  static void own_entries (Metalib&,
                           const Library& library, const symbol name, 
                           std::set<symbol>& entries, 
                           bool new_only = false);
  static void inherited_entries (Metalib&, const Library& library,
                                 const symbol name, 
                                 std::set<symbol>& entries);
  void print_sample (const symbol name,
		     const Frame& frame,
		     bool top_level);
  void print_sample (const symbol name, const Library&);
  void print_sample_name (const symbol name, bool top_level);
  void print_sample_end ();
  void print_sample_entries (const symbol name,
                             const Frame& frame,
                             const std::vector<symbol>& order,
                             const std::set<symbol>& own_entries,
                             const symbol lib_name,
                             const std::set<symbol>& base_entries,
			     bool top_level);
  void print_submodel (const symbol name, int level,
		       const Frame& frame,
                       const symbol aref);
  void print_submodel_entries (const symbol name, int level,
                               const Frame& frame,
                               const std::set<symbol>& entries, 
			       const symbol aref);
  void print_submodel_entry (const symbol, int level,
                             const Frame& frame, bool& first, 
			     const symbol aref);
  void print_model (symbol name, const Library& library, Treelog&);
  void print_fixed (const symbol name, 
		    const Frame& frame,
                    const symbol description);
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
				   const Frame& frame)
{
  const Value::type type = frame.lookup (name);

  switch (type)
    {
    case Value::Number:
      {
	format->text ("number ");
	const symbol dimension = frame.dimension (name);
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
        const symbol submodel_name = frame.submodel_name (name);
	if (submodel_name != Value::None ())
	  {
	    format->bold (submodel_name);
	    format->text (" fixed component ");
	    format->see ("section", "fixed", submodel_name);
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
	const symbol domain = frame.domain (name);
	const symbol range = frame.range (name);
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
	const symbol component = frame.component (name);
	format->bold (component);
	format->text (" component ");
	format->see ("chapter", "component",  component);
      }
      break;
    case Value::Scalar:
    case Value::Reference:
    case Value::Error:
    default:
      daisy_panic ("Unknown entry '" + name + "'");
    };
}

void 
ProgramDocument::print_entry_submodel (const symbol name, 
				       const int level,
				       const Frame& frame,
				       const symbol aref)
{
  const Value::type type = frame.lookup (name);
  if (type == Value::AList)
    {
      const FrameSubmodel& child = frame.submodel (name);
      if (frame.submodel_name (name) == Value::None ())
	{
	  print_sample (name, child, false);
	  print_submodel (name, level, child, aref);
	}
    }
}
    
void 
ProgramDocument::print_entry_category (const symbol name, 
				       const Frame& frame)
{
  const Value::type type = frame.lookup (name);

  if (type == Value::Object)	// Objects and ALists don't have categories.
    {
      if (frame.is_optional (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional component");
	}
      else if (frame.check (name))
	{
	  format->hard_linebreak ();
	  format->text ("Component");
	}
    }
  else if (type == Value::AList)
    {
      if (frame.is_optional (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional submodel");
	}
      else if (frame.check (name))
	{
	  format->hard_linebreak ();
	  format->text ("Submodel");
	}
    }
  else if (frame.is_optional (name))
    {
      if (frame.is_const (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional parameter");
	}
      else if (frame.is_state (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional state variable");
	}
      else if (frame.is_log (name))
	{
	  format->hard_linebreak ();
	  format->text ("Optional log variable");
	}
      else 
	daisy_notreached ();
    }
  else
    {
      if (frame.is_const (name))
	{
	  format->hard_linebreak ();
	  format->text ("Parameter");
	}
      else if (frame.is_state (name))
	{
	  format->hard_linebreak ();
	  format->text ("State variable");
	}
      else if (frame.is_log (name))
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
				    const Frame& frame)
{
  if (frame.check (name))
    {
      const Value::type type = frame.lookup (name);
      const int size = frame.type_size (name);

      bool print_default_value = false;
      
      if (size == Value::Singleton)
	switch (type)
	  {
	  case Value::Number:
	    {
	      std::ostringstream tmp;
	      tmp << " (default " << frame.number (name) << ")";
	      format->text (tmp.str ());
	    }
	    break;
	  case Value::AList:
	    {
	      const bool has_errors 
                = !frame.check (metalib, name, Treelog::null ());
	      if (has_errors)
		format->text (" (has partially specified default value)");
	      else 
		format->text (" (has fully specified default value)");
              const symbol submodel = frame.submodel_name (name);
              if (submodel != Value::None ())
		{
		  const FrameSubmodel& nested = frame.submodel (name);
                  const FrameSubmodel& frame 
                    = Librarian::submodel_frame (submodel);
		  if (!nested.subset (metalib, frame))
		    print_default_value = true;
		}
	      else
		print_default_value = true;
	    }
	    break;
	  case Value::PLF:
	    {
	      std::ostringstream tmp;
	      tmp << " (has default value with " 
                  << frame.plf (name).size ()
		     << " points)";
	      format->text (tmp.str ());
	      if (frame.plf (name).size () > 0)
		print_default_value = true;
	    }
	    break;
	  case Value::Boolean:
	    format->text (" (default ");
	    if (frame.flag (name))
	      format->text ("true");
	    else 
	      format->text ("false");
	    format->text (")");
	    break;
	  case Value::String:
	    {
	      const std::string value = frame.name (name).name ();
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
	      tmp << " (default " << frame.integer (name) << ")";
	      format->text (tmp.str ());
	    }
	    break;
	  case Value::Object:
	    {
	      const Frame& object = frame.model (name);
	      const symbol type = object.type_name ();
	      format->text (" (default `" + type + "')");
	      const Library& library = metalib.library (frame.component (name));
	      const Frame& super = library.model (type);
	      if (!object.subset (metalib, super))
		print_default_value = true;
	    }
	    break;
	  case Value::Scalar:
          case Value::Reference:
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
	    if (frame.value_size (name) == 0)
	      format->text (" (default: an empty sequence)");
	    else
	      {
		std::ostringstream tmp;
		tmp << " (has default value with length " 
		       << frame.value_size (name) << ")";
		format->text (tmp.str ());
		print_default_value = true;
	      }
	    break;
	  case Value::Scalar:
          case Value::Reference:
	  case Value::Error:
	    daisy_notreached ();
	  }

      if (print_default_value)
	{
	  std::ostringstream tmp;
	  PrinterFile printer (metalib, tmp);
	  printer.print_entry (frame, name);
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
      format->text (component + " " + model + " ");
      for (unsigned int j = 0; j < path.size (); j++)
	format->text (" " + path[j]);
      format->text (" ");
      format->see_page ("model", component + "-" + model);
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
				     const Frame& frame,
                                     const bool last)
{ 
  std::string comment;
  {
    Format::TableCell dummy (*format);
    format->text ("(");
    print_string (name);

    if (frame.check (name))
      {
	const Value::type type = frame.lookup (name);
	const int size = frame.type_size (name);

	bool print_name = true;
	comment = "Has default value.";

	if (size == Value::Singleton)
	  switch (type)
	    {
	    case Value::Number:
	      {
		format->special ("nbsp");
		std::ostringstream tmp;
		tmp << frame.number (name);
                const symbol dimension = frame.dimension (name);
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
                  = !frame.check (metalib, name, Treelog::null ());
		if (has_errors)
		  comment = "Has partial value.";
	      }
	      break;
	    case Value::PLF:
	      break;
	    case Value::Boolean:
	      format->special ("nbsp");
	      format->text (frame.flag (name) ? "true" : "false");
	      format->text (")");
	      print_name = false;
	      break;
	    case Value::String:
	      {
		const std::string value = frame.name (name).name ();
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
		tmp << frame.integer (name) << ")";
		format->text (tmp.str ());
		print_name = false;
	      }
	      break;
	    case Value::Object:
	      {
		const FrameModel& object = frame.model (name);
		const symbol type = object.type_name ();
		comment = "Default " + type + " value.";
	      }
	      break;
	    case Value::Scalar:
            case Value::Reference:
	    case Value::Error:
	      daisy_notreached ();
	    }
	else if (frame.value_size (name) == 0)
	  {
	    format->text (")");
	    print_name = false;
	  }
	else
	  switch (type)
	    {
	    case Value::Number:
	      if (frame.value_size (name) < 5)
		{
		  const std::vector<double>& numbers
		    = frame.number_sequence (name);
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
	    case Value::Scalar:
            case Value::Reference:
	    case Value::Error:
	      daisy_notreached ();
	    }
	if (print_name)
	  {
	    format->special ("nbsp");
	    format->italic (name);
	    if (frame.type_size (name) != Value::Singleton)
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
	if (frame.type_size (name) != Value::Singleton)
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
ProgramDocument::own_entries (Metalib& metalib,
                              const Library& library, const symbol name, 
			      std::set<symbol>& entries,
                              const bool new_only)
{
  const FrameModel& frame = library.model (name);

  frame.entries (entries);

  // Remove base entries.
  const symbol base_model = frame.base_name ();
  if (base_model != Value::None ())
    {
      daisy_assert (base_model != name);

      const FrameModel& base_frame = library.model (base_model);
      std::set<symbol> base_entries;
      base_frame.entries (base_entries);
      for (std::set<symbol>::const_iterator i = base_entries.begin (); 
           i != base_entries.end (); 
           i++)
        {
          const symbol key = *i;
          if (new_only
              || key == "description"
              || frame.subset (metalib, base_frame, key))
            entries.erase (find (entries.begin (), entries.end (), key));
        }
    }
}

void
ProgramDocument::inherited_entries (Metalib& metalib,
                                    const Library& library, const symbol name, 
				    std::set<symbol>& entries)
{
  const FrameModel& frame = library.model (name);

  const symbol base_model = frame.base_name ();
          
  if (base_model != Value::None ())
    {
      const FrameModel& base_frame = library.model (base_model);
      base_frame.entries (entries);
      for (std::set<symbol>::const_iterator i = entries.begin ();
           i != entries.end (); 
           i++)
        {
          const symbol key = *i;
          if (key != "description" 
              && !frame.subset (metalib, base_frame, key))
            entries.erase (find (entries.begin (), entries.end (), key));
        }
    }
}

void 
ProgramDocument::print_sample (const symbol name,
			       const Frame& frame,
                               const bool top_level)
{
  const std::vector<symbol>& order = frame.order ();
  std::set<symbol> own;
  frame.entries (own);
  const std::set<symbol> base;
  print_sample_entries (name, frame, order, own, "dummy", base, 
			top_level);
}

void 
ProgramDocument::print_sample (const symbol name, const Library& library)
{
  const FrameModel& frame = library.model (name);

  const std::vector<symbol>& order = frame.order ();
  std::set<symbol> own;
  own_entries (metalib, library, name, own);
  std::set<symbol> base;
  inherited_entries (metalib, library, name, base);

  print_sample_entries (name, frame, order, own, 
                        library.name (), base, true);
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
                                       const Frame& frame,
                                       const std::vector<symbol>& order,
                                       const std::set<symbol>& own_entries,
                                       const symbol lib_name,
                                       const std::set<symbol>& base_entries, 
				       const bool top_level)
{
  // Remove uninteresting entries
  std::set<symbol> own; 
  for (std::set<symbol>::const_iterator i = own_entries.begin (); 
       i != own_entries.end (); 
       i++)
    if (frame.order_index (*i) < 0 && !frame.is_log (*i))
      own.insert (*i);
  std::set<symbol> base;
  for (std::set<symbol>::const_iterator i = base_entries.begin ();
       i != base_entries.end();
       i++)
    if (frame.order_index (*i) < 0 && !frame.is_log (*i))
      base.insert (*i);

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
	  if (frame.type_size (order[i]) != Value::Singleton)
	    format->special ("...");
	  format->special ("nbsp");
	  left--;
	  if (left == 0)
	    print_sample_end ();
	}
    }
  
  // Then own members.
  for (std::set<symbol>::const_iterator i = own.begin ();
       i != own.end (); 
       i++)
    {
      Format::TableRow row (*format);
      if (left == count)
	print_sample_name (name, top_level);
      else
	Format::TableCell empty (*format);
	  
      print_sample_entry (*i, frame, left == 1);
      left--;
    }
 
  // Finally inherited members.
  if (base.begin () != base.end ())
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
		       lib_name + "-" + frame.type_name ());
	}
      }
      for (std::set<symbol>::const_iterator i = base.begin ();
           i != base.end (); 
           i++)
	{
	  Format::TableRow row (*format);
	  { Format::TableCell empty (*format); }
	  print_sample_entry (*i, frame, left == 1);
	  left--;
	}
    }
  daisy_assert (left == 0);
}

void 
ProgramDocument::print_submodel (const symbol name, int level,
				 const Frame& frame,
                                 const symbol aref)
{
  std::set<symbol> entries;
  frame.entries (entries);
  print_submodel_entries (name, level, frame, entries, aref);
}

void 
ProgramDocument::print_submodel_entries (const symbol name, int level,
					 const Frame& frame,
                                         const std::set<symbol>& entries, 
					 const symbol aref)
{
  const std::string bref = aref + "-" + name;
  const std::vector<symbol>& order = frame.order ();
  int log_count = 0;
  for (std::set<symbol>::const_iterator i = entries.begin ();
       i != entries.end (); 
       i++)
    if (frame.is_log (*i))
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
	    print_submodel_entry (order[i], level, frame, first, bref);
      
	  // Then the remaining members, except log variables.
	  for (std::set<symbol>::const_iterator i = entries.begin (); 
               i != entries.end (); 
               i++)
	    if (frame.order_index (*i) < 0 
                && !frame.is_log (*i))
	      print_submodel_entry (*i, level, frame, first, bref);
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
	  for (std::set<symbol>::const_iterator i = entries.begin ();
               i != entries.end (); 
               i++)
	    if (frame.is_log (*i))
	      print_submodel_entry (*i, level, frame, first, bref);
	}
    }
}

void 
ProgramDocument::print_submodel_entry (const symbol name, int level,
				       const Frame& frame, bool& first,
				       const symbol aref)
{
  if (first)
    first = false;
  else
    format->soft_linebreak ();

  const int size = frame.type_size (name);

  // Print name.
  Format::Item dummy (*format, name);
  format->label ("parameter", aref + "-" + name);
  format->index (name);
  
  // Print type.
  print_entry_type (name, frame);

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

  if (!frame.is_log (name))
    {
      // Print category.
      print_entry_category (name, frame);

      // Print value.
      if (name != "description")
        print_entry_value (name, frame);
    }

  // Print description line.
  const symbol description = frame.description (name);
  if (description != Value::Unknown ())
    {
      format->hard_linebreak ();
      format->text (description);
      format->soft_linebreak ();
    }

  // print submodel entries, if applicable
  print_entry_submodel (name, level + 1, frame, aref);
}


void
ProgramDocument::print_model (const symbol name, const Library& library,
                              Treelog& msg)
{
  const FrameModel& frame = library.model (name);

  const XRef::ModelUsed used (library.name (), name);
  const symbol type = frame.base_name ();
  if (type != Value::None ())
    {
      // This is a parameterization.
      format->soft_linebreak ();
      Format::Section dummy (*format, "section", name, "model", 
			     current_component + "-" + name);
      format->index (name);
      format->text ("A `" + type + "' parameterization ");
      const Filepos& pos = frame.own_position ();
      if (pos != Filepos::none ())
	format->text ("defined in `" + pos.filename () + "'.\n");
      else
	{
	  format->text ("build into ");
	  format->special ("daisy");
	  format->text (".\n");
	}

      if (library.has_interesting_description (frame))
        format->frame_description (frame);

      print_users (xref.models[used]);
      std::set<symbol> entries;
      own_entries (metalib, library, name, entries, true);
      if (entries.size () > 0)
        print_submodel_entries (name, 0, 
                                frame, entries, 
                                library.name ());

      const std::vector<Library::doc_fun>& doc_funs = library.doc_funs ();
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
      Format::Section dummy (*format, "section", name, "model",
			     current_component + "-" + name);
      format->index (name);

      // Print description, if any.
      format->frame_description (frame);

      print_users (xref.models[used]);
      print_sample (name, library);
      
      // Print own entries.
      std::set<symbol> entries;
      own_entries (metalib, library, name, entries);
      print_submodel_entries (name, 0, frame, entries, 
			      library.name ());
    }
}

void
ProgramDocument::print_fixed (const symbol name, 
			      const Frame& frame,
                              const symbol description)
{
  format->soft_linebreak ();
  Format::Section dummy (*format, "section", name, "fixed", name);
  format->index (name);

  // Print description, if any.
  print_description (description);

  print_users (xref.submodels[name]);

  print_sample (name, frame, true);
  print_submodel (name, 0, frame, "fixed");
}

class ModelCompare
{ 
  const Library& library;

  const symbol find_next_in_line (const symbol root, const symbol leaf) const
  {
    // Find the child of root that leaf is descended from.
    daisy_assert (root != leaf);
    const FrameModel& al = library.model (leaf);
    const symbol type = al.base_name ();
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
  Format::Section dummy (*format, "chapter", name, 
			 "component", name);
  format->index (name);

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
  Librarian::submodel_all (fixed);
  for (unsigned int i = 0; i < fixed.size (); i++)
    {
      const symbol name = fixed[i];
      const Frame& frame = Librarian::submodel_frame (name);
      const symbol description = Librarian::submodel_description (name);
      print_fixed (name, frame, description);
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

// program_document.C ends here.
