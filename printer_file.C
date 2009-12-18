// printer_file.C -- Print alist in ParserFile format.
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

#define BUILD_DLL

#include "printer_file.h"
#include "metalib.h"
#include "library.h"
#include "block_model.h"
#include "plf.h"
#include "time.h"
#include "parser.h"
#include "path.h"
#include "assertion.h"
#include "librarian.h"
#include "frame_model.h"
#include "frame_submodel.h"
#include "filepos.h"
#include <sstream>
#include <algorithm>
#include <numeric>
#include <set>

struct PrinterFile::Implementation
{
  // Data.
  const Metalib& metalib;
  std::ostream& out;

  // String utilities.
  static bool is_identifier (const std::string& name);
  static void print_quoted_string (std::ostream& out,
				   const std::string& name);

  // Print entry 'key' in alist.
  void print_dimension (const Frame&, const symbol key, 
                        const symbol dim);
  void print_entry (const Frame& frame, const Frame* super,
                    const symbol key, int indent, bool need_wrapper);

  // Check if entry 'key' need a line for itself.
  bool is_complex (const Frame& frame, const Frame* super,
                   const symbol key) const;
  bool is_complex_object (const FrameModel&, const Library&) const;

  // Print support for specific types.
  void print_string (const std::string& value); 
  void print_symbol (const symbol value)
  { print_string (value.name ()); }
  void print_bool (bool); 
  void print_plf (const PLF&, int indent); 
  void print_alist (const Frame& frame, const Frame* super, 
                    int indent, bool skip);
  void print_object (const FrameModel&, const Library& library,
                     const FrameModel*, int indent);

  // Top level print functions.
  void print_parameterization (symbol library_name, symbol name,
                               bool print_description);
  void print_library_file (const std::string& filename);
  
  // Testing.
  bool good ();

  // Creation.
  Implementation (const Metalib&, std::ostream& stream);
  ~Implementation ();
};

bool 
PrinterFile::Implementation::is_complex (const Frame& frame,
                                         const Frame *const super,
					 const symbol key) const
{
  // Subsets are never complex.
  if (super && frame.subset (metalib, *super, key))
    return false;

  // Sequences are complex...
  if (frame.type_size (key) != Attribute::Singleton)
    {
      // when they are not part of a total order...
      if (!frame.total_order ())
	return true;
      // or not the last element in the order.
      if (frame.order_index (key) + 1U != frame.order ().size ())
	return true;
      return false;
    }
  // We know it is a singleton here.

  switch (frame.lookup (key))
    {
    case Attribute::Number:
    case Attribute::Integer:
    case Attribute::Boolean:
    case Attribute::String:
      return false;
    case Attribute::Model:
      return frame.order_index (key) >= 0
	|| is_complex_object (frame.model (key), 
                              metalib.library (frame.component (key)));
    case Attribute::Submodel:
    case Attribute::PLF:
      return true;
    case Attribute::Scalar:
    case Attribute::Reference:
    case Attribute::Error:
    default:
      daisy_notreached ();
    } 
}

bool 
PrinterFile::Implementation::is_complex_object (const FrameModel& value, 
						const Library& library) const
{
  const symbol element = value.type_name ();
  daisy_assert (element != Attribute::None ());
  if (!library.check (element))
    return false;

  const FrameModel& super = library.model (element);

  // Check if we added something over the library.
  if (value.subset (metalib, super))
    // We didn't.
    return false;
  return true;
}

bool 
PrinterFile::Implementation::is_identifier (const std::string& name)
{
  if (name.size () < 1)
    return false;
  
  const char c = name[0];
  if (c != '_' && !isalpha (c))
    return false;
      
  for (unsigned int i = 1; i < name.size (); i++)
    {
      const char c = name[i];
      if (c != '_' && !isalnum (c))
	return false;
    }
  return true;
}

void 
PrinterFile::Implementation::print_quoted_string (std::ostream& out,
						  const std::string& name)
{
  out << "\"";
  for (unsigned int i = 0; i < name.size (); i++)
    switch (name[i])
      {
      case '\"':
	out << "\\\"";
	break;
      case '\\':
	out << "\\\\";
	break;
      default:
	out << name[i];
      }
  out << "\"";
}

void 
PrinterFile::Implementation::print_dimension (const Frame& frame,
                                              const symbol key,
                                              const symbol dim)
{
  if (dim == Attribute::Unknown ())
    /* do nothing */;
  else if (dim == Attribute::None () || dim == Attribute::Fraction ())
    out << " []";
  else if (dim == Attribute::User ())
    out << " [" << frame.name (key) << "]";
  else
    out << " [" << dim << "]";
}

void
PrinterFile::Implementation::print_entry (const Frame& frame, 
                                          const Frame *const super, 
					  const symbol key,
					  int indent, bool need_wrapper)
{ 
  daisy_assert (frame.check (key));
  Attribute::type type = frame.lookup (key);

  const bool do_wrap 
    = (need_wrapper && is_complex (frame, super, key));
  if (do_wrap)
    {
      out << "(";
      indent++;
    }

  if (frame.type_size (key) == Attribute::Singleton)
    {
      switch (type)
	{
	case Attribute::Number:
	  out << frame.number (key);
          print_dimension (frame, key, frame.dimension (key));
	  break;
	case Attribute::Submodel:
	  if (super && super->check (key))
	    print_alist (frame.submodel (key), &super->submodel (key), 
                         indent, false); 
	  else
	    print_alist (frame.submodel (key), &frame.default_frame (key),
                         indent, false); 
	  break;
	case Attribute::PLF:
	  print_plf (frame.plf (key), indent);
	  break;
	case Attribute::Boolean:
	  print_bool (frame.flag (key));
	  break;
	case Attribute::String:
	  print_symbol (frame.name (key));
	  break;
	case Attribute::Integer:
	  out << frame.integer (key);
	  break;
	case Attribute::Model:
          {
            const symbol component = frame.component (key);
            const Library& library = metalib.library (component);
            if (super && super->check (key))
              print_object (frame.model (key), library, 
                            &super->model (key), indent);
            else
              print_object (frame.model (key), library, 
                            NULL, indent);
          }
	  break;
	case Attribute::Scalar:
        case Attribute::Reference:
	case Attribute::Error:
	default:
	  out << "<Unknown: " << Attribute::type_name (frame.lookup (key))
	      << ">";
	}
    }
  else
    {
      switch (type)
	{
	case Attribute::Number:
	  {
	    const std::vector<double>& value = frame.number_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		out << value[i]; 
	      }
            print_dimension (frame, key, frame.dimension (key));
	  }
	  break;
	case Attribute::Submodel:
	  {
	    const FrameSubmodel& other = frame.default_frame (key);
	    const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& value 
              = frame.submodel_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << "\n" << std::string (indent, ' ');
		out << "(";
		print_alist (*value[i], &other, indent + 1, false); 
		out << ")";
	      }
	  }
	  break;
	case Attribute::PLF:
	  {
	    const std::vector<boost::shared_ptr<const PLF>/**/>& value 
              = frame.plf_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << "\n" << std::string (indent, ' ');
		out << "(";
		print_plf (*value[i], indent + 1);
		out << ")";
	      }
	  }
	  break;
	case Attribute::Boolean:
	  {
	    const std::vector<bool>& value = frame.flag_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		print_bool (value[i]);
	      }
	  }
	  break;
	case Attribute::String:
	  {
	    const std::vector<symbol>& value 
	      = frame.name_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		print_symbol (value[i]);
	      }
	  }
	  break;
	case Attribute::Integer:
	  {
	    const std::vector<int>& value = frame.integer_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		out << value[i]; 
	      }
	  }
	  break;
	case Attribute::Model:
	  {
            const symbol component = frame.component (key);
            const Library& library = metalib.library (component);
	    const std::vector<boost::shared_ptr<const FrameModel>/**/>& value 
              = frame.model_sequence (key);
            // We really should check original value.
            const std::vector<boost::shared_ptr<const FrameModel>/**/>& super_value
              = (super && super->check (key))
              ? super->model_sequence (key)
              : std::vector<boost::shared_ptr<const FrameModel>/**/> ();
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
                const FrameModel& me = *value[i];
                const FrameModel* other = super_value.size () > i
                  ? super_value[i].get ()
                  : NULL;
		if (i > 0)
		  out << "\n" << std::string (indent, ' ');
                if ((other && me.subset (metalib, *other))
                    || !is_complex_object (me, library))
		  print_object (me, library, other, indent);
		else 
		  {
		    out << "(";
		    print_object (me, library, other, indent + 1);
		    out << ")";
		  }
	      }
	  }
	  break;
	case Attribute::Scalar:
        case Attribute::Reference:
	case Attribute::Error:
	default:
	  out << "<" << Attribute::type_name (frame.lookup (key)) 
	      << " sequence>";
	}
    }
  if (do_wrap)
    {
      out << ")";
    }
}

void 
PrinterFile::Implementation::print_string (const std::string& value) 
{ PrinterFile::print_string (out, value); }

void 
PrinterFile::Implementation::print_bool (bool value) 
{ 
  if (value)
    out << "true";
  else
    out << "false";
}

void 
PrinterFile::Implementation::print_plf (const PLF& plf, int indent) 
{ 
  int column = indent;
  for (unsigned int i = 0; i < plf.size (); i++)
    {
      std::ostringstream tmp;
      tmp << "(" << plf.x (i) << " " << plf.y (i) << ")";
      int entry = tmp.str ().length ();
      
      if (column == indent)
	/* do nothing */;
      else if (column + entry > 71)
	{
	  out << "\n" << std::string (indent, ' ');
	  column = indent;
	}
      else
	{
	  out << " ";
	  column ++;
	}
      out << tmp.str ();
      column += entry;
    }
}

void 
PrinterFile::Implementation::print_alist (const Frame& frame, 
                                          const Frame *const super,
					  int indent, bool skip)
{
  // Always print ordered items.
  const std::vector<symbol>& order = frame.order ();
  bool complex_printing = false;
  for (unsigned int i = 0; i < order.size (); i++)
    {
      const symbol key = order[i];
      if (!complex_printing && is_complex (frame, super, key))
	complex_printing = true;
	
      if (!skip)
	skip = true;
      else if (complex_printing)
	out << "\n" << std::string (indent, ' ');
      else
	out << " ";

      if (frame.check (key))
	{
#if 0
	  // Design bug: We usually need to put parentheses around
	  // ordered complex values.  However, the parser doesn't
	  // expect these for alist sequences, so we don't print them
	  // either. 
	  if (frame.lookup (key) == Attribute::Submodel
	      && frame.type_size (key) != Attribute::Singleton)
	    print_entry (frame, super, key, indent, false);
	  else
#endif
	    print_entry (frame, super, key, indent, true);
	}
      else if (!frame.is_optional (key))
	out << "<missing " << key << ">";
    }

  // Print unordered items.
  std::set<symbol> entries;
  frame.entries (entries);

  // Print new declarations.
  std::set<symbol> super_set;
  if (super)
    super->entries (super_set);

  for (std::set<symbol>::const_iterator i = entries.begin ();
       i != entries.end ();
       i++)
    {
      const symbol key = *i;
      
      // Skip already printed members.
      if (frame.order_index (key) >= 0)
	continue;

      // Declare new members.
      if (super_set.find (key) == super_set.end ())
        {
          if (!skip)
            skip = true;
          else
            out << "\n" << std::string (indent, ' ');

          out << "(declare " << key << " ";

          const int size = frame.type_size (key);
          if (size == Attribute::Singleton)
            /* do nothing */;
          else if (size == Attribute::Variable)
            out << "[] ";
          else
            out << "[" << size << "] ";
          
          const Attribute::type type = frame.lookup (key);
          switch (type)
            {
            case Attribute::Boolean:
            case Attribute::String:
            case Attribute::Integer:
              out << Attribute::type_name (type);
              break;
            case Attribute::Number:
              out << Attribute::type_name (type) << " ";
              print_dimension (frame, key, frame.dimension (key));
              break;
            case Attribute::Submodel:
              out << "fixed " << frame.submodel_name (key);
              break;
            case Attribute::Model:
              out << frame.component (key);
              break;
            case Attribute::PLF: 
            case Attribute::Scalar:
            case Attribute::Reference:
            case Attribute::Error:
            default:
              out << "<Error>";
            }
          out << "\n " << std::string (indent, ' ');
          print_symbol (frame.description (key));
          out << ")";
        }

      // Skip subset members.
      if (super && frame.subset (metalib, *super, key))
	continue;

      if (!skip)
	skip = true;
      else
	out << "\n" << std::string (indent, ' ');

      // Now print it.
      out << "(" << key << " ";
      print_entry (frame, super, key, 
		   indent + key.name ().length () + 2, false);
      out << ")";
    }
}

void 
PrinterFile::Implementation::print_object (const FrameModel& value,
                                           const Library& library, 
                                           const FrameModel *const original, 
                                           int indent)
{
  const symbol element = value.type_name ();
  if (!library.check (element))
    {
      out << "<unknown " << element << ">";
      return;
    }

  // Check original.
  if (original && original->type_name () == element)
    {
      out << "original";
      // Check if we added something over the original.
      if (value.subset (metalib, *original))
        return;
      out << " ";
      print_alist (value, original, indent + 9, false);
      return;

    }
  
  const FrameModel& element_frame = library.model (element);
  
  // Check if we added something over the library.
  if (value.subset (metalib, element_frame))
    {
      // We didn't.
      print_symbol (element);
      return;
    }

  // Library element with additional attributes.
  print_symbol (element);
  out << " ";
  print_alist (value, &element_frame,
               indent + 1 + element.name ().length ()
               // Buglet: Wrong indentation for elements with strange chars.
               + (is_identifier (element.name ()) ? 0 : 2),
               false);
}


void 
PrinterFile::Implementation
/**/::print_parameterization (const symbol library_name, const symbol name,
                              bool print_description)
{
  Library& library = metalib.library (library_name);
  std::auto_ptr<FrameModel> frame (&library.model (name).clone ());
  daisy_assert (frame.get ());
  if (frame->type_name () != name)
    {
      daisy_bug ("Asking for '" + name + "' getting '" + frame->type_name ()
                 + "' (base " + frame->base_name () + ")");
    }
#if 0
  if (!print_description && frame->alist ().check ("description"))
    frame->alist ().remove ("description");
#endif
  const FrameModel& root = library.model ("component");

  out << "(def" << library_name << " ";
  print_symbol (name);
  out << " ";
  const symbol super = frame->base_name ();
  if (super != Attribute::None ())
    {
      print_symbol (super);
      if (!library.check (super))
	{
	  out << " ;; unknown superclass\n ";
	  print_alist (*frame, &root, 2, true);
	}
      else
        print_alist (*frame, &library.model (super), 2, true);
    }
  else
    {
      out << "<unknown>\n  ";
      print_alist (*frame, &root, 2, true);
    }
  out << ")\n";
}

// We store all matching entries here.
struct FoundEntry
{
  symbol library_name;
  symbol element;
  int sequence;

  bool operator < (const FoundEntry& e) const
  { return sequence < e.sequence; }

  FoundEntry (const symbol l, const symbol e, const int s)
    : library_name (l),
      element (e),
      sequence (s)
  { }
  FoundEntry (const FoundEntry& e)
    : library_name (e.library_name),
      element (e.element),
      sequence (e.sequence)
  { }
};

void
PrinterFile::Implementation::print_library_file (const std::string& filename)
{
  std::vector<FoundEntry> found;
  
  // Search all the libraries for matching entries.
  {
    std::vector<symbol> all;
    metalib.all (all);

    for (unsigned int i = 0; i < all.size (); i++)
      {
	const symbol library_name = all[i];
	Library& library = metalib.library (library_name);
	std::vector<symbol> elements;
	library.entries (elements);
      
	for (unsigned int j = 0; j < elements.size (); j++)
	  {
	    const symbol element = elements[j];
	    const FrameModel& frame = library.model (element);
            const Filepos& pos = frame.own_position ();
	    if (pos.filename () ==  filename)
	      {
		found.push_back (FoundEntry (library_name, element, 
					     frame.sequence_id ()));
	      }
	  }
      }
  }
  // Sort the entries.
  sort (found.begin (), found.end ());

  // Print the entries.
  bool first = true;
  for (unsigned int i = 0; i < found.size (); i++)
    {
      if (first)
	first = false;
      else
	out << "\n";

      print_parameterization (found[i].library_name, found[i].element, true);
    }
}

bool
PrinterFile::Implementation::good ()
{ return out.good (); }

PrinterFile::Implementation::Implementation (const Metalib& mlib,
                                             std::ostream& stream)
  : metalib (mlib),
    out (stream)
{ }

PrinterFile::Implementation::~Implementation ()
{ }

void 
PrinterFile::print_string (std::ostream& out,
			   const std::string& value) 
{ 
  if (Implementation::is_identifier (value))
    out << value; 
  else
    Implementation::print_quoted_string (out, value);
}

void
PrinterFile::print_comment (const symbol comment_s)
{
  const std::string& comment = comment_s.name ();

  std::vector<std::string> text;

  int last = 0;
  for (;;)
    {
      const int next = comment.find ('\n', last);
      if (next < 0)
	break;
      text.push_back (comment.substr (last, next - last));
      last = next + 1;
    }
  text.push_back (comment.substr (last));

  for (unsigned int i = 0; i < text.size (); i++)
    impl->out << ";; " << text[i] << "\n";
}

void 
PrinterFile::print_entry (const Frame& frame,
			  const symbol key)
{ 
  if (frame.check (key))
    {
      impl->out << "(" << key << " ";
      const int indent = 2 + key.name ().length ();
      impl->print_entry (frame, NULL, key, indent, false);
      impl->out << ")\n";
    }
}

void 
PrinterFile::print_parameterization (const symbol library_name, 
                                     const symbol name, bool print_description)
{ impl->print_parameterization (library_name, name, print_description); }

void
PrinterFile::print_library_file (const std::string& filename)
{ impl->print_library_file (filename); }

void
PrinterFile::print_input (const Frame& frame)
{
  const symbol type = frame.type_name ();
  const Library& library = impl->metalib.library (Parser::component);
  const FrameModel& base_frame = library.model (type);

  impl->out << "(input " << type << " ";
  impl->print_alist (frame, &base_frame, 7, false);
  impl->out << ")\n";
}

bool
PrinterFile::good ()
{ return impl->good (); }
  
PrinterFile::PrinterFile (const Metalib& mlib,
                          std::ostream& stream)
  : impl (new Implementation (mlib, stream))
{ }
    
PrinterFile::~PrinterFile ()
{ }

// printer_file.C ends here.
