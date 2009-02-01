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
#include "block.h"
#include "plf.h"
#include "time.h"
#include "parser.h"
#include "path.h"
#include "assertion.h"
#include "librarian.h"
#include "frame_model.h"
#include "alist.h"
#include <sstream>
#include <fstream>
#include <algorithm>
#include <numeric>
#include <set>

struct PrinterFile::Implementation
{
  // Data.
  Metalib& metalib;
  const std::auto_ptr<std::ofstream> owned_stream; // If we opened it.
  std::ostream& out;

  // String utilities.
  static bool is_identifier (const std::string& name);
  static void print_quoted_string (std::ostream& out,
				   const std::string& name);

  // Print entry 'key' in alist.
  void print_dimension (const Frame&, const symbol key, 
                        const symbol dim);
  void print_entry (const Frame& frame, const Frame& super,
                    const symbol key, int indent, bool need_wrapper);

  // Check if entry 'key' need a line for itself.
  bool is_complex (const Frame& frame, const Frame& super,
                   const symbol key) const;
  bool is_complex_object (const Frame&, const Library&) const;

  // Print support for specific types.
  void print_string (const std::string& value); 
  void print_symbol (const symbol value)
  { print_string (value.name ()); }
  void print_bool (bool); 
  void print_plf (const PLF&, int indent); 
  void print_alist (const Frame& frame, const Frame& super, 
                    int indent, bool skip);
  void print_object (const Frame&, const Library& library,
                     const Frame&, int indent);

  // Top level print functions.
  void print_parameterization (symbol library_name, symbol name,
                               bool print_description);
  void print_library_file (const std::string& filename);
  
  // Testing.
  bool good ();

  // Creation.
  Implementation (Metalib&, const symbol name);
  Implementation (Metalib&, std::ostream& stream);
  ~Implementation ();
};

bool 
PrinterFile::Implementation::is_complex (const Frame& frame,
                                         const Frame& super,
					 const symbol key) const
{
  // Subsets are never complex.
  if (frame.subset (metalib, super, key))
    return false;

  // Sequences are complex...
  if (frame.type_size (key) != Value::Singleton)
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
    case Value::Number:
    case Value::Integer:
    case Value::Boolean:
    case Value::String:
      return false;
    case Value::Object:
      return frame.order_index (key) >= 0
	|| is_complex_object (frame.frame (key), 
                              frame.library (metalib, key));
    case Value::AList:
    case Value::PLF:
      return true;
    case Value::Library:
    case Value::Error:
    default:
      daisy_notreached ();
    } 
}

bool 
PrinterFile::Implementation::is_complex_object (const Frame& value, 
						const Library& library) const
{
  daisy_assert (value.check ("type"));
  const symbol element = value.name ("type");
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
  if (dim == Value::Unknown ())
    /* do nothing */;
  else if (dim == Value::None () || dim == Value::Fraction ())
    out << " []";
  else if (dim == Value::User ())
    out << " [" << frame.name (key) << "]";
  else
    out << " [" << dim << "]";
}

void
PrinterFile::Implementation::print_entry (const Frame& frame, 
                                          const Frame& super, 
					  const symbol key,
					  int indent, bool need_wrapper)
{ 
  daisy_assert (frame.check (key));
  Value::type type = frame.lookup (key);

  const bool do_wrap 
    = (need_wrapper && is_complex (frame, super, key));
  if (do_wrap)
    {
      out << "(";
      indent++;
    }

  if (frame.type_size (key) == Value::Singleton)
    {
      switch (type)
	{
	case Value::Number:
	  out << frame.number (key);
          print_dimension (frame, key, frame.dimension (key));
	  break;
	case Value::AList:
	  if (super.check (key))
	    print_alist (frame.frame (key), super.frame (key), indent, false); 
	  else
	    print_alist (frame.frame (key), frame.default_frame (key),
                         indent, false); 
	  break;
	case Value::PLF:
	  print_plf (frame.plf (key), indent);
	  break;
	case Value::Boolean:
	  print_bool (frame.flag (key));
	  break;
	case Value::String:
	  print_symbol (frame.name (key));
	  break;
	case Value::Integer:
	  out << frame.integer (key);
	  break;
	case Value::Object:
          {
            const Library& library = frame.library (metalib, key);
            if (super.check (key))
              print_object (frame.frame (key), library, 
                            super.frame (key), indent);
            else
              print_object (frame.frame (key), library, 
                            FrameModel::root (), indent);
          }
	  break;
	case Value::Library:
	case Value::Error:
	default:
	  out << "<Unknown: " << Value::type_name (frame.lookup (key))
	      << ">";
	}
    }
  else
    {
      switch (type)
	{
	case Value::Number:
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
	case Value::AList:
	  {
	    const Frame& other = frame.default_frame (key);
	    const std::vector<const Frame*>& value 
              = frame.frame_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << "\n" << std::string (indent, ' ');
		out << "(";
		print_alist (*value[i], other, indent + 1, false); 
		out << ")";
	      }
	  }
	  break;
	case Value::PLF:
	  {
	    const std::vector<const PLF*>& value = frame.plf_sequence (key);
	    
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
	case Value::Boolean:
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
	case Value::String:
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
	case Value::Integer:
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
	case Value::Object:
	  {
	    const Library& library = frame.library (metalib, key);
	    const std::vector<const Frame*>& value = frame.frame_sequence (key);
            // We really should check original value.
            const std::vector<const Frame*>& super_value = super.check (key)
              ? super.frame_sequence (key)
              : std::vector<const Frame*> ();
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
                const Frame& me = *value[i];
                const Frame& other = super_value.size () > i
                  ? *super_value[i]
                  : FrameModel::root ();
		if (i > 0)
		  out << "\n" << std::string (indent, ' ');
                if (me.subset (metalib, other)
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
	case Value::Library:
	case Value::Error:
	default:
	  out << "<" << Value::type_name (frame.lookup (key)) 
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
                                          const Frame& super,
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
	  if (frame.lookup (key) == Value::AList
	      && frame.type_size (key) != Value::Singleton)
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
  super.entries (super_set);

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
          if (size == Value::Singleton)
            /* do nothing */;
          else if (size == Value::Sequence)
            out << "[] ";
          else
            out << "[" << size << "] ";
          
          const Value::type type = frame.lookup (key);
          switch (type)
            {
            case Value::Boolean:
            case Value::String:
            case Value::Integer:
              out << Value::type_name (type);
              break;
            case Value::Number:
              out << Value::type_name (type) << " ";
              print_dimension (frame, key, frame.dimension (key));
              break;
            case Value::AList:
              out << "fixed " << frame.submodel_name (key);
              break;
            case Value::Object:
              out << frame.library (metalib, key).name ();
              break;
            case Value::PLF: 
            case Value::Library:
            case Value::Error:
            default:
              out << "<Error>";
            }
          out << "\n " << std::string (indent, ' ');
          print_symbol (frame.description (key));
          out << ")";
        }

      // Skip subset members.
      if (frame.subset (metalib, super, key))
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
PrinterFile::Implementation::print_object (const Frame& value,
                                           const Library& library, 
                                           const Frame& original, 
                                           int indent)
{
  daisy_assert (value.check ("type"));
  const symbol element = value.name ("type");
  if (!library.check (element))
    {
      out << "<unknown " << element << ">";
      return;
    }

  // Check original.
  if (original.check ("type") && original.name ("type") == element)
    {
      out << "original";
      // Check if we added something over the original.
      if (value.subset (metalib, original))
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
  print_alist (value, element_frame,
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
  if (!print_description && frame->alist ().check ("description"))
    frame->alist ().remove ("description");
  const FrameModel& root = library.model ("component");

  out << "(def" << library_name << " ";
  print_symbol (name);
  out << " ";
  if (frame->check ("type"))
    {
      const symbol super = frame->name ("type");
      print_symbol (super);
      if (!library.check (super))
	{
	  out << " ;; unknown superclass\n ";
	  print_alist (*frame, root, 2, true);
	}
      else
        print_alist (*frame, library.model (super), 2, true);
    }
  else
    {
      out << "<unknown>\n  ";
      print_alist (*frame, root, 2, true);
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

	    if (frame.check ("parsed_from_file") 
		&& frame.name ("parsed_from_file") == filename)
	      {
		found.push_back (FoundEntry (library_name, element, 
					     frame.integer
                                             /**/ ("parsed_sequence")));
	      }
	  }
      }
  }
  // Sort the entries.
  sort (found.begin (), found.end ());

  // Print the entries.
  bool first = true;
  const AttributeList empty_alist;
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

PrinterFile::Implementation::Implementation (Metalib& mlib,
                                             const symbol name)
  : metalib (mlib),
    owned_stream (new std::ofstream (name.name ().c_str ())),
    out (*owned_stream)
{ }

PrinterFile::Implementation::Implementation (Metalib& mlib,
                                             std::ostream& stream)
  : metalib (mlib),
    owned_stream (NULL),
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
      impl->print_entry (frame, FrameModel::root (), key, indent, false);
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
  daisy_assert (frame.check ("type"));
  const symbol type = frame.name ("type");
  const Library& library = impl->metalib.library (Parser::component);
  const FrameModel& base_frame = library.model (type);

  impl->out << "(input " << type << " ";
  impl->print_alist (frame, base_frame, 7, false);
  impl->out << ")\n";
}

bool
PrinterFile::good ()
{ return impl->good (); }
  
PrinterFile::PrinterFile (Metalib& mlib,
                          const symbol filename)
  : Printer ("file"),
    impl (new Implementation (mlib, filename))
{ }
    
PrinterFile::PrinterFile (Metalib& mlib,
                          std::ostream& stream)
  : Printer ("stream"),
    impl (new Implementation (mlib, stream))
{ }
    
PrinterFile::PrinterFile (Block& al)
  : Printer (al.name ("type")),
    impl (new Implementation (al.metalib (), al.name ("where")))
{ }
    
PrinterFile::~PrinterFile ()
{ }

static struct PrinterFileSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new PrinterFile (al); }

  PrinterFileSyntax ()
    : DeclareModel (Printer::component, "file", 
               "Print internal datastructures with lots of parentheses.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add ("where", Value::String, Value::Const,
                "File to print in.");
    frame.order ("where");
  }
} PrinterFile_syntax;
