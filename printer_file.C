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


#include "printer_file.h"
#include "library.h"
#include "block.h"
#include "alist.h"
#include "plf.h"
#include "time.h"
#include "parser.h"
#include "path.h"
#include <sstream>
#include <fstream>
#include <algorithm>
#include <numeric>
#include <set>

struct PrinterFile::Implementation
{
  // Data.
  Path::Output* output;
  std::ostream& out;

  // String utilities.
  static bool is_identifier (const std::string& name);
  static bool is_identifier (const symbol name)
  { return is_identifier (name.name ()); }
  static void print_quoted_string (std::ostream& out,
				   const std::string& name);

  // Print entry 'key' in alist.
  void print_dimension (const AttributeList&, const std::string& key, 
                        const std::string& dim);
  void print_entry (const AttributeList& alist, const Syntax&,
		    const AttributeList& super_alist, 
                    const Syntax& super_syntax, const std::string& key,
		    int indent, bool need_wrapper);

  // Check if entry 'key' need a line for itself.
  bool is_complex (const AttributeList& alist, const Syntax& syntax,
		   const AttributeList& super, const std::string& key) const;
  bool is_complex_object (const AttributeList&, const Library&) const;

  // Print support for specific types.
  void print_string (const std::string& value); 
  void print_symbol (const symbol value)
  { print_string (value.name ()); }
  void print_bool (bool); 
  void print_plf (const PLF&, int indent); 
  void print_alist (const AttributeList& alist, const Syntax&,
                    const AttributeList& super_alist, 
                    const Syntax& super_syntax, int indent, bool skip);
  void print_object (const AttributeList&, const Library& library,
                     const AttributeList&, int indent);

  // Top level print functions.
  void print_parameterization (symbol library_name, symbol name,
                               bool print_description);
  void print_library_file (const std::string& filename);
  
  // Testing.
  bool good ();

  // Creation.
  Implementation (const std::string& name);
  Implementation (std::ostream& stream);
  ~Implementation ();
};

bool 
PrinterFile::Implementation::is_complex (const AttributeList& alist, 
					 const Syntax& syntax,
					 const AttributeList& super,
					 const std::string& key) const
{
  // Subsets are never complex.
  if (alist.subset (super, syntax, key))
    return false;

  // Sequences are complex...
  if (syntax.size (key) != Syntax::Singleton)
    {
      // when they are not part of a total order...
      if (!syntax.total_order ())
	return true;
      // or not the last element in the order.
      if (syntax.order (key) + 1U != syntax.order ().size ())
	return true;
      return false;
    }
  // We know it is a singleton here.

  switch (syntax.lookup (key))
    {
    case Syntax::Number:
    case Syntax::Integer:
    case Syntax::Boolean:
    case Syntax::String:
      return false;
    case Syntax::Object:
      return syntax.order (key) >= 0
	|| is_complex_object (alist.alist (key), syntax.library (key));
    case Syntax::AList:
    case Syntax::PLF:
      return true;
    case Syntax::Library:
    case Syntax::Error:
    default:
      daisy_notreached ();
    } 
}

bool 
PrinterFile::Implementation::is_complex_object (const AttributeList& value, 
						const Library& library) const
{
  daisy_assert (value.check ("type"));
  const symbol element = value.identifier ("type");
  if (!library.check (element))
    return false;

  const Syntax& element_syntax = library.syntax (element);
  const AttributeList& element_alist = library.lookup (element);

	// Check if we added something over the library.
  if (value.subset (element_alist, element_syntax))
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
PrinterFile::Implementation::print_dimension (const AttributeList& alist,
                                              const std::string& key,
                                              const std::string& dim)
{
  if (dim == Syntax::Unknown ())
    /* do nothing */;
  else if (dim == Syntax::None () || dim == Syntax::Fraction ())
    out << " []";
  else if (dim == Syntax::User ())
    out << " [" << alist.name (key) << "]";
  else
    out << " [" << dim << "]";
}

void
PrinterFile::Implementation::print_entry (const AttributeList& alist, 
					  const Syntax& syntax,
					  const AttributeList& super_alist, 
					  const Syntax& super_syntax, 
					  const std::string& key,
					  int indent, bool need_wrapper)
{ 
  daisy_assert (alist.check (key));
  Syntax::type type = syntax.lookup (key);

  const bool do_wrap 
    = (need_wrapper && is_complex (alist, syntax, super_alist, key));
  if (do_wrap)
    {
      out << "(";
      indent++;
    }

  if (syntax.size (key) == Syntax::Singleton)
    {
      switch (type)
	{
	case Syntax::Number:
	  out << alist.number (key);
          print_dimension (alist, key, syntax.dimension (key));
	  break;
	case Syntax::AList:
	  if (super_alist.check (key))
	    print_alist (alist.alist (key), syntax.syntax (key), 
			 super_alist.alist (key), 
                         super_syntax.syntax (key), indent, false); 
	  else
	    print_alist (alist.alist (key), syntax.syntax (key), 
			 syntax.default_alist (key), 
                         syntax.syntax (key), indent, false); 
	  break;
	case Syntax::PLF:
	  print_plf (alist.plf (key), indent);
	  break;
	case Syntax::Boolean:
	  print_bool (alist.flag (key));
	  break;
	case Syntax::String:
	  print_string (alist.name (key));
	  break;
	case Syntax::Integer:
	  out << alist.integer (key);
	  break;
	case Syntax::Object:
	  if (super_alist.check (key))
	    print_object (alist.alist (key), syntax.library (key), 
                          super_alist.alist (key), indent);
	  else
            print_object (alist.alist (key), syntax.library (key), 
                          AttributeList (), indent);
	  break;
	case Syntax::Library:
	case Syntax::Error:
	default:
	  out << "<Unknown: " << Syntax::type_name (syntax.lookup (key))
	      << ">";
	}
    }
  else
    {
      switch (type)
	{
	case Syntax::Number:
	  {
	    const std::vector<double>& value = alist.number_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		out << value[i]; 
	      }
            print_dimension (alist, key, syntax.dimension (key));
	  }
	  break;
	case Syntax::AList:
	  {
	    const AttributeList& other = syntax.default_alist (key);
	    const Syntax& nested = syntax.syntax (key);
	    const std::vector<AttributeList*>& value = alist.alist_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << "\n" << std::string (indent, ' ');
		out << "(";
		print_alist (*value[i], nested, other, nested, 
                             indent + 1, false); 
		out << ")";
	      }
	  }
	  break;
	case Syntax::PLF:
	  {
	    const std::vector<const PLF*>& value = alist.plf_sequence (key);
	    
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
	case Syntax::Boolean:
	  {
	    const std::vector<bool>& value = alist.flag_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		print_bool (value[i]);
	      }
	  }
	  break;
	case Syntax::String:
	  {
	    const std::vector<symbol>& value 
	      = alist.identifier_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		print_symbol (value[i]);
	      }
	  }
	  break;
	case Syntax::Integer:
	  {
	    const std::vector<int>& value = alist.integer_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		out << value[i]; 
	      }
	  }
	  break;
	case Syntax::Object:
	  {
	    const Library& library = syntax.library (key);
	    const std::vector<AttributeList*>& value = alist.alist_sequence (key);

	    for (unsigned int i = 0; i < value.size (); i++)
	      {
                const AttributeList empty;
		if (i > 0)
		  out << "\n" << std::string (indent, ' ');
		if (is_complex_object (*value[i], library))
		  {
		    out << "(";
		    print_object (*value[i], library, empty, indent + 1);
		    out << ")";
		  }
		else 
		  print_object (*value[i], library, empty, indent);
	      }
	  }
	  break;
	case Syntax::Library:
	case Syntax::Error:
	default:
	  out << "<" << Syntax::type_name (syntax.lookup (key)) 
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
PrinterFile::Implementation::print_alist (const AttributeList& alist, 
					  const Syntax& syntax,
					  const AttributeList& super_alist,
					  const Syntax& super_syntax,
					  int indent, bool skip)
{
  // Always print ordered items.
  const std::vector<std::string>& order = syntax.order ();
  bool complex_printing = false;
  for (unsigned int i = 0; i < order.size (); i++)
    {
      const std::string& key = order[i];
      if (!complex_printing && is_complex (alist, syntax, super_alist, key))
	complex_printing = true;
	
      if (!skip)
	skip = true;
      else if (complex_printing)
	out << "\n" << std::string (indent, ' ');
      else
	out << " ";

      if (alist.check (key))
	{
#if 0
	  // Design bug: We usually need to put parentheses around
	  // ordered complex values.  However, the parser doesn't
	  // expect these for alist sequences, so we don't print them
	  // either. 
	  if (syntax.lookup (key) == Syntax::AList
	      && syntax.size (key) != Syntax::Singleton)
	    print_entry (alist, syntax, super_alist, super_syntax, 
                         key, indent, false);
	  else
#endif
	    print_entry (alist, syntax, super_alist, super_syntax,
                         key, indent, true);
	}
      else if (!syntax.is_optional (key))
	out << "<missing " << key << ">";
    }

  // Print unordered items.
  std::vector<std::string> entries;
  syntax.entries (entries);

  // Print new declarations.
  std::vector<std::string> super_entries;
  super_syntax.entries (super_entries);
  std::set<std::string> super_set (super_entries.begin (),
                                   super_entries.end ());

  for (unsigned int i = 0; i < entries.size (); i++)
    {
      const std::string key = entries[i];
      
      // Skip already printed members.
      if (syntax.order (key) >= 0)
	continue;

      // Declare new members.
      if (super_set.find (key) == super_set.end ())
        {
          if (!skip)
            skip = true;
          else
            out << "\n" << std::string (indent, ' ');

          out << "(declare " << key << " ";

          const int size = syntax.size (key);
          if (size == Syntax::Singleton)
            /* do nothing */;
          else if (size == Syntax::Sequence)
            out << "[] ";
          else
            out << "[" << size << "] ";
          
          const Syntax::type type = syntax.lookup (key);
          switch (type)
            {
            case Syntax::Boolean:
            case Syntax::String:
            case Syntax::Integer:
              out << Syntax::type_name (type);
              break;
            case Syntax::Number:
              out << Syntax::type_name (type) << " ";
              print_dimension (alist, key, syntax.dimension (key));
              break;
            case Syntax::AList:
              {
                out << "fixed ";
                daisy_assert (size == Syntax::Singleton);
                daisy_assert (alist.check (key));
                const AttributeList& fixed = alist.alist (key);
                daisy_assert (fixed.check ("submodel"));
                out << fixed.name ("submodel");
              }
              break;
            case Syntax::Object:
              out << syntax.library (key).name ();
              break;
            case Syntax::PLF: 
            case Syntax::Library:
            case Syntax::Error:
            default:
              out << "<Error>";
            }
          out << "\n " << std::string (indent, ' ');
          print_string (syntax.description (key));
          out << ")";
        }

      // Skip subset members.
      if (alist.subset (super_alist, syntax, key))
	continue;

      if (!skip)
	skip = true;
      else
	out << "\n" << std::string (indent, ' ');

      // Now print it.
      out << "(" << key << " ";
      print_entry (alist, syntax, super_alist, super_syntax, key, 
		   indent + key.length () + 2, false);
      out << ")";
    }
}

void 
PrinterFile::Implementation::print_object (const AttributeList& value,
                                           const Library& library, 
                                           const AttributeList& original, 
                                           int indent)
{
  daisy_assert (value.check ("type"));
  const symbol element = value.identifier ("type");
  if (!library.check (element))
    {
      out << "<unknown " << element << ">";
      return;
    }
  const Syntax& element_syntax = library.syntax (element);
  const AttributeList& element_alist = library.lookup (element);

  // Check if we added something over the library.
  if (value.subset (element_alist, element_syntax))
    {
      // We didn't.
      print_symbol (element);
      return;
    }

  // Check original.
  if (original.check ("type") && original.identifier ("type") == element)
    {
      out << "original";
      // Check if we added something over the original.
      if (value.subset (original, element_syntax))
        return;
      out << " ";
      print_alist (value, element_syntax, original, element_syntax,
                   indent + 9,
                   false);
      return;

    }
  // Library element with additional attributes.
  print_symbol (element);
  out << " ";
  print_alist (value, element_syntax, element_alist, element_syntax,
               indent + 1 + element.name ().length ()
               // Buglet: Wrong indentation for elements with strange chars.
               + (is_identifier (element) ? 0 : 2),
               false);
}


void 
PrinterFile::Implementation
/**/::print_parameterization (const symbol library_name, const symbol name,
                              bool print_description)
{
  Library& library = Library::find (library_name);
  AttributeList alist (library.lookup (name));
  if (!print_description)
    alist.remove ("description");
  const AttributeList empty_alist;

  out << "(def" << library_name << " ";
  print_symbol (name);
  out << " ";
  if (alist.check ("type"))
    {
      const symbol super = alist.identifier ("type");
      print_symbol (super);
      if (!library.check (super))
	{
	  out << " ;; unknown superclass\n ";
	  print_alist (alist, library.syntax (name), 
		       empty_alist, library.syntax (name), 2, true);
	}
      else
	{
	  print_alist (alist, library.syntax (name), 
		       library.lookup (super), library.syntax (super),
                       2, true);
	}
    }
  else
    {
      out << "<unknown>\n  ";
      print_alist (alist, library.syntax (name), 
		   empty_alist, library.syntax (name), 2, true);
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
    Library::all (all);

    for (unsigned int i = 0; i < all.size (); i++)
      {
	const symbol library_name = all[i];
	Library& library = Library::find (library_name);
	std::vector<symbol> elements;
	library.entries (elements);
      
	for (unsigned int j = 0; j < elements.size (); j++)
	  {
	    const symbol element = elements[j];
	    const AttributeList& alist = library.lookup (element);

	    if (alist.check ("parsed_from_file") 
		&& alist.name ("parsed_from_file") == filename)
	      {
		found.push_back (FoundEntry (library_name, element, 
					     alist.integer
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

PrinterFile::Implementation::Implementation (const std::string& name)
  : output (new Path::Output (name)),
    out (output->stream ())
{ }

PrinterFile::Implementation::Implementation (std::ostream& stream)
  : output (NULL),
    out (stream)
{ }

PrinterFile::Implementation::~Implementation ()
{
  if (output)
    delete output;
}

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
PrinterFile::print_comment (const std::string& comment)
{
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
PrinterFile::print_alist (const AttributeList& alist, const Syntax& syntax, 
			  const AttributeList& super_alist, 
                          const Syntax& super_syntax) 
{ 
  impl->print_alist (alist, syntax, super_alist, super_syntax, 0, false);
  impl->out << "\n";
}

void 
PrinterFile::print_entry (const AttributeList& alist, const Syntax& syntax,
			  const std::string& key)
{ 
  if (alist.check (key))
    {
      const AttributeList empty_alist;
      impl->out << "(" << key << " ";
      const int indent = 2 + key.length ();
      impl->print_entry (alist, syntax, empty_alist, syntax,
                         key, indent, false);
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
PrinterFile::print_input (const AttributeList& alist)
{
  daisy_assert (alist.check ("type"));
  const symbol type = alist.identifier ("type");
  const Syntax& syntax = Librarian<Parser>::library ().syntax (type);

  impl->out << "(input " << type << " ";
  impl->print_alist (alist, syntax, AttributeList (), syntax, 7, false);
  impl->out << ")\n";
}

bool
PrinterFile::good ()
{ return impl->good (); }
  
static const AttributeList& 
get_file_alist ()
{
  static AttributeList alist;
  if (!alist.check ("type"))
    alist.add ("type", "file");
  return alist;
}
    
PrinterFile::PrinterFile (const std::string& filename)
  : Printer (get_file_alist ()),
    impl (new Implementation (filename))
{ }
    
static const AttributeList& 
get_stream_alist ()
{
  static AttributeList alist;
  if (!alist.check ("type"))
    alist.add ("type", "stream");
  return alist;
}
PrinterFile::PrinterFile (std::ostream& stream)
  : Printer (get_stream_alist ()),
    impl (new Implementation (stream))
{ }
    
PrinterFile::PrinterFile (Block& al)
  : Printer (al.alist ()),
    impl (new Implementation (al.name ("where")))
{ }
    
PrinterFile::~PrinterFile ()
{ }

static struct PrinterFileSyntax
{
  static Model& make (Block& al)
  { return *new PrinterFile (al); }

  PrinterFileSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "Print internal datastructures with lots of parentheses.");
    syntax.add ("where", Syntax::String, Syntax::Const,
                "File to print in.");
    syntax.order ("where");
    Librarian<Printer>::add_type ("file", alist, syntax, &make);
  }
} PrinterFile_syntax;
