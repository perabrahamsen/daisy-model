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
#include "plf.h"
#include "time.h"
#include "parser.h"
#include <fstream>
#include <algorithm>
#include <numeric>

struct PrinterFile::Implementation
{
  // Data.
  ofstream out;

  // String utilities.
  bool is_identifier (const string& name) const;
  void print_quoted_string (const string& name);

  // Print entry 'key' in alist.
  void print_entry (const AttributeList& alist, const Syntax&,
		    const AttributeList& super, const string& key,
		    int indent, bool need_wrapper);

  // Check if entry 'key' need a line for itself.
  bool is_complex (const AttributeList& alist, const Syntax& syntax,
		   const AttributeList& super, const string& key) const;
  bool is_complex_object (const AttributeList&, const Library&) const;

  // Print support for specific types.
  void print_string (const string& value); 
  void print_bool (bool); 
  void print_time (const Time&); 
  void print_plf (const PLF&, int indent); 
  void print_alist (const AttributeList& alist, const Syntax&,
		   const AttributeList& super, int indent, bool skip);
  void print_object (const AttributeList&, const Library& library, int indent);

  // Top level print functions.
  void print_library_file (const string& filename);
  
  // Testing.
  bool good ();

  // Creation.
  Implementation (const string& name);
};

bool 
PrinterFile::Implementation::is_complex (const AttributeList& alist, 
					 const Syntax& syntax,
					 const AttributeList& super,
					 const string& key) const
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
    case Syntax::Date:
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
      assert (false);
    } 
  return true;
}

bool 
PrinterFile::Implementation::is_complex_object (const AttributeList& value, 
						const Library& library) const
{
  assert (value.check ("type"));
  const string element = value.name ("type");
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
PrinterFile::Implementation::is_identifier (const string& name) const
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
PrinterFile::Implementation::print_quoted_string (const string& name)
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
PrinterFile::Implementation::print_entry (const AttributeList& alist, 
					  const Syntax& syntax,
					  const AttributeList& super, 
					  const string& key,
					  int indent, bool need_wrapper)
{ 
  assert (alist.check (key));
  Syntax::type type = syntax.lookup (key);

  const bool do_wrap 
    = (need_wrapper && is_complex (alist, syntax, super, key));
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
	  break;
	case Syntax::AList:
	  if (super.check (key))
	    print_alist (alist.alist (key), syntax.syntax (key), 
			 super.alist (key), indent, false); 
	  else
	    print_alist (alist.alist (key), syntax.syntax (key), 
			 syntax.default_alist (key), indent, false); 
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
	case Syntax::Date:
	  print_time (alist.time (key));
	  break;
	case Syntax::Integer:
	  out << alist.integer (key);
	  break;
	case Syntax::Object:
	  print_object (alist.alist (key), syntax.library (key), indent);
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
	    const vector<double>& value = alist.number_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		out << value[i]; 
	      }
	  }
	  break;
	case Syntax::AList:
	  {
	    const AttributeList& other = syntax.default_alist (key);
	    const Syntax& nested = syntax.syntax (key);
	    const vector<AttributeList*>& value = alist.alist_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << "\n" << string (indent, ' ');
		out << "(";
		print_alist (*value[i], nested, other, indent + 1, false); 
		out << ")";
	      }
	  }
	  break;
	case Syntax::PLF:
	  {
	    const vector<const PLF*>& value = alist.plf_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << "\n" << string (indent, ' ');
		out << "(";
		print_plf (*value[i], indent + 1);
		out << ")";
	      }
	  }
	  break;
	case Syntax::Boolean:
	  {
	    const vector<bool>& value = alist.flag_sequence (key);
	    
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
	    const vector<string>& value = alist.name_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << " ";
		print_string (value[i]);
	      }
	  }
	  break;
	case Syntax::Date:
	  {
	    const vector<const Time*>& value = alist.time_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << "\n" << string (indent, ' ');
		out << "(";
		print_time (*value[i]);
		out << ")";
	      }
	  }
	  break;
	case Syntax::Integer:
	  {
	    const vector<int>& value = alist.integer_sequence (key);
	    
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
	    const vector<AttributeList*>& value = alist.alist_sequence (key);

	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0)
		  out << "\n" << string (indent, ' ');
		if (is_complex_object (*value[i], library))
		  {
		    out << "(";
		    print_object (*value[i], library, indent + 1);
		    out << ")";
		  }
		else 
		  print_object (*value[i], library, indent);
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
PrinterFile::Implementation::print_string (const string& value) 
{ 
  if (is_identifier (value))
    out << value; 
  else
    print_quoted_string (value);
}

void 
PrinterFile::Implementation::print_bool (bool value) 
{ 
  if (value)
    out << "true";
  else
    out << "false";
}

void 
PrinterFile::Implementation::print_time (const Time& value) 
{ 
  out << value.year () << " " << value.month () << " " 
      << value.mday () << " " << value.hour (); 
}

void 
PrinterFile::Implementation::print_plf (const PLF& plf, int indent) 
{ 
  for (unsigned int i = 0; i < plf.size (); i++)
    {
      if (i > 0)
	out << "\n" << string (indent, ' ');
      out << "(" << plf.x (i) << " " << plf.y (i) << ")";
    }
}

void 
PrinterFile::Implementation::print_alist (const AttributeList& alist, 
					  const Syntax& syntax,
					  const AttributeList& super,
					  int indent, bool skip)
{
  // Always print ordered items.
  const vector<string>& order = syntax.order ();
  bool complex_printing = false;
  for (unsigned int i = 0; i < order.size (); i++)
    {
      const string& key = order[i];
      if (!complex_printing && is_complex (alist, syntax, super, key))
	complex_printing = true;
	
      if (!skip)
	skip = true;
      else if (complex_printing)
	out << "\n" << string (indent, ' ');
      else
	out << " ";

      if (alist.check (key))
	{
	  // Design bug: We usually need to put parentheses around
	  // ordered complex values.  However, the parser doesn't
	  // expect these for alist sequences, so we don't print them
	  // either. 
	  if (syntax.lookup (key) == Syntax::AList
	      && syntax.size (key) != Syntax::Singleton)
	    print_entry (alist, syntax, super, key, indent, false);
	  else
	    print_entry (alist, syntax, super, key, indent, true);
	}
      else if (!syntax.is_optional (key))
	out << " <missing " << key << ">";
    }


  // Print unordered items.
  vector<string> entries;
  syntax.entries (entries);

  {for (unsigned int i = 0; i < entries.size (); i++)
    {
      const string key = entries[i];
      
      // Skip already printed members.
      if (syntax.order (key) >= 0)
	continue;

      // Skip subset members.
      if (alist.subset (super, syntax, key))
	continue;

      if (!skip)
	skip = true;
      else
	out << "\n" << string (indent, ' ');

      // Now print it.
      out << "(" << key << " ";
      print_entry (alist, syntax, super, key, 
		   indent + key.length () + 2, false);
      out << ")";
  }}
}

void 
PrinterFile::Implementation::print_object (const AttributeList& value,
					   const Library& library, int indent)
{
  assert (value.check ("type"));
  const string element = value.name ("type");
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
      print_string (element);
      return;
    }

  // Library element with additional attributes.
  print_string (element);
  out << " ";
  print_alist (value, element_syntax, element_alist, 
	       indent + 1 + element.length ()
	       // Buglet: Wrong indentation for elements with strange chars.
	       + (is_identifier (element) ? 0 : 2),
	       false);
}

// We store all matching entries here.
struct FoundEntry
{
  string library_name;
  string element;
  int sequence;

  bool operator < (const FoundEntry& e) const
    { return sequence < e.sequence; }

  FoundEntry ()
    { }
  FoundEntry (const string& l, const string& e, const int s)
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
PrinterFile::Implementation::print_library_file (const string& filename)
{
  vector<FoundEntry> found;
  
  // Search all the libraries for matching entries.
  {
    vector<string> all;
    Library::all (all);

    for (unsigned int i = 0; i < all.size (); i++)
      {
	const string& library_name = all[i];
	Library& library = Library::find (library_name);
	vector<string> elements;
	library.entries (elements);
      
	for (unsigned int j = 0; j < elements.size (); j++)
	  {
	    const string element = elements[j];
	    const AttributeList& alist = library.lookup (element);

	    if (alist.check ("parsed_from_file") 
		&& alist.name ("parsed_from_file") == filename)
	      {
		found.push_back (FoundEntry (library_name, element, 
					     alist.integer ("parsed_sequence")));
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
      const string library_name = found[i].library_name;
      const string name = found[i].element;
      Library& library = Library::find (library_name);
      const AttributeList& alist = library.lookup (name);

      if (first)
	first = false;
      else
	out << "\n";
      out << "(def" << library_name << " ";
      print_string (name);
      out << " ";
      if (alist.check ("type"))
	{
	  const string super = alist.name ("type");
	  print_string (super);
	  if (!library.check (super))
	    {
	      out << " ;; unknown superclass\n ";
	      print_alist (alist, library.syntax (name), 
			   empty_alist, 2, true);
	    }
	  else
	    {
	      print_alist (alist, library.syntax (name), 
			   library.lookup (super), 2, true);
	    }
	}
      else
	{
	  out << "<unknown>\n  ";
	  print_alist (alist, library.syntax (name), 
		       empty_alist, 2, true);
	}
      out << ")\n";
    }
}

bool
PrinterFile::Implementation::good ()
{ return out.good (); }

PrinterFile::Implementation::Implementation (const string& name)
#ifdef BORLAND_PERMISSIONS
  : out (name.c_str (), ios::out|ios::trunc, 0666)
#else
  : out (name.c_str ())
#endif
{ }

void
PrinterFile::print_comment (const string& comment)
{
  vector<string> text;

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
    impl.out << ";; " << text[i] << "\n";
}

void 
PrinterFile::print_alist (const AttributeList& alist, const Syntax& syntax)
{ 
  const AttributeList empty_alist;
  impl.print_alist (alist, syntax, empty_alist, 0, false);
  impl.out << "\n";
}

void 
PrinterFile::print_entry (const AttributeList& alist, const Syntax& syntax,
			  const string& key)
{ 
  if (alist.check (key))
    {
      const AttributeList empty_alist;
      impl.out << "(" << key << " ";
      impl.print_entry (alist, syntax, empty_alist, key, 0, false);
      impl.out << ")\n";
    }
}

void
PrinterFile::print_library_file (const string& filename)
{
  impl.print_library_file (filename);
}

void
PrinterFile::print_input (const AttributeList& alist)
{
  assert (alist.check ("type"));
  const string type = alist.name ("type");
  const Syntax& syntax = Librarian<Parser>::library ().syntax (type);

  impl.out << "(input " << type << " ";
  impl.print_alist (alist, syntax, AttributeList (), 7, false);
  impl.out << ")\n";
}

bool
PrinterFile::good ()
{ return impl.good (); }
  
PrinterFile::PrinterFile (const string& filename)
  : Printer ("file"),
    impl (*new Implementation (filename))
{ }
    
PrinterFile::PrinterFile (const AttributeList& al)
  : Printer (al.name ("type")),
    impl (*new Implementation (al.name ("where")))
{ }
    
PrinterFile::~PrinterFile ()
{ delete &impl; }

static struct PrinterFileSyntax
{
  static Printer& make (const AttributeList& al)
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
