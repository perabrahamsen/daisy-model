// printer_file.C -- Print alist in ParserFile format.

#include "printer_file.h"
#include "csmp.h"
#include "time.h"
#include "options.h"
#include <fstream.h>
#include <algorithm>
#include <numeric>

struct PrinterFile::Implementation
{
  // Data.
  ofstream out;

  // String utilities.
  bool is_identifier (const string& name) const;
  void print_quoted_string (const string& name);

  // Print entry `key' in alist.
  void print_entry (const AttributeList& alist, const Syntax&,
		    const AttributeList& super, const string& key,
		    int indent, bool need_wrapper);

  // Check if entry `key' need a line for itself.
  bool is_complex (const AttributeList& alist, const Syntax& syntax,
		   const AttributeList& super, const string& key) const;
  bool is_complex_object (const AttributeList&, const Library&) const;

  // Print support for specific types.
  void print_string (const string& value); 
  void print_bool (bool); 
  void print_time (const Time&); 
  void print_csmp (const CSMP&, int indent); 
  void print_alist (const AttributeList& alist, const Syntax&,
		   const AttributeList& super, int indent, bool skip);
  void print_object (const AttributeList&, const Library& library, int indent);

  // Top level print functions.
  void print_library_file (const string& filename);
  
  // Testing.
  bool good () const;

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

  // Sequences are always complex.
  if (syntax.size (key) != Syntax::Singleton)
    return true;

  switch (syntax.lookup (key))
    {
    case Syntax::Number:
    case Syntax::Integer:
    case Syntax::Boolean:
    case Syntax::String:
    case Syntax::Date:
      return false;
    case Syntax::Object:
      return is_complex_object (alist.alist (key), syntax.library (key));
    case Syntax::AList:
    case Syntax::CSMP:
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
  if (c != '_' && c != '-' && !isalpha (c))
    return false;
      
  for (unsigned int i = 1; i < name.size (); i++)
    {
      const char c = name[i];
      if (c != '_' && c != '-' && !isalnum (c))
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
			 AttributeList::empty, indent, false); 
	  break;
	case Syntax::CSMP:
	  print_csmp (alist.csmp (key), indent);
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
	    // Kludge: An _alist_ value in the superclass where an
	    // _alist_sequence_ is expected works as a default for each of
	    // the sequence members in the derived object.
	    const AttributeList& other =
	      (super.check (key) && super.size (key) == Syntax::Singleton)
	      ? super.alist (key) : AttributeList::empty;
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
	case Syntax::CSMP:
	  {
	    const vector<const CSMP*>& value = alist.csmp_sequence (key);
	    
	    for (unsigned int i = 0; i < value.size (); i++)
	      {
		if (i > 0) 
		  out << "\n" << string (indent, ' ');
		out << "(";
		print_csmp (*value[i], indent + 1);
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
PrinterFile::Implementation::print_csmp (const CSMP& csmp, int indent) 
{ 
  for (unsigned int i = 0; i < csmp.size (); i++)
    {
      if (i > 0)
	out << "\n" << string (indent, ' ');
      out << "(" << csmp.x (i) << " " << csmp.y (i) << ")";
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
	  // expect these for alist sequences, so we don'tprint them
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

  for (unsigned int i = 0; i < entries.size (); i++)
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
    }
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
  out << element << " ";
  print_alist (value, element_syntax, element_alist, 
	       indent + 1 + element.length (), false);
}

// We store all matching entries here.
struct Entry
{
  string library_name;
  string element;
  int sequence;

  operator < (const Entry& e) const
    { return sequence < e.sequence; }

  Entry ()
    { }
  Entry (const string& l, const string& e, const int s)
    : library_name (l),
      element (e),
      sequence (s)
    { }
  Entry (const Entry& e)
    : library_name (e.library_name),
      element (e.element),
      sequence (e.sequence)
    { }
};

void
PrinterFile::Implementation::print_library_file (const string& filename)
{
  vector<Entry> entries;
  
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
		assert (alist.check ("parsed_sequence"));
		entries.push_back (Entry (library_name, element, 
					  alist.integer ("parsed_sequence")));
	      }
	  }
      }
  }
  // Sort the entries.
  sort (entries.begin (), entries.end ());

  // Print the entries.
  bool first = true;

  for (unsigned int i = 0; i < entries.size (); i++)
    {
      const string library_name = entries[i].library_name;
      const string name = entries[i].element;
      Library& library = Library::find (library_name);
      const AttributeList& alist = library.lookup (name);

      if (first)
	first = false;
      else
	out << "\n";
      out << "(def" << library_name << " " << name << " ";
      if (alist.check ("type"))
	{
	  const string super = alist.name ("type");
	  out << super;
	  if (!library.check (super))
	    {
	      out << " ;; unknown superclass\n ";
	      print_alist (alist, library.syntax (name), 
			   AttributeList::empty, 2, true);
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
		       AttributeList::empty, 2, true);
	}
      out << ")\n";
    }
}

bool
PrinterFile::Implementation::good () const
{ return out.good (); }

PrinterFile::Implementation::Implementation (const string& name)
  : out (name.c_str ())
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
  impl.print_alist (alist, syntax, AttributeList::empty, 0, false);
  impl.out << "\n";
}

void
PrinterFile::print_library_file (const string& filename)
{
  impl.print_library_file (filename);
}

bool
PrinterFile::good () const
{ return impl.good (); }
  
PrinterFile::PrinterFile (const string& filename)
  : Printer (filename),
    impl (*new Implementation (filename))
{ }
    
PrinterFile::~PrinterFile ()
{ }
