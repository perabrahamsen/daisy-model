// parser_file.C
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


#include "parser_file.h"
#include "lexer.h"
#include "plf.h"
#include "time.h"
#include "tmpstream.h"
#include "treelog_stream.h"
#include "path.h"
#include "units.h"
#include "mathlib.h"

#include <set>
using namespace std;

struct ParserFile::Implementation
{
  // Inputs.
  vector<AttributeList*> inputs;

  // Lexer.
  string file;
  Lexer* lexer;
  Treelog::Open* nest;

  int get ()
  { return lexer->get (); }
  int peek ()
  { return lexer->peek (); }
  bool good ()
  { return lexer->good (); }
  void error (const string& str)
  { lexer->error (str); }
  void error (const string& str, const Lexer::Position& pos)
  { lexer->error (str, pos); }
  void warning (const string& str)
  { lexer->warning (str); }
  void eof ()
  { lexer->eof (); }

  // Lisp lexer.
  string get_string ();
  symbol get_symbol ()
  { return symbol (get_string ()); }
  int get_integer ();
  double get_number ();
  string get_dimension ();
  double get_number (const string& dim);
  bool check_dimension (const string& syntax, const string& read);
  double convert (double value, const string& syntax, const string& read, 
		  const Lexer::Position&);
  void skip (const char*);
  void skip ();
  void skip_to_end ();
  void skip_token ();
  bool looking_at (char);

  // Parser.
  void add_derived (Library&);
  AttributeList& load_derived (const Library& lib, bool in_sequence,
			       const AttributeList* original);
  void load_list (AttributeList&, const Syntax&);
  Time get_time ();
  const Syntax* global_syntax_table;

  // Create and destroy.
  void initialize (const Syntax&, Treelog&);
  Implementation (const string&);
  ~Implementation ();
};

string
ParserFile::Implementation::get_string ()
{
  skip ();
  int c = peek ();
  
  if (c == '"')
    {
      // Get a string.
      string str ("");
      skip ("\"");

      for (c = get (); good () && c != '"'; c = get ())
	{
	  if (c == '\\')
	    {
	      c = get ();
	      switch (c)
		{
		case 'n':
		  c = '\n';
		  break;
		case '\n':
		  c = get ();
		  break;
		case '\\':
		case '"':
		  break;
		default:
		  error (string ("Unknown character escape '")
			 + char (c) + "'");
		}
	    }
	  str += int2char (c);
	}
      return str;
    }
  else if (c != '_' && c != '-' && !isalpha (c))
    {
      error ("Identifier or string expected");
      skip_to_end ();
      return "error";
    }
  else
    {
      // Get an identifier.
      string str ("");
      do
	{
	  str += char (c);
	  get ();
	  c = peek ();
	}
      while (good() && (c == '_' || c == '-' || isalnum (c)));
    
      return str;
    }
}

int
ParserFile::Implementation::get_integer ()
{
  skip ();
  string str;
  int c = peek ();

  while (good () && (isdigit (c) || c == '-' || c == '+'))
    {
      str += int2char (c);
      get ();
      c = peek ();
    }
  // Empty number?
  if (str.size () < 1U)
    {
      error ("Integer expected");
      skip_to_end ();
      return -42;
    }
  return atoi (str.c_str ());
}

double
ParserFile::Implementation::get_number ()
{
  skip ();
  string str;
  int c = peek ();

  while (good () && (isdigit (c) 
		     || c == '.' || c == '-' || c == '+' 
		     || c == 'e' || c == 'E'))
    {
      str += int2char (c);
      get ();
      c = peek ();
    }
  // Empty number?
  if (str.size () < 1U)
    {
      error ("Number expected");
      skip_to_end ();
      return -42.42e42;
    }
  const char *c_str = str.c_str ();
  const char *endptr = c_str;
  const double value = strtod (c_str, const_cast<char**> (&endptr));
  
  if (*endptr != '\0')
    error (string ("Junk at end of number '") + endptr + "'");

  return value;
}

string
ParserFile::Implementation::get_dimension ()
{
  skip ("[");
  string str;
  int c = peek ();

  while (good () && c != ']' && c != '\n')
    {
      str += int2char (c);
      get ();
      c = peek ();
    }
  skip ("]");
  return str;
}

double
ParserFile::Implementation::get_number (const string& syntax_dim)
{
  double value = get_number ();
  Lexer::Position pos = lexer->position ();

  if (looking_at ('['))
    {
      const string read_dim = get_dimension ();
      if (check_dimension (syntax_dim, read_dim))
	value = convert (value, syntax_dim, read_dim, pos);
    }
  return value;
}

bool 
ParserFile::Implementation::check_dimension (const string& syntax,
					     const string& read)
{
  if (syntax != read)
    {
      if (syntax == Syntax::Unknown ())
	{
	  if (read.length () == 0 || read[0] != '?')
	    warning ("you must use [?<dim>] for entries with unknown syntax");
	}
      else if (syntax == Syntax::Fraction () && read == "%")
	return true;
      else if (syntax == Syntax::None ()
	  || syntax == Syntax::Fraction ())
	{
	  if (read != "")
	    {
	      error (string ("expected [] got [") + read + "]");
	      return false;
	    }
	}
      else if (!Units::can_convert (read, syntax))
	{
	  error (string ("expected [") + syntax + "] got ["
		 + read + "]");
	  return false; 
	}
    }
  return true;
}

double 
ParserFile::Implementation::convert (double value,
				     const string& syntax, 
				     const string& read, 
				     const Lexer::Position& pos)
{ 
  if (syntax == Syntax::Unknown ())
    return value; 
  if (syntax == read)
    return value;
  if (syntax == Syntax::Fraction () && read == "%")
    return value * 0.01;
  
  try
    {
      if (syntax == Syntax::None () || syntax == Syntax::Fraction ())
	if (read == "")
	  return value;
	else
	  return Units::convert (read, "", value);
      return Units::convert (read, syntax, value);
    }
  catch (const string& message)
    { 
      error (message, pos); 
      return value;
    }
}

void
ParserFile::Implementation::skip (const char* str)
{ 
  skip ();
  for (const char* p = str; *p; p++)
    if (*p != peek ())
      {
	error (string("Expected '") + str + "'");
	skip_token ();
	break;
      }
    else
      get ();
}

void
ParserFile::Implementation::skip ()
{ 
  while (true)
    if (!good ())
      return;
    else if (isspace (peek ()))
      get ();
    else if (peek () == ';')
      while (good () && get () != '\n')
      ;
    else
      return;
}

void
ParserFile::Implementation::skip_token () {
  if (peek () == ';' || isspace (peek ()))
    skip ();
  if (peek () == '"')
    get_string ();
  else if (peek () == '.' || isdigit (peek ()))
    get_number ();
  else if (peek () == '(') 
    {
      get ();
      skip_to_end ();
      skip (")");
    }
  else if (isalnum (peek ()) || peek () == '_' || peek () == '-')
    get_string ();
  else if (peek () == '[')
    get_dimension ();
  else
    get ();
}

void
ParserFile::Implementation::skip_to_end ()
{
  while (peek () != ')' && good ())
    {
      skip_token ();
      skip ();
    }
}

bool
ParserFile::Implementation::looking_at (char c)
{ 
  skip ();
  return peek () == c;
}

void
ParserFile::Implementation::add_derived (Library& lib)
{
  // Get the name of the class and the existing superclass to derive from.
  const symbol name = get_symbol ();
  // Check for duplicates.
  if (lib.check (name))
    {
      const AttributeList& old = lib.lookup (name);
      if (old.check ("parsed_from_file"))
	warning (name + " is already defined in " 
		 + old.identifier ("parsed_from_file") + ", overwriting");
      else
	warning (name + " is already defined, overwriting");
      lib.remove (name);
    }
  const symbol super = get_symbol ();
  if (!lib.check (super))
    {
      error (string ("Unknown '") + lib.name () + "' model '" + super + "'");
      skip_to_end ();
      return;
    }
  const Syntax& syntax = lib.syntax (super);
  // Create new attribute derived from its superclass.
  const AttributeList& sl = lib.lookup (super);
  AttributeList& atts = *new AttributeList (sl);
  // Remember where we got this object.
  atts.add ("parsed_from_file", file);
  atts.add ("parsed_sequence", Library::get_sequence ());
  // Doc string.
  daisy_assert (!syntax.ordered () 
                || syntax.order ().begin () != syntax.order ().end ());
  if ((!syntax.ordered () 
       || syntax.lookup (*(syntax.order ().begin ())) != Syntax::String) 
      && looking_at ('"'))
    atts.add ("description", get_string ());
  // Add separate attributes for this object.
  load_list (atts, syntax);
  // Add new object to library.
  lib.add_derived (name, atts, super);
}

AttributeList&
ParserFile::Implementation::load_derived (const Library& lib, bool in_sequence,
					  const AttributeList *const original)
{
  AttributeList* alist;
  bool skipped = false;
  if (looking_at ('('))
    {
      skip ("(");
      skipped = true;
    }
  symbol type = get_symbol ();
  try
    {
      static const symbol original_symbol ("original");
      if (type == original_symbol)
	{
	  if (!original)
	    throw (string ("No original value"));
	  alist = new AttributeList (*original);
	  daisy_assert (alist->check ("type"));
	  type = alist->identifier ("type");
	  daisy_assert (lib.check (type));
	}
      else
	{ 
	  if (!lib.check (type))
	    throw (string ("Unknown '") + lib.name () + "' model '"
		   + type + "'");
	  alist = new AttributeList (lib.lookup (type));
	  alist->add ("type", type);
	}
      if (skipped || !in_sequence)
	load_list (*alist, lib.syntax (type));
    }
  catch (const string& msg)
    {
      error (msg);
      skip_to_end ();
      alist = new AttributeList ();
      alist->add ("type", "error");
    }
  if (skipped)
    skip (")");
  daisy_assert (alist != NULL);
  return *alist;
}

void
ParserFile::Implementation::load_list (AttributeList& atts,
				       const Syntax& syntax)
{ 
  vector<string>::const_iterator current = syntax.order ().begin ();
  const vector<string>::const_iterator end = syntax.order ().end ();
  set<string> found;

  while ( good () && !looking_at (')'))
    {
      bool skipped = false;
      bool in_order = false;
      string name = "";
      if (current == end)
	// Unordered association list, get name.
	{
	  skip ("(");
	  skipped = true;
	  name = get_string ();
	}
      else
	// Ordered tupple, name know.
	{
	  in_order = true;
	  name = *current;
	  current++;
	}

      // Duplicate warning.
      if (found.find (name) != found.end ())
	warning (name + " specified twice, last takes precedence");
      else if (syntax.lookup (name) != Syntax::Library // (deffoo ...)
	       && (syntax.lookup (name) != Syntax::Object
		   || (&syntax.library (name) // (input file ...)
		       != &Librarian<Parser>::library ())))
	found.insert (name);

      if (syntax.size (name) == Syntax::Singleton)
	switch (syntax.lookup (name))
	  {
	  case Syntax::Number:
	    {
	      double value = get_number (syntax.dimension (name));

	      try
		{
		  syntax.check (name, value);
		}
	      catch (const string& message)
		{
		  error (name + ": " + message);
		}
	      atts.add (name, value);
	      break;
	    }
	  case Syntax::AList: 
	    {
	      bool alist_skipped = false;
	      if (in_order)
		{
		  // Last elelement of a total order does not need '('.
		  if (looking_at ('(') 
		      || current != end 
		      || !syntax.total_order ())
		    {
		      alist_skipped = true;
		      skip ("(");
		    }
		}
	      AttributeList list (atts.check (name) 
				  ? atts.alist (name)
				  : syntax.default_alist (name));
	      
	      load_list (list, syntax.syntax (name));
	      atts.add (name, list);
	      if (alist_skipped)
		skip (")");
	      break;
	    }
	  case Syntax::PLF:
	    {
	      if (in_order)
		skip ("(");
	      PLF plf;
	      double last_x = -42;
	      int count = 0;
	      const string domain = syntax.domain (name);
	      const string range = syntax.range (name);
	      bool ok = true;
	      while (!looking_at (')') && good ())
		{
		  skip ("(");
		  double x = get_number (domain);
		  {
		    if (count > 0 && x <= last_x)
		      {
			error ("Non increasing x value");
			ok = false;
		      }
		    last_x = x;
		    count++;
		  }
		  const double y = get_number (range);
		  try
		    {
		      syntax.check (name, y);
		    }
		  catch (const string& message)
		    {
		      error (name + ": " + message);
		      ok = false;
		    }
		  skip (")");
		  if (ok)
		    plf.add (x, y);
		}
	      if (count < 2)
		{
		  error ("Need at least 2 points");
		  ok = false;
		}
	      if (ok)
		atts.add (name, plf);
	      if (in_order)
		skip (")");
	      break;
	    }
	  case Syntax::String:
	    atts.add (name, get_string ());
	    // Handle "directory" immediately.
	    if (&syntax == global_syntax_table && name == "directory")
	      if (!Path::set_directory (atts.name (name)))
		error (string ("Could not set directory '") + atts.name (name)
		       + "'");
	    break;
	  case Syntax::Boolean:
	    {
	      const string flag = get_string ();

	      if (flag == "true")
		atts.add (name, true);
	      else
		{
		  atts.add (name, false);
		  if (flag != "false")
		    error ("Expected 'true' or 'false'");
		}
	      break;
	    }
	  case Syntax::Integer:
	    atts.add (name, get_integer ());
	    break;
	  case Syntax::Library:
	    // Handled specially: Put directly in global library.
	    add_derived (syntax.library (name));
	    break;
	  case Syntax::Object:
	    {
	      const Library& lib = syntax.library (name);
#ifdef SLOPPY_PARENTHESES
	      AttributeList& al = (atts.check (name) 
				   ? load_derived (lib, current != end,
						   &atts.alist (name))
				   : load_derived (lib, current != end, NULL));
#else // !SLOPPY_PARENTHESES
	      AttributeList& al = (atts.check (name) 
				   ? load_derived (lib, in_order, 
						   &atts.alist (name))
				   : load_derived (lib, in_order, NULL));
#endif // !SLOPPY_PARENTHESES						   
	      if (&lib == &Librarian<Parser>::library ())
		{
		  Parser& parser = Librarian<Parser>::create (al);
		  parser.initialize (*global_syntax_table, lexer->err);
		  parser.load_nested (atts);
		  lexer->error_count += parser.error_count ();
		  delete &parser;
		  inputs.push_back (&al);
		}
	      else
		{
		  const string obj = al.name ("type");
		  if (obj == "error")
		    break;
#ifdef REQUIRE_COMPLETE_OBJECTS
		  // We can only use complete objects as attribute
		  // values.
		  // NO LONGER TRUE with the "original" type.
		  TmpStream tmp;
		  TreelogStream treelog (tmp ());
		  Treelog::Open nest (treelog, obj);
		  if (!lib.syntax (obj).check (al, treelog))
		    error (string ("Error for member '") + obj 
			   + "' in library '" + name + "'\n--- details:\n"
			   + tmp.str () + "---");
		  else if (treelog.count)
		    warning (string ("Warning for member '") + obj 
			     + "' in library '" + name + "'\n--- details:\n"
			     + tmp.str () + "---");
#endif // REQUIRE_COMPLETE_OBJECTS
		  atts.add (name, al);
		  delete &al;
		}
	    }
	    break;
	  case Syntax::Error:
	    error (string("Unknown singleton '") + name + "'");
	    skip_to_end ();
	    break;
	  default:
	    daisy_assert (false);
	  }
      else
	{
	  // If this is part of an order, expect parentheses arund the
	  // list, EXCEPT when there cannot possible be any more
	  // elements after this one.  I.e. when the element is the
	  // last of a totally ordered sequence.
	  if (!in_order)
	    // Unordered, we already skipped this one
	    daisy_assert (skipped);
	  else if (current != end || !syntax.total_order ())
	    // This is not the last element or the order is not total.
	    {
	      daisy_assert (!skipped);
	      skip ("(");
	      skipped = true;
	    }

	  // Support for sequences not really finished yet.
	  switch (syntax.lookup (name))
	    {
	    case Syntax::Object:
	      {
		// We don't support fixed sized object arrays yet.
		daisy_assert (syntax.size (name) == Syntax::Sequence);
		const Library& lib = syntax.library (name);
		static const vector<AttributeList*> no_sequence;
		vector<AttributeList*> sequence;
		const vector<AttributeList*>& old_sequence
		  = atts.check (name) 
		  ? atts.alist_sequence (name) 
		  : no_sequence;
		while (!looking_at (')') && good ())
		  {
		    const int element = sequence.size ();
		    AttributeList& al 
		      = (old_sequence.size () > element
			 ? load_derived (lib, true, old_sequence[element])
			 : load_derived (lib, true, NULL));
		    const string obj = al.name ("type");
		    if (obj != "error")
		      {
#ifdef REQUIRE_COMPLETE_OBJECTS
			// We can only use complete objects as attribute
			// values.
			TmpStream tmp;
			TreelogStream treelog (tmp ());
			Treelog::Open nest (treelog, obj);
			const bool ok = lib.syntax (obj).check (al, treelog);
			if (!ok)
			  // Maybe don't put it on the list?
			  error (string ("Error for member '") + obj 
				 + "' in library '" + name
				 + "'\n--- details:\n" + tmp.str () + "---");
			else if (strlen (tmp.str ()) > 0)
			  warning (string ("Warning for member '") + obj 
				   + "' in library '" + name
				   + "'\n--- details:\n" + tmp.str () + "---");
#endif // REQUIRE_COMPLETE_OBJECTS
			sequence.push_back (&al);
		      }
		  }
		atts.add (name, sequence);
		sequence_delete (sequence.begin (), sequence.end ());
		break;
	      }
	    case Syntax::AList:
	      {
		const int size = syntax.size (name);
		static const vector<AttributeList*> no_sequence;
		const Syntax& syn = syntax.syntax (name);
		const vector<AttributeList*>& old_sequence
		  = atts.check (name) 
		  ? atts.alist_sequence (name) 
		  : no_sequence;
		vector<AttributeList*> sequence;
		bool skipped = false;
		// We do not force parentheses around the alist if it
		// is the last member of a fully ordered list.
		if (in_order && (current != end || !syntax.total_order ()))
		  // in order and (not the last or unordered may follow)
		  {
		    daisy_assert (!skipped);
		    skip ("(");
		    skipped = true;
		  }
		while (!looking_at (')') && good ())
		  {
		    skip ("(");
		    const int element = sequence.size ();
		    AttributeList& al
		      = *new AttributeList (old_sequence.size () > element
					    ? *old_sequence[element]
					    : syntax.default_alist (name));
		    load_list (al, syn);
		    sequence.push_back (&al);
		    skip (")");
		  }
		if (skipped)
		  skip (")");
		if (size != Syntax::Sequence 
		    && sequence.size () != size)
		  {
		    TmpStream str;
		    str () << "Got " << sequence.size ()
			   << " array members, expected " << size;
		    error (str.str ());
		  }
		atts.add (name, sequence);
		sequence_delete (sequence.begin (), sequence.end ());
		break;
	      }
	    case Syntax::PLF:
	      {
		vector<const PLF*> plfs;
		int total = 0;
		const int size = syntax.size (name);
		while (good () && !looking_at (')'))
		  {
		    skip ("(");
		    PLF& plf = *new PLF ();
		    double last_x = -42;
		    int count = 0;
		    const string domain = syntax.domain (name);
		    const string range = syntax.range (name);
		    while (!looking_at (')') && good ())
		      {
			skip ("(");
			double x = get_number (domain);
			{
			  if (count > 0 && x <= last_x)
			    error ("Non increasing x value");
			  last_x = x;
			  count++;
			}
			double y = get_number (range);
			try
			  {
			    syntax.check (name, y);
			  }
			catch (const string& message)
			  {
			    error (name + ": " + message);
			  }
			skip (")");
			plf.add (x, y);
		      }
		    if (count < 2)
		      error ("Need at least 2 points");
		    total++;
		    skip (")");
		    plfs.push_back (&plf);
		  }
		if (size != Syntax::Sequence && total != size)
		  {
		    TmpStream str;
		    str () << "Got " << total
			   << " array members, expected " << size;
		    error (str.str ());

		    for (;total < size; total++)
		      plfs.push_back (new PLF ());
		  }
		atts.add (name, plfs);
		sequence_delete (plfs.begin (), plfs.end ());
		break;
	      }
	    case Syntax::Number:
	      {
		vector<double> array;
		vector<Lexer::Position> positions;
		int count = 0;
		const int size = syntax.size (name);
		const string syntax_dim = syntax.dimension (name);
		unsigned int first_unchecked = 0;
		while (good () && !looking_at (')'))
		  {
		    if (looking_at ('*'))
		      {
			skip ("*");
			const int same = get_integer ();
			if (array.size () == 0)
			  error ("must specify number before '*'");
			else
			  {
			    // Append same - 1 copies of last value.
			    for (int i = 1; i < same; i++)
			      {
				array.push_back (array.back ());
				positions.push_back (Lexer::no_position ());
				count++;
			      }
			  }
		      }
		    else
		      {
			array.push_back (get_number ());
			positions.push_back (lexer->position ());
			count++;
			
			if (looking_at ('['))
			  {
			    const string read_dim = get_dimension ();
			    if (check_dimension (syntax_dim, read_dim))
			      {
				daisy_assert (positions.size () == array.size ());
				for (unsigned int i = first_unchecked;
				     i < array.size ();
				     i++)
				  {
				    array[i] = convert (array[i],
							syntax_dim, read_dim, 
							positions[i]);
				  }
				first_unchecked = array.size ();
			      }
			  }
		      }
		  }
		daisy_assert (positions.size () == array.size ());
		for (unsigned int i = 0; i < array.size (); i++)
		  {
		    if (positions[i] != Lexer::no_position ())
		      try
			{
			  syntax.check (name, array[i]);
			}
		      catch (const string& message)
			{
			  TmpStream str;
			  str () << name << "[" << i << "]: " << message;
			  error (str.str (), positions[i]);
			}
		  }
		if (size != Syntax::Sequence && count != size)
		  {
		    TmpStream str;
		    str () << "Got " << count 
			   << " array members, expected " << size;
		    error (str.str ());

		    for (;count < size; count++)
		      array.push_back (-1.0);
		  }
		atts.add (name, array);
		break;
	      }
	    case Syntax::String:
	      {
		vector<symbol> array;
		int count = 0;
		const int size = syntax.size (name);

		while (!looking_at (')') && good ())
		  {
		    array.push_back (get_symbol ());
		    count++;
		  }
		if (size != Syntax::Sequence && count != size)
		  {
		    TmpStream str;
		    str () << "Got " << count 
			   << " array members, expected " << size;
		    error (str.str ());

		    for (;count < size; count++)
		      array.push_back (symbol ("<error>"));
		  }
		atts.add (name, array);
		// Handle "path" immediately.
		if (&syntax == global_syntax_table && name == "path")
		  {
		    const vector<symbol>& symbols 
		      = atts.identifier_sequence (name);
		    vector<string> names;
		    for (unsigned int i = 0; i < symbols.size (); i++)
		      names.push_back (symbols[i].name ());
		    Path::set_path (names);
		  }
		break;
	      }
	    case Syntax::Integer:
	      {
		vector<int> array;
		int count = 0;
		const int size = syntax.size (name);

		while (!looking_at (')') && good ())
		  {
		    array.push_back (get_integer ());
		    count++;
		  }
		if (size != Syntax::Sequence && count != size)
		  {
		    TmpStream str;
		    str () << "Got " << count 
			   << " array members, expected " << size;
		    error (str.str ());

		    for (;count < size; count++)
		      array.push_back (-42);
		  }
		atts.add (name, array);
		// Handle "path" immediately.
		break;
	      }
	    case Syntax::Error:
	      error (string("Unknown attribute '") + name + "'");
	      skip_to_end ();
	      break;
	    default:
	      error (string("Unsupported sequence '") + name + "'");
	      skip_to_end ();
	    }
	}

      // Value check.
      if (atts.check (name))
	{
	  TmpStream tmp;
	  TreelogStream treelog (tmp ());
	  Treelog::Open nest (treelog, name);
	  if (!syntax.check (atts, name, treelog))
	    error (tmp.str ());
	}

      if (skipped)
	skip (")");
      skip ();
    }
}

Time
ParserFile::Implementation::get_time ()
{
  int year = get_integer ();
  int month = get_integer ();
  int mday = get_integer ();
  int hour = (looking_at (')') ? 0 : get_integer ());

  if (month < 1 || month > 12)
    error ("There are only 12 month in a year");
  else if (mday < 1 || mday > Time::month_length (year, month))
    error ("That day doesn't exists in the selected month");
  else if (hour < 0 || hour > 23)
    error ("Specify an hour between 0 and 23 only");
  else
    return Time (year, month, mday, hour); 

  return Time (-999, 1, 1, 0);
}

void
ParserFile::Implementation::initialize (const Syntax& syntax, Treelog& out)
{ 
  global_syntax_table = &syntax; 
  lexer = new Lexer (file, out);
  nest = new Treelog::Open (out, file);
}

ParserFile::Implementation::Implementation (const string& name)
  : file (name),
    lexer (NULL),
    nest (NULL)
{ }

ParserFile::Implementation::~Implementation ()
{ 
  sequence_delete (inputs.begin (), inputs.end ());
  daisy_assert (lexer != NULL);
  delete lexer; 
  delete nest;
}

void
ParserFile::load_nested (AttributeList& alist)
{
  impl.skip ();
  impl.load_list (alist, *impl.global_syntax_table);
  impl.skip ();
  impl.eof ();
}

void
ParserFile::load (AttributeList& alist)
{
  load_nested (alist);
  
  // Add inputs.
  alist.add ("parser_inputs", impl.inputs);
  sequence_delete (impl.inputs.begin (), impl.inputs.end ());
  impl.inputs.erase (impl.inputs.begin (), impl.inputs.end ());

  // Remember filename.
  vector<symbol> files;
  if (alist.check ("parser_files"))
    files = alist.identifier_sequence ("parser_files");
  files.push_back (symbol (impl.file));
  alist.add ("parser_files", files);
}

int
ParserFile::error_count () const
{
 return impl.lexer->error_count; 
}

void
ParserFile::initialize (const Syntax& syntax, Treelog& out)
{ impl.initialize (syntax, out); }

static const AttributeList& 
get_alist ()
{
  static AttributeList alist;
  if (!alist.check ("type"))
    alist.add ("type", "file");
  return alist;
}
    
ParserFile::ParserFile (const Syntax& syntax, const string& name, Treelog& out)
  : Parser (get_alist ()),
    impl (*new Implementation (name))
{ initialize (syntax, out); }

ParserFile::ParserFile (const AttributeList& al)
  : Parser (al),
    impl (*new Implementation (al.name ("where")))
{  }

ParserFile::~ParserFile ()
{ delete &impl; }

static struct ParserFileSyntax
{
  static Parser& make (const AttributeList& al)
  { return *new ParserFile (al); }

  ParserFileSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Read a setup file containing lots of parentheses.");
    syntax.add ("where", Syntax::String, Syntax::Const,
		"File to read from.");
    syntax.order ("where");
    Librarian<Parser>::add_type ("file", alist, syntax, &make);
  }
} ParserFile_syntax;
