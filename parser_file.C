// parser_file.C

#include "parser_file.h"
#include "options.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"
#include "csmp.h"
#include "time.h"
#include "log.h"
#include "filter.h"
#include <fstream.h>

struct ParserFile::Implementation
{
  int get ();
  int peek ();
  bool good ();
  string get_string ();
  int get_integer ();
  double get_number ();
  void error (string str);
  void skip (const char*);
  void skip ();
  void skip_to_end ();
  void skip_token ();
  bool looking_at (char);
  void eof ();
  void load_library (Library& lib);
  void add_derived (const Library&, derive_fun);
  AttributeList& load_derived (const Library& lib, bool in_sequence = false);
  void load_list (AttributeList&, const Syntax&);
  Time get_time ();
  ifstream in;
  string file;
  int line;
  int column;
  const Syntax& global_syntax_table;
  Implementation (const Syntax&, string);
  ~Implementation ();
};

int
ParserFile::Implementation::get ()
{
  int c = in.get ();

  switch (c)
    {
    case '\n':
      column = 0;
      line++;
      break;
    case '\t':
      column += 8 - column % 8;
      break;
    default:
      column++;
    }
  return c;
}

int
ParserFile::Implementation::peek ()
{
  return in.peek ();
}

bool
ParserFile::Implementation::good ()
{
#ifdef BORLAND_EOF  
// BCC requires that you try to read beyond the eof to detect eof.
  char c;
  in.get (c);
  bool ok = in.good ();
  if (ok)
    in.putback (c);
  return ok;
#else
  return in.good ();
#endif
}

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

      for (c = get (); good() && c != '"'; c = get ())
	{
	  if (c == '\\')
	    {
	      c = get ();
	      switch (c)
		{
		case '\\':
		case '"':
		  break;
		default:
		  error (string ("Unknown character escape '")
			 // BUG: SHOULD USE DYNAMIC CAST!
			 + char (c) + "'");
		}
	    }
	  str += static_cast<char> (c);
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
	  str += c;
	  get ();
	  c = peek ();
	}
      while (good() && (c == '_' || c == '-' || isalnum (c)));
    
      return str;
    }
}

double
ParserFile::Implementation::get_number ()
{
  skip ();
  // Cheat... This doesn't give us the right error handling.
  double d;
  in >> d;
  return d;
}

int
ParserFile::Implementation::get_integer ()
{
  skip ();
  // Cheat... This doesn't give us the right error handling.
  int i;
  in >> i;
  return i;
}

void 
ParserFile::Implementation::error (string str)
{
  cerr << file << ":" << line << ":" << (column + 1) << ": " << str << "\n";
}

void
ParserFile::Implementation::skip (const char* str)
{ 
  skip ();
  for (const char* p = str; *p; p++)
    if (*p != peek ())
      {
	error (string("Expected `") + str + "'");
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
ParserFile::Implementation::skip_token ()
{
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
  else
    get ();
}

void
ParserFile::Implementation::skip_to_end ()
{
  while (peek () != ')' && good ())
    skip_token ();
}

bool
ParserFile::Implementation::looking_at (char c)
{ 
  skip ();
  return peek () == c;
}

void
ParserFile::Implementation::eof ()
{ 
  skip ();
  if (!in.eof ())
    error ("Expected end of file");
}
    
void
ParserFile::Implementation::load_library (Library& lib)
{ 
  string name = get_string ();
  string super = get_string ();
  if (!lib.check (super))
    {
      error (string ("Unknown superclass `") + super + "'");
      skip_to_end ();
      return;
    }
  const AttributeList& sl = lib.lookup (super);
  AttributeList& atts = *new AttributeList (sl);
  load_list (atts, lib.syntax (super));
  lib.add (name, atts, lib.syntax (super));
}

void
ParserFile::Implementation::add_derived (const Library& lib, derive_fun derive)
{

  const string name = get_string ();
  const string super = get_string ();
  if (!lib.check (super))
    {
      error (string ("Unknown superclass `") + super + "'");
      skip_to_end ();
      return;
    }
  const AttributeList& sl = lib.lookup (super);
  AttributeList& atts = *new AttributeList (sl);
  load_list (atts, lib.syntax (super));
  derive (name, atts, super);
}

AttributeList&
ParserFile::Implementation::load_derived (const Library& lib, bool in_sequence)
{
  AttributeList* alist /* = NULL */;
  bool skipped = false;
  if (looking_at ('('))
    {
      skip ("(");
      skipped = true;
    }
  const string type = get_string ();
  if (lib.check (type))
    {
      alist = new AttributeList (lib.lookup (type));
      alist->add ("type", type);
      if (skipped || !in_sequence)
	load_list (*alist, lib.syntax (type));
    }
  else
    {
      error (string ("Unknown superclass `") + type + "'");
      alist = new AttributeList ();
    }
  if (skipped)
    skip (")");
  assert (alist != NULL);
  return *alist;
}

void
ParserFile::Implementation::load_list (AttributeList& atts, const Syntax& syntax)
{ 
  vector<string>::const_iterator current = syntax.order ().begin ();
  const vector<string>::const_iterator end = syntax.order ().end ();

  while ( good () && !looking_at (')'))
    {
      bool skipped = false;
      string name;
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
	  name = *current;
	  current++;
	}
	
      if (syntax.size (name) == Syntax::Singleton)
	switch (syntax.lookup (name))
	  {
	  case Syntax::Number:
	    atts.add (name, get_number ());
	    break;
	  case Syntax::AList: 
	    {
	      AttributeList& list = (atts.check (name) 
				     ? *new AttributeList (atts.alist (name))
				     : *new AttributeList ());
	      
	      load_list (list, syntax.syntax (name));
	      atts.add (name, list);
	      break;
	    }
	  case Syntax::CSMP:
	    {
	      CSMP& csmp = *new CSMP ();
	      double last_x = -42;
	      int count = 0;
	      while (!looking_at (')') && good ())
		{
		  skip ("(");
		  double x = get_number ();
		  {
		    if (count > 0 && x <= last_x)
		      error ("Non increasing x value");
		    last_x = x;
		    count++;
		  }
		  double y = get_number ();
		  skip (")");
		  csmp.add (x, y);
		}
	      if (count < 2)
		error ("Need at least 2 points");
	      atts.add (name, csmp);
	      break;
	    }
	  case Syntax::String:
	    atts.add (name, get_string ());
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
		    error ("Expected `true' or `false'");
		}
	      break;
	    }
	  case Syntax::Integer:
	    atts.add (name, get_integer ());
	    break;
	  case Syntax::Date:
	    atts.add (name, get_time ());
	    break;
	  case Syntax::Library:
	    // Handled specially: Put directly in global library.
	    add_derived (syntax.library (name), syntax.derive (name));
	    break;
	  case Syntax::Object:
	    {
	      const Library& lib = syntax.library (name);
	      AttributeList& al = load_derived (lib, current != end);
	      if (&lib == &Parser::library ())
		{
		  Parser& parser = Parser::create (global_syntax_table, al);
		  delete &al;
		  parser.load (atts);
		  delete &parser;
		}
	      else
		{
		  const string obj = al.name ("type");
		  if (!lib.syntax (obj).check (al, obj))
		    error (string ("Error for member `") + obj 
			   + "' in library `" + name + "'");
		  atts.add (name, al);
		}
	    }
	    break;
	  case Syntax::Error:
	    error (string("Unknown attribute `") + name + "'");
	    skip_to_end ();
	    break;
	  default:
	    assert (false);
	  }
      else
	// Support for sequences not really finished yet.
	switch (syntax.lookup (name))
	  {
	  case Syntax::Object:
	    {
	      // We don't support fixed sized object arrays yet.
	      assert (syntax.size (name) == Syntax::Sequence);
	      const Library& lib = syntax.library (name);
	      vector<AttributeList*>& sequence
		= *new vector<AttributeList*> ();
	      while (!looking_at (')') && good ())
		{
		  AttributeList& al = load_derived (lib, true);
		  const string obj = al.name ("type");
		  lib.syntax (obj).check (al, obj);
		  sequence.push_back (&al);
		}
	      atts.add (name, sequence);
	      break;
	    }
	  case Syntax::AList:
	    {
	      int size = syntax.size (name);
	      const Syntax& syn = syntax.syntax (name);
	      vector<AttributeList*>& sequence
		= *new vector<AttributeList*> ();
	      bool skipped = false;
	      if (current != end)
		{
		  skip ("(");
		  skipped = true;
		}
	      while (!looking_at (')') && good ())
		{
		  skip ("(");
		  // AttributeList& al = *new AttributeList ();
		  AttributeList& al = (atts.check (name) 
				       ? *new AttributeList (atts.alist (name))
				       : *new AttributeList ());

		  load_list (al, syn);
		  sequence.push_back (&al);
		  skip (")");
		}
	      if (skipped)
		skip (")");
	      if (size != Syntax::Sequence 
		  && (int) sequence.size () != size)
		{
		  ostrstream str;
		  str << "Got " << sequence.size ()
		      << " array members, expected " << size << '\0';
		  error (str.str ());
		}
	      atts.add (name, sequence);
	      break;
	    }
	  case Syntax::Number:
	    {
	      vector<double>& array = *new vector<double> ();
	      int count = 0;
	      int size = syntax.size (name);
	      double last = 0.0;
	      while (!looking_at (')') && good ())
		{
		  if (looking_at ('*'))
		    {
		      skip ("*");
		      int same = get_integer ();
		      // Append same - 1 copies of last value.
		      for (int i = 1; i < same; i++)
			{
			  array.push_back (last);
			  count++;
			}
		    }
		  else
		    {
		      last = get_number ();
		      array.push_back (last);
		      count++;
		    }
		}
	      if (size != Syntax::Sequence && count != size)
		{
		  ostrstream str;
		  str << "Got " << count 
		      << " array members, expected " << size << '\0';
		  error (str.str ());

		  for (;count < size; count++)
		    array.push_back (-1.0);
		}
	      atts.add (name, array);
	      break;
	    }
	  case Syntax::Error:
	    error (string("Unknown sequence `") + name + "'");
	    skip_to_end ();
	    break;
	  default:
	    error (string("Unsupported sequence `") + name + "'");
	    skip_to_end ();
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

ParserFile::Implementation::Implementation (const Syntax& syntax, string name)
  : in (Options::find_file (name)),
    file (name),
    line (1),
    column (0), 
    global_syntax_table (syntax)
{  
  if (!in.good ())
    cerr << "Open `" << file << "' failed\n";
}

ParserFile::Implementation::~Implementation ()
{
  cerr << "Close `" << file << "'\n";
  if (in.bad ())
    cerr << "There were trouble parsing `" << file << "'\n";
  close (in.rdbuf ()->fd ());
}

void
ParserFile::load (AttributeList& alist)
{
  impl.load_list (alist, impl.global_syntax_table);
  impl.eof ();
}

ParserFile::ParserFile (const Syntax& syntax, string name)
  : impl (*new Implementation (syntax, name))
{  }

ParserFile::ParserFile (const Syntax& s, const AttributeList& al)
  : impl (*new Implementation (s, al.name ("where")))
{  }

ParserFile::~ParserFile ()
{ }

// Add the ParserFile syntax to the syntax table.
Parser&
ParserFile::make (const Syntax& s, const AttributeList& al)
{
  return *new ParserFile (s, al);
}

static struct ParserFileSyntax
{
  ParserFileSyntax ();
} ParserFile_syntax;

ParserFileSyntax::ParserFileSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("where", Syntax::String, Syntax::Const);
  syntax.order ("where");
  Parser::add_type ("file", alist, syntax, &ParserFile::make);
}
