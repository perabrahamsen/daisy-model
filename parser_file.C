// parser_file.C

#include "parser_file.h"
#include "lexer.h"
#include "csmp.h"
#include "time.h"
#include "log.h"


struct ParserFile::Implementation : public Lexer
{
  void add_derived (Library&);
  AttributeList& load_derived (const Library& lib, bool in_sequence = false);
  void load_list (AttributeList&, const Syntax&);
  Time get_time ();
  const Syntax* global_syntax_table;
  void initialize (const Syntax&);
  Implementation (const string&);
  ~Implementation ();
};

void
ParserFile::Implementation::add_derived (Library& lib)
{
  // Get the name of the class and the existing superclass to derive from.
  const string name = get_string ();
  const string super = get_string ();
  if (!lib.check (super))
    {
      error (string ("Unknown superclass `") + super + "'");
      skip_to_end ();
      return;
    }
  // Create new attribute derived from its superclass.
  const AttributeList& sl = lib.lookup (super);
  AttributeList& atts = *new AttributeList (sl);
  // Remember where we got this object.
  atts.add ("parsed_from_file", file);
  atts.add ("parsed_sequence", Library::get_sequence ());
  // Add separate attributes for this object.
  load_list (atts, lib.syntax (super));
  // Add new object to library.
  lib.add_derived (name, atts, super);
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
      skip_to_end ();
      alist = new AttributeList ();
      alist->add ("type", "error");
    }
  if (skipped)
    skip (")");
  assert (alist != NULL);
  return *alist;
}

void
ParserFile::Implementation::load_list (AttributeList& atts,
				       const Syntax& syntax)
{ 
  vector<string>::const_iterator current = syntax.order ().begin ();
  const vector<string>::const_iterator end = syntax.order ().end ();

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
	    add_derived (syntax.library (name));
	    break;
	  case Syntax::Object:
	    {
	      const Library& lib = syntax.library (name);
	      AttributeList& al = load_derived (lib, current != end);
	      if (&lib == &Librarian<Parser>::library ())
		{
		  Parser& parser = Librarian<Parser>::create (al);
		  parser.initialize (*global_syntax_table);
		  delete &al;
		  parser.load (atts);
		  error_count += parser.error_count ();
		  delete &parser;
		}
	      else
		{
		  const string obj = al.name ("type");
		  if (obj == "error")
		    break;
		  if (!lib.syntax (obj).check (al, obj))
		    error (string ("Error for member `") + obj 
			   + "' in library `" + name + "'");
		  atts.add (name, al);
		}
	    }
	    break;
	  case Syntax::Error:
	    error (string("Unknown singleton `") + name + "'");
	    skip_to_end ();
	    break;
	  default:
	    assert (false);
	  }
      else
	{
	  // If this is part of an order, expect parentheses arund the
	  // list, EXCEPT when there cannot possible be any more
	  // elements after this one.  I.e. when the element is the
	  // last of a totally ordered sequence.
	  if (!in_order)
	    // Unordered, we already skipped this one
	    assert (skipped);
	  else if (current != end || !syntax.total_order ())
	    // This is not the last element or the order is not total.
	    {
	      assert (!skipped);
	      skip ("(");
	      skipped = true;
	    }

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
		    if (obj != "error")
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
		// Design bug:  We do not force parentheses around the
		// alist if it is the last member of an ordered list.
		// The consequences of this is that there must be *no*
		// ordered or unordered elements after the alist.
		//
		// The correct way to solve this would be to check that
		// these requirements are really fulfilled, and require
		// the parentheses otherwise.
		//
		// The purpose of this kludge seems to be to allow a
		// library object to -- in effect -- have an alist
		// sequence as its syntax.  Maybe I should support that
		// directly instead.
		if (current != end)
		  {
		    skip ("(");
		    skipped = true;
		  }
		while (!looking_at (')') && good ())
		  {
		    skip ("(");
		    AttributeList& al
		      = *new AttributeList (syntax.default_alist (name));
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
		while (good () && !looking_at (')'))
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
	    case Syntax::String:
	      {
		vector<string> array;
		int count = 0;
		int size = syntax.size (name);

		while (!looking_at (')') && good ())
		  {
		    array.push_back (get_string ());
		    count++;
		  }
		if (size != Syntax::Sequence && count != size)
		  {
		    ostrstream str;
		    str << "Got " << count 
			<< " array members, expected " << size << '\0';
		    error (str.str ());

		    for (;count < size; count++)
		      array.push_back ("<error>");
		  }
		atts.add (name, array);
		break;
	      }
	    case Syntax::Error:
	      error (string("Unknown attribute `") + name + "'");
	      skip_to_end ();
	      break;
	    default:
	      error (string("Unsupported sequence `") + name + "'");
	      skip_to_end ();
	    }
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
ParserFile::Implementation::initialize (const Syntax& syntax)
{ global_syntax_table = &syntax; }

ParserFile::Implementation::Implementation (const string& name)
  : Lexer (name)
{ }

ParserFile::Implementation::~Implementation ()
{ }

void
ParserFile::load (AttributeList& alist)
{
  impl.skip ();
  impl.load_list (alist, *impl.global_syntax_table);
  impl.eof ();
}

int
ParserFile::error_count () const
{
 return impl.error_count; 
}

void
ParserFile::initialize (const Syntax& syntax)
{ impl.initialize (syntax); }

ParserFile::ParserFile (const Syntax& syntax, const string& name)
  : Parser ("file"),
    impl (*new Implementation (name))
{ initialize (syntax); }

ParserFile::ParserFile (const AttributeList& al)
  : Parser (al.name ("type")),
    impl (*new Implementation (al.name ("where")))
{  }

ParserFile::~ParserFile ()
{ 
  delete &impl; 
}

static struct ParserFileSyntax
{
  static Parser& make (const AttributeList& al)
    { return *new ParserFile (al); }

  ParserFileSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", 
		 "Read Daisy a setup file containing lots of parentheses.");
      syntax.add ("where", Syntax::String, Syntax::Const,
		  "File to read from.");
      syntax.order ("where");
      Librarian<Parser>::add_type ("file", alist, syntax, &make);
    }
} ParserFile_syntax;
