// input.C

#include "input.h"
#include "log.h"
#include "alist.h"
#include "csmp.h"
#include "rules.h"
#include "library.h"
#include "syntax.h"
#include "action.h"
#include "condition.h"
#include "filter.h"
#include "crop.h"
#include <fstream.h>
#include <strstream.h>

const char*
Usage::what () const
{
  return "Usage: daisy file";
}

struct Parser
{
  Log& log;
  const AttributeList& load (const Syntax& syntax);
  int get ();
  int peek ();
  bool good ();
  string get_string ();
  string get_id ();
  int get_integer ();
  double get_number ();
  void error (string str);
  void skip (const char*);
  void skip ();
  void skip_to_end ();
  void skip_token ();
  bool looking_at (char);
  void eof ();
  const Layers& load_layers (const Library& lib);
  void load_library (Library& lib);
  void add_derived (const Library&, derive_fun);
  AttributeList& load_derived (const Library& lib);
  void load_list (AttributeList&, const Syntax&);
  Time get_time ();
  const Condition* get_condition ();
  const Action* get_action ();
  const Filter* get_filter (const Syntax&);
  const Filter* get_filter_object (const Library&);
  const Filter* get_filter_sequence (const Library&);
  istream* in;
  ostream& err;
  string file;
  int line;
  int column;
  Parser (int& argc, char**& argv, Log&);
  ~Parser ();
};

const AttributeList&
Parser::load (const Syntax& syntax)
{
  AttributeList& alist = *new AttributeList ();
  skip ("(");
  load_list (alist, syntax);
  skip (")");
  eof ();
  syntax.check ("daisy", alist, log);
  return alist;
}

int
Parser::get ()
{
  int c = in->get ();

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
Parser::peek ()
{
  return in->peek ();
}

bool
Parser::good ()
{
  return in->good ();
}

string
Parser::get_string ()
{
  string str ("");
  skip ("\"");

  for (int c = get (); good() && c != '"'; c = get ())
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

string
Parser::get_id ()
{
  skip ();
  int c = peek ();
    
  if (c != '_' && !isalpha (c))
    {
      error ("Identifier expected");
      skip_to_end ();
      return "error";
    }

  string str ("");
  do
    {
      str += c;
      get ();
      c = peek ();
    }
  while (good() && (c == '_' || isalnum (c)));
    
  return str;
}

double
Parser::get_number ()
{
  skip ();
  // Cheat... This doesn't give us the right error handling.
  double d;
  *in >> d;
  return d;
}

int
Parser::get_integer ()
{
  skip ();
  // Cheat... This doesn't give us the right error handling.
  int i;
  *in >> i;
  return i;
}

void 
Parser::error (string str)
{
  err << file << ":" << line << ":" << column + 1 << ": " << str << "\n";
}

void
Parser::skip (const char* str)
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
Parser::skip ()
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
Parser::skip_token ()
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
  else if (isalnum (peek ()) || peek () == '_')
    get_id ();
  else
    get ();
}

void
Parser::skip_to_end ()
{
  while (peek () != ')' && good ())
    skip_token ();
}

bool
Parser::looking_at (char c)
{ 
  skip ();
  return peek () == c;
}

void
Parser::eof ()
{ 
  skip ();
  if (!in->eof ())
    error ("Expected end of file");
}
    
const Layers&
Parser::load_layers (const Library& lib)
{
  Layers& layers = *new Layers ();
  double last = 0.0;

  while (!looking_at (')') && good ())
    {
      skip ("(");
      double zplus = get_number ();

      if (zplus == last)
	error ("Ignoring empty layer");
      else if (zplus >= last)
	error ("Ignoring negative layer");
      else
	{
	  const AttributeList& al = load_derived (lib);
	  const string name = al.name ("type");
	  lib.syntax (name).check (name, al, log);
	  layers.push_back (make_pair (zplus, &al));
	  last = zplus;
	}
      skip (")");
    }
  if (last == 0.0)
    error ("Zero depth soil");
  return layers;
}

void
Parser::load_library (Library& lib)
{ 
  string name = get_id ();
  string super = get_id ();
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
Parser::add_derived (const Library& lib, derive_fun derive)
{

  const string name = get_id ();
  const string super = get_id ();
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
Parser::load_derived (const Library& lib)
{
  const string type = get_id ();
  if (lib.check (type))
    {
      AttributeList& alist = *new AttributeList (lib.lookup (type));
      alist.add ("type", type);
      load_list (alist, lib.syntax (type));
      return alist;
    }
  else
    {
      error (string ("Unknown superclass `") + type + "'");
      return *new AttributeList ();
    }
}

void
Parser::load_list (AttributeList& atts, const Syntax& syntax)
{ 
  while (!looking_at (')') && good ())
    {
      skip ("(");
      string name = get_id ();
      switch (syntax.lookup (name))
	{
	case Syntax::Number:
	  atts.add (name, get_number ());
	  break;
	case Syntax::List: 
	  {
	    AttributeList& list = *new AttributeList ();
	    load_list (list, syntax.syntax (name));
	    atts.add (name, list);
	    break;
	  }
	case Syntax::Rules:
	  {
	    Rules* rules = atts.check (name) 
	      ? new Rules (&atts.rules (name))
	      : new Rules ();
	    skip ("(");
	    while (!looking_at (')') && good ())
	      {
		skip ("(");
		const Condition* c = get_condition ();
		const Action* a = get_action ();
		rules->add (c, a);
		skip (")");
	      }
	    atts.add (name, rules);
	    skip (")");
	    break;
	  }
	case Syntax::CSMP:
	  {
	    CSMP* csmp = new CSMP ();
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
		csmp->add (x, y);
	      }
	    if (count < 2)
	      error ("Need at least 2 points");
	    atts.add (name, csmp);
	    break;
	  }
	case Syntax::Function:
	  atts.add (name, get_id ());
	  break;
	case Syntax::String:
	  atts.add (name, get_string ());
	  break;
	case Syntax::Array:
	  {
	    vector<double> array;
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
	    if (size >= 0 && count != size)
	      {
		ostrstream str;
		str << "Got " << count 
		    << " array members, expected " << size;
		error (str.str ());

		for (;count < size; count++)
		  array.push_back (-1.0);
	      }
	    atts.add (name, array);
	    break;
	  }
	case Syntax::Boolean:
	  {
	    string flag = get_id ();

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
	case Syntax::Layers:
	  atts.add (name, load_layers (syntax.library (name)));
	  break;
	case Syntax::Output:
	  // Handled specially: Put directly in log.
	  {
	    while (!looking_at (')') && good ())
	      {
		skip ("(");
		string s = get_string ();
		const Condition* c = get_condition ();
		const Filter* f = get_filter (syntax.syntax (name));
		log.add (s, c, f);
		skip (")");
	      }
	    break;
	  }
	case Syntax::Class:
	  // Handled specially: Put directly in global library.
	  add_derived (syntax.library (name), syntax.derive (name));
	  break;
	case Syntax::Object:
	  {
	    const Library& lib = syntax.library (name);
	    AttributeList& al = load_derived (lib);
	    const string obj = al.name ("type");
	    lib.syntax (obj).check (obj, al, log);
	    atts.add (name, al);
	  }
	  break;
	case Syntax::Sequence:
	  {
	    const Library& lib = syntax.library (name);
	    Sequence& sequence = *new Sequence ();
	    while (!looking_at (')') && good ())
	      {
		skip ("(");
		const AttributeList& al = load_derived (lib);
		const string obj = al.name ("type");
		lib.syntax (obj).check (obj, al, log);
		sequence.push_back (&al);
		skip (")");
	      }
	    atts.add (name, sequence);
	    break;
	  }
	case Syntax::Error:
	  error (string("Unknown attribute `") + name + "'");
	  skip_to_end ();
	  break;
	default:
	  assert (false);
	}
      skip (")");
    }
}

Time
Parser::get_time ()
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

const Condition*
Parser::get_condition ()
{ 
  Condition* condition = &Condition::null;
  skip ("(");
  string name = get_id ();
  if (name == "at")
    { 
      condition = new ConditionAt (get_time ());
    }
  else if (name == "before")
    { 
      condition = new ConditionBefore (get_time ());
    }
  else if (name == "after")
    { 
      condition = new ConditionAfter (get_time ());
    }
  else if (name == "hourly")
    { 
      int step = looking_at (')') ? 1 : get_integer ();
      condition = new ConditionHourly (step);
    }
  else if (name == "daily")
    { 
      int step = looking_at (')') ? 1 : get_integer ();
      condition = new ConditionDaily (step);
    }
  else if (name == "weekly")
    { 
      int step = looking_at (')') ? 1 : get_integer ();
      condition = new ConditionWeekly (step);
    }
  else if (name == "monthly")
    { 
      int step = looking_at (')') ? 1 : get_integer ();
      condition = new ConditionMonthly (step);
    }
  else if (name == "yearly")
    { 
      int step = looking_at (')') ? 1 : get_integer ();
      condition = new ConditionYearly (step);
    }
  else
    {
      error (string("Unknown condition `") + name + "'");
      skip_to_end ();
    }
  skip (")");
  return condition;
}

const Action*
Parser::get_action ()
{
  Action* action = &Action::null;
  skip ("(");
  string name = get_id ();
  if (name == "sow")
    action = new ActionSow (load_derived (Crop::library ()));
  else if (name == "stop")
    action = new ActionStop ();
  else
    {
      error (string ("Unknown action `") + name + "'");
      skip_to_end ();
    }
  skip (")");
  return action;
}

const Filter*
Parser::get_filter (const Syntax& syntax)
{
  if (looking_at ('*'))
    {
      skip ("*");
      return Filter::all;
    }
  FilterSome* filter = new FilterSome ();
  while (!looking_at (')') && good ())
    {
      if (looking_at ('('))
	{
	  skip ("(");
	  string name = get_id ();
	  switch (syntax.lookup (name))
	    {
	    case Syntax::List:
	      filter->add (name, get_filter (syntax.syntax (name)));
	      break;
	    case Syntax::Object:
	      filter->add (name, get_filter_object (syntax.library (name)));
	      break;
	    case Syntax::Sequence:
	      filter->add (name, get_filter_sequence (syntax.library (name)));
	      break;
	    case Syntax::Error:
	      error (string ("Unknown attribute `")
		     + name + "'");
	      skip_to_end ();   
	      break;
	    default:
	      error (string ("Atomic attribute `")
		     + name + "'");
	      skip_to_end ();   
	    }
	  skip (")");
	}
      else 
	{
	  string name = get_id ();
	  if (syntax.lookup (name) != Syntax::Error)
	    filter->add (name);
	  else
	    error (string ("Attribute `") + name + "' not known");
	}
    }
  return filter;
}

const Filter*
Parser::get_filter_object (const Library& library)
{
  const Filter* filter = Filter::all;
  skip ("(");
  string name = get_id ();
  if (library.check (name))
    filter = get_filter (library.syntax (name));
  else 
    error (string ("Unknown object `") + name + "' in filter");
  skip (")");
  return filter;
}

const Filter*
Parser::get_filter_sequence (const Library& library)
{
  FilterSome* filter = new FilterSome ();

  while (!looking_at (')') && good ())
    {	
      skip ("(");
      string name = get_id ();
      if (library.check (name))
	filter->add (name, get_filter (library.syntax (name)));
      else 
	error (string ("Unknown object `") + name + "' in filter sequence");
      skip (")");
    }
  return filter;
}

Parser::Parser (int& argc, char**& argv, Log& log)
  : log (log),
    err (cerr),
    line (1),
    column (0)
{ 
  if (argc != 2)
    THROW (Usage ());
  file = argv[1];
  in = new ifstream (file.data ());
}

Parser::~Parser ()
{
  delete in;
}

pair<Log*, const AttributeList*>
parse (const Syntax& syntax, int& argc, char**& argv)
{
  Log& log = *new Log (cerr);
  Parser parser(argc, argv, log);
  return make_pair (&log, &parser.load (syntax));
}
  
