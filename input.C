// input.C

#include "input.h"
#include "manager.h"
#include "weather.h"
#include "groundwater.h"
#include "log.h"
#include "horizon.h"
#include "column.h"
#include "crop.h"
#include "alist.h"
#include "csmp.h"
#include "rules.h"
#include "library.h"
#include "syntax.h"
#include "action.h"
#include "condition.h"
#include "filter.h"
#include <fstream.h>
#include <strstream.h>

const char*
Usage::what () const
{
  return "Usage: daisy file";
}

struct Input::Implementation
{
  Log& log;
  AttributeList alist;
  Time time;
  Syntax syntax;
  void load ();
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
  const AttributeList& load_derived (const Library& lib);
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
  Implementation (int& argc, char**& argv, ostream&);
};

Time& 
Input::makeTime () const
{
  impl.time = impl.alist.time ("time");
  return impl.time;
}
Manager& 
Input::makeManager () const
{ 
  
  return Manager::create (impl.alist.list ("chief"));
}

Weather& 
Input::makeWeather () const 
{     
  return Weather::create (impl.time, impl.alist.list ("weather"));
}

Groundwater& 
Input::makeGroundwater () const 
{     
  return Groundwater::create (impl.time, impl.alist.list ("groundwater"));
}

Log& 
Input::makeLog () const
{ 
  return impl.log;
}

ColumnList&
Input::makeColumns () const
{
  return *new ColumnList (impl.alist.sequence ("field"));
}

Input::Input (int& argc, char**& argv, ostream& e)
  : impl (*new Implementation (argc, argv, e))
{ }

void 
Input::Implementation::load ()
{
  skip ("(");
  load_list (alist, syntax);
  skip (")");
  eof ();
  syntax.check ("daisy", alist, log);
}

int
Input::Implementation::get ()
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
Input::Implementation::peek ()
{
  return in->peek ();
}

bool
Input::Implementation::good ()
{
  return in->good ();
}

string
Input::Implementation::get_string ()
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
Input::Implementation::get_id ()
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
Input::Implementation::get_number ()
{
  skip ();
  // Cheat... This doesn't give us the right error handling.
  double d;
  *in >> d;
  return d;
}

int
Input::Implementation::get_integer ()
{
  skip ();
  // Cheat... This doesn't give us the right error handling.
  int i;
  *in >> i;
  return i;
}

void 
Input::Implementation::error (string str)
{
  err << file << ":" << line << ":" << column + 1 << ": " << str << "\n";
}

void
Input::Implementation::skip (const char* str)
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
Input::Implementation::skip ()
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
Input::Implementation::skip_token ()
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
Input::Implementation::skip_to_end ()
{
  while (peek () != ')' && good ())
    skip_token ();
}

bool
Input::Implementation::looking_at (char c)
{ 
  skip ();
  return peek () == c;
}

void
Input::Implementation::eof ()
{ 
  skip ();
  if (!in->eof ())
    error ("Expected end of file");
}
    
const Layers&
Input::Implementation::load_layers (const Library& lib)
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
	  layers.push_back (make_pair (zplus, &load_derived (lib)));
	  last = zplus;
	}
      skip (")");
    }
  if (last == 0.0)
    error ("Zero depth soil");
  return layers;
}

void
Input::Implementation::load_library (Library& lib)
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
Input::Implementation::add_derived (const Library& lib, derive_fun derive)
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

const AttributeList&
Input::Implementation::load_derived (const Library& lib)
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
      error (string ("Unknown chief `") + type + "'");
      return *new AttributeList ();
    }
}

void
Input::Implementation::load_list (AttributeList& atts, const Syntax& syntax)
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
	  atts.add (name, load_derived (syntax.library (name)));
	  break;
	case Syntax::Sequence:
	  {
	    Sequence& sequence = *new Sequence ();
	    while (!looking_at (')') && good ())
	      {
		skip ("(");
		sequence.push_back (&load_derived (syntax.library (name)));
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
Input::Implementation::get_time ()
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
Input::Implementation::get_condition ()
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
Input::Implementation::get_action ()
{
  Action* action = &Action::null;
  skip ("(");
  string name = get_id ();
  if (name == "sow")
    action = new ActionSow (load_derived (Crop::var_library ()));
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
Input::Implementation::get_filter (const Syntax& syntax)
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
Input::Implementation::get_filter_object (const Library& library)
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
Input::Implementation::get_filter_sequence (const Library& library)
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

Input::Implementation::Implementation (int& argc, char**& argv, ostream& e)
  : log (*new Log ()),
    time (0, 1, 1, 0),
    err (e),
    line (1),
    column (0)
{ 
  if (argc != 2)
    THROW (Usage ());
  file = argv[1];
  in = new ifstream (file.data ());
  syntax.add_class ("crop", Crop::par_library (), &Crop::derive_type);
  syntax.add_class ("horizon", Horizon::library (), &Horizon::derive_type);
  syntax.add_class ("column", Column::par_library (), &Column::derive_type);
  syntax.add_class ("manager", Manager::library (), &Manager::derive_type);
  syntax.add_object ("chief", Manager::library ());
  syntax.add ("time", Syntax::Date);
  syntax.add_sequence ("field", Column::var_library ());
  syntax.add_output ("log", syntax, Syntax::Sparse);
  syntax.add_object ("weather", Weather::library ());
  syntax.add_object ("groundwater", Groundwater::library ());
  load ();
  delete in;
  in = NULL;
}
