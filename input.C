// input.C

#include "input.h"
#include "manager.h"
#include "weather.h"
#include "groundwater.h"
#include "log.h"
#include "uzmodel.h"
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
  Library managers;
  Weather* weather;
  Groundwater* groundwater;
  string chief;
  Time time;
  ColumnList field;
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
  void load_log (Log&);
  void load_soil (HorizonList&);
  void load_columns (ColumnList&);
  void load_crops (CropList&);
  void load_library (Library& lib);
  typedef void (*derive_fun) (string, const AttributeList&, string);
  void add_derived (const Library&, derive_fun);
  void load_list (AttributeList*, const Syntax*);
  Time get_time ();
  const Condition* get_condition ();
  const Action* get_action ();
  const Filter* get_filter (const Syntax*);
  const Filter* get_filter_columns ();
  const Filter* get_filter_crops ();
  istream* in;
  ostream& err;
  string file;
  int line;
  int column;
  Implementation (int& argc, char**& argv, ostream&);
};

const Time& 
Input::makeTime () const
{
  impl.weather->set (impl.time);
  return impl.time;
}
Manager& 
Input::makeManager () const
{ 
  return *new Manager (impl.managers.lookup (impl.chief));
}

Weather& 
Input::makeWeather () const 
{     
  return *impl.weather;
}

Groundwater& 
Input::makeGroundwater () const 
{     
  return *impl.groundwater;
}

Log& 
Input::makeLog () const
{ 
  return impl.log;
}

ColumnList&
Input::makeColumns () const
{
  return impl.field;
}

Input::Input (int& argc, char**& argv, ostream& e)
  : impl (*new Implementation (argc, argv, e))
{ }

void 
Input::Implementation::load ()
{
  skip ("(");
  while (!looking_at (')') && good ())
    {
      skip ("(");
      string item = get_id ();
      if (item == "crop")
	add_derived (Crop::par_library (), &Crop::derive_type);
      else if (item == "horizon")
	add_derived (Horizon::library (), &Horizon::derive_type);
      else if (item == "column")
	add_derived (Column::par_library (), &Column::derive_type);
      else if (item == "manager")
	load_library (managers);
      else if (item == "chief")
	chief = get_id ();
      else if (item == "time")
	time = get_time ();
      else if (item == "field")
	load_columns (field);
      else if (item == "log")
	load_log (log);
      else if (item == "weather")
	{
	  if (weather)
	    delete weather;
	  AttributeList atts;
	  load_list (&atts, syntax_table->syntax ("weather"));
	  weather = new Weather (atts);
	}
      else if (item == "groundwater")
	{
	  if (groundwater)
	    delete groundwater;
	  AttributeList atts;
	  load_list (&atts, syntax_table->syntax ("groundwater"));
	  groundwater = new Groundwater (atts);
	}
      else
	{
	  error (string ("Unknown item `") + item + "'");
	  skip_to_end ();
	}
      skip (")");
    }
  skip (")");
  eof ();
  if (weather == 0)
    error ("No weather specified.");
  if (groundwater == 0)
    error ("No groundwater specified.");
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
    
void
Input::Implementation::load_log (Log& l)
{
  while (!looking_at (')') && good ())
    {
      skip ("(");
      string s = get_string ();
      const Condition* c = get_condition ();
      const Filter* f = get_filter (syntax_table->syntax ("daisy"));
      l.add (s, c, f);
      skip (")");
    }
}

void
Input::Implementation::load_soil (HorizonList& sl)
{
  double last = 0.0;

  while (!looking_at (')') && good ())
    {
      skip ("(");
      double zplus = get_number ();
      skip ();
      string name = get_id ();

      if (zplus == last)
	error (string ("Ignoring empty horizon `" + name + "'") );
      else if (zplus >= last)
	error (string ("Ignoring negative horizon `" + name + "'") );
      else if (!Horizon::library ().check (name))
	error (string ("Unknown horizon `") + name + "'");
      else
	{
	  sl.add (zplus, Horizon::create (name));
	  last = zplus;
	}
      skip (")");
    }
  if (last == 0.0)
    error ("Zero depth soil");
}

void
Input::Implementation::load_columns (ColumnList& cl)
{
  while (!looking_at (')') && good ())
    {
      skip ("(");
      string name = get_id ();

      if (Column::par_library ().check (name))
	{
	  const Syntax* parSyntax = Column::par_library ().syntax (name);
	  const AttributeList& parList = Column::par_library ().lookup (name);
	  const Syntax* varSyntax = Column::var_library ().syntax (name);
	  AttributeList varList (Column::var_library ().lookup (name));
	  load_list (&varList, varSyntax);
	  if (   parSyntax->check (name, parList, log) 
	      && varSyntax->check (name, varList, log))
	    {
	      Column* column = Column::create (name, varList);
	      if (column->check (log))
		cl.push_back (column);
	      else
		{
		  error (string ("Ignoring malformed  column `") 
			 + name + "'");
		  delete column;
		}
	    }
	  else
	    error (string ("Ignoring incomplete column `") 
		   + name + "'");
	}
      else
	{
	  error (string ("Unknown column `") + name + "'");
	  skip_to_end ();
	}
      skip (")");
    }
}

void
Input::Implementation::load_crops (CropList& cl)
{
  while (!looking_at (')') && good ())
    {
      skip ("(");
      string name = get_id ();

      if (Crop::par_library ().check (name))
	{
	  const Syntax* parSyntax = Crop::par_library ().syntax (name);
	  const AttributeList& parList = Crop::par_library ().lookup (name);
	  const Syntax* varSyntax = Crop::var_library ().syntax (name);
	  AttributeList varList (Crop::var_library ().lookup (name));
	  load_list (&varList, varSyntax);
	  if (   parSyntax->check (name, parList, log) 
	      && varSyntax->check (name, varList, log))
	    {
	      cl.push_back (Crop::create (name, varList));
	    }
	  else
	    error (string ("Ignoring incomplete crop `") 
		   + name + "'");
	}
      else
	{
	  error (string ("Unknown crop `") + name + "'");
	  skip_to_end ();
	}
      skip (")");
    }
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
  AttributeList* atts = new AttributeList (sl);
  load_list (atts, lib.syntax (super));
  lib.add (name, *atts, lib.syntax (super));
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
  AttributeList* atts = new AttributeList (sl);
  load_list (atts, lib.syntax (super));
  derive (name, *atts, super);
}

void
Input::Implementation::load_list (AttributeList* atts, const Syntax* syntax)
{ 
  while (!looking_at (')') && good ())
    {
      skip ("(");
      string name = get_id ();
      switch (syntax->lookup (name))
	{
	case Syntax::Number:
	  atts->add (name, get_number ());
	  break;
	case Syntax::List: 
	  {
	    AttributeList* list = new AttributeList ();
	    load_list (list, syntax->syntax (name));
	    atts->add (name, list);
	    break;
	  }
	case Syntax::Rules:
	  {
	    Rules* rules = atts->check (name) 
	      ? new Rules (&atts->rules (name))
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
	    atts->add (name, rules);
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
	    atts->add (name, csmp);
	    break;
	  }
	case Syntax::Function:
	  atts->add (name, get_id ());
	  break;
	case Syntax::String:
	  atts->add (name, get_string ());
	  break;
	case Syntax::Array:
	  {
	    vector<double> array;
	    int count = 0;
	    int size = syntax->size (name);
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
	    atts->add (name, array);
	    break;
	  }
	case Syntax::Columns:
	  {
	    ColumnList* cl = new ColumnList ();
	    load_columns (*cl);
	    atts->add (name, cl);
	    break;
	  }
	case Syntax::Crops:
	  {
	    CropList* cl = new CropList ();
	    load_crops (*cl);
	    atts->add (name, cl);
	    break;
	  }
	case Syntax::Boolean:
	  {
	    string flag = get_id ();

	    if (flag == "true")
	      atts->add (name, true);
	    else
	      {
		atts->add (name, false);
		if (flag != "false")
		  error ("Expected `true' or `false'");
	      }
	    break;
	  }
	case Syntax::Integer:
	  atts->add (name, get_integer ());
	  break;
	case Syntax::Date:
	  atts->add (name, get_time ());
	  break;
	case Syntax::Soil:
	  {
	    HorizonList* sl = new HorizonList ();
	    load_soil (*sl);
	    atts->add (name, sl);
	    break;
	  }
	case Syntax::UZmodel:
	  {
	    const string type = get_id ();
	    AttributeList* list = new AttributeList (UZmodel::library().lookup (type));
	    list->add ("type", type);
	    load_list (list, UZmodel::library().syntax (type));
	    atts->add (name, list);
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
    action = new ActionSow (get_id ());
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
Input::Implementation::get_filter (const Syntax* syntax)
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
	  switch (syntax->lookup (name))
	    {
	    case Syntax::List:
	      {
		const Filter* f
		  = get_filter (syntax->syntax (name));
		filter->add (name, f);
		break;
	      }
	    case Syntax::Columns:
	      {
		const Filter* f = get_filter_columns ();
		filter->add (name, f);
		break;
	      }
	    case Syntax::Crops:
	      {
		const Filter* f = get_filter_crops ();
		filter->add (name, f);
		break;
	      }
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
	  if (syntax->lookup (name) != Syntax::Error)
	    filter->add (name);
	  else
	    error (string ("Attribute `") + name + "' not known");
	}
    }
  return filter;
}

const Filter*
Input::Implementation::get_filter_columns ()
{
  FilterSome* filter = new FilterSome ();

  while (!looking_at (')') && good ())
    {	
      skip ("(");
      string name = get_id ();
      if (Column::par_library ().check (name))
	{
	  const Syntax* syntax = Column::var_library ().syntax (name);
	  filter->add (name, get_filter (syntax));
	}
      else 
	error (string ("Unknown column `") + name + "' in filter");
      skip (")");
    }
  return filter;
}

const Filter*
Input::Implementation::get_filter_crops ()
{
  FilterSome* filter = new FilterSome ();

  while (!looking_at (')') && good ())
    {	
      skip ("(");
      string name = get_id ();
      if (Crop::par_library ().check (name))
	{
	  const Syntax* syntax = Crop::var_library ().syntax (name);
	  filter->add (name, get_filter (syntax));
	}
      else 
	error (string ("Unknown crop `") + name + "' in filter");
      skip (")");
    }
  return filter;
}

Input::Implementation::Implementation (int& argc, char**& argv, ostream& e)
  : log (*new Log ()),
    chief ("manager"),
    time (0, 1, 1, 0),
    err (e),
    line (1),
    column (0)
{ 
  if (argc != 2)
    THROW (Usage ());
  file = argv[1];
  in = new ifstream (file.data ());
  load ();
  delete in;
  in = NULL;
}
