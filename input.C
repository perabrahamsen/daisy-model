// input.C

#include "input.h"
#include "manager.h"
#include "wheather.h"
#include "log.h"
#include "horizon.h"
#include "column.h"
#include "value.h"
#include "library.h"
#include "syntax.h"
#include "action.h"
#include "condition.h"
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
    Library crops;
    Library horizons;
    Library columns;
    Library managers;
    string chief;
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
    void load_library (Library& lib);
    void load_list (ValueList*, const Syntax*);
    Condition* get_condition ();
    Action* get_action ();
    istream* in;
    ostream& err;
    string file;
    int line;
    int column;
    Implementation (int& argc, char**& argv, ostream&);
};

Manager& 
Input::makeManager () const
{ 
    return *new Manager (impl.log, 
			 impl.managers.lookup (impl.chief));
}

Wheather& 
Input::makeWheather () const 
{     
    return *new Wheather (impl.log);
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

const Library& Input::makeCrops () const
{ 
    return impl.crops;
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
		load_library (crops);
	    else if (item == "horizon")
		load_library (horizons);
	    else if (item == "column")
		load_library (columns);
	    else if (item == "manager")
		load_library (managers);
	    else if (item == "chief")
		chief = get_id ();
	    else if (item == "field")
		{
		    while (!looking_at (')'))
			{
			    string name = get_id ();
			    const ValueList* values 
				= columns.lookup (name);
			    field.push_back (new Column(log, name, 
							values, horizons));
			}
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
    const ValueList* sl = lib.lookup (super);
    ValueList* atts = new ValueList (*sl);
    load_list (atts, lib.syntax (super));
    lib.add (name, atts, lib.syntax (super));
}

void
Input::Implementation::load_list (ValueList* atts, const Syntax* syntax)
{ 
    while (!looking_at (')') && good ())
	{
	    skip ("(");
	    string name = get_id ();
	    switch (syntax->lookup (name))
		{
		case Syntax::Number:
		    atts->add (name, new ValueNumber (get_number ()));
		    break;
		case Syntax::List: 
		{
		    ValueList* list = new ValueList ();
		    load_list (list, syntax->syntax (name));
		    atts->add (name, list);
		    break;
		}
		case Syntax::Rules:
		{
		    // BUG: SHOULD USE DYNAMIC CAST
		    ValueRules* rules  
			= new ValueRules ((ValueRules*) (atts->check (name)));
		    skip ("(");
		    while (!looking_at (')') && good ())
			{
			    skip ("(");
			    Condition* c = get_condition ();
			    Action* a = get_action ();
			    rules->add (c, a);
			    skip (")");
			}
		    atts->add (name, rules);
		    skip (")");
		    break;
		}
		case Syntax::CSMP:
		{
		    ValueCSMP* csmp = new ValueCSMP ();
		    double last_x;
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
		    atts->add (name, new ValueString (get_id ()));
		    break;
		case Syntax::Array:
		{
		    ValueArray* array = new ValueArray ();
		    int count = 0;
		    int size = syntax->size (name);
		    while (!looking_at (')') && good ())
			{
			    array->add (get_number ());
			    count++;
			}
		    if (size >= 0 && count != size)
			{
			    ostrstream str;
			    str << "Got " << count 
				<< " array members, expected " << size;
			    error (str.str ());

			    for (;count < size; count++)
				array->add (-1.0);
			}
		    atts->add (name, array);
		    break;
		}
		case Syntax::Boolean:
		{
		    string flag = get_id ();

		    if (flag == "true")
			atts->add (name, new ValueBool (true));
		    else
			{
			    atts->add (name, new ValueBool (false));
			    if (flag != "false")
				error ("Expected `true' or `false'");
			}
		    break;
		}
		case Syntax::Error:
		    error (string("Unknown attribute `") + name + "'");
		    skip_to_end ();
		    break;
		default:
		    assert (0);
		}
	    skip (")");
	}
}

Condition*
Input::Implementation::get_condition ()
{ 
    Condition* condition = &Condition::null;
    skip ("(");
    string name = get_id ();
    if (name == "at")
	{ 
	    int day = get_integer ();
	    int hour = get_integer ();
	    condition = new ConditionAt (day, hour);
	}
    else if (name == "before")
	{ 
	    int day = get_integer ();
	    int hour = get_integer ();
	    condition = new ConditionBefore (day, hour);
	}
    else if (name == "after")
	{ 
	    int day = get_integer ();
	    int hour = get_integer ();
	    condition = new ConditionAfter (day, hour);
	}
    else
	{
	    error (string("Unknown condition `") + name + "'");
	    skip_to_end ();
	}
    skip (")");
    return condition;
}

Action*
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
	    error (string("Unknown action `") + name + "'");
	    skip_to_end ();
	}
    skip (")");
    return action;
}

Input::Implementation::Implementation (int& argc, char**& argv, ostream& e)
    : log (*new Log ()),
      crops (),
      horizons (),
      columns (),
      chief ("manager"),
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
