// weather.C

#include "weather.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "common.h"
#include <map.h>
#include <algobase.h>

static Library* Weather_library = NULL;
typedef map<string, Weather::constructor, less<string> > Weather_map_type;
static Weather_map_type* Weather_constructors;

const Library&
Weather::library ()
{
  assert (Weather_library);
  return *Weather_library;
}

void
Weather::add_type (const string name, 
		   const AttributeList& al, 
		   const Syntax& syntax,
		   constructor cons)
{
  assert (Weather_library);
  Weather_library->add (name, al, syntax);
  Weather_constructors->insert(Weather_map_type::value_type (name, cons));
}

void 
Weather::derive_type (string name, const AttributeList& al, string super)
{
  add_type (name, al, library ().syntax (super), 
	    (*Weather_constructors)[super]);
}

Weather&
Weather::create (const Time& t, const AttributeList& al)
{
  assert (al.check ("type"));
  const string name = al.name ("type");
  assert (library ().check (name));
  assert (library ().syntax (name).check (al));
  return (*Weather_constructors)[name] (t, al);
}

void
Weather::output (const string, Log&, const Filter*) const
{ }

double
Weather::DayLength () const
{
  return DayLength (Latitude, time);
}

double
Weather::DayLength (double Latitude, const Time& time)
{
  double t = 2 * M_PI / 365 * time.yday ();
  double Dec = (0.3964 - 22.97 * cos (t) + 3.631 * sin (t)
		- 0.03885 * cos (2 * t) 
		+ 0.03838 * sin (2 * t) - 0.15870 * cos (3 * t) 
		+ 0.07659 * sin (3 * t) - 0.01021 * cos (4 * t));
  return (24 / M_PI
	  * acos (-tan (M_PI / 180 * Dec) * tan (M_PI / 180 * Latitude)));
}

Weather::Weather (const Time& t, double l)
  : time (t),
    Latitude (l)
{ }

Weather::~Weather ()
{ }

int Weather_init::count;

Weather_init::Weather_init ()
{ 
  if (count++ == 0)
    {
      Weather_library = new Library ();
      Weather_constructors = new Weather_map_type ();
    }
  assert (count > 0);
}

Weather_init::~Weather_init ()
{ 
  if (--count == 0)
    {
      delete Weather_library;
      Weather_library = NULL;
      delete Weather_constructors;
      Weather_constructors = NULL;
    }
  assert (count >= 0);
}
