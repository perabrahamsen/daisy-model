// am.C

#include "am.h"
#include "om.h"
#include "im.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "common.h"
#include "time.h"
#include "log.h"
#include "soil.h"
#include "mathlib.h"
#include <algo.h>
#include <map>

static Library* AM_library = NULL;

bool 
AM::check (const AttributeList& al)
{ 
  bool ok = true;
  ::check (al, "dry_matter_fraction", ok);
  ::check (al, "total_C_fraction", ok);
  ::check (al, "total_N_fraction", ok);
  const vector<const AttributeList*>& om_alist = al.list_sequence ("om");
  bool has_all_initial_fraction = true;
  bool has_all_C_per_N = true;
  for (unsigned int i = 0; i < om_alist.size(); i++)
    {
      bool om_ok = true;
      if (has_all_initial_fraction)
	{
	  if (!om_alist[i]->check ("initial_fraction"))
	    has_all_initial_fraction = false;
	}
      else
	::check (*om_alist[i], "initial_fraction", om_ok);
      if (has_all_C_per_N)
	{
	  if (!om_alist[i]->check ("C_per_N"))
	    has_all_C_per_N = false;
	}
      else
	::check (*om_alist[i], "C_per_N", om_ok);
      ::check (*om_alist[i], "turnover_rate", om_ok);
      ::check (*om_alist[i], "efficiency", om_ok);
      if (!om_ok)
	{
	  cerr << "in om[" << i << "]\n";
	  ok = false;
	}
      }
  if (has_all_initial_fraction)
    {
      cerr << "you should leave initial_fraction in one om unspecified\n";
      ok = false;
    }
  if (has_all_C_per_N)
    {
      cerr << "you should leave C_per_N in one om unspecified\n";
      ok = false;
    }
  ::check (al, "weight", ok);
  if (!ok)
    cerr << "in am\n";
  
  return ok;
}

bool 
AM::check () const
{ 
  bool ok = true;

  for (unsigned int i = 0; i < om.size(); i++)
    {
      bool om_ok = true;
      
      non_negative (om[i]->top_C, "top_C", om_ok);

      for (unsigned int j = 0; j < om[i]->C_per_N.size (); j++)
	non_negative (om[i]->C_per_N[j], "C_per_N", om_ok, j);

      non_negative (om[i]->turnover_rate, "turnover_rate", om_ok);

      for (unsigned int j = 0; j < om[i]->efficiency.size (); j++)
	non_negative (om[i]->efficiency[j], "efficiency", om_ok, j);

      non_negative (om[i]->maintenance, "maintenance", om_ok);

      if (!om_ok)
	{
	  cerr << "in om[" << i << "]\n";
	  ok = false;
	}
      }
  if (!ok)
    cerr << "in am\n";
  
  return ok;
}

void
AM::output (Log& log, const Filter& filter) const
{ 
  log.output ("creation", filter, creation);
  output_vector (om, "om", log, filter);
}

void 
AM::mix (const Soil& soil, double from, double to, double penetration)
{
  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->mix (soil, from, to, penetration);
}

void
AM::swap (const Soil& soil, double from, double middle, double to)
{
  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->swap (soil, from, middle, to);
}

const Library&
AM::library ()
{
  assert (AM_library);
  return *AM_library;
}

void 
AM::derive_type (string name, const AttributeList& al, string super)
{
  assert (AM_library);
  AM_library->add (name, al, AM_library->syntax (super));
}

AM& 
AM::create (const AttributeList& al)
{
  return *new AM (al);
}

AM& 
AM::create (const Time& t, const AttributeList& al)
{
  return *new AM (t, al);
}

AM& 
AM::create (const Time& t, const AttributeList& al, 
	     const string name, const string part,
	     double C, double N, const vector<double>& density)
{
  return *new AM (t, al, name, part, C, N, density);
}
  
AM& 
AM::create (const Time& t, const AttributeList& al, 
	     const string name, const string part,
	     double C, double N)
{
  const vector<double> density;
  AM::create (t, al, name, part, C, N, density);
}

vector<OM*>& 
AM::create_om (const AttributeList& al)
{
  const string syntax = al.name ("syntax");
  
  if (syntax == "state")
    assert (0);
  else if (syntax == "organic")
    {
      // Get initialization parameters.
      const double weight = al.number ("weight") 
	* al.number ("dry_matter_fraction") 
	* 0.1;			// kg / m^2 --> g / cm^2
      const double C = weight * al.number ("total_C_fraction");
      const double N = weight * IM::N_left (al);
      
      return create_om (al, C, N);
    }
  else if (syntax == "mineral")
    assert (0);
  else if (syntax == "crop")
    assert (0);
  else if (syntax == "initial")
    {
      vector<OM*>& om = map_construct<OM> (al.list_sequence ("om"));
      // BUG: Handle layers.
      return om;
    }
  else if (syntax == "root")
    {
      vector<OM*>& om = map_construct<OM> (al.list_sequence ("om"));
      // Fixed C/N
      // BUG: Handle density.
      return om;
    }
  assert (0);
}

vector<OM*>& 
AM::create_om (const AttributeList& al, 
	       const double C, const double N, const vector<double>& density)
{
  vector<OM*>& om = create_om (al, C, N);
  const int size = om.size();

  for (int i = 0; i < size; i++)
    om[i]->distribute (density);

  return om;
}
  
vector<OM*>& 
AM::create_om (const AttributeList& al, const double C, const double N)
{
  // Get initialization parameters.
  const vector<const AttributeList*>& om_alist = al.list_sequence ("om");
  const int size = om_alist.size();

  // Fill out the blanks.
  int missing_fraction = -1;
  int missing_C_per_N = -1;
  vector<double> om_C (size, 0.0);
  vector<double> om_N (size, 0.0);
  
  for (int i = 0; i < size; i++)
    {
      if (om_alist[i]->check ("initial_fraction"))
	{
	  const double fraction = om_alist[i]->number ("initial_fraction");
	  om_C[i] = C * fraction;
      
	  if (om_alist[i]->check ("C_per_N"))
	    {
	      const vector<double> v = om_alist[i]->number_sequence("C_per_N");
	      assert (v.size () == 1); // BUG: Should check before!
	      const double C_per_N = v[0];
	      om_N[i] = om_C[i] / C_per_N;
	    }
	  else
	    missing_C_per_N = i;
	}
      else
	{
	  missing_fraction = i;
	  if (!om_alist[i]->check ("C_per_N"))
	    missing_C_per_N = i;
	}
    }
  om_C[missing_fraction] = C - accumulate (om_C.begin (), om_C.end (), 0.0);
  
  if (missing_fraction != missing_C_per_N)
    {
      const vector<double> v
	= om_alist[missing_fraction]->number_sequence("C_per_N");
      assert (v.size () == 1); // BUG: Should check before!
      const double C_per_N = v[0];
      om_N[missing_fraction] = om_C[missing_fraction] / C_per_N;
    }
  om_N[missing_C_per_N] = N - accumulate (om_N.begin (), om_N.end (), 0.0);

  // Create the OM's
  vector<OM*>& om = *new vector<OM*> ();
  for (int i = 0; i < size; i++)
    om.push_back (new OM (*om_alist[i], om_C[i], om_N[i]));

  return om;
}

void 
AM::initialize (const Soil& soil)
{
  for (int i = 0; i +0U < om.size (); i++)
    om[i]->initialize (soil);
}

AM::AM (const Time& t, const AttributeList& al, 
	  const string type, const string part,
	  double C, double N, const vector<double>& density)
  : creation (t),
    name (type + "/" + part),
    om (create_om (al, C, N, density))
{ }

AM::AM (const Time& t, const AttributeList& al)
  : creation (t),
    name (al.name ("type")),
    om (create_om (al))
{ }

AM::AM (const AttributeList& al)
  : creation (al.time ("creation")),
    name (al.name ("type")),
    om (map_construct<OM> (al.list_sequence ("om")))
{ }

AM::~AM ()
{ }

int AM_init::count;

AM_init::AM_init ()
{ 
  if (count++ == 0)
    {
      // Library.
      AM_library = new Library ("am");

      // State.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();

	syntax.add ("creation", Syntax::Date, Syntax::Const);
	alist.add ("syntax", "state");
	add_sequence<OM> ("om", syntax, alist);
	AM_library->add ("state", alist, syntax);
      }
      // Organic fertilizer.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "organic");
	syntax.add ("weight", Syntax::Number, Syntax::Const);
	syntax.add ("dry_matter_fraction",
		    Syntax::Number, Syntax::Const);
	syntax.add ("total_C_fraction",
		    Syntax::Number, Syntax::Const);
	syntax.add ("total_N_fraction",
		    Syntax::Number, Syntax::Const);
	add_sequence<OM> ("om", syntax, alist);
	add_submodule<IM> ("im", syntax, alist);
	AM_library->add ("organic", alist, syntax);
      }
      // Mineral fertilizer.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "mineral");
	IM::load_syntax (syntax, alist);
	AM_library->add ("mineral", alist, syntax);
      }
      // Crop part.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "crop");
	add_sequence<OM> ("om", syntax, alist);
	AM_library->add ("crop", alist, syntax);
      }	
      // Initialization.
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "initial");
	Syntax& layer_syntax = *new Syntax ();
	AttributeList& layer_alist = *new AttributeList ();
	layer_syntax.add ("end", Syntax::Number, Syntax::Const);
	layer_syntax.add ("weight", Syntax::Number, Syntax::Const); // Kg C/m^2
	layer_syntax.order ("end", "weight");
	syntax.add ("layers", layer_syntax, Syntax::Const, Syntax::Sequence);
	alist.add ("layers", layer_alist);
	add_sequence<OM> ("om", syntax, alist);
	AM_library->add ("initial", alist, syntax);
      }
      // Root initialization,
      {
	Syntax& syntax = *new Syntax ();
	AttributeList& alist = *new AttributeList ();
	syntax.add ("creation", Syntax::Date, Syntax::State);
	alist.add ("creation", Time (1, 1, 1, 1));
	alist.add ("syntax", "root");
	syntax.add ("depth", Syntax::Number, Syntax::Const);
	syntax.add ("dist", Syntax::Number, Syntax::Const);
	syntax.add ("weight", Syntax::Number, Syntax::Const); // Tons DM / ha
	syntax.add ("total_C_fraction", Syntax::Number, Syntax::Const);
	add_sequence<OM> ("om", syntax, alist);
	AM_library->add ("root", alist, syntax);
      }
    }
  assert (count > 0);
}

AM_init::~AM_init ()
{ 
  if (--count == 0)
    {
      assert (AM_library);
      delete AM_library;
      AM_library = NULL;
    }
  assert (count >= 0);
}
