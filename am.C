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
// Borland C++ 5.01 doesn't have this.
// #include <algo.h>
#include <map>
#include <numeric>

static Library* AM_library = NULL;

static vector<OM*>&
create_om (const vector<const AttributeList*>& om_alist,
	   const Soil& soil, double C, double N,
	   const vector<double>& content)
{
  // Get initialization parameters.
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
	      assert (om_N[i] >= 0.0);
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
      assert (C_per_N >= 0.0);
      om_N[missing_fraction] = om_C[missing_fraction] / C_per_N;
      assert (om_N[missing_fraction] >= 0.0);
    }
  om_N[missing_C_per_N] = N - accumulate (om_N.begin (), om_N.end (), 0.0);

  // Create the OM's
  vector<OM*>& om = *new vector<OM*> ();
  for (int i = 0; i < size; i++)
    if (i == missing_C_per_N)
      om.push_back (new OM (*om_alist[i], soil, om_C[i], om_N[i]));
    else
      {
	om.push_back (new OM (*om_alist[i], soil));
	om[i]->top_C = om_C[i];
      }

  if (content.size ())
    for (int i = 0; i < size; i++)
      om[i]->distribute (soil, content);

  return om;
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

double 
AM::total_C (const Soil& soil) const
{
  double total = 0.0;
  for (unsigned int i = 0; i < om.size (); i++)
    total += om[i]->total_C (soil);
  return total;
}

double 
AM::total_N (const Soil& soil) const
{
  double total = 0.0;
  for (unsigned int i = 0; i < om.size (); i++)
    total += om[i]->total_N (soil);
  return total;
}

void 
AM::pour (vector<double>& cc, vector<double>& nn)
{
  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->pour (cc, nn);
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
AM::create (const AttributeList& al , const Soil& soil)
{
  return create (al, soil, Time (1, 1, 1, 1));
}

AM& 
AM::create (const AttributeList& al, const Soil& soil, const Time& time)
{
  return *new AM (al, soil, time);
}
  // Crop part.

AM& 
AM::create (const Soil& soil, const Time& time,
	    vector<const AttributeList*> ol,
	    const string name, const string part,
	    double C, double N, const vector<double>& content)
{
  return *new AM (soil, time, ol, name, part, C, N, content);
}

static vector<OM*>&
create_om (const AttributeList& al, const Soil& soil)
{
  const string syntax = al.name ("syntax");
  
  if (syntax == "state")
    return map_construct1<OM, const Soil&> (al.list_sequence ("om"), soil);
  else if (syntax == "organic")
    {
      // Get initialization parameters.
      const double weight = al.number ("weight") 
	* al.number ("dry_matter_fraction") 
	* 0.1;			// kg / m² --> g / cm²
      const double C = weight * al.number ("total_C_fraction");
      const double N = weight * IM::N_left (al);
      
      const vector<const AttributeList*>& oms = al.list_sequence ("om");
      vector<double> content;
	
      return create_om (oms, soil, C, N, content);
    }
  else if (syntax == "mineral")
    assert (0);
  else if (syntax == "crop")
    assert (0);
  else if (syntax == "initial")
    {
      const vector<const AttributeList*>& oms = al.list_sequence ("om");
      vector<OM*>& om = map_construct1<OM, const Soil&> (oms, soil);
      
      const vector<const AttributeList*>& layers = al.list_sequence ("layers");
      
      double last = 0.0;
      for (unsigned int i = 0; i < layers.size (); i++)
	{
	  const double end = layers[i]->number ("end");
	  const double weight = layers[i]->number ("weight"); // Kg C/m²
	  const double C = weight * 1000.0 / (100.0 * 100.0); // g C / cm²
	  int missing_number = -1;
	  double missing_fraction = 1.0;
	  for (unsigned int j = 0; j < oms.size (); j++)
	    {
	      if (oms[j]->check ("initial_fraction"))
		{
		  const double fraction = oms[j]->number ("initial_fraction");
		  missing_fraction -= fraction;
		  soil.add (om[j]->C, last, end, C * fraction);
		}
	      else if (missing_number != -1)
		// Should be catched by syntax check.
		cerr << "Missing initial fraction in initial am.\n";
	      else
		missing_number = j;
	    }
	  if (missing_number > -1)
	    {
	      if (missing_fraction < -0.1e-10)
		cerr << "Specified over 100% C in om in initial am.\n";
	      else if (missing_fraction > 0.0)
		soil.add (om[missing_number]->C, 
			  last, end, C * missing_fraction);
	    }
	  else if (missing_fraction < -0.1e-10)
	    cerr << "Specified more than all C in om in initial am.\n";
	  else if (missing_fraction > 0.1e-10)
	    cerr << "Specified less than all C in om in initial am.\n";
	  
	  last = end;
	}
      return om;
    }
  else if (syntax == "root")
    {
      const vector<const AttributeList*>& oms = al.list_sequence ("om");
      vector<OM*>& om
	= map_construct1<OM, const Soil&> (oms, soil);

      const double weight = al.number ("weight"); // Kg DM /m²
      const double total_C_fraction = al.number ("total_C_fraction");
      const double C = weight * 1000.0 / (100.0 * 100.0)
	* total_C_fraction; // g C / cm²;
      const double k = M_LN2 / al.number ("dist");
      const double depth = al.number ("depth");

      int missing_number = -1;
      double missing_fraction = 1.0;

      for (unsigned int j = 0; j < om.size (); j++)
	{
	  if (oms[j]->check ("initial_fraction"))
	    {
	      const double fraction = oms[j]->number ("initial_fraction");
	      missing_fraction -= fraction;
	      
	      for (int l = 0; l < soil.size () && soil.z (l) > -depth; l++)
		om[j]->C[l] = (C * fraction * k) * exp (k * soil.z (l));
	    }
	  else if (missing_number != -1)
	    // Should be catched by syntax check.
	    cerr << "Missing initial fraction in initial am.\n";
	  else
	    missing_number = j;
	}
      if (missing_number > -1)
	{
	  if (missing_fraction < -0.1e-10)
	    cerr << "Specified over 100% C in om in initial am.\n";
	  else if (missing_fraction > 0.0)
	    for (int l = 0; l < soil.size () && soil.z (l) > -depth; l++)
	      om[missing_number]->C[l]
		= (C * missing_fraction * k) * exp (k * soil.z (l));
	}
      else if (missing_fraction < -0.1e-10)
	cerr << "Specified more than all C in om in initial am.\n";
      else if (missing_fraction > 0.1e-10)
	cerr << "Specified less than all C in om in initial am.\n";

      return om;
    }
  assert (0);
}

AM::AM (const Soil& soil, const Time& t, vector<const AttributeList*> ol,
	const string sort, const string part, 
	double C, double N, const vector<double>& content)
  : creation (t),
    name (sort + "/" + part),
    om (create_om (ol, soil, C, N, content))
{ }

AM::AM (const AttributeList& al, const Soil& soil, const Time& time)
  : creation (al.check ("creation") ? al.time ("creation") : time),
    name (al.name ("type")),
    om (create_om (al, soil))
{ }

AM::~AM ()
{ }

static bool check_organic (const AttributeList& al)
{ 
  const string syntax = al.name ("syntax");
  assert (syntax == "organic");
  
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

int AM_init::count;

#ifdef BORLAND_TEMPLATES
template class add_submodule<OM>;
template class add_submodule<IM>;
#endif

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
	add_submodule<OM> ("om", syntax, alist,
			   Syntax::Const, Syntax::Sequence);
	AM_library->add ("state", alist, syntax);
      }
      // Organic fertilizer.
      {
	Syntax& syntax = *new Syntax (check_organic);
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
	add_submodule<OM> ("om", syntax, alist,
			  Syntax::Const, Syntax::Sequence);
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
	layer_syntax.add ("weight", Syntax::Number, Syntax::Const); // Kg C/m²
	layer_syntax.order ("end", "weight");
	syntax.add ("layers", layer_syntax, Syntax::Const, Syntax::Sequence);
	alist.add ("layers", layer_alist);
	add_submodule<OM> ("om", syntax, alist,
			   Syntax::Const, Syntax::Sequence);
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
	add_submodule<OM> ("om", syntax, alist,
			   Syntax::Const, Syntax::Sequence);
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
