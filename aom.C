// aom.C

#include "aom.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "common.h"
#include "time.h"
#include "log.h"
#include "soil.h"
#include <algo.h>

static Library* AOM_library = NULL;
static Syntax* OM_syntax = NULL;

const Syntax& OM::syntax ()
{
  assert (OM_syntax);
  return *OM_syntax;
}

OM::OM (const AttributeList& al)
  : top_C (al.check ("top_C") ? al.number ("top_C") : 0.0),
    C (al.check ("C") ? al.number_sequence ("C") : *new vector<double> ()),
    C_per_N (al.number ("C_per_N")),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number ("efficiency")),
    maintenance (al.check ("maintenance") ? al.number ("maintenance") : 0.0),
    fractions (al.check ("fractions") 
	       ? al.number_sequence ("fractions")
	       : *new vector<double> ())
{ }

OM::OM (const AttributeList& al, const double C, const double N)
  : top_C (C),
    C (*new vector<double> ()),
    C_per_N (C/N),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number ("efficiency")),
    maintenance (al.check ("maintenance") ? al.number ("maintenance") : 0.0),
    fractions (al.check ("fractions") 
	       ? al.number_sequence ("fractions")
	       : *new vector<double> ())
{ }

void
OM::output (Log& log, const Filter& filter) const
{
  log.output ("top_C", filter, top_C);
  log.output ("C", filter, C);
  // These are const and should be read from the AOM library.
  log.output ("C_per_N", filter, C_per_N);
#if 0
  log.output ("turnover_rate", filter, turnover_rate);
  log.output ("efficiency", filter, efficiency);
  log.output ("maintenance", filter, maintenance);
  log.output ("fractions", filter, fractions);
#endif
}

void 
OM::mix (const Soil& soil, double from, double to)
{ 
  soil.mix (C, top_C, from, to);
  top_C = 0.0;
}

bool 
AOM::check (const AttributeList& al)
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
AOM::check () const
{ 
  bool ok = true;

  for (unsigned int i = 0; i < om.size(); i++)
    {
      bool om_ok = true;
      
      non_negative (om[i]->top_C, "top_C", om_ok);
      // TODO: [om[i]->C]
      non_negative (om[i]->C_per_N, "C_per_N", om_ok);
      non_negative (om[i]->turnover_rate, "turnover_rate", om_ok);
      non_negative (om[i]->efficiency, "efficiency", om_ok);
      non_negative (om[i]->maintenance, "maintenance", om_ok);
      // TODO: [om[i]->fractions]
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
AOM::tick (const OrganicMatter&)
{ }

void
AOM::output (Log& log, const Filter& filter) const
{ 
  log.output ("creation", filter, creation);
  output_vector (om, "om", log, filter);
}

void 
AOM::mix (const Soil& soil, double from, double to)
{
  for (unsigned int i = 0; i < om.size (); i++)
    om[i]->mix (soil, from, to);
}

const Library&
AOM::library ()
{
  assert (AOM_library);
  return *AOM_library;
}

void 
AOM::derive_type (string name, const AttributeList& al, string /* super */)
{
  assert (AOM_library);
  AOM_library->add (name, al, AOM_library->syntax ("am"));
}

AOM& 
AOM::create (const Time& t, const AttributeList& al)
{
  return *new AOM (t, al);
}

InorganicMatter AOM::im (const AttributeList& al)
{
  return InorganicMatter (SoluteMatter (al.list ("im")), 
			  al.number ("weight"));
}

vector<OM*>& 
AOM::create_om (const AttributeList& al)
{
  // Get initialization parameters.
  const vector<const AttributeList*>& om_alist = al.list_sequence ("om");
  const int size = om_alist.size();
  const double weight = al.number ("weight")
    * al.number ("dry_matter_fraction");
  const AttributeList& im = al.list ("im");
  const double C = weight * al.number ("total_C_fraction");
  const double N = weight * (al.number ("total_N_fraction")
			     - (im.check ("NH4") ? im.number ("NH4") : 0.0)
			     - (im.check ("NO3") ? im.number ("NO3") : 0.0));

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
	      const double C_per_N = om_alist[i]->number("C_per_N");
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
      const double C_per_N = om_alist[missing_fraction]->number("C_per_N");
      om_N[missing_fraction] = om_C[missing_fraction] / C_per_N;
    }
  om_N[missing_C_per_N] = N - accumulate (om_N.begin (), om_N.end (), 0.0);

  // Create the OM's
  vector<OM*>& om = *new vector<OM*> ();
  for (int i = 0; i < size; i++)
    om.push_back (new OM (*om_alist[i], om_C[i], om_N[i]));

  return om;
}

AOM::AOM (const Time& t, const AttributeList& al)
  : creation (t),
    name (al.name ("type")),
    om (create_om (al))
{ }

AOM::AOM (const AttributeList& al)
  : creation (al.time ("creation")),
    name (al.name ("type")),
    om (map_construct<OM> (al.list_sequence ("om")))
{ }

AOM::~AOM ()
{ }

int AOM_init::count;

AOM_init::AOM_init ()
{ 
  if (count++ == 0)
    {
      // OM Syntax.
      OM_syntax = new Syntax ();
      OM_syntax->add ("top_C", Syntax::Number, Syntax::Optional);
      OM_syntax->add ("C", Syntax::Number, Syntax::Optional, Syntax::Sequence);
      OM_syntax->add ("C_per_N", Syntax::Number, Syntax::Optional);
      OM_syntax->add ("turnover_rate", Syntax::Number, Syntax::Optional);
      OM_syntax->add ("efficiency", Syntax::Number, Syntax::Optional);
      OM_syntax->add ("maintenance", Syntax::Number, Syntax::Optional);
      OM_syntax->add ("fractions", Syntax::Number, Syntax::Optional, 
		      Syntax::Sequence);

      // AOM Library.
      AOM_library = new Library ();
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();

      // These are for initialization.
      syntax.add ("dry_matter_fraction", Syntax::Number, Syntax::Optional);
      syntax.add ("total_C_fraction", Syntax::Number, Syntax::Optional);
      syntax.add ("total_N_fraction", Syntax::Number, Syntax::Optional);
      add_submodule<SoluteMatter> ("im", syntax, alist);
      syntax.add ("weight", Syntax::Number, Syntax::Optional);
      OM_syntax->add ("initial_fraction", Syntax::Number, Syntax::Optional);

      // These are the real state variables.
      syntax.add ("time", Syntax::Date, Syntax::Optional);
      syntax.add ("om", OM::syntax (), Syntax::State, Syntax::Sequence);

      // Dummy type use to signify no matter.
      alist.add ("type", "am");
      AOM_library->add ("am", alist, syntax);
    }
  assert (count > 0);
}

AOM_init::~AOM_init ()
{ 
  if (--count == 0)
    {
      assert (AOM_library);
      delete AOM_library;
      AOM_library = NULL;
      assert (OM_syntax);
      delete OM_syntax;
      OM_syntax = NULL;
    }
  assert (count >= 0);
}
