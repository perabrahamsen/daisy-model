// aom.C

#include "aom.h"
#include "library.h"
#include "alist.h"
#include "syntax.h"
#include "common.h"
#include "time.h"
#include "log.h"

static Library* AOM_library = NULL;
static Syntax* OM_syntax = NULL;
static vector<const AttributeList*>* OM_alists = NULL;

const Syntax& OM::syntax ()
{
  assert (OM_syntax);
  return *OM_syntax;
}

const vector<const AttributeList*>& OM::alists ()
{
  assert (OM_alists);
  return *OM_alists;
}

OM::OM (const AttributeList& al)
  : top_C (al.number ("top_C")),
    C (al.number_sequence ("C")),
    C_per_N (al.number ("C_per_N")),
    turnover_rate (al.number ("turnover_rate")),
    efficiency (al.number ("efficiency")),
    maintenance (al.number ("maintenance")),
    fractions (al.number_sequence ("fractions"))
{ }

void
OM::output (Log& log, const Filter& filter) const
{
  log.output ("top_C", filter, top_C);
  log.output ("C", filter, C);
  log.output ("C_per_N", filter, C_per_N);
  log.output ("turnover_rate", filter, turnover_rate);
  log.output ("efficiency", filter, efficiency);
  log.output ("maintenance", filter, maintenance);
  log.output ("fractions", filter, fractions);
}

void 
AOM::tick (const OrganicMatter&)
{ }

void
AOM::output (Log& log, const Filter& filter) const
{ 
  log.output ("creation", filter, creation);
  log.output ("name", filter, name);
  output_vector (om, "om", log, filter);
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
  AOM_library->add (name, al, AOM_library->syntax ("aom"));
}

AOM::AOM (const Time& t, const AttributeList& al)
  : creation (t),
    name (al.name ("name")),
    om (map_construct<OM> (al.list_sequence ("om")))
{ }

AOM::AOM (const AttributeList& al)
  : creation (al.time ("creation")),
    name (al.name ("name")),
    om (map_construct<OM> (al.list_sequence ("om")))
{ }

AOM::~AOM ()
{ }

int AOM_init::count;

AOM_init::AOM_init ()
{ 
  if (count++ == 0)
    {
      OM_syntax = new Syntax ();
      OM_alists = new vector<const AttributeList*> ();
      AttributeList& OM_alist = *new AttributeList ();
      OM_syntax->add ("top_C", Syntax::Number, Syntax::State);
      OM_alist.add ("top_C", 0.0);
      OM_syntax->add ("C", Syntax::Number, Syntax::State, Syntax::Sequence);
      OM_alist.add ("C", *new vector<double> ());
      OM_syntax->add ("C_per_N", Syntax::Number, Syntax::State);
      OM_alist.add ("C_per_N", 0.0);
      OM_syntax->add ("turnover_rate", Syntax::Number, Syntax::State);
      OM_alist.add ("turnover_rate", 0.0);
      OM_syntax->add ("efficiency", Syntax::Number, Syntax::State);
      OM_alist.add ("efficiency", 0.0);
      OM_syntax->add ("maintenance", Syntax::Number, Syntax::State);
      OM_alist.add ("maintenance", 0.0);
      OM_syntax->add ("fractions", Syntax::Number, Syntax::State);
      OM_alist.add ("fractions", *new vector<double> ());
      OM_alists->push_back (&OM_alist);
      OM_alists->push_back (&OM_alist);

      AOM_library = new Library ();
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();

      syntax.add ("time", Syntax::Date, Syntax::State);
      syntax.add ("name", Syntax::String, Syntax::State);

      syntax.add ("om", OM::syntax (), Syntax::State, Syntax::Sequence);
      alist.add ("om", OM::alists ());
      
      AOM_library->add ("aom", alist, syntax);
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
      assert (OM_alists);
      delete OM_alists;
      OM_alists = NULL;
    }
  assert (count >= 0);
}
