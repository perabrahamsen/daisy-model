// organic_matter.C

#include "organic_matter.h"
#include "syntax.h"
#include "alist.h"
#include "log.h"
#include "filter.h"
#include "aom.h"

struct OrganicMatter::Implementation
{
  // Content.
  vector <AOM*> aom;		// Added Organic Matter.
  const vector<OM*> smb;	// Living Organic Matter.
  const vector<OM*> som;	// Soil Organic Matter.
  struct Buffer
  {
    double C;			// Carbon.
    double N;			// Nitrogen.
    double turnover_rate;	// Absorption.
    void output (Log& log, const Filter& filter) const
    {
      log.output ("C", filter, C);
      log.output ("N", filter, N);
      // log.output ("turnover_rate", filter, turnover_rate);
    }
    static void load_syntax (Syntax& syntax, AttributeList& alist)
    {
      syntax.add ("C", Syntax::Number, Syntax::State);
      alist.add ("C", 0.0);
      syntax.add ("N", Syntax::Number, Syntax::State);
      alist.add ("N", 0.0);
      syntax.add ("turnover_rate", Syntax::Number, Syntax::Const);
      alist.add ("turnover_rate", 1.0);
    }
    Buffer (const AttributeList& al)
      : C (al.number ("C")),
	N (al.number ("N")),
	turnover_rate (al.number ("turnover_rate"))
    { }
  } buffer;

  // Simulation.
  void add (AOM& om)
  {
    aom.push_back (&om);
  }
  void output (Log& log, const Filter& filter) const;

  bool check () const
  {
    bool ok = true;
    for (unsigned int i = 0; i < aom.size (); i++)
      if (!aom[i]->check ())
	ok = false;
    if (!ok)
      cerr << "in OrganicMatter\n";
    return ok;
  }

  // Create & Destroy.
  Implementation (const AttributeList& al)
    : aom (map_construct <AOM> (al.list_sequence ("am"))),
      smb (map_construct <OM> (al.list_sequence ("smb"))),
      som (map_construct <OM> (al.list_sequence ("som"))),
      buffer (al.list ("buffer"))
  { }
};

void
OrganicMatter::Implementation::output (Log& log, const Filter& filter) const
{
  output_list (aom, "am", log, filter);
  output_vector (smb, "smb", log, filter);
  output_vector (som, "som", log, filter);
  output_submodule (buffer, "buffer", log, filter);
}

OrganicMatter::OrganicMatter (const AttributeList& al)
  : impl (*new Implementation (al))
{ }

OrganicMatter::~OrganicMatter ()
{
  delete &impl;
}

void 
OrganicMatter::add (AOM& aom)
{
  impl.add (aom);
}

void 
OrganicMatter::output (Log& log, const Filter& filter) const
{
  impl.output (log, filter);
}

bool
OrganicMatter::check (const AttributeList& al)
{
  bool ok = true;

  const vector<const AttributeList*>& smb_alist = al.list_sequence ("smb");
  for (unsigned int i = 0; i < smb_alist.size(); i++)
    {
      bool om_ok = true;
      ::check (*smb_alist[i], "C_per_N", om_ok);
      ::check (*smb_alist[i], "turnover_rate", om_ok);
      ::check (*smb_alist[i], "efficiency", om_ok);
      ::check (*smb_alist[i], "maintenance", om_ok);
      if (!om_ok)
	{
	  cerr << "in smb[" << i << "]\n";
	  ok = false;
	}
    }

  const vector<const AttributeList*>& som_alist = al.list_sequence ("som");
  for (unsigned int i = 0; i < som_alist.size(); i++)
    {
      bool om_ok = true;
      ::check (*som_alist[i], "C_per_N", om_ok);
      ::check (*som_alist[i], "turnover_rate", om_ok);
      ::check (*som_alist[i], "efficiency", om_ok);
      if (!om_ok)
	{
	  cerr << "in som[" << i << "]\n";
	  ok = false;
	}
    }

  if (!ok)
    cerr << "in OrganicMatter\n";

  return ok;
}

bool
OrganicMatter::check () const
{
  return impl.check ();
}

void
OrganicMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("am", AOM::library (), Syntax::State, Syntax::Sequence);
  syntax.add ("smb", OM::syntax (), Syntax::State, 2);
  syntax.add ("som", OM::syntax (), Syntax::State, 2);
  add_submodule<Implementation::Buffer> ("buffer", syntax, alist);
}
