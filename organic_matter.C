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
      log.output ("turnover_rate", filter, turnover_rate);
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
  void output (Log& log, const Filter& filter) const;

  // Create & Destroy.
  Implementation (const AttributeList& al)
    : aom (map_construct <AOM> (al.list_sequence ("aom"))),
      smb (map_construct <OM> (al.list_sequence ("smb"))),
      som (map_construct <OM> (al.list_sequence ("som"))),
      buffer (al.list ("buffer"))
  { }
};

void
OrganicMatter::Implementation::output (Log& log, const Filter& filter) const
{
  output_list (aom, "aom", log, filter);
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
OrganicMatter::output (Log& log, const Filter& filter) const
{
  impl.output (log, filter);
}

void
OrganicMatter::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("aom", AOM::library (), Syntax::State, Syntax::Sequence);
  alist.add ("aom", *new vector<const AttributeList*> ());
  syntax.add ("smb", OM::syntax (), Syntax::State, 2);
  alist.add ("smb", OM::alists ());
  syntax.add ("som", OM::syntax (), Syntax::State, 2);
  alist.add ("som", OM::alists ());
  add_submodule<Implementation::Buffer> ("buffer", syntax, alist);
}
