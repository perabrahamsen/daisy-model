// filter.C

#include "filter.h"

Librarian<Filter>::Content* Librarian<Filter>::content = NULL;

const vector<double>
Filter::select (const Geometry&, const vector<double>& value)
{ return value; }

void
Filter::accumulate (const Geometry&, const vector<double>&)
{ }

bool
Filter::accumulating () const
{ return false; }

bool
Filter::check (const Library&, int /* size */) const
{ return true; }

bool
Filter::check (const Syntax&, int /* size */) const
{ return true; }

bool
Filter::check (const Syntax::type, int /* size */) const
{ return true; }

Filter::Filter ()
{ }

Filter::~Filter ()
{ }
