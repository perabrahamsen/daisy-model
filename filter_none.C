// filter_none.C

#include "filter.h"

class FilterNone : public Filter
{
  // Use.
public:
  bool check (const string&, bool) const
  { return false; }
  Filter& lookup (const string&) const
  { assert (false); }

    // Create and Destroy.
public:
  bool check (const Library& lib, int size) const
    { return Filter::check (lib, size); }
  bool check (const Syntax& syntax, int size) const
    { return Filter::check (syntax, size); }
  bool check (Syntax::type type, int size) const
    { return Filter::check (type, size); }
  static Filter& make (const AttributeList&)
  { return *new FilterNone (); }
  FilterNone ()
  { };
};

static struct FilterNoneSyntax
{
  FilterNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Librarian<Filter>::add_type ("none", alist, syntax, &FilterNone::make);
  }
} FilterNone_syntax;
