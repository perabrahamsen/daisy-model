// filter_all.C

#include "filter.h"

class FilterAll : public Filter
{
  // Use.
public:
  bool check (string, bool) const
  { return true; }
  const Filter& lookup (string) const
  { return *this; }

    // Create and Destroy.
public:
  static Filter& make (const AttributeList&)
  { return *new FilterAll (); }
  FilterAll ()
  { };
};

static struct FilterAllSyntax
{
  FilterAllSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Librarian<Filter>::add_type ("all", alist, syntax, &FilterAll::make);
  }
} FilterAll_syntax;
