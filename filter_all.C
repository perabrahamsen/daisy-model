// filter_all.C

#include "filter.h"

class FilterAll : public Filter
{
  // Use.
public:
  bool check (string, bool) const
  { return true; }
  Filter& lookup (string) const
  { 
    static Filter* all = NULL;
    if (!all)
      {
	AttributeList all_alist;
	all_alist.add ("type", "all");
	all = &Librarian<Filter>::create (all_alist);
      }
    return *all; 
  }

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
