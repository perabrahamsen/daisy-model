// filter_all.C

#include "filter.h"

class FilterAll : public Filter
{
  // Use.
public:
  bool check (const string&, bool) const
  { return true; }
  Filter& lookup (const string&) const
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
  bool check (const Library& lib, int size) const
    { return Filter::check (lib, size); }
  bool check (const Syntax& syntax, int size) const
    { return Filter::check (syntax, size); }
  bool check (Syntax::type type, int size) const
    { return Filter::check (type, size); }
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
    alist.add ("description", "Match all variables.");
    Librarian<Filter>::add_type ("all", alist, syntax, &FilterAll::make);
  }
} FilterAll_syntax;
