// filter_checkpoint.C

#include "filter.h"

class FilterCheckpoint : public Filter
{
  // Use.
public:
  bool check (const string&, bool log_only = false) const
    { return !log_only; }
  Filter& lookup (const string&) const
    { 
      static Filter* all = NULL;
      if (!all)
	{
	  AttributeList all_alist;
	  all_alist.add ("type", "checkpoint");
	  all = &Librarian<Filter>::create (all_alist);
	}
      return *all; 
    }

    // Create and Destroy.
public:
  static Filter& make (const AttributeList&)
    { return *new FilterCheckpoint (); }
  FilterCheckpoint ()
    { };
};

static struct FilterCheckpointSyntax
{
  FilterCheckpointSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Match all state variables.");
      Librarian<Filter>::add_type ("checkpoint", alist, syntax,
				   &FilterCheckpoint::make);
    }
} FilterCheckpoint_syntax;
