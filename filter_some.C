// filter_some.C

#include "filter.h"
#include <map>

class FilterSome : public Filter
{
  // Content.
  typedef map<string, Filter*, less<string> > filter_map;
  filter_map filters;
  bool accumulating_;

  // Use.
public:
  bool check (string key, bool) const;
  Filter& lookup (string key) const;
  void add (string key, Filter& filter);

  bool accumulating ()
  { return accumulating_; }

  // Create and Destroy.
public:
  bool check (const Library&, int /* size */) const;
  bool check (const Syntax&, int /* size */) const;
  bool check (const Syntax::type type, int /* size */) const;
  static Filter& make (const AttributeList& al);
  FilterSome (const AttributeList& al);
};

bool
FilterSome::check (string key, bool) const
{ 
  return filters.find (key) != filters.end ();
}

Filter& 
FilterSome::lookup (string key) const
{ 
  filter_map::const_iterator i = filters.find (key);
  
  if (i != filters.end ())
    return *(*i).second;
  else
    THROW (AttributeList::Uninitialized ());
}

void 
FilterSome::add (string key, Filter& filter)
{
  filters[key] = &filter;
}

bool
FilterSome::check (const Library& library, int /* size */) const
{ 
  bool all_ok = true;

  for (filter_map::const_iterator i = filters.begin ();
       i != filters.end ();
       i++)
    {
      bool ok = true;
      const string name = (*i).first;
      Filter* filter = (*i).second;

      if (!library.check (name))
	{
	  cerr << "Unknown";
	  ok = false;
	}
      else if (!filter->check (library.syntax (name), Syntax::Singleton))
	{
	  cerr << "- in";
	  ok = false;
	}
      if (!ok)
	{
	  cerr << " object `" << name << "'\n";
	  all_ok = false;
	}
    }
  return all_ok;
}

bool
FilterSome::check (const Syntax& syntax, int /* size */) const
{ 
  bool all_ok = true;

  for (filter_map::const_iterator i = filters.begin ();
       i != filters.end ();
       i++)
    {
      bool ok = true;
      const string name = (*i).first;
      Filter* filter = (*i).second;
      const Syntax::type type = syntax.lookup (name);
      const int size = syntax.size (name);
	
      if (type == Syntax::Error)
	{
	  cerr << "Unknown";
	  ok = false;
	}
      else if (syntax.is_const (name))
	{
	  cerr << "Constant";
	  ok = false;
	}
      else switch (type)
	{
	case Syntax::Number:
	case Syntax::CSMP:
	case Syntax::Boolean:
	case Syntax::String:
	case Syntax::Date:
	case Syntax::Integer:
	  if (!filter->check (type, size))
	    {
	      cerr << "- in";
	      ok = false;
	    }
	  break;
	case Syntax::AList:
	  if (!filter->check (syntax.syntax (name), size))
	    {
	      cerr << "- in";
	      ok = false;
	    }
	  break;
	case Syntax::Object:
	  if (!filter->check (syntax.library (name), size))
	    {
	      cerr << "- in";
	      ok = false;
	    }
	  break;
	case Syntax::Library:
	  cerr << "Class";
	  ok = false;
	  break;
	case Syntax::Error:
	default:
	  assert (false);
	}
      if (!ok)
	{
	  cerr << " entry `" << name << "'\n";
	  all_ok = false;
	}
    }
  return all_ok;
}

bool
FilterSome::check (const Syntax::type type, int /* size */) const
{ 
  cerr << "Cannot use `some' filter on `" 
       << Syntax::type_name (type) << "'\n";
  return false;
}

Filter& 
FilterSome::make (const AttributeList& al)
{ 
  return *new FilterSome (al); 
}

FilterSome::FilterSome (const AttributeList& al)
  : accumulating_ (false)
{ 
  static Filter* all = NULL;
  if (!all)
    {
      AttributeList all_alist;
      all_alist.add ("type", "all");
      all = &Librarian<Filter>::create (all_alist);
    }
    
  const vector<AttributeList*>& members = al.alist_sequence ("members");
  for (unsigned int i = 0; i < members.size (); i++)
    { 
      const string name = members[i]->name ("name");
      
      if (members[i]->check ("filter"))
	{
	  Filter& filter 
	    = Librarian<Filter>::create (members[i]->alist ("filter")); 
	  add (name, filter);
	  if (!accumulating_ && filter.accumulating ())
	    accumulating_ = true;
	}
      else
	add (name, *all);
    }
}

static struct FilterSomeSyntax
{
  FilterSomeSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Syntax& entry_syntax = *new Syntax ();
    entry_syntax.add ("name", Syntax::String, Syntax::Const);
    entry_syntax.add ("filter", 
		      Librarian<Filter>::library (), 
		      Syntax::Optional);
    entry_syntax.order ("name", "filter");
    syntax.add ("members", entry_syntax, Syntax::Const, Syntax::Sequence);
    syntax.order ("members");
    Librarian<Filter>::add_type ("some", alist, syntax, &FilterSome::make);
  }
} FilterSome_syntax;
