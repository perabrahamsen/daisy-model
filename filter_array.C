// filter_array.C

#include "filter.h"
#include "geometry.h"

class FilterArray : public Filter
{
  // Content.
  enum selector { At, Average, Content };

  struct selection 
  {
    selector sel;
    double from;
    double to;
    selection (selector s, double f, double t)
      : sel (s),
	from (f),
	to (t)
    { }
    selection ()
      : sel (selector (-1)),
	from (42.42e42),
	to (13.13e13)
    { }
  };
  vector<selection> selections;
    
  // Use.
public:
  bool check (string, bool) const
  { 
    assert (false);
  }

  const Filter& lookup (string) const
  { 
    assert (false);
  }

  const vector<double>
  select (const Geometry& geometry, const vector<double>& value) const
  { 
    vector<double> result;
    for (unsigned int i = 0; i < selections.size (); i++)
      {
	double answer;
	double from = selections[i].from;
	double to = selections[i].to;

	switch (selections[i].sel)
	  {
	  case At:
	    answer = value[geometry.interval_plus (from)];
	    break;
	  case Average:
	    answer = geometry.total (value, from, to) / (from - to);
	    break;
	  case Content:
	    answer = geometry.total (value, from, to);
	    break;
	  default:
	    assert (false);
	  }
	result.push_back (answer);
      }
    return result;
  }

  // Create and Destroy.
public:
  bool check (const Library&, int /* size */) const
  {
    cerr << "This filter works only array of numbers\n";
    return false;
  }

  bool check (const Syntax&, int /* size */) const
  {
    cerr << "This filter works only array of numbers\n";
    return false;
  }

  bool check (const Syntax::type type, int size) const
  {
    if (type != Syntax::Number || size != Syntax::Sequence)
      {
	cerr << "This filter works only array of numbers\n";
	return false;
      }
    return true;
  }

  static Filter& make (const AttributeList& al)
  { return *new FilterArray (al); }

  FilterArray (const AttributeList& al)
  { 
    const vector<const AttributeList*>& members = al.list_sequence ("members");
    for (unsigned int i = 0; i < members.size (); i++)
      {
	bool error = false;
	string name = members[i]->name ("selector");
	double from = members[i]->number ("pos");
	double to = members[i]->check ("end")
	  ? members[i]->number ("end") : 1.0;

	if (from > 0.0)
	  {
	    cerr << "Positive depth";
	    error = true;
	  }
	else if (name == "at")
	  selections.push_back (selection (At, from, to));
	else if (to > from)
	  {
	    cerr << "Region should end below start";
	    error = true;
	  }
	else if (name == "avg_content")
	  selections.push_back (selection (Average, from, to));
	else if (name == "content")
	  selections.push_back (selection (Content, from, to));
	else
	  cerr << "Unknown selector `" << name << "'";

	if (error)
	  cerr << " in output filter `" << al.name ("type") << "' ["
	       << i << "]\n";
      }
  }
};

static struct FilterArraySyntax
{
  FilterArraySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Syntax& entry_syntax = *new Syntax ();
    entry_syntax.add ("selector", Syntax::String, Syntax::Const);
    entry_syntax.add ("pos", Syntax::Number, Syntax::Const);
    entry_syntax.add ("end", Syntax::Number, Syntax::Optional);
    entry_syntax.order ("selector", "pos", "end");
    syntax.add ("members", entry_syntax, Syntax::Const, Syntax::Sequence);
    syntax.order ("members");
    Librarian<Filter>::add_type ("array", alist, syntax, &FilterArray::make);
  }
} FilterArray_syntax;
