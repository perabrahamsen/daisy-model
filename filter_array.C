// filter_array.C

#include "filter.h"
#include "geometry.h"

class FilterArray : public Filter
{
  // Content.
  enum selector { At, Density, Content };
  enum accumulator { Now, Sum, Average, Accumulate };
  struct selection 
  {
    accumulator acc;
    selector sel;
    double from;
    double to;
    double sum;
    int ticks;
    
    selection (accumulator a, selector s, double f, double t)
      : acc (a),
	sel (s),
	from (f),
	to (t),
	sum (0.0),
	ticks (0)
    { }
    selection ()
      : acc (accumulator (-42)),
	sel (selector (-117)),
	from (42.42e42),
	to (13.13e13),
	sum (-42.42e42),
	ticks (-42117)
    { }
  };
  vector<selection> selections;
    
  // Use.
public:
  bool check (const string&, bool) const;
  Filter& lookup (const string&) const;

  bool accumulating () const;
  void accumulate (const Geometry&, const vector<double>&);
  const vector<double> select (const Geometry&, const vector<double>&);

  // Create and Destroy.
public:
  bool check (const Library&, int /* size */) const;
  bool check (const Syntax&, int /* size */) const;
  bool check (const Syntax::type type, int size) const;

  static Filter& make (const AttributeList& al);
  FilterArray (const AttributeList& al);
  ~FilterArray () { }
};

#ifdef BORLAND_PRAGMA
#pragma warn -rvl
#endif
bool 
FilterArray::check (const string&, bool) const
{ 
  assert (false); 
}

Filter& 
FilterArray::lookup (const string&) const
{ 
  assert (false); 
}

#ifdef BORLAND_PRAGMA
#pragma warn +rvl
#endif
bool
FilterArray::accumulating () const
{
  for (unsigned int i = 0; i < selections.size (); i++)
    {
      if (selections[i].acc != Now)
	return true;
    }
  return false;
}

void
FilterArray::accumulate (const Geometry& geometry, const vector<double>& value)
{ 
  for (unsigned int i = 0; i < selections.size (); i++)
    {
      const accumulator acc = selections[i].acc;
      const selector sel = selections[i].sel;
      const double from = selections[i].from;
      const double to = selections[i].to;
      double& sum = selections[i].sum;

      double answer;

      switch (sel)
	{
	case At:
	  answer = value[geometry.interval_plus (from)];
	  break;
	case Density:
	  answer = geometry.total (value, from, to) / (from - to);
	  break;
	case Content:
	  answer = geometry.total (value, from, to);
	  break;
	default:
	  assert (false);
	}
      switch (acc)
	{
	case Now:
	  sum = answer;
	  break;
	case Average:
	  selections[i].ticks++;
	  sum += answer;
	  break;
	case Sum:
	case Accumulate:
	  sum += answer;
	  break;
	default: 
	  assert (false);
	}
    }
}

const vector<double>
FilterArray::select (const Geometry& geometry, const vector<double>& value)
{ 
  accumulate (geometry, value);

  vector<double> result;
  for (unsigned int i = 0; i < selections.size (); i++)
    {
      const accumulator acc = selections[i].acc;
      double& sum = selections[i].sum;
      int& ticks = selections[i].ticks;

      double answer;

      switch (acc)
	{
	case Now:
	  answer = sum;
	  break;
	case Sum:
	  answer = sum;
	  sum = 0.0;
	  break;
	case Average:
	  answer = sum / (ticks + 0.0);
	  sum = 0.0;
	  ticks = 0;
	  break;
	case Accumulate:
	  answer = sum;
	  break;
	default: 
	  assert (false);
	}
      result.push_back (answer);
    }
  return result;
}

bool 
FilterArray::check (const Library&, int /* size */) const
{
  CERR << "This filter works only array of numbers\n";
  return false;
}

bool 
FilterArray::check (const Syntax&, int /* size */) const
{
  CERR << "This filter works only array of numbers\n";
  return false;
}

bool 
FilterArray::check (const Syntax::type type, int size) const
{
  if (type != Syntax::Number || size != Syntax::Sequence)
    {
      CERR << "This filter works only array of numbers\n";
      return false;
    }
  return true;
}

Filter& 
FilterArray::make (const AttributeList& al)
{ return *new FilterArray (al); }

FilterArray::FilterArray (const AttributeList& al)
{ 
  const vector<AttributeList*>& members = al.alist_sequence ("members");
  for (unsigned int i = 0; i < members.size (); i++)
    {
      bool error = false;
      string acc_name = members[i]->name ("accumulator");
      string sel_name = members[i]->name ("selector");
      accumulator acc = Now;
      selector sel = At;
      double from = members[i]->number ("pos");
      double to = members[i]->check ("end") ? members[i]->number ("end") : 1.0;

      if (acc_name == "now")
	acc = Now;
      else if (acc_name == "sum")
	acc = Sum;
      else if (acc_name == "average")
	acc = Average;
      else if (acc_name == "accumulate")
	acc = Accumulate;
      else
	{
	  CERR << "Unknown accumulator `" << acc_name << "'";
	  error = true;
	}

      if (from > 0.0)
	{
	  CERR << "Positive depth";
	  error = true;
	}
      else if (sel_name == "at")
	sel = At;
      else if (to > from)
	{
	  CERR << "Region should end below start";
	  error = true;
	}
      else if (sel_name == "density")
	sel = Density;
      else if (sel_name == "content")
	sel = Content;
      else
	{
	  CERR << "Unknown selector `" << sel_name << "'";
	  error = true;
	}

      if (error)
	CERR << " in output filter `" << al.name ("type") << "' ["
	     << i << "]\n";
      else
	selections.push_back (selection (acc, sel, from, to));
    }
}

static struct FilterArraySyntax
{
  FilterArraySyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Summarize number sequences.");
    Syntax& entry_syntax = *new Syntax ();
    entry_syntax.add ("accumulator", Syntax::String, Syntax::Const,
		      "How to accumulate the value.  Possible values are:\n\
Now: Log the latest value.\n\
Average: Log the average value since last entry.\n\
Sum: Log the sum since last entry.\n\
Accumulate: Log the accumulated value since the start of the simulation.");
    entry_syntax.add ("selector", Syntax::String, Syntax::Const, 
		      "How to select from the array.  Possible values are:\n\
At: Log the value at `pos'.\n\
Density: Log the average density between `pos' and `end'.\n\
Content: Log the total content between `pos' and `end'.");
    entry_syntax.add ("pos", "cm", Syntax::Const, 
		      "Depth to start logging (a negative number).");
    entry_syntax.add ("end", "cm", Syntax::OptionalConst,
		      "Depth to end logging (a negative number).\n\
Is not used together the `At' selected.");
    entry_syntax.order ("accumulator", "selector", "pos", "end");
    syntax.add ("members", entry_syntax, Syntax::Const, Syntax::Sequence,
		"Each member describe an entry in the log.");
    syntax.order ("members");
    Librarian<Filter>::add_type ("array", alist, syntax, &FilterArray::make);
  }
} FilterArray_syntax;
