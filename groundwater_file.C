// groundwater_file.C

#include "groundwater.h"
#include "time.h"
#include <fstream.h>

class GroundwaterFile : public Groundwater
{
  // Data.
private:
  Time previous_time;
  Time next_time;
  double previous_depth;
  double next_depth;
  double depth;

  // File.
  const string file_name;
  ifstream file;
  int line;			// Current line number in weather file.
  
  // UZbottom.
public:
  bool flux_bottom () const;
  bool accept_bottom (double);

  // Simulation.
public:
  void tick (const Time&);
  double table () const;

  // Create and Destroy.
public:
  void initialize (const Time& time);
  GroundwaterFile (const AttributeList&);
  ~GroundwaterFile ();
};

bool
GroundwaterFile::flux_bottom () const
{
  return depth > 0;		// Positive numbers indicate flux bottom.
}

bool 
GroundwaterFile::accept_bottom (double)
{
  return true;
}

void
GroundwaterFile::tick (const Time& time)
{ 
  while (next_time < time)
    {
      // Remember old value.
      previous_time = next_time;
      previous_depth = next_depth;

      // Read in new value.
      int year;
      int month; 
      int day;
      int end;

      file >> year >> month >> day >> next_depth;
      end = file.get ();
      if (year < 100)
	year += 1900;
      while (file.good () && strchr (" \t", end))
	end = file.get ();
      
      if (!file.good ())
	{
	  throw ("groundwater read error");
	}

      assert (month > 0 && month < 13);
      next_time = Time (year, month, day, 23);
    }
  // We should be somewhere in the interval.
  assert (previous_time < time || time == previous_time);
  assert (time < next_time  || time == next_time);

  // Interpolate depth values.
  const double total_interval = Time::days_between (previous_time, next_time);
  const double covered_interval = Time::days_between (previous_time, time);
  const double covered_fraction = covered_interval / total_interval;
  const double total_change = next_depth - previous_depth;
  depth = previous_depth + covered_fraction * total_change;
}

double
GroundwaterFile::table () const
{
  return depth;
}

void
GroundwaterFile::initialize (const Time& time)
{ tick (time); }

GroundwaterFile::GroundwaterFile (const AttributeList& al)
  : Groundwater (al),
    previous_time (42, 1, 1, 0),
    next_time (42, 1, 1, 0),
    previous_depth (-42.42e42),
    next_depth (-42.42e42),
    depth (-42.42e42),
    file_name (al.name ("file")),
    file (Options::find_file (al.name ("file"))),
    line (0)
{ }

GroundwaterFile::~GroundwaterFile ()
{ 
#if 0
      // Code guard claims the file handle is bad.
      close (file.rdbuf ()->fd ()); 
#endif
}

static struct GroundwaterFileSyntax
{
  static Groundwater& make (const AttributeList& al)
    { 
      return *new GroundwaterFile (al);
    }
  GroundwaterFileSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Read groundwater table from a file.");
      Groundwater::load_syntax (syntax, alist);
      syntax.add ("file", Syntax::String, Syntax::Const,
		  "Name of file to read data from.\n\
The format of each line in the file is `YEAR MONTH DAY HEIGHT',\n\
where HEIGHT should in cm above ground (i.e. a negative number).\n\
Linear interpolation is used between the datapoints.");
      syntax.order ("file");
      Librarian<Groundwater>::add_type ("file", alist, syntax, &make);
    }
} GroundwaterFile_syntax;


