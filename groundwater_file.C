// groundwater_file.C

#include "groundwater.h"
#include "lexer_data.h"
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
  LexerData* lex;
  
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
  void initialize (const Time& time, const Soil&, Treelog&);
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
  assert (lex);
  while (next_time < time)
    {
      if (!lex->good ())
	throw ("groundwater file read error");

      // Remember old value.
      previous_time = next_time;
      previous_depth = next_depth;

      // Read in new value.
      int year;
      int month;
      int day;

      year = lex->get_cardinal ();
      if (year < 0 || year > 9999)
	lex->error ("Bad year");
      if (year < 100)
	year += 1900;
      lex->skip_space ();
      month = lex->get_cardinal ();
      if (month < 1 || month > 12)
	lex->error ("Bad month");
      lex->skip_space ();
      day = lex->get_cardinal ();
      if (day < 1 || day > 31)
	lex->error ("Bad day");
      lex->skip_space ();
      next_depth = lex->get_number ();
      if (next_depth > 0.0)
	lex->error ("positive depth");
      if (!Time::valid (year, month, day, 23))
	{
	  lex->error ("Bad date");
	  lex->skip_line ();
	  lex->next_line ();
	  next_time = previous_time;
	  next_depth = previous_depth;
	  continue;
	}

      lex->next_line ();
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
GroundwaterFile::initialize (const Time& time, const Soil&, Treelog& err)
{
  assert (lex == NULL);
  lex = new LexerData (file_name, err);
  tick (time); 
}

GroundwaterFile::GroundwaterFile (const AttributeList& al)
  : Groundwater (al),
    previous_time (42, 1, 1, 0),
    next_time (42, 1, 1, 0),
    previous_depth (-42.42e42),
    next_depth (-42.42e42),
    depth (-42.42e42),
    file_name (al.name ("file")),
    lex (NULL)
{ }

GroundwaterFile::~GroundwaterFile ()
{ 
  delete lex;
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


