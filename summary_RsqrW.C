// summary_RsqrW.C --- Calculate weighted coefficient of determination.
// 
// Copyright 2011 KU.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL

#include "summary.h"
#include "destination.h"
#include "select.h"
#include "block_submodel.h"
#include "block_model.h"
#include "librarian.h"
#include "treelog.h"
#include "time.h"
#include "memutils.h"
#include "submodeler.h"
#include "mathlib.h"
#include "vcheck.h"
#include <numeric>
#include <sstream>
#include <map>

struct SummaryRsqrW : public Summary
{
  // Content.
  struct Data
  {
    const double weight;
    const symbol tag;
    const std::vector<double> value;

    static void load_syntax (Frame&);
    Data (const BlockSubmodel&);
  };
  
  struct Measure
  {
    const Time time;
    const auto_vector<const Data*> data;

    static void load_syntax (Frame&);
    Measure (const BlockSubmodel&);
  };
  const auto_vector<const Measure*> measure;
  const bool print_data;
  mutable std::ostringstream pds;
  symbol dimension;
  symbol square_dimension;
  const double average;         // Measured average.
  const double SStot;           // Total sum of squares.

  struct Compare : public Destination
  {
    const symbol tag;
    SummaryRsqrW& summary;

    // Destination.
    void missing ()
    { }
	NORETURN void add (const std::vector<double>& value)
    { daisy_notreached (); }
    void add (const double value)
    { summary.compare (tag, value); }
	NORETURN void add (const symbol value)
    { daisy_notreached (); }

    Compare (SummaryRsqrW& sum, const symbol key)
      : tag (key),
        summary (sum)
    { }
  };
  auto_vector<const Compare*> comparison;
  void compare (symbol tag, double value);

  double SSerr;                 // Residual sum of of squares.
  size_t current;               // Next measure to compae with.
  bool active;                  // True if current measure is active.
  double R2;                    // Coefficient of determination;

  const struct MyScope : public Scope
  {
    const SummaryRsqrW& summary;
    std::map<symbol, const double*> all;
    static const struct DescMap : public std::map<symbol, symbol>
    { 
      DescMap (); 
    } descriptions;

    symbol title () const
    { return summary.objid; }

    void entries (std::set<symbol>& entries) const
    {
      for (std::map<symbol, const double*>::const_iterator i = all.begin ();
           i != all.end ();
           i++)
        entries.insert ((*i).first);
    }

    Attribute::type lookup (const symbol key) const
    {
      std::map<symbol, const double*>::const_iterator i = all.find (key);
      return (i == all.end () ? Attribute::Error : Attribute::Number);
    }

    symbol dimension (const symbol key) const
    { 
      if (key == "R2")
        return Attribute::None ();
      if (key == "average")
        return summary.dimension;
      if (all.find (key) == all.end ())
        return Attribute::Unknown ();

      return summary.square_dimension;
    }

    symbol description (const symbol key) const
    { return (*descriptions.find (key)).second; }
    int type_size (const symbol) const
    { return Attribute::Singleton; }
    bool check (const symbol key) const
    { return all.find (key) != all.end (); }
    double number (const symbol key) const
    { return *(*all.find (key)).second; }
    
    static std::map<symbol, const double*> build_all (const SummaryRsqrW& s)
    {
      std::map<symbol, const double*> all;
      all["R2"] = &s.R2;
      all["average"] = &s.average;
      all["SStot"] = &s.SStot;
      all["SSerr"] = &s.SSerr;
      return all;
    }   

    MyScope (const SummaryRsqrW& s)
      : summary (s),
        all (build_all (s))
    { }
  } scope;

  void find_scopes (std::vector<const Scope*>& scopes) const
  { scopes.push_back (&scope); }

  // Simulation.
  void tick (const Time&);

  // Create and Destroy.
  void clear ();
  void initialize (std::vector<Select*>&, Treelog&);
  bool check (Treelog& msg) const;
  static double find_average (const std::vector<const Measure*>&);
  static double find_SStot (const std::vector<const Measure*>&, double average);
  static bool by_time (const Measure* a, const Measure* b);
  static std::vector<const Measure*> find_measure (const BlockModel&);
  explicit SummaryRsqrW (const BlockModel&);
  void summarize (Treelog&) const;
};

SummaryRsqrW::MyScope::DescMap::DescMap ()
{
  (*this)["R2"] = "Coefficient of determination.";
  (*this)["average"] = "Average measured value.";
  (*this)["SStot"] = "Total sum of squares.";
  (*this)["SSerr"] = "Residual sum of squares.";
}

const SummaryRsqrW::MyScope::DescMap SummaryRsqrW::MyScope::descriptions;

void
SummaryRsqrW::Data::load_syntax (Frame& frame)
{
  frame.declare ("weight", Attribute::None (), Attribute::Const, "\
Weight given this measurement.");
  frame.declare_string ("tag", Attribute::Const, "\
Name of simulated data to compare with.");
  frame.declare ("value", Attribute::Unknown (), 
                 Attribute::Const, Attribute::Variable, "\
Measured data.");
  frame.set_check ("value", VCheck::min_size_1 ());
  frame.order ("weight", "tag", "value");
}

SummaryRsqrW::Data::Data (const BlockSubmodel& al)
  : weight (al.number ("weight")),
    tag (al.name ("tag")),
    value (al.number_sequence ("value"))
{ }

void
SummaryRsqrW::Measure::load_syntax (Frame& frame)
{
  frame.declare_submodule ("time", Attribute::OptionalConst, "\
Time of measurement.\n\
Measured data will be compared to the first simulated value at\
or after this time.\n\
By default, compare with the first available simulated data.",
                           Time::load_syntax);
  frame.declare_submodule_sequence ("data", Attribute::Const, "\
Data measured at this time.", Data::load_syntax);
  frame.set_check ("data", VCheck::min_size_1 ());
}

SummaryRsqrW::Measure::Measure (const BlockSubmodel& al)
  : time (al.check ("time") 
          ? submodel_value<Time> (al, "time")
          : Time::null ()),
    data (map_submodel_const<Data> (al, "data"))
{ }

void 
SummaryRsqrW::compare (const symbol tag, const double value)
{
  if (!active)
    return;
  
  const Measure& m = *measure[current];

  for (size_t i = 0; i < m.data.size (); i++)
    if (m.data[i]->tag == tag)
      {
        const double weight = m.data[i]->weight;
        const std::vector<double>& v = m.data[i]->value;
        for (size_t j = 0; j < v.size (); j++)
          SSerr += weight * sqr (value - v[j]);
        if (print_data)
          {
            pds << "\n" << m.time.print () << "\t" << tag << "\t" << value;
            for (size_t j = 0; j < v.size (); j++)
              pds << "\t" << v[j];
          }
        break;
      }

  R2 = 1.0 - SSerr / SStot;
}   

void 
SummaryRsqrW::tick (const Time& time)
{ 
  if (active)
    current++;

  active = (current < measure.size () && time >= measure[current]->time);
}

void
SummaryRsqrW::clear ()
{ }

void
SummaryRsqrW::initialize (std::vector<Select*>& select, Treelog& msg)
{ 
  std::set<symbol> tags;
  for (size_t i = 0; i < measure.size (); i++)
    for (size_t j = 0; j < measure[i]->data.size (); j++)
      tags.insert (measure[i]->data[j]->tag);

  for (std::set<symbol>::const_iterator i = tags.begin (); 
       i != tags.end ();
       i++)
    {
      const symbol tag = *i;
      bool found = false;
      for (std::vector<Select*>::iterator j = select.begin (); 
           j != select.end (); 
           j++)
        {
          Select& s = **j;
          if (tag != s.tag ())
            continue;
          if (found)
            msg.warning ("Duplicate tag '" + tag + "' ignored");
          else
            {
              found = true;
              Compare* c = new Compare (*this, *i);
              comparison.push_back (c);
              s.add_dest (c);
            }
          if (s.dimension () == Attribute::Unknown ())
            msg.warning ("'" + tag + "' has unknown dimension");
          else if (dimension == Attribute::Unknown ())
            dimension = s.dimension ();
          else if (dimension != s.dimension ())
            msg.warning ("Conflicting dimensions [" + dimension 
                         + "] and [" + s.dimension () + "] for '" + tag + "'");
        }
      if (!found)
        msg.warning ("Tag '" + tag + "' not found");
    }
  square_dimension = Units::multiply (dimension, dimension);
}

bool 
SummaryRsqrW::check (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  bool ok = true;
  return ok;
}

double 
SummaryRsqrW::find_average (const std::vector<const Measure*>& measure)
{ 
  size_t count = 0;
  double sum = 0.0;
  
  for (size_t i = 0; i < measure.size (); i++)
    for (size_t j = 0; j < measure[i]->data.size (); j++)
      {
        count += measure[i]->data[j]->value.size ();
        sum += std::accumulate (measure[i]->data[j]->value.begin (),
                                measure[i]->data[j]->value.end (), 0.0);
      }

  daisy_assert (count > 0);
  return sum / (count + 0.0);
}

double 
SummaryRsqrW::find_SStot (const std::vector<const Measure*>& measure,
                         double average)
{
  double sum = 0.0;
  
  for (size_t i = 0; i < measure.size (); i++)
    {
      const Measure& m = *measure[i];
      for (size_t j = 0; j < m.data.size (); j++)
        for (size_t k = 0; k < m.data[j]->value.size (); k++)
          sum += m.data[j]->weight * sqr (m.data[j]->value[k] - average);
    }

 return sum;
}

bool
SummaryRsqrW::by_time (const Measure* a, const Measure* b)
{ return a->time < b->time; }

std::vector<const SummaryRsqrW::Measure*>
SummaryRsqrW::find_measure (const BlockModel& al)
{
  std::vector<const Measure*> result 
    = map_submodel_const<Measure> (al, "measure");
  std::sort (result.begin (), result.end (), by_time);
  return result;
}

SummaryRsqrW::SummaryRsqrW (const BlockModel& al)
  : Summary (al),
    measure (find_measure (al)),
    print_data (al.flag ("print_data")),
    dimension (Attribute::Unknown ()),
    square_dimension (Attribute::Unknown ()),
    average (find_average (measure)),
    SStot (find_SStot (measure, average)),
    SSerr (0.0),
    current (0),
    active (false),
    scope (*this)
{ 
  if (print_data)
    pds << "Time\tTag\tSim\tObs...";
}

void 
SummaryRsqrW::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  if (print_data)
    pds << "\n\n";
  pds << "average = " << average << "\n"
      << "SStot = " << SStot << "\n"
      << "SSerr = " << SSerr << "\n"
      << "R^2 = " << R2;
  msg.message (pds.str ());
}

static struct SummaryRsqrWSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SummaryRsqrW (al); }
  SummaryRsqrWSyntax ()
    : DeclareModel (Summary::component, "RsqrW", "\
Calculate coefficient of determination.")
  { }
  static bool check_alist (const Metalib&, const Frame& frame, Treelog& msg)
  {
    bool ok = true;

    const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& measure 
      = frame.submodel_sequence ("measure");
    std::set<Time> found;
    for (size_t i = 0; i < measure.size (); i++)
      {
        if (!measure[i]->check ("time"))
          {
            msg.error ("All measures must contain time");
            return false;
          }
        const Time time (measure[i]->submodel ("time"));
        const std::set<Time>::const_iterator j = found.find (time);
        if (j != found.end ())
          {
            msg.error (time.print () + ": duplicate time");
            ok = false;
          }
        else
          found.insert (time);
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare_submodule_sequence ("measure", Attribute::Const, "\
Measured data.", 
                                      SummaryRsqrW::Measure::load_syntax);
    frame.set_check ("measure", VCheck::min_size_1 ());
    frame.declare_boolean ("print_data", Attribute::Const, "\
Print a table with all data in the summary.");
    frame.set ("print_data", false);
  }
} SummaryRsqrW_syntax;

// summary_RsqrW.C ends here.
