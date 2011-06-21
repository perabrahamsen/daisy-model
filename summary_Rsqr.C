// summary_Rsqr.C --- Calculate coefficient of determination.
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
#include "block_submodel.h"
#include "block_model.h"
#include "librarian.h"
#include "treelog.h"
#include "time.h"
#include "memutils.h"
#include "submodeler.h"
#include "mathlib.h"
#include "vcheck.h"
#include <boost/scoped_ptr.hpp>
#include <numeric>
#include <sstream>

struct SummaryRsqr : public Summary
{
  // Content.
  struct Data
  {
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
  const double average;         // Measured average.
  const double SStot;           // Total sum of squares.
  double SSerr;                 // Residual sum of of squares.
  
  // Create and Destroy.
  void clear ();
  void initialize (std::vector<Select*>&, Treelog&);
  bool check (Treelog& msg) const;
  static double find_average (const std::vector<const Measure*>&);
  static double find_SStot (const std::vector<const Measure*>&, double average);
  explicit SummaryRsqr (const BlockModel&);
  void summarize (Treelog&) const;
};

void
SummaryRsqr::Data::load_syntax (Frame& frame)
{
  frame.declare_string ("tag", Attribute::Const, "\
Name of simulated data to compare with.");
  frame.declare ("value", Attribute::Unknown (), 
                 Attribute::Const, Attribute::Variable, "\
Measured data.");
  frame.set_check ("value", VCheck::min_size_1 ());
  frame.order ("tag", "value");
}

SummaryRsqr::Data::Data (const BlockSubmodel& al)
  : tag (al.name ("tag")),
    value (al.number_sequence ("value"))
{ }

void
SummaryRsqr::Measure::load_syntax (Frame& frame)
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

SummaryRsqr::Measure::Measure (const BlockSubmodel& al)
  : time (al.check ("time") 
          ? submodel_value<Time> (al, "time")
          : Time::null ()),
    data (map_submodel_const<Data> (al, "data"))
{ }

void
SummaryRsqr::clear ()
{ SSerr = 0.0; }

void
SummaryRsqr::initialize (std::vector<Select*>& select, Treelog& msg)
{ 
  // TREELOG_MODEL (msg);
}

bool 
SummaryRsqr::check (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  bool ok = true;
  return ok;
}

double 
SummaryRsqr::find_average (const std::vector<const Measure*>& measure)
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
SummaryRsqr::find_SStot (const std::vector<const Measure*>& measure,
                         double average)
{
  double sum = 0.0;
  
  for (size_t i = 0; i < measure.size (); i++)
    for (size_t j = 0; j < measure[i]->data.size (); j++)
      for (size_t k = 0; k < measure[i]->data[j]->value.size (); k++)
        sum += sqr (measure[i]->data[j]->value[k] - average);

 return sum;
}

SummaryRsqr::SummaryRsqr (const BlockModel& al)
  : Summary (al),
    measure (map_submodel_const<Measure> (al, "measure")),
    average (find_average (measure)),
    SStot (find_SStot (measure, average)),
    SSerr (0.0)
{ }

void 
SummaryRsqr::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  std::ostringstream tmp;
  tmp << "average = " << average << "\n"
      << "SStot = " << SStot << "\n"
      << "SSerr = " << SSerr << "\n"
      << "R^2 = " << (1.0 - SSerr / SStot);
  msg.message (tmp.str ());
}

static struct SummaryRsqrSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SummaryRsqr (al); }
  SummaryRsqrSyntax ()
    : DeclareModel (Summary::component, "Rsqr", "\
Calculate coefficient of determination.")
  { }
  static bool check_alist (const Metalib&, const Frame& frame, Treelog& msg)
  {
    bool ok = true;

    const std::vector<boost::shared_ptr<const FrameSubmodel>/**/>& measure 
      = frame.submodel_sequence ("measure");
    if (measure.size () > 1)
      {
        for (size_t i = 0; i < measure.size (); i++)
          if (!measure[i]->check ("time"))
            {
              msg.error ("All measures must contain time");
              return false;
            }
        Time last (measure[0]->submodel ("time"));
        for (size_t i = 1; i < measure.size (); i++)
          {
            Time next (measure[1]->submodel ("time"));
            if (next <= last)
              {
                msg.error ("Measurement time must be increasing");
                ok = false;
                break;
              }
          }
      }
    return ok;
  }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare_submodule_sequence ("measure", Attribute::Const, "\
Measured data.", 
                                      SummaryRsqr::Measure::load_syntax);
    frame.set_check ("measure", VCheck::min_size_1 ());
  }
} SummaryRsqr_syntax;

// summary_Rsqr.C ends here.
