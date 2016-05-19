// summary_fractiles.C --- Show fractiles of data.
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

struct SummaryFractiles : public Summary
{
  struct Data : public Destination
  {
    const symbol tag;
    mutable std::vector<double> data;
    
    // Destination.
    void missing ()
    { }
    void add (const std::vector<double>& value) NORETURN
    { daisy_notreached (); }
    void add (const double value)
    { data.push_back (value); }
    void add (const symbol value) NORETURN
    { daisy_notreached (); }

    explicit Data (const symbol key)
      : tag (key)
    { }
  };
  std::vector<Data> data;
  const std::vector<double> fractiles;
  
  // Create and Destroy.
  void clear ();
  void initialize (std::vector<Select*>&, Treelog&);
  bool check (Treelog& msg) const;
  static std::vector<Data> find_data (const BlockModel& al);
  explicit SummaryFractiles (const BlockModel& al)
    : Summary (al),
      data (find_data (al)),
      fractiles (al.number_sequence ("fractiles"))
  { }
  static double find_fractile (const double f, const std::vector<double>& data);
  void summarize (Treelog&) const;
};

void
SummaryFractiles::clear ()
{ }

void
SummaryFractiles::initialize (std::vector<Select*>& select, Treelog& msg)
{
  std::map<symbol, Data*> all;
  for (auto& datum : data)
    all[datum.tag] = &datum;

  std::set<symbol> found;
  for (auto& s : select)
    {
      const symbol tag = s->tag ();
      const auto i = all.find (tag);
      if (i == all.end ())
        continue;
      if (found.find (tag) != found.end ())
        msg.warning ("Duplicate tag '" + tag + "'");
      else
        found.insert (tag);
      s->add_dest (i->second);
      all.erase (i);
    }
  if (all.size () == 0)
    return;
  for (auto& i : all)
    msg.warning ("tag: '" + i.first + "' not found");
}

bool 
SummaryFractiles::check (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  bool ok = true;
  return ok;
}

std::vector<SummaryFractiles::Data>
SummaryFractiles::find_data (const BlockModel& al)
{
  std::vector<Data> data;
  std::vector<symbol> tags = al.name_sequence ("tags");
  for (auto tag : tags)
    data.push_back (Data (tag));
  return data;
}

double
SummaryFractiles::find_fractile (const double f,
                                 const std::vector<double>& data)
{
  daisy_assert (f >= 0.0 && f <= 1.0);
  const size_t i = f * data.size ();
  daisy_assert (i < data.size ());
  return data[i];
}

void 
SummaryFractiles::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  std::ostringstream tmp;
  tmp << "\tn";
  for (auto fractile : fractiles)
    tmp << "\t" << fractile;
  for (auto& datum : data)
    {
      std::sort (datum.data.begin (), datum.data.end ());
      const size_t n = datum.data.size ();
      tmp << "\n" << datum.tag << "\t" << n;
      for (size_t i = 0; i < fractiles.size (); i++)
        if (n > 0)
          tmp << "\t" << find_fractile (fractiles[i], datum.data);
        else
          tmp << "\tn/a";
    }
  msg.message (tmp.str ());
}

static struct SummaryFractilesSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new SummaryFractiles (al); }
  SummaryFractilesSyntax ()
    : DeclareModel (Summary::component, "fractiles", "\
Show fractiles for specified tags.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("tags", Attribute::Const, Attribute::Variable, "\
List of tags to summarize.");
    frame.declare_fraction ("fractiles",
                            Attribute::Const, Attribute::Variable, "\
List of fractiles to summarize.");
  }
} SummaryFractiles_syntax;

// summary_fractiles.C ends here.
