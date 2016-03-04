// number_source.C -- Extract a single number from a time series.
// 
// Copyright 2005 Per Abrahamsen and KVL.
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

#include "number.h"
#include "block_model.h"
#include "source.h"
#include "assertion.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include <sstream>
#include <memory>

struct NumberSource : public Number
{
  const std::unique_ptr<Source> source;
  const std::unique_ptr<const Time> begin;
  const std::unique_ptr<const Time> end;
  enum { uninitialized, error, is_missing, has_value } state;
  double val;

  symbol title () const
  {
    daisy_assert (state != uninitialized);
    return source->title ();
  }
  void tick (const Units&, const Scope&, Treelog&)
  { }
  bool missing (const Scope&) const 
  { 
    daisy_assert (state != uninitialized);
    return state != has_value;
  }
  double value (const Scope&) const
  { 
    daisy_assert (state == has_value);
    return val;
  }
  symbol dimension (const Scope&) const 
  {     
    daisy_assert (state != uninitialized);
    return symbol (source->dimension ());
  }

  // Create.
  virtual void initialize_derived (Treelog& msg) = 0;
  bool initialize (const Units& units, const Scope& scope, Treelog& msg)
  {
    TREELOG_MODEL (msg);
    msg.touch ();
    daisy_assert (state == uninitialized);
    daisy_assert (source.get ());
    if (!source->load (msg))
      state = error;
    else
      initialize_derived (msg);
    return state != error;
  }
  bool check (const Units&, const Scope&, Treelog&) const
  { 
    daisy_assert (state != uninitialized);
    return state != error; 
  }
  NumberSource (const BlockModel& al)
    : Number (al),
      source (Librarian::build_item<Source> (al, "source")),
      begin (al.check ("begin") ? new Time (al.submodel ("begin")) : NULL),
      end (al.check ("end") ? new Time (al.submodel ("end")) : NULL),
      state (uninitialized),
      val (-42.42e42)
  { }
};

static struct NumberSourceSyntax : public DeclareBase
{
  NumberSourceSyntax ()
    : DeclareBase (Number::component, "source", 
                   "Extract information from a time series.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("source", Source::component, "\
The time series we want to extract a number from.");
    frame.declare_submodule ("begin", Attribute::OptionalConst,
			  "Ignore values before or at this date.", 
                          Time::load_syntax);
    frame.declare_submodule ("end", Attribute::OptionalConst,
			  "Ignore values after this date.", Time::load_syntax);
  }
} NumberSource_syntax;


struct NumberSourceUnique : public NumberSource
{
  void initialize_derived (Treelog& msg)
  {
    const std::vector<Time>& time = source->time ();
    const size_t size = time.size ();
    int count = 0;
    for (size_t i = 0; i < size; i++)
      if ((!begin.get () || time[i] > *begin) 
          && (!end.get () || time[i] <= *end))
        {
          val = source->value ()[i];
          count++;
        }

    if (count == 1U)
      state = has_value;
    else if (count == 0U)
      {
        msg.warning ("Got zero elements, expected one");
        state = is_missing;
      }
    else
      {
        std::ostringstream tmp;
        tmp << "Got " << count << " elements, expected 1";
        msg.error (tmp.str ());
        state = error;
      }
  }
  NumberSourceUnique (const BlockModel& al)
    : NumberSource (al)
  { }
};

static struct NumberSourceUniqueSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSourceUnique (al); }
  NumberSourceUniqueSyntax ()
    : DeclareModel (Number::component, "source_unique", "source",
	       "Find unique number in time series.")
  { }
  void load_frame (Frame&) const
  { }
} NumberSourceUnique_syntax;

struct NumberSourceAverage : public NumberSource
{
  void initialize_derived (Treelog& msg)
  {
    const std::vector<Time>& time = source->time ();
    const size_t size = time.size ();
    int count = 0;
    val = 0.0;
    for (size_t i = 0; i < size; i++)
      if ((!begin.get () || time[i] > *begin) 
          && (!end.get () || time[i] <= *end))
        {
          val += source->value ()[i];
          count++;
        }

    if (count == 0U)
      {
        msg.warning ("Can't take average of zero elements");
        state = is_missing;
      }
    else
      {
        val /= (count + 0.0);
        state = has_value;
      }
  }
  NumberSourceAverage (const BlockModel& al)
    : NumberSource (al)
  { }
};

static struct NumberSourceAverageSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSourceAverage (al); }
  NumberSourceAverageSyntax ()
    : DeclareModel (Number::component, "source_average", "source",
                    "Find average number in time series.")
  { }
  void load_frame (Frame&) const
  { }
} NumberSourceAverage_syntax;

struct NumberSourceSum : public NumberSource
{
  void initialize_derived (Treelog&)
  {
    const std::vector<Time>& time = source->time ();
    const size_t size = time.size ();
    val = 0.0;
    state = has_value;
    for (size_t i = 0; i < size; i++)
      if ((!begin.get () || time[i] > *begin)
          && (!end.get () || time[i] <= *end))
        val += source->value ()[i];
  }
  NumberSourceSum (const BlockModel& al)
    : NumberSource (al)
  { }
};

static struct NumberSourceSumSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSourceSum (al); }
  NumberSourceSumSyntax ()
    : DeclareModel (Number::component, "source_sum", "source", 
	       "Calculate the sum of the values in a time series.")
  { }
  void load_frame (Frame&) const
  { }
} NumberSourceSum_syntax;

struct NumberSourceIncrease : public NumberSource
{
  void initialize_derived (Treelog& msg)
  {
    const std::vector<Time>& time = source->time ();
    const std::vector<double>& value = source->value ();
    const size_t size = time.size ();
    daisy_assert (value.size () == size);
    state = has_value;
    if (size < 2)
      {
         msg.warning ("Need two elements to make a difference");
         val = 0;
         return;
      }
    double first = value[0];
    double last = value[0];
    for (size_t i = 1; i < size; i++)
      {
        if (begin.get () && time[i] < *begin)
          first = value[i];
        if (!end.get () || time[i] <= *end)
          last = value[i];
      }
    val = last - first;
  }
  NumberSourceIncrease (const BlockModel& al)
    : NumberSource (al)
  { }
};

static struct NumberSourceIncreaseSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NumberSourceIncrease (al); }
  NumberSourceIncreaseSyntax ()
    : DeclareModel (Number::component, "source_increase", "source",
	       "Find increase in value during time series.")
  { }
  void load_frame (Frame&) const
  { }
} NumberSourceIncrease_syntax;

// number_source.C ends here
