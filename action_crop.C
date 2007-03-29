// action_crop.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "action.h"
#include "daisy.h"
#include "field.h"
#include "crop.h"
#include "time.h"
#include "am.h"
#include "log.h"
#include "harvest.h"
#include "check_range.h"
#include "im.h"
#include "submodeler.h"
#include "vcheck.h"
#include "memutils.h"
#include "mathlib.h"
#include <sstream>

using namespace std;

struct ActionCrop : public Action
{
  // Submodules.
  struct MM_DD			// Dates.
  {
    // Parameters.
    const int month;
    const int day;
    const int hour;

    // Simulation.
    bool match (const Time& time) const;
    
    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    MM_DD (const AttributeList&);
    ~MM_DD ();
  };

  struct Sow			// Sowing.
  {
    // Parameters.
    const MM_DD date;
    const AttributeList& crop;
    
    // State.
    bool done;
  
    // Simulation.
    void doIt (Daisy&, Treelog&);
    void output (Log&) const;

    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    Sow (const AttributeList&);
    ~Sow ();
  };
  Sow *const primary;
  Sow *const secondary;

  struct Annual			// Annual harvest.
  {
    // Parameters.
    MM_DD latest;
    const double loss;
    const bool remove_residuals;

    // State.
    bool done;
  
    // Simulation.
    bool doIt (Daisy&, Treelog&, symbol name);
    void output (Log&) const;

    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    Annual (const AttributeList&);
    ~Annual ();
  };
  Annual *const harvest_annual;

  struct Perennial		// Perinnial harvest.
  {
    // Content.
    const int seasons;
    MM_DD end;
    const double DS;
    const double DM;
    int year_of_last_harvest;
    const vector<AttributeList*> *const fertilize;
    int fertilize_index;
    const vector<AttributeList*> *const fertilize_rest;
    int fertilize_rest_index;
    int fertilize_year;

    // Simulation.
  private:
    void harvest (Daisy&, Treelog&);
  public:
    bool doIt (Daisy&, Treelog&, symbol name);
    bool doIt (Daisy&, Treelog&, symbol primary, symbol secondary);
    bool done (const Daisy&, Treelog&) const;
    void output (Log&) const;

    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    Perennial (const AttributeList&);
    ~Perennial ();
  };
  Perennial *const harvest_perennial;

  struct Fertilize		// Fertilizing.
  {
    const int month;
    const int day;
    const AttributeList& what;
    
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    Fertilize (const AttributeList&);
    ~Fertilize ();
  };
  const vector <const Fertilize*> fertilize_at;
  int fertilize_at_index;
  const bool fertilize_incorporate;
  void fertilize (Daisy& daisy, Treelog&, const AttributeList& am) const;

  struct Tillage		// Tillage operations.
  {
    // Content.
    const int month;
    const int day;
    auto_ptr<Action> operation;

    // Simulation.
    void output (Log&) const;

    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    Tillage (Block&);
    ~Tillage ();
  };
  const vector<const Tillage*> tillage;
  int tillage_index;

  struct Spray		// Spray operations.
  {
    // Content.
    const int month;
    const int day;
    const symbol name;
    const double amount;
    
    // Simulation.
    void output (Log&) const;

    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    Spray (const AttributeList&);
    ~Spray ();
  };
  const vector<const Spray*> spray;
  int spray_index;

  struct Irrigation		// Irrigation.
  {
    // Content.
    const MM_DD from;
    const MM_DD to;
    const double amount;
    const double potential;

    // Simulation.
    bool doIt (Daisy&, Treelog&) const;

    // Create and Destroy.
    static void load_syntax (Syntax&, AttributeList&);
    Irrigation (const AttributeList&);
    ~Irrigation ();
  };
  const Irrigation *const irrigation;
  const Irrigation *const irrigation_rest;
  int irrigation_year;
  int irrigation_delay;

  // Simulation.
  void tick (const Daisy&, Treelog&);
  void doIt (Daisy&, Treelog&);
  bool done (const Daisy&, Treelog&) const;
  void output (Log&) const;

  // Create and Destroy.
  ActionCrop (Block& al);
  ~ActionCrop ();
};

bool 
ActionCrop::MM_DD::match (const Time& time) const
{ return time.month () == month 
    && time.mday () == day 
    && time.hour () == hour; }

bool 
ActionCrop::MM_DD::check_alist (const AttributeList& alist, Treelog& err)
{
  bool ok = true;

  const int mm = alist.integer ("month");
  const int dd = alist.integer ("day");
  const int hh = alist.integer ("hour");

  if (mm < 1 || mm > 12)
    {
      err.entry ("month should be between 1 and 12");
      ok = false;
    }
  // don't test for bad month.
  else if (dd < 1 || dd > Time::month_length (1 /* not a leap year */, mm))
    {
      ostringstream tmp;
      tmp << "day should be between 1 and " << Time::month_length (1, mm);
      err.entry (tmp.str ());
      ok = false;
    }
  if (hh < 0 || hh > 23)
    {
      err.entry ("hour should be between 0 and 23");
      ok = false;
    }
  return ok;
}

void 
ActionCrop::MM_DD::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add_check (check_alist);
  syntax.add ("month", Syntax::Integer, Syntax::Const, 
	      "Month in the year.");
  syntax.add ("day", Syntax::Integer, Syntax::Const, 
	      "Day in the month.");
  syntax.add ("hour", Syntax::Integer, Syntax::Const, 
	      "Hour in the day.");
  alist.add ("hour", 8);
  syntax.order ("month", "day");
}

ActionCrop::MM_DD::MM_DD (const AttributeList& al)
  : month (al.integer ("month")),
    day (al.integer ("day")),
    hour (al.integer ("hour"))
{ }

ActionCrop::MM_DD::~MM_DD ()
{ }

void 
ActionCrop::tick (const Daisy& daisy, Treelog& out)
{ 
  for (vector<const Tillage*>::const_iterator i = tillage.begin ();
       i != tillage.end ();
       i++)
    (*i)->operation->tick (daisy, out);
}

void 
ActionCrop::Sow::doIt (Daisy& daisy, Treelog& out)
{
  if (!done && date.match (daisy.time))
    {
      out.message ("Sowing " + crop.name ("type"));      
      daisy.field->sow (daisy.metalib, crop, daisy.time, daisy.dt, out); 
      done = true;
    }
}
void 
ActionCrop::Sow::output (Log& log) const
{
  output_variable (done, log);
}

bool 
ActionCrop::Sow::check_alist (const AttributeList&, Treelog&)
{
  bool ok = true;
  return ok;
}

void 
ActionCrop::Sow::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  syntax.add_submodule ("date", alist, Syntax::Const, "Date to sow.",
			MM_DD::load_syntax);
  syntax.add_object ("crop", Librarian<Crop>::library (), "Crop to sow.");
  syntax.add ("done", Syntax::Boolean, Syntax::State, 
	      "True iff the crop has been sowed.");
  alist.add ("done", false);
}

ActionCrop::Sow::Sow (const AttributeList& al)
  : date (al.alist ("date")),
    crop (al.alist ("crop")),
    done (al.flag ("done"))
{ }

ActionCrop::Sow::~Sow ()
{ }

bool
ActionCrop::Annual::doIt (Daisy& daisy, Treelog& out, symbol name)
{
  if (!done && (daisy.field->crop_ds (name) >= 2.0
		|| latest.match (daisy.time)))
    {
      const double stub = 8.0;
      const double stem = remove_residuals ? 1.0 : 0.0;
      const double leaf = remove_residuals ? 1.0 : 0.0;
      const double sorg = (1.0 - loss);
      static const symbol all_symbol ("all");
      daisy.field->harvest (daisy.time, daisy.dt, 
                           all_symbol, stub, stem, leaf, sorg, 
			   false, daisy.harvest, out);
      out.message ("Annual harvest of " + name);
      done = true;
      return true;
    }
  return false;
}

void 
ActionCrop::Annual::output (Log& log) const
{
  output_variable (done, log);
}

bool 
ActionCrop::Annual::check_alist (const AttributeList&, Treelog&)
{
  bool ok = true;
  return ok;
}

void 
ActionCrop::Annual::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  syntax.add_fraction ("loss", Syntax::Const, "Fraction lost during harvest.");
  syntax.add ("remove_residuals", Syntax::Boolean, Syntax::Const,
	      "Remove residuals at harvest.");
  syntax.add_submodule ("latest", alist, Syntax::Const, 
			"Latest harvest date.\n\
If the crop is ripe before this date, it will be harvested at that point.",
			MM_DD::load_syntax);
  syntax.add ("done", Syntax::Boolean, Syntax::State, 
	      "True iff the crop has been sowed.");
  alist.add ("done", false);
}

ActionCrop::Annual::Annual (const AttributeList& al)
  : latest (al.alist ("latest")),
    loss (al.number ("loss")),
    remove_residuals (al.flag ("remove_residuals")),
    done (al.flag ("done"))
{ }

ActionCrop::Annual::~Annual ()
{ }

void
ActionCrop::Perennial::harvest (Daisy& daisy, Treelog& out)
{
  const double stub = 8.0;
  const double stem = 1.0;
  const double leaf = 1.0;
  const double sorg = 1.0;
  static const symbol all_symbol ("all");
  daisy.field->harvest (daisy.time, daisy.dt,
                       all_symbol, stub, stem, leaf, sorg, 
		       false, daisy.harvest, out);
  out.message ("Perennial harvest");
}

bool
ActionCrop::Perennial::doIt (Daisy& daisy, Treelog& out, symbol name)
{
  const double stub = 8.0;

  if (year_of_last_harvest < 0)
    year_of_last_harvest = daisy.time.year () + seasons - 1;

  if (daisy.field->crop_ds (name) >= DS 
      || daisy.field->crop_dm (name, stub) >= DM)
    {
      harvest (daisy, out);
      return true;
    }
  return false;
}

bool
ActionCrop::Perennial::doIt (Daisy& daisy, Treelog& out,
			     symbol primary, symbol secondary)
{
  const double stub = 8.0;

  if (year_of_last_harvest < 0)
    year_of_last_harvest = daisy.time.year () + seasons - 1;

  if (daisy.field->crop_ds (primary) >= DS 
      || daisy.field->crop_ds (secondary) >= DS 
      || (daisy.field->crop_dm (primary, stub)
	  + daisy.field->crop_dm (secondary, stub)) >= DM)
    {
      harvest (daisy, out);
      return true;
    }
  return false;
}

bool
ActionCrop::Perennial::done (const Daisy& daisy, Treelog&) const
{ 
  if (year_of_last_harvest < 0)
    return false;

  daisy_assert (daisy.time.year () <= year_of_last_harvest);
  return daisy.time.year () == year_of_last_harvest && end.match (daisy.time);
}

void 
ActionCrop::Perennial::output (Log& log) const
{
  if (year_of_last_harvest >= 0)
    output_variable (year_of_last_harvest, log);
  output_variable (fertilize_index, log);
  output_variable (fertilize_rest_index, log);
  if (fertilize_year >= 0)
    output_variable (fertilize_year, log);
}

bool 
ActionCrop::Perennial::check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;

  if (al.integer ("seasons") < 1)
    {
      err.entry ("Perennial harvest should last at least 1 season");
      ok = false;
    }
  if (al.check ("fertilize_rest") && !al.check ("fertilize"))
    {
      err.entry ("'fertilize_rest' specified, but 'fertilize' isn't");
      ok = false;
    }
  
  return ok;
}

void 
ActionCrop::Perennial::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  syntax.add ("seasons", Syntax::Integer, Syntax::Const, 
	      "Number of seasons to harvest crop.\n\
The crop will be harvested whenever the specified DS or DM are reached.\n\
The first season is the year the crop management starts.");
  syntax.add_submodule ("end", alist, Syntax::Const, 
			"End management this date the last season.",
			MM_DD::load_syntax);
  static RangeEI ds_range (0.0, 2.0);
  syntax.add ("DS", Syntax::None (), ds_range, Syntax::Const, 
	      "Development stage at or above which to initiate harvest.");
  syntax.add ("DM", "kg DM/ha", Check::positive (), Syntax::Const, 
	      "Dry matter at or above which to initiate harvest.");
  syntax.add ("year_of_last_harvest", Syntax::Integer, Syntax::OptionalState, 
	      "Year of last season.");
  syntax.add_object ("fertilize", Librarian<AM>::library (),
                     Syntax::OptionalConst, Syntax::Sequence,"\
Fertilizer applications after harvest first season.\n\
First season is defined as the year where the first harvest occurs.");
  syntax.add ("fertilize_index", Syntax::Integer, Syntax::State,
	      "Next entry in 'fertilize' to execute.");
  alist.add ("fertilize_index", 0);
  syntax.add_object ("fertilize_rest", Librarian<AM>::library (),
                     Syntax::OptionalConst, Syntax::Sequence,"\
Fertilizer applications after harvest remaining seasons.\n\
If missing, use the same fertilizer as first season.");
  syntax.add ("fertilize_rest_index", Syntax::Integer, Syntax::State,
	      "Next entry in 'fertilize_rest' to execute.");
  alist.add ("fertilize_rest_index", 0);
  syntax.add ("fertilize_year", Syntax::Integer, Syntax::OptionalState, 
	      "Year last fertilization was applid.");
}

ActionCrop::Perennial::Perennial (const AttributeList& al)
  : seasons (al.integer ("seasons")),
    end (al.alist ("end")),
    DS (al.number ("DS")),
    DM (al.number ("DM")),
    year_of_last_harvest (al.check ("year_of_last_harvest")
			  ? al.integer ("year_of_last_harvest")
			  : -1),
    fertilize (al.check ("fertilize")
	       ? &al.alist_sequence ("fertilize")
	       : NULL),
    fertilize_index (al.integer ("fertilize_index")),
    fertilize_rest (al.check ("fertilize_rest")
		    ? &al.alist_sequence ("fertilize_rest")
		    : NULL),
    fertilize_rest_index (al.integer ("fertilize_rest_index")),
    fertilize_year (al.check ("fertilize_year")
		    ? al.integer ("fertilize_year")
		    : -1)
{ }

ActionCrop::Perennial::~Perennial ()
{ }

bool 
ActionCrop::Fertilize::check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  const int mm = al.integer ("month");
  const int dd = al.integer ("day");

  if (mm < 1 || mm > 12)
    {
      err.entry ("month should be between 1 and 12");
      ok = false;
    }
  // don't test for bad month.
  else if (dd < 1 || dd > Time::month_length (1 /* not a leap year */, mm))
    {
      ostringstream tmp;
      tmp << "day should be between 1 and " << Time::month_length (1, mm);
      err.entry (tmp.str ());
      ok = false;
    }
  return ok;
}

void 
ActionCrop::Fertilize::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_check (check_alist);
  syntax.add ("month", Syntax::Integer, Syntax::Const, 
	      "Month in the year.");
  syntax.add ("day", Syntax::Integer, Syntax::Const, 
	      "Day in the month.");
  syntax.add_object ("what", Librarian<AM>::library (), "Fertilizer to apply.");
  syntax.order ("month", "day", "what");
}

ActionCrop::Fertilize::Fertilize (const AttributeList& al)
  : month (al.integer ("month")),
    day (al.integer ("day")),
    what (al.alist ("what"))
{ }

ActionCrop::Fertilize::~Fertilize ()
{ }

void
ActionCrop::fertilize (Daisy& daisy, Treelog& out,
		       const AttributeList& am) const
{
  out.message (string ("[Fertilizing ") + am.name ("type") + "]");

#if 0
  if (am.name ("syntax") != "mineral")
    am.add ("creation", daisy.time);
#endif

  const double from = 0.0;
  const double to = -18.0;
      
  if (fertilize_incorporate)
    daisy.field->fertilize (am, from, to, daisy.dt);
  else
    daisy.field->fertilize (am, daisy.dt);
}

void 
ActionCrop::Tillage::output (Log& log) const
{
  output_derived (operation, "operation", log);
}

bool 
ActionCrop::Tillage::check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  const int mm = al.integer ("month");
  const int dd = al.integer ("day");

  if (dd > Time::month_length (1 /* not a leap year */, mm))
    {
      ostringstream tmp;
      tmp << "day should be between 1 and " << Time::month_length (1, mm);
      err.entry (tmp.str ());
      ok = false;
    }
  return ok;
}

void 
ActionCrop::Tillage::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_check (check_alist);
  syntax.add ("month", Syntax::Integer, Syntax::Const, 
	      "Month in the year.");
  syntax.add_check ("month", VCheck::valid_month ());
  syntax.add ("day", Syntax::Integer, Syntax::Const, 
	      "Day in the month.");
  syntax.add_check ("mday", VCheck::valid_mday ());
  syntax.add_object ("operation", Librarian<Action>::library (), 
                     "Tillage operation.");
  syntax.order ("month", "day", "operation");
}

ActionCrop::Tillage::Tillage (Block& al)
  : month (al.integer ("month")),
    day (al.integer ("day")),
    operation (Librarian<Action>::build_item (al, "operation"))
{ }

ActionCrop::Tillage::~Tillage ()
{ }

void 
ActionCrop::Spray::output (Log&) const
{ }

bool 
ActionCrop::Spray::check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  const int mm = al.integer ("month");
  const int dd = al.integer ("day");

  if (mm < 1 || mm > 12)
    {
      err.entry ("month should be between 1 and 12");
      ok = false;
    }
  // don't test for bad month.
  else if (dd < 1 || dd > Time::month_length (1 /* not a leap year */, mm))
    {
      ostringstream tmp;
      tmp << "day should be between 1 and " << Time::month_length (1, mm);
      err.entry (tmp.str ());
      ok = false;
    }

  return ok;
}

void 
ActionCrop::Spray::load_syntax (Syntax& syntax, AttributeList&)
{ 
  syntax.add_check (check_alist);
  syntax.add ("month", Syntax::Integer, Syntax::Const, 
	      "Month in the year.");
  syntax.add ("day", Syntax::Integer, Syntax::Const, 
	      "Day in the month.");
  syntax.add ("name", Syntax::String, Syntax::Const,
	      "Name of chemichal to spray.");
  syntax.add ("amount", "g/ha", Check::non_negative (), Syntax::Const,
	      "Amount of chemichal to spray.");
  syntax.order ("month", "day", "name", "amount");
}

ActionCrop::Spray::Spray (const AttributeList& al)
  : month (al.integer ("month")),
    day (al.integer ("day")),
    name (al.identifier ("name")),
    amount (al.number ("amount"))
{ }

ActionCrop::Spray::~Spray ()
{ }

bool
ActionCrop::Irrigation::doIt (Daisy& daisy, Treelog& out) const
{
  if (iszero (amount))
    return false;

  const int mm = daisy.time.month ();
  const int dd = daisy.time.yday ();

  if (mm < from.month || (mm == from.month && dd < from.day))
    return false;
  if (mm > to.month || (mm == to.month && dd > to.day))
    return false;

  const double depth = -20;

  if (daisy.field->soil_water_potential (depth) >= potential)
    return false;

  ostringstream tmp;
  tmp << "Irrigating " << amount << " mm";
  out.message (tmp.str ());
  daisy.field->irrigate_overhead (amount, IM (), daisy.dt);
  return true;
}

void 
ActionCrop::Irrigation::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_submodule ("from", alist, Syntax::Const, 
			"Start of irrigation period.",
			MM_DD::load_syntax);
  syntax.add_submodule ("to", alist, Syntax::Const, 
			"End of irrigation period.",
			MM_DD::load_syntax);
  syntax.add ("amount", "mm", Check::non_negative (), Syntax::Const, 
	      "Amount of water to apply on irrigation.");
  syntax.add ("potential", "cm", Check::negative (), Syntax::Const, 
	      "Soil potential at which to irrigate.");
}

ActionCrop::Irrigation::Irrigation (const AttributeList& al)
  : from (al.alist ("from")),
    to (al.alist ("to")),
    amount (al.number ("amount")),
    potential (al.number ("potential"))
{ }

ActionCrop::Irrigation::~Irrigation ()
{ }

void
ActionCrop::doIt (Daisy& daisy, Treelog& out)
{
  // Sowing.
  primary->doIt (daisy, out);
  if (secondary)
    secondary->doIt (daisy, out);

  // Harvesting.
  bool harvested = false;
  if (harvest_annual && harvest_perennial)
    {
      // We have both annual and perennial crops.
      daisy_assert (secondary);
      if (harvest_annual->done)
	{
	  // If annual done, do perennial.
	  if (secondary->done 
	      && harvest_perennial->doIt (daisy, out,
					  secondary->crop.identifier ("type")))
	    harvested = true;
	}
      else if (primary->done 
	       && harvest_annual->doIt (daisy, out, 
					primary->crop.identifier ("type")))
	// else do annual.
	harvested = true;
    }
  else if (harvest_annual)
    {
      // We have only annual crops.  Let 'primary' when they are harvested.
      if (primary->done 
	  && harvest_annual->doIt (daisy, out, primary->crop.identifier ("type")))
	harvested = true;
    }
  else
    { 
      // We have only perennial crops.
      daisy_assert (harvest_perennial);
      if (secondary)
	{
	  // If we have two, let them both control.
	  if ((primary->done || secondary->done)
	      && harvest_perennial->doIt (daisy, out,
					  primary->crop.identifier ("type"),
					  secondary->crop.identifier ("type")))
	    harvested = true;
	}
      else if (primary->done 
	       && harvest_perennial->doIt (daisy, out, 
					   primary->crop.identifier ("type")))
	// If we have only one, it is of course in control.
	harvested = true;
    }

  // Fertilize.
  if (fertilize_at_index < fertilize_at.size ()
      && daisy.time.hour () == 8
      && daisy.time.month () == fertilize_at[fertilize_at_index]->month
      && daisy.time.mday () == fertilize_at[fertilize_at_index]->day)
    {
      // Fertilize by date.
      fertilize (daisy, out, fertilize_at[fertilize_at_index]->what);
      fertilize_at_index++;
    }
  if (harvested && harvest_perennial && harvest_perennial->fertilize)
    {
      // Fertilize after harvest.
      if (harvest_perennial->fertilize_year < 0)
	{
	  // First season initialization.
	  harvest_perennial->fertilize_year = daisy.time.year ();
	  harvest_perennial->fertilize_rest_index
	    = harvest_perennial->fertilize_rest->size ();
	}
      else if (harvest_perennial->fertilize_year < daisy.time.year ())
	{
	  // New season initialization.
	  if (harvest_perennial->fertilize_rest)
	    {
	      // If we have 'fertilize_rest', use it.
	      harvest_perennial->fertilize_rest_index = 0;
	      harvest_perennial->fertilize_index
		= harvest_perennial->fertilize->size ();
	    }
	  else 
	    {
	      // Otherwise, reuse 'fertilize'.
	      harvest_perennial->fertilize_index = 0;
	    }
	  harvest_perennial->fertilize_year = daisy.time.year ();
	}
      if (harvest_perennial->fertilize_index 
	  < harvest_perennial->fertilize->size ())
	{
	  // If 'fertilize' is active, use it.
	  fertilize (daisy, out, *(*harvest_perennial->fertilize)
		     [harvest_perennial->fertilize_index]);
	  harvest_perennial->fertilize_index++;
	}
      else if (harvest_perennial->fertilize_rest 
	       && (harvest_perennial->fertilize_rest_index
		   < harvest_perennial->fertilize_rest->size ()))
	{
	  // Else, if 'fertilize_rest' is active, us that.
	  fertilize (daisy, out, *(*harvest_perennial->fertilize_rest)
		     [harvest_perennial->fertilize_rest_index]);
	  harvest_perennial->fertilize_rest_index++;
	}
    }

  // Tillage.
  if (tillage_index < tillage.size ()
      && daisy.time.hour () == 8
      && daisy.time.month () == tillage[tillage_index]->month
      && daisy.time.mday () == tillage[tillage_index]->day)
    {
      tillage[tillage_index]->operation->doIt (daisy, out);
      tillage_index++;
    }

  // Spray.
  if (spray_index < spray.size ()
      && daisy.time.hour () == 8
      && daisy.time.month () == spray[spray_index]->month
      && daisy.time.mday () == spray[spray_index]->day)
    {
      symbol chemical = spray[spray_index]->name;
      const double amount = spray[spray_index]->amount;
      out.message ("Spraying " + chemical);
      daisy.field->spray (chemical, amount, daisy.dt, out); 

      spray_index++;
    }

  // Irrigation.
  if (irrigation_year < 0)
    irrigation_year = daisy.time.year ();
  if (irrigation_delay > 0)
    irrigation_delay--;
  else if (irrigation)
    {
      if (irrigation_rest && irrigation_year != daisy.time.year ())
	{
	  if (irrigation_rest->doIt (daisy, out))
	    irrigation_delay = 48;
	}
      else if (irrigation->doIt (daisy, out))
	irrigation_delay = 48;
    }
}

bool 
ActionCrop::done (const Daisy& daisy, Treelog& msg) const
{ return (!harvest_annual || harvest_annual->done) 
    && (!harvest_perennial || harvest_perennial->done (daisy, msg)); }

void 
ActionCrop::output (Log& log) const
{ 
  output_submodule (*primary, "primary", log);
  if (secondary)
    output_submodule (*secondary, "secondary", log);
  if (harvest_annual)
    output_submodule (*harvest_annual, "harvest_annual", log);
  if (harvest_perennial)
    output_submodule (*harvest_perennial, "harvest_perennial", log);
  output_variable (fertilize_at_index, log);
  output_vector (tillage, "tillage", log);
  output_variable (tillage_index, log);
  output_vector (spray, "spray", log);
  output_variable (spray_index, log);
  if (irrigation_year >= 0)
    output_variable (irrigation_year, log);
  output_variable (irrigation_delay, log);
}

ActionCrop::ActionCrop (Block& al)
  : Action (al),
    primary (new Sow (al.alist ("primary"))),
    secondary (al.check ("secondary") 
	       ? new Sow (al.alist ("secondary"))
	       : NULL),
    harvest_annual (al.check ("harvest_annual") 
	       ? new Annual (al.alist ("harvest_annual"))
	       : NULL),
    harvest_perennial (al.check ("harvest_perennial") 
		       ? new Perennial (al.alist ("harvest_perennial"))
		       : NULL),
    fertilize_at (map_construct_const<Fertilize> 
		  (al.alist_sequence ("fertilize_at"))),
    fertilize_at_index (al.integer ("fertilize_at_index")),
    fertilize_incorporate (al.flag ("fertilize_incorporate")),
    tillage (map_submodel_const<Tillage> (al, "tillage")),
    tillage_index (al.integer ("tillage_index")),
    spray (map_construct_const<Spray> (al.alist_sequence ("spray"))),
    spray_index (al.integer ("spray_index")),
    irrigation (al.check ("irrigation") 
	       ? new Irrigation (al.alist ("irrigation"))
	       : NULL),
    irrigation_rest (al.check ("irrigation_rest") 
		     ? new Irrigation (al.alist ("irrigation_rest"))
		     : NULL),
    irrigation_year (al.check ("irrigation_year")
		     ? al.integer ("irrigation_year")
		     : -1),
    irrigation_delay (al.check ("irrigation_delay")
		      ? al.integer ("irrigation_delay")
		      : 0)
{ }

ActionCrop::~ActionCrop ()
{ 
  delete primary;
  delete secondary;
  delete harvest_annual;
  delete harvest_perennial;
  sequence_delete (fertilize_at.begin (), fertilize_at.end ());
  sequence_delete (tillage.begin (), tillage.end ());
  sequence_delete (spray.begin (), spray.end ());
  delete irrigation;
  delete irrigation_rest;
}

// Add the ActionCrop syntax to the syntax table.
static struct ActionCropSyntax
{
  static Model& make (Block& al)
  { return *new ActionCrop (al); }

  static bool check_alist (const AttributeList& al, Treelog& err)
  {
    bool ok = true;

    if (al.check ("irrigation_rest") && !al.check ("irrigation"))
      {
	err.entry ("'irrigation_rest' specified, but 'irrigation' isn't");
	ok = false;
      }
    if (!al.check ("harvest_annual") && !al.check ("harvest_perennial"))
      {
	err.entry ("No harvest specified");
	ok = false;
      }
    if (al.check ("harvest_annual") && al.check ("harvest_perennial")
	&& !al.check ("secondary"))
      {
	err.entry ("You must specify a secondary crop when you specify both "
		   "annual and perennial harvest");
	ok = false;
      }
    return ok;
  }

  ActionCropSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);
    alist.add ("description", "Manage a specific crop or multicrop.");

    syntax.add_submodule ("primary", alist, Syntax::State,
			  "Primary crop.", ActionCrop::Sow::load_syntax);
    syntax.add_submodule ("secondary", alist, Syntax::OptionalState, 
			  "Secondary crop, if any.",
			  ActionCrop::Sow::load_syntax);
    syntax.add_submodule ("harvest_annual", alist,
			  Syntax::OptionalState,
			  "Harvest parameters for annual crops.", 
			  ActionCrop::Annual::load_syntax);
    syntax.add_submodule ("harvest_perennial", alist,
			  Syntax::OptionalState, "\
Harvest conditions for perennial crops.",
			  ActionCrop::Perennial::load_syntax);
    syntax.add_submodule_sequence("fertilize_at", Syntax::Const, "\
Fertilizer application by date.", ActionCrop::Fertilize::load_syntax);
    alist.add ("fertilize_at", vector<AttributeList*> ());
    syntax.add ("fertilize_at_index", Syntax::Integer, Syntax::State,
		"Next entry in 'fertilize_at' to execute.");
    alist.add ("fertilize_at_index", 0);
    syntax.add ("fertilize_incorporate", Syntax::Boolean, Syntax::Const,
		"Incorporate organic fertilizer in plowing zone.");
    alist.add ("fertilize_incorporate", false);
    syntax.add_submodule_sequence ("tillage", Syntax::State, "\
List of tillage operations to apply.", ActionCrop::Tillage::load_syntax);
    alist.add ("tillage", vector<AttributeList*> ());
    syntax.add ("tillage_index", Syntax::Integer, Syntax::State,
		"Next entry in 'tillage' to execute.");
    alist.add ("tillage_index", 0);
    syntax.add_submodule_sequence ("spray", Syntax::State, "\
List of chemicals to apply.", ActionCrop::Spray::load_syntax);
    alist.add ("spray", vector<AttributeList*> ());
    syntax.add ("spray_index", Syntax::Integer, Syntax::State,
		"Next entry in 'spray' to execute.");
    alist.add ("spray_index", 0);
    syntax.add_submodule ("irrigation", alist, Syntax::OptionalConst, "\
Irrigation model for first season.  If missing, don't irrigate.", 
			  ActionCrop::Irrigation::load_syntax);
    syntax.add_submodule ("irrigation_rest", alist, Syntax::OptionalConst, "\
Irrigation model for remaining seasons.\n\
If missing, use the same model as first season.",
			  ActionCrop::Irrigation::load_syntax);
    syntax.add ("irrigation_year", Syntax::Integer, Syntax::OptionalState, 
		"Year management started.\n\
Negative number means it hasn't started yet.");
    syntax.add ("irrigation_delay", Syntax::Integer, Syntax::OptionalState, 
		"Hours we test for irrigation again.\n\
This is set at each irrigation, to avoid multiple applications.");
      
    Librarian<Action>::add_type ("crop", alist, syntax, &make);
  }
} ActionCrop_syntax;
