// action_crop.C

#include "action.h"
#include "daisy.h"
#include "field.h"
#include "crop.h"
#include "time.h"
#include "am.h"
#include "log.h"
#include "harvest.h"
#include "im.h"
#include "tmpstream.h"

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
    void doIt (Daisy&);
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
    bool doIt (Daisy&, const string& name);
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
    void harvest (Daisy&);
  public:
    bool doIt (Daisy&, const string& name);
    bool doIt (Daisy&, const string& primary, const string& secondary);
    bool done (const Daisy&) const;
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
  void fertilize (Daisy& daisy, const AttributeList& am) const;

  struct Tillage		// Tillage operations.
  {
    // Content.
    const int month;
    const int day;
    Action& operation;

    // Simulation.
    void output (Log&) const;

    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    Tillage (const AttributeList&);
    ~Tillage ();
  };
  const vector<const Tillage*> tillage;
  int tillage_index;

  struct Spray		// Spray operations.
  {
    // Content.
    const int month;
    const int day;
    const string name;
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
    bool doIt (Daisy&) const;

    // Create and Destroy.
    static bool check_alist (const AttributeList& al, Treelog&);
    static void load_syntax (Syntax&, AttributeList&);
    Irrigation (const AttributeList&);
    ~Irrigation ();
  };
  const Irrigation *const irrigation;
  const Irrigation *const irrigation_rest;
  int irrigation_year;
  int irrigation_delay;

  // Simulation.
  void doIt (Daisy&);
  bool done (const Daisy&) const;
  void output (Log&) const;

  // Create and Destroy.
  ActionCrop (const AttributeList& al);
  ~ActionCrop ();
};

#ifdef BORLAND_TEMPLATES
template class add_submodule<ActionCrop::MM_DD>;
template class add_submodule<ActionCrop::Sow>;
template class add_submodule<ActionCrop::Annual>;
template class add_submodule<ActionCrop::Perennial>;
template class add_submodule_sequence<ActionCrop::Fertilize>;
template class add_submodule_sequence<ActionCrop::Tillage>;
template class add_submodule_sequence<ActionCrop::Spray>;
template class add_submodule<ActionCrop::Irrigation>;
#endif

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
      TmpStream tmp;
      tmp () << "day should be between 1 and " << Time::month_length (1, mm);
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
ActionCrop::Sow::doIt (Daisy& daisy)
{
  if (!done && date.match (daisy.time))
    {
      COUT << " [Sowing " << crop.name ("type") << "]\n";      
      daisy.field.sow (crop); 
      done = true;
    }
}
void 
ActionCrop::Sow::output (Log& log) const
{
  log.output ("done", done);
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
  add_submodule<MM_DD> ("date", syntax, alist, Syntax::Const, "Date to sow.");
  syntax.add ("crop", Librarian<Crop>::library (), "Crop to sow.");
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
ActionCrop::Annual::doIt (Daisy& daisy, const string& name)
{
  if (!done && (daisy.field.crop_ds (name) >= 2.0
		|| latest.match (daisy.time)))
    {
      const double stub = 8.0;
      const double stem = remove_residuals ? 1.0 : 0.0;
      const double leaf = remove_residuals ? 1.0 : 0.0;
      const double sorg = (1.0 - loss);
      daisy.field.harvest (daisy.time, "all", stub, stem, leaf, sorg, 
			   daisy.harvest);
      COUT << " [Annual harvest of " << name << "]\n";
      done = true;
      return true;
    }
  return false;
}

void 
ActionCrop::Annual::output (Log& log) const
{
  log.output ("done", done);
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
  syntax.add ("loss", Syntax::Fraction (), Syntax::Const, 
	      "Fraction lost during harvest.");
  syntax.add ("remove_residuals", Syntax::Boolean, Syntax::Const,
	      "Remove residuals at harvest.");
  add_submodule<MM_DD> ("latest", syntax, alist, Syntax::Const, 
			"Latest harvest date.\n\
If the crop is ripe before this date, it will be harvested at that point.");
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
ActionCrop::Perennial::harvest (Daisy& daisy)
{
  const double stub = 8.0;
  const double stem = 1.0;
  const double leaf = 1.0;
  const double sorg = 1.0;
  daisy.field.harvest (daisy.time, "all", stub, stem, leaf, sorg, 
		       daisy.harvest);
  COUT << " [Perennial harvest]\n";
}

bool
ActionCrop::Perennial::doIt (Daisy& daisy, const string& name)
{
  if (year_of_last_harvest < 0)
    year_of_last_harvest = daisy.time.year () + seasons - 1;

  if (daisy.field.crop_ds (name) >= DS || daisy.field.crop_dm (name) >= DM)
    {
      harvest (daisy);
      return true;
    }
  return false;
}

bool
ActionCrop::Perennial::doIt (Daisy& daisy, 
			     const string& primary, const string& secondary)
{
  if (year_of_last_harvest < 0)
    year_of_last_harvest = daisy.time.year () + seasons - 1;

  if (daisy.field.crop_ds (primary) >= DS 
      || daisy.field.crop_ds (secondary) >= DS 
      || (daisy.field.crop_dm (primary)
	  + daisy.field.crop_dm (secondary)) >= DM)
    {
      harvest (daisy);
      return true;
    }
  return false;
}

bool
ActionCrop::Perennial::done (const Daisy& daisy) const
{ 
  if (year_of_last_harvest < 0)
    return false;

  assert (daisy.time.year () <= year_of_last_harvest);
  return daisy.time.year () == year_of_last_harvest && end.match (daisy.time);
}

void 
ActionCrop::Perennial::output (Log& log) const
{
  if (year_of_last_harvest >= 0)
    log.output ("year_of_last_harvest", year_of_last_harvest);
  log.output ("fertilize_index", fertilize_index);
  log.output ("fertilize_rest_index", fertilize_rest_index);
  if (fertilize_year >= 0)
    log.output ("fertilize_year", fertilize_year);
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
  non_negative (al.number ("DS"), "DS", ok, err);
  non_negative (al.number ("DM"), "DM", ok, err);
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
  add_submodule<MM_DD> ("end", syntax, alist, Syntax::Const, 
			"End management this date the last season.");
  syntax.add ("DS", Syntax::None (), Syntax::Const, 
	      "Development stage at or above which to initiate harvest.");
  syntax.add ("DM", "kg DM/ha", Syntax::Const, 
	      "Dry matter at or above which to initiate harvest.");
  syntax.add ("year_of_last_harvest", Syntax::Integer, Syntax::OptionalState, 
	      "Year of last season.");
  syntax.add ("fertilize", Librarian<AM>::library (),
	      Syntax::OptionalConst, Syntax::Sequence,"\
Fertilizer applications after harvest first season.\n\
First season is defined as the year where the first harvest occurs.");
  syntax.add ("fertilize_index", Syntax::Integer, Syntax::State,
	      "Next entry in 'fertilize' to execute.");
  alist.add ("fertilize_index", 0);
  syntax.add ("fertilize_rest", Librarian<AM>::library (),
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
      TmpStream tmp;
      tmp () << "day should be between 1 and " << Time::month_length (1, mm);
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
  syntax.add ("what", Librarian<AM>::library (), "Fertilizer to apply.");
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
ActionCrop::fertilize (Daisy& daisy, const AttributeList& am) const
{
  COUT << " [Fertilizing " << am.name ("type") << "]\n";

#if 0
  if (am.name ("syntax") != "mineral")
    am.add ("creation", daisy.time);
#endif

  const double from = 0.0;
  const double to = -18.0;
      
  if (fertilize_incorporate)
    daisy.field.fertilize (am, from, to);
  else
    daisy.field.fertilize (am);
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

  if (mm < 1 || mm > 12)
    {
      err.entry ("month should be between 1 and 12");
      ok = false;
    }
  // don't test for bad month.
  else if (dd < 1 || dd > Time::month_length (1 /* not a leap year */, mm))
    {
      TmpStream tmp;
      tmp () << "day should be between 1 and " << Time::month_length (1, mm);
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
  syntax.add ("day", Syntax::Integer, Syntax::Const, 
	      "Day in the month.");
  syntax.add ("operation", Librarian<Action>::library (), 
	      "Tillage operation.");
  syntax.order ("month", "day", "operation");
}

ActionCrop::Tillage::Tillage (const AttributeList& al)
  : month (al.integer ("month")),
    day (al.integer ("day")),
    operation (Librarian<Action>::create (al.alist ("operation")))
{ }

ActionCrop::Tillage::~Tillage ()
{ delete &operation; }

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
      TmpStream tmp;
      tmp ()<< "day should be between 1 and " << Time::month_length (1, mm);
      err.entry (tmp.str ());
      ok = false;
    }
  non_negative (al.number ("amount"), "amount", ok, err);

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
  syntax.add ("amount", "g/ha", Syntax::Const,
	      "Amount of chemichal to spray.");
  syntax.order ("month", "day", "name", "amount");
}

ActionCrop::Spray::Spray (const AttributeList& al)
  : month (al.integer ("month")),
    day (al.integer ("day")),
    name (al.name ("name")),
    amount (al.number ("amount"))
{ }

ActionCrop::Spray::~Spray ()
{ }

bool
ActionCrop::Irrigation::doIt (Daisy& daisy) const
{
  if (amount == 0.0)
    return false;

  const int mm = daisy.time.month ();
  const int dd = daisy.time.yday ();

  if (mm < from.month || (mm == from.month && dd < from.day))
    return false;
  if (mm > to.month || (mm == to.month && dd > to.day))
    return false;

  const double depth = -20;

  if (daisy.field.soil_water_potential (depth) >= potential)
    return false;

  COUT << " [Irrigating " << amount << " mm]\n";
  daisy.field.irrigate_overhead (amount, IM ());
  return true;
}

bool 
ActionCrop::Irrigation::check_alist (const AttributeList& al, Treelog& err)
{
  bool ok = true;
  non_negative (al.number ("amount"), "amount", ok, err);
  non_positive (al.number ("potential"), "potential", ok, err);
  return ok;
}

void 
ActionCrop::Irrigation::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  add_submodule<MM_DD> ("from", syntax, alist, Syntax::Const, 
			"Start of irrigation period.");
  add_submodule<MM_DD> ("to", syntax, alist, Syntax::Const, 
			"End of irrigation period.");
  syntax.add ("amount", "mm", Syntax::Const, 
	      "Amount of water to apply on irrigation.");
  syntax.add ("potential", "cm", Syntax::Const, 
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
ActionCrop::doIt (Daisy& daisy)
{
  // Sowing.
  primary->doIt (daisy);
  if (secondary)
    secondary->doIt (daisy);

  // Harvesting.
  bool harvested = false;
  if (harvest_annual && harvest_perennial)
    {
      // We have both annual and perennial crops.
      assert (secondary);
      if (harvest_annual->done)
	{
	  // If annual done, do perennial.
	  if (secondary->done 
	      && harvest_perennial->doIt (daisy,
					  secondary->crop.name ("type")))
	    harvested = true;
	}
      else if (primary->done 
	       && harvest_annual->doIt (daisy, primary->crop.name ("type")))
	// else do annual.
	harvested = true;
    }
  else if (harvest_annual)
    {
      // We have only annual crops.  Let 'primary' when they are harvested.
      if (primary->done 
	  && harvest_annual->doIt (daisy, primary->crop.name ("type")))
	harvested = true;
    }
  else
    { 
      // We have only perennial crops.
      assert (harvest_perennial);
      if (secondary)
	{
	  // If we have two, let them both control.
	  if ((primary->done || secondary->done)
	      && harvest_perennial->doIt (daisy, 
					  primary->crop.name ("type"),
					  secondary->crop.name ("type")))
	    harvested = true;
	}
      else if (primary->done 
	       && harvest_perennial->doIt (daisy, primary->crop.name ("type")))
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
      fertilize (daisy, fertilize_at[fertilize_at_index]->what);
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
	  fertilize (daisy, *(*harvest_perennial->fertilize)
		     [harvest_perennial->fertilize_index]);
	  harvest_perennial->fertilize_index++;
	}
      else if (harvest_perennial->fertilize_rest 
	       && (harvest_perennial->fertilize_rest_index
		   < harvest_perennial->fertilize_rest->size ()))
	{
	  // Else, if 'fertilize_rest' is active, us that.
	  fertilize (daisy, *(*harvest_perennial->fertilize_rest)
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
      tillage[tillage_index]->operation.doIt (daisy);
      tillage_index++;
    }

  // Spray.
  if (spray_index < spray.size ()
      && daisy.time.hour () == 8
      && daisy.time.month () == spray[spray_index]->month
      && daisy.time.mday () == spray[spray_index]->day)
    {
      const string& chemical = spray[spray_index]->name;
      const double amount = spray[spray_index]->amount;
      COUT << " [Spraying " << chemical << "]\n";
      daisy.field.spray (chemical, amount); 

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
	  if (irrigation_rest->doIt (daisy))
	    irrigation_delay = 48;
	}
      else if (irrigation->doIt (daisy))
	irrigation_delay = 48;
    }
}

bool 
ActionCrop::done (const Daisy& daisy) const
{ return (!harvest_annual || harvest_annual->done) 
    && (!harvest_perennial || harvest_perennial->done (daisy)); }

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
  log.output ("fertilize_at_index", fertilize_at_index);
  output_vector (tillage, "tillage", log);
  log.output ("tillage_index", tillage_index);
  output_vector (spray, "spray", log);
  log.output ("spray_index", spray_index);
  if (irrigation_year >= 0)
    log.output ("irrigation_year", irrigation_year);
  log.output ("irrigation_delay", irrigation_delay);
}

#ifdef BORLAND_TEMPLATES
template vector<const ActionCrop::Fertilize*>&
map_construct_const<ActionCrop::Fertilize> (const vector<AttributeList*>& f);
template vector<const ActionCrop::Tillage*>& 
map_construct_const<ActionCrop::Tillage> (const vector<AttributeList*>& f);
template vector<const ActionCrop::Spray*>&
map_construct_const<ActionCrop::Spray> (const vector<AttributeList*>& f);
#endif

ActionCrop::ActionCrop (const AttributeList& al)
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
    tillage (map_construct_const<Tillage> (al.alist_sequence ("tillage"))),
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
  static Action& make (const AttributeList& al)
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

    add_submodule<ActionCrop::Sow>("primary", syntax, alist, Syntax::State,
				   "Primary crop.");
    add_submodule<ActionCrop::Sow>("secondary", syntax, alist,
				   Syntax::OptionalState, 
				   "Secondary crop, if any.");
    add_submodule<ActionCrop::Annual>("harvest_annual", syntax, alist,
				      Syntax::OptionalState,
				      "Harvest parameters for annual crops.");
    add_submodule<ActionCrop::Perennial>("harvest_perennial", syntax, alist,
					 Syntax::OptionalState, "\
Harvest conditions for perennial crops.");
    add_submodule_sequence<ActionCrop::Fertilize>("fertilize_at", syntax, 
						  Syntax::Const, "\
Fertilizer application by date.");
    alist.add ("fertilize_at", vector<AttributeList*> ());
    syntax.add ("fertilize_at_index", Syntax::Integer, Syntax::State,
		"Next entry in 'fertilize_at' to execute.");
    alist.add ("fertilize_at_index", 0);
    syntax.add ("fertilize_incorporate", Syntax::Boolean, Syntax::Const,
		"Incorporate organic fertilizer in plowing zone.");
    alist.add ("fertilize_incorporate", false);
    add_submodule_sequence<ActionCrop::Tillage> ("tillage", syntax, 
						 Syntax::State, "\
List of tillage operations to apply.");
    alist.add ("tillage", vector<AttributeList*> ());
    syntax.add ("tillage_index", Syntax::Integer, Syntax::State,
		"Next entry in 'tillage' to execute.");
    alist.add ("tillage_index", 0);
    add_submodule_sequence<ActionCrop::Spray> ("spray", syntax, 
						 Syntax::State, "\
List of chemicals to apply.");
    alist.add ("spray", vector<AttributeList*> ());
    syntax.add ("spray_index", Syntax::Integer, Syntax::State,
		"Next entry in 'spray' to execute.");
    alist.add ("spray_index", 0);
    add_submodule<ActionCrop::Irrigation>("irrigation", syntax, alist,
					  Syntax::OptionalConst, "\
Irrigation model for first season.  If missing, don't irrigate.");
    add_submodule<ActionCrop::Irrigation>("irrigation_rest", syntax, alist,
					  Syntax::OptionalConst, "\
Irrigation model for remaining seasons.\n\
If missing, use the same model as first season.");
    syntax.add ("irrigation_year", Syntax::Integer, Syntax::OptionalState, 
		"Year management started.\n\
Negative number means it hasn't started yet.");
    syntax.add ("irrigation_delay", Syntax::Integer, Syntax::OptionalState, 
		"Hours we test for irrigation again.\n\
This is set at each irrigation, to avoid multiple applications.");
      
    Librarian<Action>::add_type ("crop", alist, syntax, &make);
  }
} ActionCrop_syntax;
