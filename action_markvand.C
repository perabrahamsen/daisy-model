// action_markvand.C  -- Emulate part of the MARKVAND irrigation system.
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

#include "action.h"
#include "daisy.h"
#include "field.h"
#include "crop.h"
#include "im.h"
#include "fao.h"
#include "symbol.h"
#include "tmpstream.h"
#include <vector>
#include <memory>

// MV_Soil

struct MV_Soil
{
  // Content.
  const symbol name;
  static const char *const description;

  // Simulation.
  double max_reservoir (double root_depth) const
  { return root_depth * 10 * 0.4; }

  // Create and Destroy.
  MV_Soil (const AttributeList& al)
    : name (al.identifier ("type"))
  { }
  ~MV_Soil ()
  { }
};

EMPTY_TEMPLATE
Librarian<MV_Soil>::Content* Librarian<MV_Soil>::content = NULL;

static Librarian<MV_Soil> MV_Soil_init ("MV_Soil");

const char *const MV_Soil::description = "\
Description of a soil for use by the MARKVAND model.";

static struct MV_SoilSyntax
{
  static MV_Soil&
  make (const AttributeList& al)
  { return *new MV_Soil (al); }
  MV_SoilSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Standard MARKVAND soil model.");
    Librarian<MV_Soil>::add_type ("default", alist, syntax, &make);
  }
} MV_Soil_syntax;

// MV_Crop

struct MV_Crop
{
  // Content.
  const symbol name;
  static const char *const description;

  // Phases.
  static std::vector<double> accumulated (const std::vector<double>& numbers)
  {
    std::vector<double> result;
    double sum = 0.0;
    for (size_t i = 0; i < numbers.size (); i++)
      {
	sum += numbers[i];
	result.push_back (sum);
      }
    return result;
  }
  const std::vector<double> T_sum;
  size_t phase (const double T)
  { 
    size_t i = 0; 
    while (i < T_sum.size () && T < T_sum[i])
      i++;
    return i;
  }

  // Simulation.
  double LAI (const double Ts) const
  { return 3.0; }
  double root_depth (const double Ts) const
  { return 50.0; }

  // Create and Destroy.
  MV_Crop (const AttributeList& al)
    : name (al.identifier ("type")),
      T_sum (accumulated (al.number_sequence ("S")))
  { }
  ~MV_Crop ()
  { }
};

EMPTY_TEMPLATE
Librarian<MV_Crop>::Content* Librarian<MV_Crop>::content = NULL;

static Librarian<MV_Crop> MV_Crop_init ("MV_Crop");

const char *const MV_Crop::description = "\
Description of a crop for use by the MARKVAND model.";

static struct MV_CropSyntax
{
  static MV_Crop&
  make (const AttributeList& al)
  { return *new MV_Crop (al); }
  MV_CropSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Standard MARKVAND crop model.");
    syntax.add ("S", "dg C d", Syntax::OptionalState, Syntax::Sequence,
                "Temperature sum for each phase.");
    Librarian<MV_Crop>::add_type ("default", alist, syntax, &make);
  }
} MV_Crop_syntax;

struct ActionMarkvand : public Action
{
  const std::auto_ptr<MV_Soil> soil;
  const struct crop_map_t : public std::map<std::string, const MV_Crop*>
  {
    static void load_syntax (Syntax& syntax, AttributeList&)
    { 
      syntax.add ("Daisy", Syntax::String, Syntax::Const, 
		  "Name of Daisy crop.");
      syntax.add ("MARKVAND", Librarian<MV_Crop>::library (), 
		  Syntax::Const, Syntax::Singleton,
		  "MARKVAND crop description.");
      syntax.order ("Daisy", "MARKVAND");
    }
    crop_map_t (const std::vector<AttributeList*> alists)
    {
      for (size_t i = 0; i < alists.size (); i++)
	{
	  (*this)[alists[i]->name ("Daisy")] 
	    = Librarian<MV_Crop>::create (alists[i]->alist ("MARKVAND"));
	}
    }
    ~crop_map_t ()
    { map_delete (begin (), end ()); }
  } crop_map;
  double T_sum;
  double reservoir;

  const MV_Crop* get_crop (Daisy& daisy) const;
  void doIt (Daisy& daisy, Treelog& out);
  bool done (const Daisy&) const
  { return false; }

  ActionMarkvand (const AttributeList& al)
    : Action (al),
      soil (Librarian<MV_Soil>::create (al.alist ("soil"))),
      crop_map (al.alist_sequence ("map")),
      T_sum (al.number ("T_sum", -1.0)),
      reservoir (al.number ("reservoir", -1.0))
  { }
  ~ActionMarkvand ()
  { }
};

const MV_Crop*
ActionMarkvand::get_crop (Daisy& daisy) const
{
  const std::string crop_name = daisy.field.crop_names ();
  const crop_map_t::const_iterator entry = crop_map.find (crop_name);
  return (entry != crop_map.end ()) ? entry->second : NULL;
}

void 
ActionMarkvand::doIt (Daisy& daisy, Treelog& out)
{
  // Default values.
  const double default_root_depth = 10.0;
  const double default_LAI = 3.0;

  // Daily occurence.
  if (daisy.time.hour () != 8)
    return;
  
  // Emergence and harvest.
  static const symbol all_symbol ("all");
  const bool has_crop = daisy.field.crop_dm (all_symbol, 0.1) > 0.0; 
  if (T_sum < 0.0)
    {
      if (has_crop)
        {
	  const MV_Crop *const crop = get_crop (daisy);
          T_sum = 0.0;
	  if (crop)
	    {
	      out.message ("Starting MARKVAND irrigation for " 
			   + crop->name.name () + ".");
	      reservoir = soil->max_reservoir (crop->root_depth (T_sum));
	    }
	  else
	    {
	      out.message ("Starting MARKVAND irrigation for unknown crop.");
	      reservoir = soil->max_reservoir (default_root_depth);
	    }
        }
      return;
    }
  if (!has_crop)
    {
      T_sum = -1.0;
      reservoir = -1.0;
      out.message ("Stoping MARKVAND irrigation.");
      return;
    }

  // Weather data.
  const double air_temperature = daisy.field.daily_air_temperature ();
  const double global_radiation = daisy.field.daily_global_radiation ();
  const double precipitation = daisy.field.daily_precipitation ();
  const double reference_evapotranspiration 
    = FAO::Makkink (air_temperature, global_radiation) * 24.0;
  const double potential_evapotranspiration = reference_evapotranspiration;

  // Temperature sum.
  T_sum += air_temperature;

  // Crop and soil.
  const MV_Crop *const crop = get_crop (daisy);
  const double LAI = crop ? crop->LAI (T_sum) : default_LAI;
  const double root_depth = crop
    ? crop->root_depth (T_sum) 
    : default_root_depth;
  const double max_reservoir = soil->max_reservoir (root_depth);

  // Reservior.
  const double trigger_deficit = 0.35;
  reservoir += precipitation - potential_evapotranspiration;
  if (reservoir > max_reservoir)
    reservoir = max_reservoir;
  else if (reservoir / max_reservoir < 1.0 - trigger_deficit)
    {
      const double amount = max_reservoir - reservoir;
      TmpStream tmp;
      tmp () << "MARKVAND Irrigating " << amount << " mm";
      out.message (tmp.str ());
      IM im;
      daisy.field.irrigate_overhead (amount, im);
      reservoir = max_reservoir;
    }
}

static struct ActionMarkvandSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionMarkvand (al); }
  static bool check_alist (const AttributeList&, Treelog&)
  {
    bool ok = true;
    return ok;
  }
  ActionMarkvandSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add_check (check_alist);	
    syntax.add ("soil", Librarian<MV_Soil>::library (), Syntax::Const, 
                Syntax::Singleton,
                "Soil type to schedule irrigation on.");
    syntax.add_submodule_sequence ("map", Syntax::Const, "\
Map of Daisy crop names into MARKVAND crop descriptions.",
				   &ActionMarkvand::crop_map_t::load_syntax);
    syntax.add ("T_sum", "dg C d", Syntax::OptionalState, 
                "Temperature sum since emergence.");
    syntax.add ("reservoir", "mm", Syntax::OptionalState, 
                "Amount of water in soil reservoir.\n\
By default, the reservoir will be full at plant emergence.");
    alist.add ("description", "\
Irrigate the field according to MARKVAND scheduling.");
    Librarian<Action>::add_type ("markvand", alist, syntax, &make);
  }
} ActionMarkvand_syntax;

