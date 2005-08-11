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

// MV_Soil

struct MV_Soil
{
  // Content.
  const symbol name;
  static const char *const description;

  // Simulation.

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
  static vector<double> accumulated (const vector<double>& numbers)
  {
    vector<double> result;
    double sum = 0.0;
    for (size_t i = 0; i < numbers.size (); i++)
      {
	sum += numbers[i];
	result.push_back (sum);
      }
    return result;
  }
  const vector<double> T_sum;
  size_t phase (const double T)
  { 
    size_t i = 0; 
    while (i < T_sum.size () && T < T_sum[i])
      i++;
  }

  // Simulation.

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
  const auto_ptr<MV_Soil> soil;
  const auto_ptr<MV_Crop> crop;
  double T_sum;
  double reservoir;

  void doIt (Daisy& daisy, Treelog& out);
  bool done (const Daisy&) const
  { return false; }

  ActionMarkvand (const AttributeList& al)
    : Action (al),
      soil (Librarian<MV_Soil>::create (al.alist ("soil"))),
      crop (Librarian<MV_Crop>::create (al.alist ("crop"))),
      T_sum (al.number ("T_sum", -1.0)),
      reservoir (al.number ("reservoir", -1.0))
  { }
  ~ActionMarkvand ()
  { }
};

void 
ActionMarkvand::doIt (Daisy& daisy, Treelog& out)
{

  // Daily occurence.
  if (daisy.time.hour () != 8)
    return;
  
  // Emergence and harvest.
  const double max_reservoir = 10;
  static const symbol all_symbol ("all");
  const bool has_crop = daisy.field.crop_dm (all_symbol, 0.1) > 0.0; 
  if (T_sum < 0.0)
    {
      if (has_crop)
        {
          T_sum = 0.0;
          reservoir = max_reservoir;
          out.message ("Starting MARKVAND irrigation.");
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
#if 0
  else
    {
      TmpStream tmp;
      tmp () << "res = " << reservoir << " mm, P = " << precipitation
             << " mm, Ep = " << potential_evapotranspiration 
             << " mm, full = " << 100.0 * reservoir / max_reservoir << "%";
      out.message (tmp.str ());
    }
#endif

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
                "Soil type to schedule irrigation on.");
    syntax.add ("crop", Librarian<MV_Crop>::library (), Syntax::Const, 
                "Crop type to schedule irrigation for.");
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

