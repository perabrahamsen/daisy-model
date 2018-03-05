// irrigate.C --- Manage irrigation events.
// 
// Copyright 2010 KU.
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

#include "irrigate.h"
#include "volume.h"
#include "im.h"
#include "soil_water.h"
#include "bioclimate.h"
#include "chemistry.h"
#include "chemical.h"
#include "frame.h"
#include "check.h"
#include "vcheck.h"
#include "block_submodel.h"
#include "submodeler.h"
#include "librarian.h"
#include "assertion.h"
#include "mathlib.h"
#include "units.h"
#include "treelog.h"
#include "log.h"
#include <sstream>

const symbol 
Irrigation::solute_per_mm ("g/cm^2/mm");

const symbol 
Irrigation::conc_flux ("kg/ha/mm");

const double 
Irrigation::at_air_temperature = -500.0;

class Irrigation::Event : private boost::noncopyable
{
public:
  // Content.
  double time_left;           // [h]
  const double flux;          // [mm/h]
  const double temperature;   // [dg C]
  const IM solute;            // [M/L^3]
  const target_t target;
  static target_t symbol2target (symbol s);
  static symbol target2symbol (target_t t);
  const boost::shared_ptr<Volume> volume;
  const bool silence;

  // Use.
  void tick (const Unit& u_mm, const Unit& u_storage, 
             const Geometry&, SoilWater&, Chemistry&, Bioclimate&,
             const double dt, Treelog&);
  bool done () const;
  void output (Log&) const;

  // Create and Destroy.
  static void load_solute (Frame&);
  static bool check_alist (const Metalib&, const Frame&, Treelog&);
  static void load_syntax (Frame&);
  explicit Event (const BlockSubmodel&);
  Event (const Unit& u_solute_per_mm /* [g/cm^2/mm] */, 
         const double duration /* [h] */,
         const double flux /* [mm/h] */,
         const double temperature /* dg C */,
         const target_t target,
         const IM& solute /* [M/L^3] */,
         const boost::shared_ptr<Volume> volume,
         const bool silence);
};

Irrigation::target_t 
Irrigation::Event::symbol2target (const symbol s)
{
  static struct sym_set_t : std::map<symbol, target_t>
  {
    sym_set_t ()
    {
      insert (std::pair<symbol,target_t> ("overhead", overhead));
      insert (std::pair<symbol,target_t> ("surface", surface));
      insert (std::pair<symbol,target_t> ("subsoil", subsoil));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (s);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}  

symbol 
Irrigation::Event::target2symbol (const target_t t)
{
  static struct sym_set_t : std::map<target_t, symbol>
  {
    sym_set_t ()
    {
      insert (std::pair<target_t,symbol> (overhead, "overhead"));
      insert (std::pair<target_t,symbol> (surface, "surface"));
      insert (std::pair<target_t,symbol> (subsoil, "subsoil"));
    } 
  } sym_set;
  sym_set_t::const_iterator i = sym_set.find (t);
  daisy_assert (i != sym_set.end ());
  return (*i).second;
}

void
Irrigation::Event::tick (const Unit& u_mm, const Unit& u_storage, 
                         const Geometry& geo, SoilWater& soil_water,
                         Chemistry& chemistry, Bioclimate& bioclimate,
                         const double dt, Treelog& msg)
{ 
  // Time.
  const double time_used = std::min (dt, time_left);
  time_left -= time_used;
  const double amount = flux * time_used;
  const double flux_used = amount / dt;
  
  // Water.
  switch (target)
    {
    case overhead:
      if (approximate (temperature, at_air_temperature))
        bioclimate.irrigate_overhead (flux_used);
      else
        bioclimate.irrigate_overhead (flux_used, temperature);
      break;

    case surface:
      if (approximate (temperature, at_air_temperature))
        bioclimate.irrigate_surface (flux_used);
      else
        bioclimate.irrigate_surface (flux_used, temperature);
      break;

    case subsoil:
      bioclimate.irrigate_subsoil (flux_used);
      daisy_assert (volume.get ());
      soil_water.incorporate (geo, flux_used * 0.1 /* mm -> cm */, *volume);
      break;
    }
  
  // Solute amount.
  IM im = solute;
  im.multiply_assign (Scalar (amount, u_mm), u_storage);

  // Apply it.
  switch (target)
    {
    case overhead:
      chemistry.spray_overhead (im, msg);
      break;

    case surface:
      chemistry.spray_surface (im, msg);
      break;

    case subsoil:
      daisy_assert (volume.get ());
      chemistry.incorporate (geo, im, *volume, msg);
      break;
    }
}

bool
Irrigation::Event::done () const
{ return time_left < 1e-9; }

void 
Irrigation::Event::output (Log& log) const
{
  output_variable (time_left, log);
  output_variable (flux, log);
  if (!approximate (temperature, at_air_temperature))
    output_variable (temperature, log);
  output_submodule (solute, "solute", log);
  output_value (target2symbol (target), "target", log);
  if (volume.get ())
    output_derived (volume, "volume", log);
  output_variable (silence, log);
}

void 
Irrigation::Event::load_solute (Frame& frame)
{ IM::add_syntax (frame, Attribute::State, solute_per_mm); }

bool 
Irrigation::Event::check_alist (const Metalib&, 
                                const Frame& frame, Treelog& msg)
{
  bool ok = true;
  if (frame.name ("target") == target2symbol (subsoil)
      && !frame.check ("volume"))
    {
      msg.error ("Subsoil irrigation specified with no volume");
      ok = false;
    }
  return ok;
}

void 
Irrigation::Event::load_syntax (Frame& frame)
{ 
  frame.add_check (check_alist);
  frame.declare ("time_left", "h", Check::non_negative (), Attribute::State, "\
Time left of this irrigation event.");
  frame.declare ("flux", "mm/h", Check::non_negative (), Attribute::State, "\
Water applied.");
  frame.declare ("temperature", "dg C", Attribute::OptionalState, "\
Irrigation temperature. By default, use daily air temperature.\n\
Ignored for subsoil irrigation.");
  frame.declare_submodule_sequence ("solute", Attribute::State, "\
Solutes in irrigation water.", load_solute);
  frame.declare_string ("target", Attribute::State, "\
Where to apply the irrigation.  \n\
\n\
overhead: Above crop canopy.\n\
surface: On soil surface, below crop canopy.\n\
subsoil: In the soil.  The 'volume' parameter will specify where.");
  static VCheck::Enum target_check ("overhead", "surface", "subsoil");
  frame.set_check ("target", target_check);
  frame.declare_object ("volume", Volume::component, 
                        Attribute::OptionalState, Attribute::Singleton, "\
Soil volume to apply for subsoil irrigation.\n\
Ignored for overhead and surface irrigation.");
  frame.declare_boolean ("silence", Attribute::State, "\
True if event should not declare when it is over.");
}

Irrigation::Event::Event (const BlockSubmodel& al)
  : time_left (al.number ("time_left")),
    flux (al.number ("flux")),
    temperature (al.number ("temperature", at_air_temperature)),
    solute (al, "solute"),
    target (symbol2target (al.name ("target"))),
    volume (al.check ("volume") 
            ? Librarian::build_item<Volume> (al, "volume")
            : NULL),
    silence (al.flag ("silence"))
{ }

Irrigation::Event::Event (const Unit& u_solute_per_mm /* [g/cm^2/mm] */, 
                          const double duration /* [h] */,
                          const double f /* [mm/h] */,
                          const double temp /* dg C */,
                          const target_t targ,
                          const IM& sol /* [M/L^3] */,
                          const boost::shared_ptr<Volume> vol,
                          const bool sil)
  : time_left (duration),
    flux (f),
    temperature (temp),
    solute (u_solute_per_mm, sol),
    target (targ),
    volume (vol),
    silence (sil)
{ }

void 
Irrigation::add (const double duration /* [h] */,
                 const double flux /* [mm/h] */,
                 const double temperature /* dg C */,
                 const target_t target,
                 const IM& solute /* [M/L^3] */,
                 const boost::shared_ptr<Volume> volume, 
                 const bool silence,
                 Treelog& msg)
{
  event.push_back (new Event (u_solute_per_mm,
                              duration, flux, temperature, target, 
                              solute, volume, silence));
  
  if (silence)
    return;

  std::ostringstream tmp;
  tmp << "Irrigating " << flux << " mm/h for "
      << duration << " hour";
  if (!approximate (duration, 1.0))
    tmp << "s";
  tmp << " total " << flux * duration << " mm";

  const double N = (solute.get_value (Chemical::NO3 (), u_conc_flux)
                    + solute.get_value (Chemical::NH4 (), u_conc_flux))
    * flux * duration;
  if (N > 1e-10)
    tmp << "; adding " << N << " kg N/ha";
  for (IM::const_iterator i = solute.begin (); i != solute.end (); i++)
    {
      const symbol chem = *i;
      if (chem == Chemical::NO3 () || chem == Chemical::NH4 ())
        continue;
      const double value = solute.get_value (chem, u_conc_flux)
        * flux * duration * 1000.0 /* [g/kg] */;
      if (!std::isnormal (value))
        continue;
      tmp << "; " << value << " g " << chem << "/ha";
    }
			  
  msg.message (tmp.str ());
}

double
Irrigation::suggest_dt () const
{
  double dt = NAN;
  for (const Event *const e : event)
    if (std::isnormal (e->time_left)
	&& (!std::isnormal (dt)
	    || e->time_left < dt))
      dt = e->time_left;
  return dt;
}

void
Irrigation::tick (const Geometry& geo, SoilWater& soil_water,
                  Chemistry& chemistry, Bioclimate& bioclimate,
                  const double dt, Treelog& msg)
{
  // Perform events.
  for (size_t e = 0; e < event.size (); e++)
    event[e]->tick (u_mm, u_storage,
                    geo, soil_water, chemistry, bioclimate, dt, msg);

  // Remove all dead events.
  cleanup (msg);
}

void 
Irrigation::output (Log& log) const
{ output_ordered (event, "event", log); }

void
Irrigation::cleanup (Treelog& msg)
{
  // Remove all dead events.  There has to be a better way.
  bool removed;
  do
    {
      removed = false;
      for (auto_vector<Event*>::iterator e = event.begin();
	   e != event.end();
	   e++)
	if ((*e)->done ())
	  {
            if (!(*e)->silence)
              msg.message ("Irrigation done");
            delete *e;
            event.erase (e); // This invalidates the iterator.
            // Restart the loop.
            removed = true;
            break;
	  }
    }
  while (removed);
}

void 
Irrigation::load_syntax (Frame& frame)
{
  frame.declare_submodule_sequence ("event", Attribute::State, "\
Currently active irrigation events.", Event::load_syntax);
  frame.set_empty ("event");
}

Irrigation::Irrigation (const BlockSubmodel& al)
  : u_mm (al.units ().get_unit (Units::mm ())),
    u_storage (al.units ().get_unit (IM::storage_unit ())),
    u_solute_per_mm (al.units ().get_unit (solute_per_mm)),
    u_conc_flux (al.units ().get_unit (conc_flux)),
    event (map_submodel<Event> (al, "event"))
{ }

Irrigation::~Irrigation ()
{ }

static DeclareSubmodel 
irrigation_submodel (Irrigation::load_syntax, "Irrigation", "\
Keep track of active irrigation events.\n                       \
Usually not set explicitly, but may be found in a checkpint.");

// irrigate.C ends here.

