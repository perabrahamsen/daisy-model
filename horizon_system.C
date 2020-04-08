// horizon_system.C
// 
// Copyright 1996-2001, 2003, 2004 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003, 2004 KVL.
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

#include "horizon.h"
#include "library.h"
#include "block_model.h"
#include "frame.h"
#include "texture.h"
#include "hydraulic.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "treelog.h"
#include <sstream>
#include <numeric>

struct HorizonSystem : public Horizon
{
  // Types.
  struct System : public DeclareModel
  { 
    // Content.
    std::vector<double> limits;
    std::vector<symbol> names;
    
    // Utilities. 
    const std::vector<double> get_fractions (const BlockModel& al) const;

    // DeclareModel interface.
    Model* make (const BlockModel& al) const;
    void load_frame (Frame& frame) const;

    // Create and destroy.
    Frame::check_fun checker;
    bool check_shared (const Frame& al, Treelog& err) const;
    void add (const symbol name, double limit);
    System (symbol name, Frame::check_fun check_alist);
  };

  // Content.
  const Texture texture_;

  // Simulation.
  const Texture& texture () const
  { return texture_; }
  double texture_below (double size /* [um] */) const
  { return texture_.fraction_of_minerals_smaller_than (size); }

  // Create and Destroy.
  void initialize (bool top_soil, int som_size, double center_z, Treelog& msg)
  { initialize_base (top_soil, som_size, center_z, texture_, msg); }
  HorizonSystem (const System& system, const BlockModel& al)
    : Horizon (al),
      texture_ (system.limits, system.get_fractions (al), 
               al.number ("humus"), al.number ("chalk"))
  { }
  ~HorizonSystem ()
  { }
};

const std::vector<double>
HorizonSystem::System::get_fractions (const BlockModel& al) const
{
  std::vector<double> result;
  for (unsigned int i = 0; i < names.size (); i++)
    result.push_back (al.number (names[i]));

  const double sum = accumulate (result.begin (), result.end (), 0.0);

  if (!approximate (sum, 1.0))
    {
      daisy_assert (sum > 0.0);
      for (unsigned int i = 0; i < result.size (); i++)
        result[i] /= sum;
      daisy_assert (approximate (accumulate (result.begin (),
                                             result.end (), 0.0), 
                                 1.0));
    }
  daisy_assert (result.size () == limits.size ());
  return result;
}

Model* 
HorizonSystem::System::make (const BlockModel& al) const
{ return new HorizonSystem (*this, al); }

void 
HorizonSystem::System::load_frame (Frame& frame) const
{
  frame.add_check (checker);
  daisy_assert (names.size () == limits.size ());
  for (unsigned int i = 0; i < names.size (); i++)
    {
      std::ostringstream tmp;
      tmp << "Mineral particles ";
      if (i < 1)
        tmp << "up to";
      else
        tmp << "between " << limits[i-1] << " [um] and";
      tmp << " " << limits[i] << " [um].";

      frame.declare_fraction (names[i], Attribute::Const, tmp.str ());
    }
  frame.declare_fraction ("other", Attribute::Const,
                          "Other soil components.\n\
Typically larger mineral components (stones, pebbles).\n\
These will be ignored by Daisy, but can be included here\n\
to ensure the sum is 1.0.");
  frame.set ("other", 0.0);
  frame.declare_fraction ("humus", Attribute::Const,
                       "Humus content of soil.");
  frame.declare_fraction ("chalk", Attribute::Const,
                       "Chalk content of soil.");
  frame.set ("chalk", 0.0);
  frame.declare_boolean ("normalize", Attribute::Const, "\
If this is true, normalize the mineral fraction to 1.0.\n\
Otherwise, give an error if the sum is not 1.0.");
  frame.set ("normalize", false);
}

bool 
HorizonSystem::System::check_shared (const Frame& al, Treelog& err) const
{
  bool ok = true;

  double sum = al.number ("other");
  for (unsigned int i = 0; i < names.size (); i++)
    sum += al.number (names[i]);
  
  if (!al.flag ("normalize")
      && !approximate (sum, 1.0) 
      && !approximate (sum + al.number ("humus"), 1.0)
      && !approximate (sum + al.number ("chalk"), 1.0)
      && !approximate (sum + al.number ("humus") + al.number ("chalk"), 1.0))
    {
      err.error ("The sum of all soil components must be 1.0");
      ok = false;
    }
  return ok;
}

void 
HorizonSystem::System::add (const symbol name, double limit)
{
  daisy_assert (limits.size () < 1 || limits[limits.size () - 1] < limit);
  limits.push_back (limit);
  for (unsigned int i = 0; i < names.size (); i++)
    daisy_assert (name != names[i]);
  names.push_back (name);
  daisy_assert (limits.size () == names.size ());
}

HorizonSystem::System::System (symbol name, Frame::check_fun cf)
  : DeclareModel (Horizon::component, name, 
                  "A horizon using " +  name + " texture classification."),
    checker (cf)
{ }

static const struct USDA3_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  USDA3_type ()
    : System ("USDA3", check_alist)
  {
    add ("clay", 2.0);
    add ("silt", 50.0);
    add ("sand", 2000.0);
  }
} USDA3;

bool 
USDA3_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return USDA3.check_shared (al, err); }

static const struct USDA7_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  USDA7_type ()
    : System ("USDA7", check_alist)
  {
    add ("clay", 2.0);
    add ("silt", 50.0);
    add ("very_fine_sand", 100.0);
    add ("fine_sand", 250.0);
    add ("medium_sand", 500.0);
    add ("coarse_sand", 1000.0);
    add ("very_coarse_sand", 2000.0);
  }
} USDA7;

bool 
USDA7_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return USDA7.check_shared (al, err); }

static const struct ISSS3_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  ISSS3_type ()
    : System ("ISSS3", check_alist)
  {
    add ("clay", 2.0);
    add ("silt", 20.0);
    add ("sand", 2000.0);
  }
} ISSS3;

bool 
ISSS3_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return ISSS3.check_shared (al, err); }

static const struct ISSS4_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  ISSS4_type ()
    : System ("ISSS4", check_alist)
  {
    add ("clay", 2.0);
    add ("silt", 20.0);
    add ("fine_sand", 200.0);
    add ("coarse_sand", 2000.0);
  }
} ISSS4;

bool 
ISSS4_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return ISSS4.check_shared (al, err); }

static const struct USPRA3_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  USPRA3_type ()
    : System ("USPRA3", check_alist)
  {
    add ("clay", 5.0);
    add ("silt", 50.0);
    add ("sand", 2000.0);
  }
} USPRA3;

bool 
USPRA3_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return USPRA3.check_shared (al, err); }

static const struct USPRA4_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  USPRA4_type ()
    : System ("USPRA4", check_alist)
  {
    add ("clay", 5.0);
    add ("silt", 50.0);
    add ("fine_sand", 250.0);
    add ("coarse_sand", 2000.0);
  }
} USPRA4;

bool 
USPRA4_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return USPRA4.check_shared (al, err); }

static const struct BSI3_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  BSI3_type ()
    : System ("BSI3", check_alist)
  {
    add ("clay", 2.0);
    add ("silt", 60.0);
    add ("sand", 2000.0);
  }
} BSI3;

bool 
BSI3_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return BSI3.check_shared (al, err); }

static const struct BSI7_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  BSI7_type ()
    : System ("BSI7", check_alist)
  {
    add ("clay", 2.0);
    add ("fine_silt", 6.0);
    add ("medium_silt", 20.0);
    add ("coarse_silt", 60.0);
    add ("fine_sand", 200.0);
    add ("medium_sand", 600.0);
    add ("coarse_sand", 2000.0);
  }
} BSI7;

bool 
BSI7_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return BSI7.check_shared (al, err); }

static const struct GEUS7_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  GEUS7_type ()
    : System ("GEUS7", check_alist)
  {
    add ("clay", 2.0);
    add ("fine_silt", 6.0);
    add ("medium_silt", 20.0);
    add ("coarse_silt", 63.0);
    add ("fine_sand", 200.0);
    add ("medium_sand", 600.0);
    add ("coarse_sand", 2000.0);
  }
} GEUS7;

bool 
GEUS7_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return GEUS7.check_shared (al, err); }

static const struct DIN5_type : public HorizonSystem::System
{
  static bool check_alist (const Metalib&, const Frame& al, Treelog& err);

  DIN5_type ()
    : System ("DIN5", check_alist)
  {
    add ("clay", 2.0);
    add ("silt", 60.0);
    add ("fine_sand", 200.0);
    add ("medium_sand", 600.0);
    add ("coarse_sand", 2000.0);
  }
} DIN5;

bool 
DIN5_type::check_alist (const Metalib&, const Frame& al, Treelog& err)
{ return DIN5.check_shared (al, err); }


DeclareAlias FAO3 (Horizon::component, "FAO3", "USDA3");
DeclareAlias FAO7 (Horizon::component, "FAO7", "USDA7");
DeclareAlias MIT3 (Horizon::component, "MIT3", "BSI3");
DeclareAlias MIT7 (Horizon::component, "MIT7", "BSI7");
DeclareAlias DIN3 (Horizon::component, "DIN3", "BSI3");

// horizon_system.C ends here.
