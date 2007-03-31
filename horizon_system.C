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


#include "horizon.h"
#include "library.h"
#include "block.h"
#include "alist.h"
#include "texture.h"
#include "hydraulic.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include <sstream>
#include <numeric>

using namespace std;

struct HorizonSystem : public Horizon
{
  // Types.
  struct System 
  { 
    // Content.
    const string name;
    vector<double> limits;
    vector<string> names;

    // Utilities. 
    const vector<double> get_fractions (Block& al) const;
    bool check_alist (const AttributeList& al, Treelog& err) const;

    // Create and destroy.
    void add (const string& name, double limit);
    System (const string n)
      : name (n)
    { }
    void add_to_lib (Model& (*make)(Block&),
                     Syntax::check_fun check_list) const;
  };

  // Content.
  const Texture texture;

  // Simulation.
  double texture_below (double size /* [um] */) const
  { return texture.fraction_of_minerals_smaller_than (size); }

  // Create and Destroy.
  void initialize (bool top_soil, int som_size, Treelog& msg)
  { initialize_base (top_soil, som_size, texture, msg); }
  HorizonSystem (const System& system,
                   Block& al)
    : Horizon (al),
      texture (system.limits, system.get_fractions (al), 
               al.number ("humus"), 0.0)
  { }
  ~HorizonSystem ()
  { }
};

const vector<double>
HorizonSystem::System::get_fractions (Block& al) const
{
  vector<double> result;
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

bool 
HorizonSystem::System::check_alist (const AttributeList& al,
                                    Treelog& err) const
{
  bool ok = true;

  double sum = 0.0;
  for (unsigned int i = 0; i < names.size (); i++)
    sum += al.number (names[i]);
  
  if (!al.flag ("normalize")
      && !approximate (sum, 1.0) 
      && !approximate (sum + al.number ("humus"), 1.0))
    {
      err.error ("The sum of all soil components must be 1.0");
      ok = false;
    }
  return ok;
}

void 
HorizonSystem::System::add (const string& name, double limit)
{
  daisy_assert (limits.size () < 1 || limits[limits.size () - 1] < limit);
  limits.push_back (limit);
  for (unsigned int i = 0; i < names.size (); i++)
    daisy_assert (name != names[i]);
  names.push_back (name);
  daisy_assert (limits.size () == names.size ());
}

void
HorizonSystem::System::add_to_lib (Model& (make)(Block&),
                                   Syntax::check_fun check_alist) const
{
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    Horizon::load_syntax (syntax, alist);
    syntax.add_check (check_alist);
    alist.add ("description",
               "A horizon using " +  name + " texture classification.");

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
        
        syntax.add_fraction (names[i], Syntax::Const, tmp.str ());
      }
    syntax.add_fraction ("humus", Syntax::Const,
                         "Humus content of soil.");
    syntax.add ("normalize", Syntax::Boolean, Syntax::Const, "\
If this is true, normalize the mineral fraction to 1.0.\n\
Otherwise, give an error if the sum is not 1.0.");
    alist.add ("normalize", false);
    Librarian::add_type (Horizon::component, symbol (name), alist, syntax, make);
}

static const struct USDA3_type : public HorizonSystem::System
{
  USDA3_type ()
    : System ("USDA3")
  {
    add ("clay", 2.0);
    add ("silt", 50.0);
    add ("sand", 2000.0);
  }
} USDA3;

static const struct USDA7_type : public HorizonSystem::System
{
  USDA7_type ()
    : System ("USDA7")
  {
    add ("clay", 2.0);
    add ("silt", 50.0);
    add ("very_fine_sand", 100.0);
    add ("fine_sand", 250.0);
    add ("medium_sand", 500.0);
    add ("coarse_sand", 1000.0);
    add ("very_corase_sand", 2000.0);
  }
} USDA7;

static const struct ISSS3_type : public HorizonSystem::System
{
  ISSS3_type ()
    : System ("ISSS3")
  {
    add ("clay", 2.0);
    add ("silt", 20.0);
    add ("sand", 2000.0);
  }
} ISSS3;

static const struct ISSS4_type : public HorizonSystem::System
{
  ISSS4_type ()
    : System ("ISSS4")
  {
    add ("clay", 2.0);
    add ("silt", 20.0);
    add ("fine_sand", 200.0);
    add ("coarse_sand", 2000.0);
  }
} ISSS4;

static const struct USPRA3_type : public HorizonSystem::System
{
  USPRA3_type ()
    : System ("USPRA3")
  {
    add ("clay", 5.0);
    add ("silt", 50.0);
    add ("sand", 2000.0);
  }
} USPRA3;

static const struct USPRA4_type : public HorizonSystem::System
{
  USPRA4_type ()
    : System ("USPRA4")
  {
    add ("clay", 5.0);
    add ("silt", 50.0);
    add ("fine_sand", 250.0);
    add ("coarse_sand", 2000.0);
  }
} USPRA4;

static const struct BSI3_type : public HorizonSystem::System
{
  BSI3_type ()
    : System ("BSI3")
  {
    add ("clay", 2.0);
    add ("silt", 60.0);
    add ("sand", 2000.0);
  }
} BSI3;

static const struct BSI7_type : public HorizonSystem::System
{
  BSI7_type ()
    : System ("BSI7")
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

static const struct DIN5_type : public HorizonSystem::System
{
  DIN5_type ()
    : System ("DIN5")
  {
    add ("clay", 2.0);
    add ("silt", 60.0);
    add ("fine_sand", 200.0);
    add ("medium_sand", 600.0);
    add ("coarse_sand", 2000.0);
  }
} DIN5;

static struct HorizonSystemSyntax
{
  static Model& make_USDA3 (Block& al)
  { return *new HorizonSystem (USDA3, al); }
  static bool check_USDA3 (const AttributeList& al, Treelog& err)
  { return USDA3.check_alist (al, err); }

  static Model& make_USDA7 (Block& al)
  { return *new HorizonSystem (USDA7, al); }
  static bool check_USDA7 (const AttributeList& al, Treelog& err)
  { return USDA7.check_alist (al, err); }

  static Model& make_ISSS3 (Block& al)
  { return *new HorizonSystem (ISSS3, al); }
  static bool check_ISSS3 (const AttributeList& al, Treelog& err)
  { return ISSS3.check_alist (al, err); }

  static Model& make_ISSS4 (Block& al)
  { return *new HorizonSystem (ISSS4, al); }
  static bool check_ISSS4 (const AttributeList& al, Treelog& err)
  { return ISSS4.check_alist (al, err); }

  static Model& make_USPRA3 (Block& al)
  { return *new HorizonSystem (USPRA3, al); }
  static bool check_USPRA3 (const AttributeList& al, Treelog& err)
  { return USPRA3.check_alist (al, err); }

  static Model& make_USPRA4 (Block& al)
  { return *new HorizonSystem (USPRA4, al); }
  static bool check_USPRA4 (const AttributeList& al, Treelog& err)
  { return USPRA4.check_alist (al, err); }

  static Model& make_BSI3 (Block& al)
  { return *new HorizonSystem (BSI3, al); }
  static bool check_BSI3 (const AttributeList& al, Treelog& err)
  { return BSI3.check_alist (al, err); }

  static Model& make_BSI7 (Block& al)
  { return *new HorizonSystem (BSI7, al); }
  static bool check_BSI7 (const AttributeList& al, Treelog& err)
  { return BSI7.check_alist (al, err); }

  static Model& make_DIN5 (Block& al)
  { return *new HorizonSystem (DIN5, al); }
  static bool check_DIN5 (const AttributeList& al, Treelog& err)
  { return DIN5.check_alist (al, err); }

  static void derive (const string& d, const string& b)
  {
    const symbol derived = symbol (d);
    const symbol base = symbol (b);
    Librarian::add_alias (Horizon::component, derived, base);
  }

  HorizonSystemSyntax ()
  { 
    USDA3.add_to_lib (make_USDA3, check_USDA3);
    USDA7.add_to_lib (make_USDA7, check_USDA7);
    derive ("FAO3", "USDA3");
    derive ("FAO7", "USDA7");
    ISSS3.add_to_lib (make_ISSS3, check_ISSS3);
    ISSS4.add_to_lib (make_ISSS4, check_ISSS4);
    USPRA3.add_to_lib (make_USPRA3, check_USPRA3);
    USPRA4.add_to_lib (make_USPRA4, check_USPRA4);
    BSI3.add_to_lib (make_BSI3, check_BSI3);
    BSI7.add_to_lib (make_BSI7, check_BSI7);
    derive ("MIT3", "BSI3");
    derive ("MIT7", "BSI7");
    DIN5.add_to_lib (make_DIN5, check_DIN5);
    derive ("DIN3", "BSI3");

    // DIN3 = BSI3
    // MIT = BSI
    // FAO = USDA
  }
} HorizonSystem_syntax;
