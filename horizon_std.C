// horizon_std.C
// 
// Copyright 1996-2001, 2003 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2003 KVL.
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
#include "block.h"
#include "texture.h"
#include "hydraulic.h"
#include "check.h"
#include "mathlib.h"
#include "librarian.h"
#include "treelog.h"
#include "frame_model.h"
#include "intrinsics.h"
#include "library.h"

struct HorizonStandard : public Horizon
{
  // Content.
  const Texture texture;

  // Simulation.
  double texture_below (double size /* [um] */) const
  { return texture.fraction_of_minerals_smaller_than (size); }

  // Create and Destroy.
  static const std::vector<double>& get_limits ();
  static const std::vector<double> get_fractions (const Block& al);
  static const std::vector<double> get_fractions (const Frame& al);
  static double get_humus (const Block& al);
  static double get_humus (const Frame& al);
  void initialize (bool top_soil, int som_size, Treelog& msg);
  HorizonStandard (const Block& al);
  HorizonStandard (const Frame& al, const double K_sat);
  ~HorizonStandard ();
};

const std::vector<double>&
HorizonStandard::get_limits ()
{
  static std::vector<double> USDA_limits;
  if (USDA_limits.size () < 1)
    {
      USDA_limits.push_back (2.0); // Clay < 2 [um]
      USDA_limits.push_back (50.0); // Silt < 50 [um]
      USDA_limits.push_back (2000.0); // Sand < 2 [mm]
    }
  return USDA_limits;
}

const std::vector<double>
HorizonStandard::get_fractions (const Block& al)
{
  daisy_assert (al.check ("sand") || 
		(al.check ("fine_sand") && al.check ("coarse_sand")));
  const double sand = al.check ("sand") 
    ? al.number ("sand") 
    : (al.number ("fine_sand")
       + al.number ("coarse_sand"));
  const double silt =  al.number ("silt");
  const double clay =  al.number ("clay");
  const double total = sand + silt + clay;
  daisy_assert (total > 0.0);

  std::vector<double> result;
  result.push_back (clay / total);
  result.push_back (silt / total);
  result.push_back (sand / total);
  daisy_assert (get_limits().size () == result.size ());

  return result;
}

const std::vector<double>
HorizonStandard::get_fractions (const Frame& al)
{
  daisy_assert (al.check ("sand") || 
		(al.check ("fine_sand") && al.check ("coarse_sand")));
  const double sand = al.check ("sand") 
    ? al.number ("sand") 
    : (al.number ("fine_sand")
       + al.number ("coarse_sand"));
  const double silt =  al.number ("silt");
  const double clay =  al.number ("clay");
  const double total = sand + silt + clay;
  daisy_assert (total > 0.0);

  std::vector<double> result;
  result.push_back (clay / total);
  result.push_back (silt / total);
  result.push_back (sand / total);
  daisy_assert (get_limits().size () == result.size ());

  return result;
}

double
HorizonStandard::get_humus (const Block& al)
{
  daisy_assert (al.check ("sand") || 
		(al.check ("fine_sand") && al.check ("coarse_sand")));
  const double sand = al.check ("sand") 
    ? al.number ("sand") 
    : (al.number ("fine_sand")
       + al.number ("coarse_sand"));
  const double silt =  al.number ("silt");
  const double clay =  al.number ("clay");
  const double humus =  al.number ("humus");
  const double total =  sand + silt + clay + humus;
  return humus / total;
}

double
HorizonStandard::get_humus (const Frame& al)
{
  daisy_assert (al.check ("sand") || 
		(al.check ("fine_sand") && al.check ("coarse_sand")));
  const double sand = al.check ("sand") 
    ? al.number ("sand") 
    : (al.number ("fine_sand")
       + al.number ("coarse_sand"));
  const double silt =  al.number ("silt");
  const double clay =  al.number ("clay");
  const double humus =  al.number ("humus");
  const double total =  sand + silt + clay + humus;
  return humus / total;
}

void
HorizonStandard::initialize (bool top_soil, int som_size, Treelog& msg)
{ initialize_base (top_soil, som_size, texture, msg); }

HorizonStandard::HorizonStandard (const Block& al)
  : Horizon (al),
    texture (get_limits (), get_fractions (al), 
             get_humus (al), 0.0)
{ }

HorizonStandard::HorizonStandard (const Frame& al, const double K_sat)
  : Horizon (al, K_sat),
    texture (get_limits (), get_fractions (al), 
             get_humus (al), 0.0)
{ }

HorizonStandard::~HorizonStandard ()
{ }

static struct HorizonStandardSyntax : public DeclareModel
{
  Model* make (const Block& al) const
  { return new HorizonStandard (al); }

  static bool check_alist (const Metalib&, const Frame& al, Treelog& err)
  {
    bool ok = true;

    double sand = al.number ("sand", -1.0);
    const double fine_sand = al.number ("fine_sand", -1.0);
    const double coarse_sand =  al.number ("coarse_sand", -1.0);
    if (sand >= 0.0)
      {
        if (fine_sand > sand)
          {
            err.entry ("You can't have more fine sand than sand");
            ok = false;
          }
        if (coarse_sand >= sand)
          {
            err.entry ("You can't have more coarse sand than sand");
            ok = false;
          }
        if (fine_sand >= 0.0 && coarse_sand >= 0.0
            && !approximate (fine_sand + coarse_sand, sand))
          {
            err.entry ("Sand is either fine or coarse");
            ok = false;
          }
      }
    else if (fine_sand < 0.0 || coarse_sand < 0.0)
      {
        err.entry ("You must specify the total amount of sand");
        ok = false;
      }
    else
      sand = fine_sand + coarse_sand;

    static bool fine_coarse_warned = false;
    if (!fine_coarse_warned && (fine_sand >= 0.0 || coarse_sand >= 0.0))
      {
        err.entry ("\
NOTE: A division of sand between 'fine_sand' and 'coarse_sand' is not\n\
supported by the USDA/FAO texture classification.  Daisy will therefore\n\
internally treat both as the same.  Please don't use the 'default' horizon\n\
model, and if you use it anyway, please only specify the total sand\n\
content, using the fine/coarse division is misleading and confusing.\n\
Maybe you ment to use the ISSS4 texture clasification, which does\n\
distinguish between fine and coarse sand?");
        fine_coarse_warned = true;
      }
    const double silt =  al.number ("silt");
    const double clay =  al.number ("clay");
    const double total = sand + silt + clay;

    if (!std::isnormal (total) || total <= 0.0)
      {
        err.entry ("There must be some sand, silt or clay in the soil");
        ok = false;
      }
    return ok;
  }
  HorizonStandardSyntax ()
    : DeclareModel (Horizon::component, "default",
               "USDA/FAO texture classification.\n\
\n\
The soil consitutents are automatically normalized.\n\
\n\
OBSOLETE: Use the USDA or FAO model instead.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.declare ("clay", Attribute::None (), Check::non_negative (), Attribute::Const,
                "Relative fraction of clay in soil.");
    frame.declare ("silt", Attribute::None (), Check::non_negative (), Attribute::Const,
                "Relative fraction of silt in soil.");
    frame.declare ("fine_sand", Attribute::None (), Check::non_negative (), 
                Attribute::OptionalConst,
                "Relative fraction of fine sand in soil.\n\
NOTE: Not a real texture class, use 'sand' instead.");
    frame.declare ("coarse_sand", Attribute::None (), Check::non_negative (), 
                Attribute::OptionalConst,
                "Relative fraction of coarse sand in soil.\n\
NOTE: Not a real texture class, use 'sand' instead.");
    frame.declare ("sand", Attribute::None (), Check::non_negative (), 
                Attribute::OptionalConst,
                "Relative fraction of sand in soil.");
    frame.declare ("humus", Attribute::None (), Check::non_negative (), 
                Attribute::Const,
                "Relative fraction of humus in soil.");

  }
} HorizonStandard_syntax;

static struct HorizonAquitardSyntax : public DeclareParam
{ 
  HorizonAquitardSyntax ()
    : DeclareParam (Horizon::component, "aquitard", "default", "\
Tecture for implicit aquitard horizon.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("clay", 50.0);
    frame.set ("silt", 20.0);
    frame.set ("sand", 29.99);
    frame.set ("humus", 0.01);
    frame.set ("dry_bulk_density", 2.0);
  }
} HorizonAquitard_syntax;

std::auto_ptr<Horizon> 
Horizon::create_aquitard (double K_sat)
{
  const Library& library = Librarian::intrinsics ().library (component);
  daisy_assert (library.check ("aquitard"));
  const FrameModel& frame = library.model ("aquitard");
  return std::auto_ptr<Horizon> (new HorizonStandard (frame, K_sat));
}

// horizon_std.C ends here.
