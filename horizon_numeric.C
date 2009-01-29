// horizon_numeric.C --- A horizon with numeric texture classification.
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
#include "block.h"
#include "texture.h"
#include "hydraulic.h"
#include "check.h"
#include "vcheck.h"
#include "mathlib.h"
#include "librarian.h"
#include "treelog.h"
#include "frame.h"
#include "alist.h"
#include <sstream>
#include <numeric>

struct HorizonNumeric : public Horizon
{
  // Content.
  const Texture texture;

  // Simulation.
  double texture_below (double size /* [um] */) const
  { return texture.fraction_of_minerals_smaller_than (size); }

  // Create and Destroy.
  void initialize (bool top_soil, int som_size, Treelog& msg)
  { initialize_base (top_soil, som_size, texture, msg); }
  static const std::vector<double> normalize (const std::vector<double>& original);
  HorizonNumeric (Block& al)
    : Horizon (al),
      texture (al.number_sequence ("limits"),
               normalize (al.number_sequence ("fractions")),
               al.number ("humus"), 0.0)
  { }
  ~HorizonNumeric ()
  { }
};

const std::vector<double>
HorizonNumeric::normalize (const std::vector<double>& original)
{
  const double sum = accumulate (original.begin (), original.end (), 0.0);

  if (approximate (sum, 1.0))
    return original;

  daisy_assert (sum > 0.0);

  std::vector<double> normalized;

  for (size_t i = 0; i < original.size (); i++)
    normalized.push_back (original[i] / sum);
  
  daisy_assert (approximate (accumulate (normalized.begin (),
                                         normalized.end (), 0.0), 
                             1.0));
  daisy_assert (normalized.size () == original.size ());

  return normalized;
}

static struct HorizonNumericSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new HorizonNumeric (al); }

  static bool check_alist (Metalib&, const Frame& al, Treelog& err)
  {
    bool ok = true;

    const std::vector<double>& limits = al.number_sequence ("limits");
    const std::vector<double>& fractions = al.number_sequence ("fractions");

    if (limits.size () != fractions.size ())
      {
        std::ostringstream tmp;
        tmp << "You have " << limits.size () << " limits, but "
            << fractions.size () << " fractions";
        err.error (tmp.str ());
        ok = false;
      }
    const double sum = accumulate (fractions.begin (), fractions.end (), 0.0);
    if (!al.flag ("normalize")
        && !approximate (sum, 1.0)
        && !approximate (sum + al.number ("humus"), 1.0))
      {
        err.error ("The sum of all soil components must be 1.0");
        ok = false;
      }
    return ok;
  }

  HorizonNumericSyntax ()
    : DeclareModel (Horizon::component, "numeric", "\
A horizon using explicit texture classification.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add_check (check_alist);
    frame.add ("limits", "um", Check::positive (),
                Value::Const, Value::Sequence, 
                "Numerical limits for particle sizes.");
    static const VCheck::All lim_check (VCheck::increasing (), 
                                        VCheck::min_size_1 ());
    frame.add_check ("limits", lim_check);
    frame.add_fraction ("fractions", Value::Const, Value::Sequence, "\
Fraction of particles between the corresponding numrical limits.");
    frame.add_check ("fractions", VCheck::min_size_1 ());
    
    frame.add_fraction ("humus", Value::Const,
                         "Humus content of soil.");
    frame.add ("normalize", Value::Boolean, Value::Const, "\
If this is true, normalize the mineral fraction to 1.0.\n\
Otherwise, give an error if the sum is not 1.0.");
    frame.add ("normalize", false);

  }
} HorizonNumeric_syntax;

// horizon_numeric.C ends here.
