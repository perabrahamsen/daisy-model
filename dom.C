// dom.C --- A single dissolved organic matter pool.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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


#include "dom.h"
#include "element.h"
#include "smb.h"
#include "om.h"
#include "geometry.h"
#include "submodel.h"
#include "block.h"
#include "alist.h"
#include "syntax.h"
#include "soil.h"
#include "soil_water.h"
#include "log.h"
#include "check.h"

using namespace std;

void 
DOM::output (Log& log) const
{
  output_submodule (C, "C", log);
  output_submodule (N, "N", log);
  output_derived (adsorption, "adsorption", log);
}

void 
DOM::mix (const Geometry& geo, const Soil& soil, const SoilWater& soil_water,
	  double from, double to)
{
  C.mix (geo, soil, soil_water, *adsorption, from, to);
  N.mix (geo, soil, soil_water, *adsorption, from, to);
}

void 
DOM::swap (const Geometry& geo, const Soil& soil, const SoilWater& soil_water,
	   double from, double middle, double to)
{
  C.swap (geo, soil, soil_water, *adsorption, from, middle, to);
  N.swap (geo, soil, soil_water, *adsorption, from, middle, to);
}

void
DOM::add_to_source (unsigned int at, double to_C, double to_N)
{
  C.S[at] += to_C;
  N.S[at] += to_N;
}

double 
DOM::soil_C (const Geometry& geo) const
{ return geo.total_soil (C.M); }

double 
DOM::soil_N (const Geometry& geo) const
{ return geo.total_soil (N.M); }

double 
DOM::soil_C (const Geometry& geo, double from, double to) const
{ return geo.total_soil (C.M, from, to); }

double 
DOM::soil_N (const Geometry& geo, double from, double to) const
{ return geo.total_soil (N.M, from, to); }

double
DOM::C_source (const Geometry& geo) const
{ return geo.total_soil (C.S); }

double
DOM::N_source (const Geometry& geo) const
{ return geo.total_soil (N.S); }

double 
DOM::C_at (unsigned int at) const
{ return C.M[at]; }
  
double 
DOM::N_at (unsigned int at) const
{ return N.M[at]; }

void
DOM::clear ()
{
  // Clear sources.
  fill (C.S.begin (), C.S.end (), 0.0);
  fill (N.S.begin (), N.S.end (), 0.0);
}

void 
DOM::turnover (const std::vector<bool>& active, const double* turnover_factor, 
	       const double* N_soil, double* N_used,
	       double* CO2, const vector<SMB*>& smb, const double dt)
{
  // Find size.
  const size_t cell_size = active.size ();
  daisy_assert (C.M.size () == cell_size);
  daisy_assert (N.M.size () == cell_size);
  const unsigned int smb_size = smb.size ();
  daisy_assert (fractions.size () == smb_size);
  // Distribute to all biological pools.
  for (unsigned int j = 0; j < smb_size; j++)
    {
      const double fraction = fractions[j];
      if (fraction > 1e-50)
	tock (active, turnover_factor, turnover_rate * fraction, efficiency[j],
	      N_soil, N_used, CO2, *smb[j], dt);
    }
}

void
DOM::tock (const std::vector<bool>& active, 
	   const double* factor, double fraction, double efficiency,
	   const double* N_soil, double* N_used, double* CO2, OM& om,
           const double dt)
{
  const size_t cell_size = active.size ();
  daisy_assert (C.M.size () == cell_size);
  daisy_assert (N.M.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
    {
      if (!active[i])
        continue;
      double rate = min (factor[i] * fraction, 0.1);
      daisy_assert (C.M[i] >= 0.0);
      daisy_assert (isfinite (rate));
      daisy_assert (rate >=0);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (N.M[i] >= 0.0);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (om.C[i] >= 0.0);
      double C_use;
      double N_produce;
      double N_consume;
      
      OM::turnover (C.M[i], N.M[i], om.goal_C_per_N (i), N_soil[i] - N_used[i],
		    min (factor[i] * fraction, 0.1), efficiency,
		    C_use, N_produce, N_consume);
      add_to_source (i, -C_use, -N_produce);

      // Update C.
      daisy_assert (om.C[i] >= 0.0);
      CO2[i] += C_use * (1.0 - efficiency);
      om.C[i] += C_use * efficiency * dt;
      daisy_assert (om.C[i] >= 0.0);
      daisy_assert (C.M[i] >= 0.0);

      // Update N.
      N_used[i] += (N_consume - N_produce);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N.M[i] >= 0.0);
      om.N[i] += N_consume * dt;
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N.M[i] >= 0.0);
    }
}

void 
DOM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  // Submodel.
  alist.add ("submodel", "DOM");
  alist.add ("description", "\
A single Dissolved Organic Matter pool.");

  // Content.
  syntax.add_submodule ("C", alist, Syntax::State,
			"Carbon content of DOM pool.",
			Element::load_syntax);
  syntax.add_submodule ("N", alist, Syntax::State,
			"Nitrogen content of DOM pool.",
			Element::load_syntax);

  // Transport
  syntax.add_object ("adsorption", Adsorption::component, 
                     "Soil adsorption properties.");
  syntax.add ("diffusion_coefficient", "cm^2/s", Check::positive (),
	      Syntax::Const, "Diffusion coefficient.");

  // Turnover.
  syntax.add ("heat_factor", "dg C", Syntax::None (), Syntax::OptionalConst,
	      "Heat factor.  If empty, use default from 'OrganicMatter'.");
  syntax.add ("water_factor", "cm", Syntax::None (), Syntax::OptionalConst, "\
Water potential factor.  If empty, use default from 'OrganicMatter'.");
  syntax.add ("turnover_rate", "h^-1", Check::fraction (), 
	      Syntax::OptionalConst,
	      "Fraction converted to other pools each hour.\n\
You must specify either this or 'turnover_halftime'.");
  syntax.add ("turnover_halftime", "h", Check::positive (), 
	      Syntax::OptionalConst,
	      "Time until half had been converted to other pools.\n\
You must specify either this or 'turnover_rate'.");
  syntax.add_fraction ("efficiency", Syntax::Const, Syntax::Sequence, "\
the efficiency this pool can be digested by each of the SMB pools.");
  syntax.add_fraction ("fractions", Syntax::Const, Syntax::Sequence, "\
Fraction of this pool that ends up in each SMB pools");
}

void
DOM::initialize (const Geometry& geo, 
                 const Soil& soil, const SoilWater& soil_water, Treelog& msg)
{ 
  C.initialize (geo, soil, soil_water, *adsorption, msg);
  N.initialize (geo, soil, soil_water, *adsorption, msg);
}

DOM::DOM (Block& al)
  : C (*new Element (al.alist ("C"))),
    N (*new Element (al.alist ("N"))),
    adsorption (Librarian<Adsorption>::build_item (al, "adsorption")),
    diffusion_coefficient (al.number ("diffusion_coefficient")),
    turnover_rate (al.check ("turnover_rate")
		   ? al.number ("turnover_rate")
		   : halftime_to_rate (al.number ("turnover_halftime"))),
    efficiency (al.number_sequence ("efficiency")),
    fractions (al.number_sequence ("fractions"))
{
  if (al.check ("heat_factor"))
    heat_factor = al.plf ("heat_factor");
  if (al.check ("water_factor"))
    water_factor = al.plf ("water_factor");
 }

DOM::~DOM ()
{ 
  delete &C;
  delete &N;
}

static Submodel::Register dom_submodel ("DOM", DOM::load_syntax);
