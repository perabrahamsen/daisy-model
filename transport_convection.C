// transport_convection.C --- Using convection alone for solute transport.
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

#define BUILD_DLL

#include "transport.h"
#include "block.h"
#include "geometry1d.h"
#include "soil.h"
#include "soil_water.h"
#include "adsorption.h"
#include "log.h"
#include "mathlib.h"
#include "librarian.h"
#include <vector>
#include <sstream>

struct TransportConvection : public Transport
{
  // Parameters.
  const int max_time_step_reductions;

  // Simulation.
  void tick (Treelog&, const Geometry1D& geo,
             const Soil&, const SoilWater&, const Adsorption&,
	     double diffusion_coefficient,
	     std::vector<double>& M, 
	     std::vector<double>& C,
	     const std::vector<double>& S,
	     std::vector<double>& J, 
	     const double C_below, 
             double dt);

  // Create.
  TransportConvection (Block& al)
    : Transport (al),
      max_time_step_reductions (al.integer ("max_time_step_reductions"))
    { }
  static void load_syntax (Syntax& syntax, AttributeList& alist);
};

void 
TransportConvection::tick (Treelog& msg, 
			   const Geometry1D& geo,
                           const Soil& soil, const SoilWater& soil_water,
			   const Adsorption& adsorption, double,
			   std::vector<double>& M, 
			   std::vector<double>& C,
			   const std::vector<double>& S,
			   std::vector<double>& J,
                           const double C_below, 
			   const double dt)
{
  const double J_in = J[0];

  // Remember old values.
  const std::vector<double> C_prev = C;
  const std::vector<double> M_prev = M;

  // Number of soil layers.
  const size_t size = geo.cell_size ();

  // Remember old content
  const double old_total = geo.total_surface (M) + geo.total_surface (S) * dt;

  // Flux in individual time step.
  std::vector<double> dJ (size + 1, 0.0); 

  // Find time step.
  double ddt = dt;
  for (unsigned int i = 0; i < size; i++)
    {
      const double half_content = soil_water.Theta (i) * geo.dz (i) / 2.0;
      const double q_up = soil_water.q (i);
      if (q_up > 0.0)
	{
	  const double dd_up = half_content / q_up;
	  if (dd_up < ddt)
	    ddt = dd_up;
	}
      const double q_down = -soil_water.q (i+1);
      if (q_down > 0.0)
	{
	  const double dd_down = half_content / q_down;
	  if (dd_down < ddt)
	    ddt = dd_down;
	}
    }

  // We restart from here if anything goes wrong.
  int time_step_reductions = 0;
 try_again:;
  daisy_assert (ddt > 0.0);
  daisy_assert (ddt <= dt);

  // Find number of steps
  unsigned int steps = 1;
  if (ddt < dt)
    {
      steps = int (dt / ddt) + 1U;
      ddt = dt / (steps + 0.0);
    }

  // Initialize flux.
  fill (J.begin (), J.end (), 0.0);

  // Step through it.
  for (unsigned int step = 0; step < steps; step++)
    {
      // Upper boundary.
      dJ[0] = J_in;

      // Middle cells.
      for (unsigned int i = 1; i <= size; i++)
	{
	  const double q = soil_water.q (i);
	  if (q < 0)		// Downward flow, take from water above.
	    dJ[i] = q * C[i-1];
	  else if (i < size)
	    dJ[i] = q * C[i];	// Upward flow, take from water below.
	  else if (C_below >= 0)
	    dJ[i] = q * C[i];	// Use specified groundwater content.
	  else
	    dJ[i] = q * C[i-1];	// Assume the same concentration below.
	}

      // Update content.
      for (unsigned int i = 0; i < size; i++)
	{
	  J[i] += dJ[i] * ddt;
	  M[i] += (-dJ[i] + dJ[i+1]) * ddt / geo.dz (i) + S[i] * ddt;
	  C[i] = adsorption.M_to_C (soil, soil_water.Theta (i), i, M[i]);
	}
      J[size] += dJ[size] * ddt;
    }
  daisy_assert (approximate (J_in, J[0]));

  for (unsigned int i = 0; i < size; i++)
    if (M[i] < 0.0)
      {
	ddt *= 0.5;
	C = C_prev;
	M = M_prev;
	time_step_reductions++;
	if (time_step_reductions > max_time_step_reductions)
	  throw ("convection gave negative solution");
	goto try_again;
      }
  
  // Check mass conservation.
  const double new_total = geo.total_surface (M);
  if (!approximate (old_total - J[0] * dt + J[size] * dt, new_total)
      && !approximate (- J[0] * dt + J[size] * dt, 
                       new_total - old_total,
                       0.05))
    {
      Treelog::Open nest (msg, name);
      const double total_S = geo.total_surface (S) * dt;
      std::ostringstream tmp;
      tmp << "In (" << - J[0] << ") - out (" << -J[size] 
             << " != new (" << new_total 
             << ") - old (" << (old_total - total_S) 
             << ") - source (" << total_S << ")";
      msg.error (tmp.str ());
    };
}

const AttributeList& 
Transport::reserve_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      TransportConvection::load_syntax (dummy, alist);
      alist.add ("type", "convection");
    }
  return alist;
}

void 
TransportConvection::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("max_time_step_reductions",
              Syntax::Integer, Syntax::Const, "\
Number of times we may reduce the time step before giving up");
  alist.add ("max_time_step_reductions", 10);
}

static struct TransportConvectionSyntax
{
  static Model& make (Block& al)
  {
    return *new TransportConvection (al);
  }

  TransportConvectionSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Transport using convection alone.");
    TransportConvection::load_syntax (syntax, alist);
    Librarian::add_type (Transport::component, "convection", alist, syntax, &make);
  }
} TransportConvection_syntax;

// transport_convection.C ends here.
