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


#include "transport.h"
#include "soil.h"
#include "soil_water.h"
#include "solute.h"
#include "log.h"
#include "mathlib.h"

class TransportConvection : public Transport
{
  // Parameters.
private:
  int max_time_step_reductions;

  // Log variable.
private:
  vector<double> J;		// Upward matter flux [g/cm²].
  double ddt;			// Small time step [h].
  
  // Simulation.
public:
  void tick (Treelog&, const Soil&, const SoilWater&, const Solute&,
	     vector<double>& M, 
	     vector<double>& C,
	     const vector<double>& S,
	     vector<double>& J);
  void output (Log&) const;

  // Create.
public:
  TransportConvection (const AttributeList& al)
    : Transport (al.name ("type")),
      max_time_step_reductions (al.integer ("max_time_step_reductions")),
      ddt (dt)
    { }
};

void
TransportConvection::output (Log& log) const
{
  log.output ("ddt", ddt);
}

void 
TransportConvection::tick (Treelog&, 
			   const Soil& soil, const SoilWater& soil_water,
			   const Solute& solute, 
			   vector<double>& M, 
			   vector<double>& C,
			   const vector<double>& S,
			   vector<double>& J)
{
  const double J_in = J[0];

  // Remember old values.
  const vector<double> C_prev = C;
  const vector<double> M_prev = M;

  // Number of soil layers.
  const unsigned int size = soil.size ();

  // Remember old content
  const double old_total = soil.total (M) + soil.total (S) * dt;

  // Flux in individual time step.
  vector<double> dJ (size + 1, 0.0); 

  // Find time step.
  ddt = dt;
  for (unsigned int i = 0; i < size; i++)
    {
      const double half_content = soil_water.Theta (i) * soil.dz (i) / 2.0;
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

      // Middle nodes.
      for (unsigned int i = 1; i < size; i++)
	{
	  const double q = soil_water.q (i+1);
	  if (q < 0)		// Downward flow, take from water above.
	    dJ[i] = q * C[i-1];
	  else			// Upward flow, take from water below.
	    dJ[i] = q * C[i];
	}
  
      // Lower boundary.
      // We assume the same concentration below the lowest node.
      dJ[size] = soil_water.q (size) * C[size-1];

      // Update content.
      for (unsigned int i = 0; i < size; i++)
	{
	  J[i] += dJ[i] * ddt;
	  M[i] += (-dJ[i] + dJ[i+1]) * ddt / soil.dz (i) + S[i] * ddt;
	  C[i] = solute.M_to_C (soil, soil_water.Theta (i), i, M[i]);
	}
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
  const double new_total = soil.total (M);
  daisy_assert (approximate (old_total - J[0] * dt + J[size] * dt, new_total));
  daisy_assert (approximate (- J[0] * dt + J[size] * dt, new_total - old_total,
			     0.05));
}

static struct TransportConvectionSyntax
{
  static Transport& make (const AttributeList& al)
  {
    return *new TransportConvection (al);
  }

  TransportConvectionSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "Transport using convection alone.");
    syntax.add ("ddt", "h", Syntax::LogOnly, Syntax::Singleton,
		"Time step used in the numeric solution.");
    syntax.add ("max_time_step_reductions",
		Syntax::Integer, Syntax::Const, "\
Number of times we may reduce the time step before giving up");
    alist.add ("max_time_step_reductions", 10);
    Librarian<Transport>::add_type ("convection", alist, syntax, &make);
  }
} TransportConvection_syntax;
