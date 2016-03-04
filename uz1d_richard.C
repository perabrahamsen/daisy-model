// uz1d_richard.C --- Using Richard's Equation to calculate 1D water flow.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2006 Per Abrahamsen and KVL.
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
#include "uz1d.h"
#include "block_model.h"
#include "frame.h"
#include "average.h"
#include "librarian.h"
#include "treelog.h"
#include "assertion.h"
#include "mathlib.h"
#include <sstream>

struct UZ1DRichard : public UZ1D
{
  // Parameters.
  const int max_time_step_reductions;
  const int time_step_reduction;
  const int max_iterations;
  const double max_absolute_difference;
  const double max_relative_difference;
  std::unique_ptr<Average> K_average;

  // Interface.
  void tick (SMM1D&, double gravity, double dt, Treelog&);

  // Utilities.
  static void internode (const SMM1D&, const Average&, 
                         const std::vector<double>& K,
                         std::vector<double>& Kplus);
  static bool converges (const std::vector<double>& previous,
                         const std::vector<double>& current,
                         double max_absolute_difference,
                         double max_relative_difference);

  // Create and Destroy.
  UZ1DRichard (const BlockModel& al);
  ~UZ1DRichard ();
};

void
UZ1DRichard::tick (SMM1D& smm, const double gravity, const double dt,
                   Treelog& msg)
{
  // Check input.
  const size_t cell_size = smm.cell_size ();
  const size_t edge_size = cell_size + 1;
  daisy_assert (gravity >= -1);
  daisy_assert (gravity <= 1);
  daisy_assert (K_average.get ());

  // Matrix.
  std::vector<double> a (cell_size);
  std::vector<double> b (cell_size);
  std::vector<double> c (cell_size);
  std::vector<double> d (cell_size);

  // Intermeditate results.
  std::vector<double> h (cell_size);
  std::vector<double> h_previous (cell_size);
  std::vector<double> h_conv (cell_size);
  std::vector<double> Theta_previous (cell_size);
  std::vector<double> Theta (cell_size);
  std::vector<double> Ksum (cell_size);
  std::vector<double> Kold (cell_size);
  std::vector<double> K (cell_size);
  std::vector<double> Kedge (edge_size);

  // First guess is the old value.
  smm.reset (h, Theta);

  double time_left = dt;	// How much of the large time step left.
  double ddt = dt;		// We start with small == large time step.
  int number_of_time_step_reductions = 0;
  int iterations_with_this_time_step = 0;

  while (time_left > 0.0)
    {
      // Initialization for each small time step.
      int iterations_used = 0;
      if (ddt > time_left)
	ddt = time_left;

      for (size_t i = 0; i < cell_size; i++)
	{
	  Ksum[i] = 0.0;
	  Kold[i] = smm.K (i, h[i]);
	}
      h_previous = h;
      Theta_previous = Theta;

      do
	{
	  h_conv = h;
	  iterations_used++;

	  // Calculate parameters.
	  for (size_t i = 0; i < cell_size; i++)
	    {
	      Ksum[i] += smm.K (i, h[i]);
	      K[i] = (Ksum[i] / (iterations_used + 0.0) + Kold[i]) / 2.0;
	    }

          // Find K between cells.
	  internode (smm, *K_average, K, Kedge);
          const double *const Kminus = &Kedge[0];
          const double *const Kplus = &Kedge[1];
          daisy_assert (iszero (Kminus[0]));
          daisy_assert (iszero (Kplus[cell_size-1]));
	  // Calcualte cells.
	  for (size_t i = 0; i < cell_size; i++)
	    {
              // Elements.
	      const double Cw1 = smm.Cw1 (i, h[i]);
	      const double Cw2 = smm.Cw2 (i, h[i]);
	      const double dx = smm.cell_length (i);
              const size_t e_minus = smm.edge_previous (i);
              const size_t e_plus = smm.edge_next (i);
              const double dx_plus = smm.edge_distance (e_plus);
              const double dx_minus = smm.edge_distance (e_minus);
              const double K_plus = Kplus[i];
              const double K_minus = Kminus[i];
              // const double S = smm.S (i);
              const double S = 0.0; // Already incorporated in vertical.

              // Common subexpressions.
              const double ddt_dx = ddt / dx;
              const double K_dx_minus = K_minus / dx_minus;
              const double K_dx_plus = K_plus / dx_plus;
              
              // The equations.
              a[i] = - ddt_dx * K_dx_minus;
              b[i] = Cw2 + ddt_dx * (K_dx_minus + K_dx_plus);
              c[i] = - ddt_dx * K_dx_plus;
              d[i] = Theta[i] - Cw1 - ddt * S 
                + gravity * ddt_dx * (K_plus - K_minus);
	    }
	  tridia (0, cell_size, a, b, c, d, h.begin ());

          daisy_assert (h.size () > 0);
	  if (h[0] < -1e9  || h[cell_size-1] < -1e9)
	    {
	      std::ostringstream tmp;
	      tmp << "ABSURD: h[0] = " << h[0] << " h[1] = " << h[1] 
                  << " h[" << (cell_size-1) << "] = " << h[cell_size-1]
		     << " stepping down";
	      msg.error (tmp.str ());
	      iterations_used = max_iterations + 42;
	      break;
	    }

	}
      while (!converges (h_conv, h, 
                         max_absolute_difference, max_absolute_difference)
	     && iterations_used <= max_iterations);

      if (iterations_used > max_iterations)
	{
	  number_of_time_step_reductions++;

	  if (number_of_time_step_reductions > max_time_step_reductions)
	    {
              std::ostringstream tmp;
              tmp << "Exceeded " << max_time_step_reductions 
                  << " time step reductions, giving up";
              throw tmp.str ();
	    }
	  else
	    ddt /= time_step_reduction;
	  h = h_previous;
	}
      else
	{
	  // Calculate new water content.
	  for (size_t i = 0; i < cell_size; i++)
	    Theta[i] = smm.Theta (i, h[i]);

          // Less time left.
          time_left -= ddt;
          iterations_with_this_time_step++;

          // Try a larger timestep again?
          if (iterations_with_this_time_step > time_step_reduction)
            {
              number_of_time_step_reductions--;
              iterations_with_this_time_step = 0;
              ddt *= time_step_reduction;
	    }
	}
    }


  // Check upper boundary.
  std::vector<double> q (edge_size);
  q[0] = 0.0;
  for (size_t i = 0; i < cell_size; i++)
    {
      const double S = 0.0;
      const double Theta_new = Theta[i];
      const double Theta_old = smm.Theta_old (i);
      const double dx = smm.cell_length (i);
      q[i + 1] = q[i] - (((Theta_new - Theta_old) / dt) + S) * dx;
    }

  // Make it official.
  smm.update (h, Theta, q);
}

static double min (const double a, const double b, const double c)
{ return std::min (std::min (a, b), c); }

void 
UZ1DRichard::internode (const SMM1D& smm, 
                        const Average& average,
                        const std::vector<double>& K, 
                        std::vector<double>& Kedge)
{
  const size_t cell_size = smm.cell_size ();
  const size_t edge_size = cell_size + 1;
  daisy_assert (Kedge.size () == edge_size);

  Kedge[0] = 0.0;
  double Ksat_prev = smm.K (0, 0.0);
  double Kprev = K[0];
  for (size_t i = 1; i < cell_size; i++)
    {
      const double Ksat_next = smm.K (i, 0.0);
      const double Knext = K[i];
      Kedge[i] = min (Ksat_prev, average (Kprev, Knext), Ksat_next);
      Ksat_prev = Ksat_next;
      Kprev = Knext;
    }
  Kedge[cell_size] = 0.0;
}

bool
UZ1DRichard::converges (const std::vector<double>& previous,
                        const std::vector<double>& current,
                        const double max_absolute_difference,
                        const double max_relative_difference)
{
  const size_t size = previous.size ();
  daisy_assert (current.size () == size);

  for (size_t i = 0; i < size; i++)
    {
      const double cur = current[i];
      const double prev = previous[i];
      const double diff = cur - prev;
      if (fabs (diff) < max_absolute_difference)
        continue;
      if (iszero (prev))
        continue;
      if (fabs (diff / prev) > max_relative_difference)
        return false;
    }
  return true;
}

UZ1DRichard::UZ1DRichard (const BlockModel& al)
  : UZ1D (al),
    max_time_step_reductions (al.integer ("max_time_step_reductions")),
    time_step_reduction (al.integer ("time_step_reduction")),
    max_iterations (al.integer ("max_iterations")),
    max_absolute_difference (al.number ("max_absolute_difference")),
    max_relative_difference (al.number ("max_relative_difference")),
    K_average (Librarian::build_item<Average> (al, "K_average"))
{ }

UZ1DRichard::~UZ1DRichard ()
{ }

static struct UZ1DRichardSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UZ1DRichard (al); }
  UZ1DRichardSyntax ()
    : DeclareModel (UZ1D::component, "richards", "\
A numerical solution to Richard's Equation.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_integer ("max_time_step_reductions", Attribute::Const, "\
Number of times we may reduce the time step before giving up");
    frame.set ("max_time_step_reductions", 4);
    frame.declare_integer ("time_step_reduction", Attribute::Const, 
                "Divide the time step with this at each reduction.");
    frame.set ("time_step_reduction", 4);
    frame.declare_integer ("max_iterations", Attribute::Const, "\
Maximum number of iterations when seeking convergence before reducing\n\
the time step.");
    frame.set ("max_iterations", 25);
    frame.declare ("max_absolute_difference", "cm", Attribute::Const, "\
Maximum absolute difference in 'h' values for convergence.");
    frame.set ("max_absolute_difference", 0.02);
    frame.declare ("max_relative_difference", Attribute::None (), Attribute::Const, "\
Maximum relative difference in 'h' values for convergence.");
    frame.set ("max_relative_difference", 0.001);
    frame.declare_object ("K_average", Average::component,
                       Attribute::OptionalConst, Attribute::Singleton,
                       "Model for calculating average K between cells.");
    frame.set ("K_average", "arithmetic");
  }
} UZ1DRichard_syntax;

// uz1d_richard.C ends here.
