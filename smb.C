// smb.C --- A single soil microbiological pool.
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

#define BUILD_DLL

#include "smb.h"
#include "dom.h"
#include "librarian.h"
#include "frame.h"
#include "assertion.h"
#include "check.h"
#include "mathlib.h"
#include "block_model.h"

const char *const SMB::component = "SMB";

symbol
SMB::library_id () const
{
  static const symbol id (component);
  return id;
}

void
SMB::maintain (const std::vector<bool>& active, const double* abiotic_factor, 
	       double* N_used, double* CO2, const double dt)
{
  const size_t cell_size = active.size ();
  daisy_assert (C.size () == cell_size);
  daisy_assert (N.size () == cell_size);

  for (size_t i = 0; i < cell_size; i++)
    {
      if (!active[i])
        continue;

      // Maintenance.
      const double C_use = C[i] * clay_maintenance[i] * abiotic_factor[i];
      const double N_use = N[i] * clay_maintenance[i] * abiotic_factor[i];
      CO2[i] += C_use;
      C[i] -= C_use * dt;
      N[i] -= N_use * dt;
      N_used[i] -= N_use;
      daisy_assert (C[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
    }
}

void
SMB::turnover_pool (const std::vector<bool>& active, const double* factor,
		    double fraction, double efficiency,
		    const double* N_soil, double* N_used, double* CO2, OM& om,
                    const double dt)
{
  const size_t cell_size = active.size ();
  daisy_assert (C.size () == cell_size);
  daisy_assert (N.size () == cell_size);

  // Maintenance.
  for (size_t i = 0; i < cell_size; i++)
    {
      if (!active[i])
        continue;
      const double rate 
        = std::min (factor[i] * clay_turnover[i] * fraction, 0.1);
      daisy_assert (C[i] >= 0.0);
      daisy_assert (std::isfinite (rate));
      daisy_assert (rate >=0);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (N[i] >= 0.0);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (om.C[i] >= 0.0);
      double C_use;
      double N_produce;
      double N_consume;
      
      turnover (C[i], N[i], om.goal_C_per_N (i), N_soil[i] - N_used[i],
		rate, efficiency, C_use, N_produce, N_consume);

      // Update C.
      daisy_assert (om.C[i] >= 0.0);
      CO2[i] += C_use * (1.0 - efficiency);
      om.C[i] += C_use * efficiency * dt;
      C[i] -= C_use * dt;
      daisy_assert (om.C[i] >= 0.0);
      daisy_assert (C[i] >= 0.0);

      // Update N.
      N_used[i] += (N_consume - N_produce);
      daisy_assert (N_soil[i] * 1.001 >= N_used[i]);
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
      om.N[i] += N_consume * dt;
      N[i] -= N_produce * dt;
      daisy_assert (om.N[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
    }
}

void
SMB::turnover_dom (const std::vector<bool>& active, const double* factor,
                   double fraction, DOM& dom, const double dt)
{
  const size_t cell_size = active.size ();

  for (unsigned int i = 0; i < cell_size; i++)
    {
      const double rate = std::min (clay_turnover[i] * fraction * factor[i], 0.1);
      const double C_use = C[i] * rate;
      const double N_use = N[i] * rate;
      dom.add_to_source (i, C_use, N_use);
      C[i] -= C_use * dt;
      N[i] -= N_use * dt;
      daisy_assert (C[i] >= 0.0);
      daisy_assert (N[i] >= 0.0);
    }
}

SMB::SMB (const BlockModel& al)
  : OM (al),
    maintenance (al.number ("maintenance"))
{ }

static struct SMBInit : public DeclareSolo
{
  Model* make (const BlockModel& al) const
  { return new SMB (al); }
  void load_frame (Frame& frame) const
  {
    OM::load_syntax (frame, "\
The first numbers corresponds to each of the SMB pools, the next\n\
numbers corresponds to the SOM pools, and the last numbers to each of\n\
the DOM pools.  The length of the sequence should thus be the number\n\
of SMB pools plus the number of SOM pools plus the number of DOM pools."); 
    frame.declare ("maintenance", "h^-1", Check::fraction (), Attribute::Const, "\
The fraction used for staying alive each hour.");
  }
  SMBInit ()
    : DeclareSolo (SMB::component, "\
A single Soil MicroBiological pool.")
  { }
} SMB_init;

static struct SMBSlowSyntax : public DeclareParam
{
  SMBSlowSyntax ()
    : DeclareParam (SMB::component, "SMB-SLOW", root_name (), "\
Slow SMB pool parameterization.")
  { }
  void load_frame (Frame& frame) const
  {
    std::vector<double> SMB1_C_per_N;
    SMB1_C_per_N.push_back (6.7);
    frame.set ("C_per_N", SMB1_C_per_N);
    frame.set ("turnover_rate", 7.708e-6);
    std::vector<double> SMB1_efficiency;
    SMB1_efficiency.push_back (0.60);
    SMB1_efficiency.push_back (0.60);
    frame.set ("efficiency", SMB1_efficiency);
    frame.set ("maintenance", 7.500e-5);
    std::vector<double> SMB1_fractions;
    SMB1_fractions.push_back (0.0);
    SMB1_fractions.push_back (0.6);
    SMB1_fractions.push_back (0.0);
    SMB1_fractions.push_back (0.4);
    SMB1_fractions.push_back (0.0);
    frame.set ("fractions", SMB1_fractions);
  }
} SMBSlow_syntax;

static struct SMBFastSyntax : public DeclareParam
{
  SMBFastSyntax ()
    : DeclareParam (SMB::component, "SMB-FAST", root_name (), "\
Fast SMB pool parameterization.")
  { }
  void load_frame (Frame& frame) const
  {
    std::vector<double> SMB2_C_per_N;
    SMB2_C_per_N.push_back (6.7);
    frame.set ("C_per_N", SMB2_C_per_N);
    frame.set ("turnover_rate", 4.16666666667e-4);
    std::vector<double> SMB2_efficiency;
    SMB2_efficiency.push_back (0.60);
    SMB2_efficiency.push_back (0.60);
    frame.set ("efficiency", SMB2_efficiency);
    frame.set ("maintenance", 4.16666666667e-4);
    std::vector<double> SMB2_fractions;
    SMB2_fractions.push_back (0.0);
    SMB2_fractions.push_back (0.4);
    SMB2_fractions.push_back (0.0);
    SMB2_fractions.push_back (0.6);
    SMB2_fractions.push_back (0.0);
    frame.set ("fractions", SMB2_fractions);
  }
} SMBFast_syntax;

// smb.C ends here.
