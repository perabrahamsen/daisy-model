// smb.h --- A single soil microbiological pool.
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


#ifndef SMB_H
#define SMB_H

#include "om.h"
#include <vector>

class Frame;

class SMB : public OM
{ 
  // Model.
public:
  static const char *const component;
  symbol library_id () const;

  void output(Log& log) const
  { OM::output (log); }

  // Content.
public:
  const double maintenance;	// How fast does it eat itself?
  std::vector<double> clay_turnover;	// Clay dependent turnover rate.
  std::vector<double> clay_maintenance; // Clay dependent maintenance rate.

  // Simulation.
public:
  void maintain (const std::vector<bool>& active,
                 const double* abiotic_factor, 
		 double* N_used, double* CO2, double dt);
private:
  void turnover_pool (const std::vector<bool>& active, const double* factor,
		      double fraction, double efficiency,
		      const double* N_soil, double* N_used, 
		      double* CO2, OM& om, double dt);
  void turnover_dom (const std::vector<bool>& active, const double* factor,
		     double fraction, DOM& dom, double dt);

  // Create & Destroy.
public:
  SMB (const BlockModel& al);
};

#endif // SMB_H
