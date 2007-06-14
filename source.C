// source.C -- Time series for gnuplot interface 
// 
// Copyright 2005 Per Abrahamsen and KVL.
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

#include "source.h"
#include "block.h"
#include "librarian.h"

const char *const Source::component = "source";

void 
Source::limit (Time& begin, Time& end, double& ymin, double& ymax) const
{
  const std::vector<Time>& times = time ();
  const std::vector<double>& values = value ();

  for (size_t i = 0; i < times.size (); i++)
    {
      if (times[i] < begin)
	begin = times[i];
      if (times[i] > end)
	end = times[i];
      if (values[i] < ymin)
	ymin = values[i];
      if (values[i] > ymax)
	ymax = values[i];
    }
}

void 
Source::distance (const Time begin, const Time end, 
		  const double ymin, const double ymax,
		  double& nw, double& ne, double& sw, double& se) const
  // Find relative distances to each corner.
{
  if (begin >= end || ymin >= ymax)
    // Null plot.
    return;
    
  const std::vector<Time>& times = time ();
  const std::vector<double>& values = value ();

  for (size_t i = 0; i < times.size (); i++)
    {
      const double xr = (Time::hours_between (begin, times[i]) + 0.0)
	/ (Time::hours_between (begin, end) + 0.0);
      if (xr < 0.0 || xr > 1.0)
	// Outside graph.
	continue;
      const double yr = (values[i] - ymin) / (ymax - ymin);
      if (yr < 0.0 || yr > 1.0)
	// Outside graph.
	continue;
      
      // Distance from borders.
      const double  west = xr;
      const double  east = 1.0 - xr;
      const double north = 1.0 - yr;
      const double south = yr;
      
      // Distance from corners.
      nw = std::min (nw, std::max (north, west));
      ne = std::min (ne, std::max (north, east));
      sw = std::min (sw, std::max (south, west));
      se = std::min (se, std::max (south, east));
    }
}

void
Source::load_syntax (Syntax&, AttributeList&)
{ }

Source::Source (Block& al)
  : name (al.identifier ("type"))
{ }

Source::~Source ()
{ }

static Librarian Source_init (Source::component, "\
Time series, with possible error bars and formatting information.");
