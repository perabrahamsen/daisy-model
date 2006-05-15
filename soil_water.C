// soil_water.C
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


#include "soil_water.h"
#include "geometry.h"
#include "soil.h"
#include "log.h"
#include <sstream>

void
SoilWater::root_uptake (const std::vector<double>& v)
{
  daisy_assert (S_sum_.size () == v.size ());
  daisy_assert (S_root_.size () == v.size ());
  for (unsigned i = 0; i < v.size (); i++)
    {
      S_sum_[i] += v[i];
      S_root_[i] += v[i];
    }
}

double 
SoilWater::content (const Geometry& geo, double from, double to) const
{ return geo.total (Theta_, from, to); }

void 
SoilWater::incorporate (const Geometry& geo, const double amount,
                        const double from, const double to)

{ geo.add (S_incorp_, from, to, -amount); }

void
SoilWater::mix (const Geometry& geo,
                const Soil& soil, double from, double to)
{
  geo.mix (Theta_, from, to, tillage_);
  for (size_t i = 0; i < soil.size(); i++)
    h_[i] = soil.h (i, Theta_[i]);
}

void
SoilWater::swap (Treelog& msg, const Geometry& geo,
                 const Soil& soil,
                 double from, double middle, double to)
{
  geo.swap (Theta_, from, middle, to, tillage_);

  for (size_t i = 0; i < soil.size(); i++)
    {
      const double Theta_sat = soil.Theta (i, 0.0, 0.0);
      if (Theta_[i] > Theta_sat)
	{
	  std::ostringstream tmp;
	  tmp << "BUG: Theta[ " << i << "] (" << Theta_[i]
              << ") > Theta_sat (" << Theta_sat << ")";
	  msg.error (tmp.str ());
	  Theta_[i] = Theta_sat;
	}
      h_[i] = soil.h (i, Theta_[i]);
    }
}

void 
SoilWater::output_base (Log& log) const
{
  output_value (h_, "h", log);
  output_value (Theta_, "Theta", log);
  output_value (S_sum_, "S_sum", log);
  output_value (S_root_, "S_root", log);
  output_value (S_drain_, "S_drain", log);
  output_value (S_incorp_, "S_incorp", log);
  output_value (tillage_, "tillage", log);
  output_value (X_ice_, "X_ice", log);
}

bool 
SoilWater::check_base (size_t n, Treelog& msg) const
{
  bool ok = true;

  if (Theta_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << Theta_.size () << " Theta values";
      msg.error (tmp.str ());
      ok = false;
    }
  if (h_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << h_.size () << " h values";
      msg.error (tmp.str ());
      ok = false;
    }
  if (X_ice_.size () != n)
    {
      std::ostringstream tmp;
      tmp << "You have " << n 
          << " intervals but " << X_ice_.size () << " X_ice values";
      msg.error (tmp.str ());
      ok = false;
    }
  return ok;
}

void
SoilWater::load_base (Syntax& syntax, AttributeList&)
{
  Geometry::add_layer (syntax, Syntax::OptionalState, 
                       "h", "cm", "Soil water pressure.");
  Geometry::add_layer (syntax, Syntax::OptionalState,
                       "Theta", Syntax::Fraction (),
                       "Soil water content.");
  syntax.add ("S_sum", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Total water sink (due to root uptake and macropores).");
  syntax.add ("S_root", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink due to root uptake.");
  syntax.add ("S_drain", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Water sink due to soil drainage.");
  syntax.add ("S_incorp", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Incorporated water sink, typically from subsoil irrigation.");
  syntax.add ("tillage", "h^-1", Syntax::LogOnly, Syntax::Sequence,
	      "Changes in water content due to tillage operations.");
  syntax.add_fraction ("X_ice", Syntax::OptionalState, Syntax::Sequence,
		       "Ice volume fraction in soil.");
}

SoilWater::SoilWater (Block&)
{ }

SoilWater::~SoilWater ()
{ }

