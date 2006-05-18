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
SoilWater::clear_base ()
{
  fill (S_sum_.begin (), S_sum_.end (), 0.0);
  fill (S_drain_.begin (), S_drain_.end (), 0.0);
  fill (S_root_.begin (), S_root_.end (), 0.0);
  fill (S_incorp_.begin (), S_incorp_.end (), 0.0);
  fill (tillage_.begin (), tillage_.end (), 0.0);
  // We don't clear S_p and S_drain, because they are needed in solute.
}

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

double 
SoilWater::h_ice (size_t) const
{ return 0.0; }

double 
SoilWater::X_ice (size_t) const
{ return 0.0; }

double
SoilWater::Theta_ice (const Soil& soil, size_t i, double h) const
{ return soil.Theta (i, h, h_ice (i)); }
 
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
}

void
SoilWater::initialize_base (const AttributeList& al, const Geometry& geo,
                            const Soil& soil, Treelog& msg)
{
  Treelog::Open nest (msg, "SoilWater base");

  const size_t size = geo.cell_size ();

  geo.initialize_layer (Theta_, al, "Theta", msg);
  geo.initialize_layer (h_, al, "h", msg);

  for (size_t i = 0; i < Theta_.size () && i < h_.size (); i++)
    {
      const double Theta_h = soil.Theta (i, h_[i], h_ice (i));
      if (!approximate (Theta_[i], Theta_h))
	{
	  std::ostringstream tmp;
	  tmp << "Theta[" << i << "] (" << Theta_[i] << ") != Theta (" 
              << h_[i] << ") (" << Theta_h << ")";
	  msg.error (tmp.str ());
	}
      Theta_[i] = Theta_h;
    }
  if (Theta_.size () > 0)
    {
      while (Theta_.size () < size)
	Theta_.push_back (Theta_[Theta_.size () - 1]);
      if (h_.size () == 0)
	for (size_t i = 0; i < size; i++)
	  h_.push_back (soil.h (i, Theta_[i]));
    }
  if (h_.size () > 0)
    {
      while (h_.size () < size)
	h_.push_back (h_[h_.size () - 1]);
      if (Theta_.size () == 0)
	for (size_t i = 0; i < size; i++)
	  Theta_.push_back (soil.Theta (i, h_[i], h_ice (i)));
    }
  daisy_assert (h_.size () == Theta_.size ());

  S_sum_.insert (S_sum_.begin (), size, 0.0);
  S_root_.insert (S_root_.begin (), size, 0.0);
  S_drain_.insert (S_incorp_.begin (), size, 0.0);
  S_incorp_.insert (S_incorp_.begin (), size, 0.0);
  tillage_.insert (tillage_.begin (), size, 0.0);
}

SoilWater::SoilWater (Block&)
{ }

SoilWater::~SoilWater ()
{ }

