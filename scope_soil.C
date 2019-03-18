// scope_soil.C --- Look up stuff in the soil.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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

#include "scope_soil.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "soil_heat.h"
#include "chemical.h"
#include "units.h"
#include "assertion.h"
#include "librarian.h"
#include <sstream>
#include <iterator>

const symbol 
ScopeSoil::rho_b ("rho_b");

const symbol 
ScopeSoil::rho_b_unit ("g/cm^3");

const symbol 
ScopeSoil::clay ("clay");

const symbol 
ScopeSoil::humus ("humus");

const symbol 
ScopeSoil::h ("h");

const symbol 
ScopeSoil::Theta ("Theta");

const symbol 
ScopeSoil::T ("T");

void 
ScopeSoil::set_cell (int c)
{ cell = c; }

void 
ScopeSoil::set_old_water (const bool old)
{ old_water = old; }

void 
ScopeSoil::set_domain (const domain_t d)
{ domain = d; }

void 
ScopeSoil::set_dry_bulk_density (const double rho_b)
{ dry_bulk_density = rho_b; }

void
ScopeSoil::set_extra_water (const double extra)
{ Theta_extra = extra; }

void 
ScopeSoil::entries (std::set<symbol>& all) const
{ 
  all.insert (rho_b);
  all.insert (clay);
  all.insert (humus);
  all.insert (h);
  all.insert (Theta);
  all.insert (T);
  if (geo.cell_is_internal (cell))
    soil.append_attributes (cell, all); 
  else 
    {
      daisy_assert (geo.cell_is_external (cell));

      std::set<symbol> shared;
      // We look at each neighbor cell.
      const std::vector<size_t>& edge_around = geo.cell_edges (cell);
      const size_t edge_around_size = edge_around.size ();
      daisy_assert (edge_around_size > 0U);
      for (size_t i = 0; i < edge_around_size; i++)
        {
          const size_t edge = edge_around[i];
          const int c = geo.edge_other (edge, cell);
          daisy_assert (geo.cell_is_internal (c));

          // Only use common attributes.
          if (i == 0)
            soil.append_attributes (c, shared);
          else
            {
              std::set<symbol> mine;
              soil.append_attributes (c, shared);
              std::set<symbol> result;
              std::set_intersection (mine.begin (), mine.end (), 
                                     shared.begin (), shared.end (),
                                     std::inserter (result, result.end ()));
              shared = result;
            }
        }

      // Combine with build-in and old tags.
      std::set<symbol> result;
      std::set_union (all.begin (), all.end (), 
                      shared.begin (), shared.end (),
                      std::inserter (result, result.end ()));
      all = result;
    }
}

Attribute::type 
ScopeSoil::lookup (const symbol tag) const
{ return check (tag) ? Attribute::Number : Attribute::Error; }

bool 
ScopeSoil::check (const symbol tag) const
{
  if (tag == rho_b || tag == clay || tag == humus 
      || tag == h || tag == Theta || tag == T)
    return true;

  if (geo.cell_is_internal (cell))
    return soil.has_attribute (cell, tag);
  
  daisy_assert (geo.cell_is_external (cell));
  
  // Must be on all neighbors.
  const std::vector<size_t>& edge_around = geo.cell_edges (cell);
  const size_t edge_around_size = edge_around.size ();
  daisy_assert (edge_around_size > 0U);
  for (size_t i = 0; i < edge_around_size; i++)
    {
      const size_t edge = edge_around[i];
      const int c = geo.edge_other (edge, cell);
      daisy_assert (geo.cell_is_internal (c));
      if (!soil.has_attribute (c, tag))
        return false;
    }
  return true;
}

double
ScopeSoil::number (const symbol tag) const
{
  daisy_assert (geo.cell_is_valid (cell));

  if (tag == rho_b)
    {
      if (dry_bulk_density > 0.0)
        return dry_bulk_density;
      else if (domain == secondary)
        return 0.0;             // No soil in secondary domain.
      else
        return geo.content_cell_or_hood (soil, &Soil::dry_bulk_density, cell);
    }
  if (tag == clay)
    return geo.content_cell_or_hood (soil, &Soil::clay, cell);
  if (tag == humus)
    return geo.content_cell_or_hood (soil, &Soil::humus, cell);
  if (tag == h)
    {
      if (old_water)
        return geo.content_cell_or_hood (soil_water, &SoilWater::h_old, cell);
      else 
        return geo.content_cell_or_hood (soil_water, &SoilWater::h, cell);
    }
  if (tag == Theta)
    {
      double water;
      switch (domain)
        {
        case primary:
          water = old_water 
            ? geo.content_cell_or_hood (soil_water, 
                                        &SoilWater::Theta_primary_old, cell)
            : geo.content_cell_or_hood (soil_water, 
                                        &SoilWater::Theta_primary, cell);
          break;
        case secondary:
          water = old_water 
            ? geo.content_cell_or_hood (soil_water,
                                        &SoilWater::Theta_secondary_old, cell)
            : geo.content_cell_or_hood (soil_water, 
                                        &SoilWater::Theta_secondary, cell);
          break;
        case matrix:
          water = old_water 
            ? geo.content_cell_or_hood (soil_water, 
                                        &SoilWater::Theta_old, cell)
            : geo.content_cell_or_hood (soil_water, &SoilWater::Theta, cell);
          break;
        default:
          daisy_notreached ();
        }
      return water + Theta_extra; 
    }
  if (tag == T)
    return geo.content_cell_or_hood (soil_heat, &SoilHeat::T, cell);

  struct AttAccess : public Geometry::Access
  {
    const Soil& soil;
    const symbol tag;

    double operator()(size_t c) const
    { return soil.get_attribute (c, tag); }
    
    AttAccess (const Soil& s, const symbol t)
      : soil (s), 
        tag (t)
    { }
  } att_access (soil, tag);
      
  return geo.access_content_cell_or_hood (att_access, cell);
}

symbol 
ScopeSoil::dimension (const symbol tag) const
{
  if (tag == rho_b)
    return rho_b_unit;
  if (tag == humus || tag == clay)
    return Attribute::Fraction ();
  if (tag == h)
    return Units::cm ();
  if (tag == Theta)
    return Attribute::None ();

  static const symbol T_dim ("dg C");
  if (tag == T)
    return T_dim;

  if (geo.cell_is_internal (cell))
    return soil.get_dimension (cell, tag);
  
  // Must be the same for all neighbors.
  symbol shared = Attribute::Unknown ();
  const std::vector<size_t>& edge_around = geo.cell_edges (cell);
  const size_t edge_around_size = edge_around.size ();
  daisy_assert (edge_around_size > 0U);
  for (size_t i = 0; i < edge_around_size; i++)
    {
      const size_t edge = edge_around[i];
      const int c = geo.edge_other (edge, cell);
      daisy_assert (geo.cell_is_internal (c));
      
      const symbol mine = soil.get_dimension (c, tag);
      if (shared == Attribute::Unknown ())
        shared = mine;
      else if (shared != mine)
        return Attribute::Unknown ();
    }
  return shared;
}

symbol
ScopeSoil::description (symbol tag) const
{
  static const symbol rho_b_description ("Dry bulk density.\n\
For some uses, this may be replaced with colloids.");
  if (tag == rho_b)
    return rho_b_description;

  static const symbol clay_description ("Clay fraction of dry matter.");
  if (tag == clay)
    return clay_description;

  static const symbol humus_description ("Humus fraction of dry matter.");
  if (tag == humus)
    return humus_description;

  static const symbol h_description ("Soil hydraulic pressure.");
  if (tag == h)
    return h_description;

  static const symbol Theta_description ("Soil water content.");
  if (tag == Theta)
    return Theta_description;

  static const symbol T_description ("Soil temperatur.");
  if (tag == T)
    return T_description;

  static const symbol description ("Soil attribute.");
  return description;
}

std::vector<symbol>
ScopeSoil::find_numbers (const Soil&)
{
  std::vector<symbol> result;
  result.push_back (rho_b);
  result.push_back (clay);
  result.push_back (humus);
  result.push_back (h);
  result.push_back (Theta);
  result.push_back (T);
  return result;
}

ScopeSoil::ScopeSoil (const Geometry& g, 
                      const Soil& s, const SoilWater& sw, const SoilHeat& sh)
  : geo (g),
    soil (s),
    soil_water (sw),
    soil_heat (sh),
    all_numbers_ (find_numbers (soil)),
    old_water (false),
    domain (matrix),
    dry_bulk_density (-42.42e42),
    Theta_extra (0.0),
    cell (Geometry::cell_error)
{ }

ScopeSoil::~ScopeSoil ()
{ }

// scope_soil.C ends here.
