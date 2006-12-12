// uzrect_2x1.C --- Vertical and horiziontal matrix water flow.
// 
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

#include "uzrect.h"
#include "uzmodel.h"
#include "uz1d.h"
#include "soil_water.h"
#include "groundwater.h"
#include "surface.h"
#include "syntax.h"
#include "alist.h"
#include "mathlib.h"
#include "assertion.h"
#include "memutils.h"
#include <sstream>

struct UZRect2x1 : public UZRect
{
  // Parameters.
  const std::vector<UZmodel*> vertical;
  const std::vector<UZ1D*> horizontal;

  // Interface.
  void tick (const GeometryRect&, const Soil&, SoilWater&, 
             const SoilHeat&, const Surface&, const Groundwater&, 
             double dt, Treelog&);

  // Internal function.
  void water_column (const GeometryRect&, const Soil& soil,
                     const SoilHeat& soil_heat, 
                     const Surface& surface, const Groundwater& groundwater,
                     const size_t top_cell, const size_t bottom_cell,
                     const std::vector<double>& S,
                     std::vector<double>& h_old,
                     const std::vector<double>& Theta_old,
                     const std::vector<double>& h_ice,
                     std::vector<double>& h,
                     std::vector<double>& Theta,
                     const size_t q_offset,
                     std::vector<double>& q,
                     std::vector<double>& q_p,
                     double dt, Treelog& msg);

  // Create and Destroy.
  void has_macropores (bool);
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  UZRect2x1 (Block& al);
  ~UZRect2x1 ();
};

void 
UZRect2x1::tick (const GeometryRect& geo, const Soil& soil, 
                 SoilWater& soil_water, const SoilHeat& soil_heat,
                 const Surface& surface, const Groundwater& groundwater, 
                 const double dt, Treelog& msg)
{
  const size_t cell_rows = geo.cell_rows ();
  const size_t cell_columns = geo.cell_columns ();
  const size_t edge_rows = geo.edge_rows ();

  // Vertical movement.
  for (size_t col = 0; col < cell_columns; col++)
    {
      std::ostringstream tmp;
      tmp << "Column " << col;
      Treelog::Open nest (msg, tmp.str ());

      // Find relevant cells.
      const size_t c_first = col * cell_rows;
      const size_t c_last = (col + 1U) * cell_rows - 1U;

      // Find relevant edges.
      const size_t e_first = col * edge_rows;
      const size_t e_last = (col + 1U) * edge_rows - 1U;

      // Check that they match.
      daisy_assert (geo.edge_to (e_first) == Geometry::cell_above);
      daisy_assert (geo.edge_from (e_first) == c_first);
      daisy_assert (geo.edge_to (e_last) == c_last);
      daisy_assert (geo.edge_from (e_last) == Geometry::cell_below);

      water_column (geo, soil, soil_heat, surface, groundwater, 
                    c_first, c_last,
                    soil_water.S_sum_, soil_water.h_old_, 
                    soil_water.Theta_old_,
                    soil_water.h_ice_, soil_water.h_, soil_water.Theta_,
                    col, soil_water.q_, soil_water.q_p_,
                    dt, msg);
   }

  // Horizontal movement.
  for (size_t row = 0; row < cell_rows; row++)
    {
      std::ostringstream tmp;
      tmp << "Row " << row;
      Treelog::Open nest (msg, tmp.str ());

      std::vector<size_t> cells;
      std::vector<int> edges;
      
      for (size_t col = 0; col < cell_columns; col++)
        cells.push_back (geo.cell_index (row, col));

      int from = Geometry::cell_left;
      for (size_t col = 0; col <= cell_columns; col++)
        {
          const int to = (col == cell_columns 
                          ? Geometry::cell_right
                          : static_cast<int> (cells[col]));
          const int edge = geo.edge_index (from, to);
          daisy_assert (edge >= 0);
          daisy_assert (edge < geo.edge_size ());
          daisy_assert (geo.edge_from (edge) == from);
          daisy_assert (geo.edge_to (edge) == to);
          daisy_assert (col == 0 
                        || col == cell_columns
                        || approximate (geo.z (cells[col-1]),
                                        geo.z (cells[col])));
          edges.push_back (edge);
          from = to;
        }

      SMM1D smm (geo, soil, soil_water, soil_heat, cells, edges);

      for (size_t i = 0; i < horizontal.size (); i++)
        {
          Treelog::Open nest (msg, horizontal[i]->name);
          try 
            {
              horizontal[i]->tick (smm, 0.0, dt, msg);
              goto success;
            }
          catch (const char* error)
            {
              msg.warning (std::string ("UZhor problem: ") + error);
            }
          catch (const std::string& error)
            {
              msg.warning (std::string ("UZhor trouble: ") + error);
            }
        }
      msg.error ("No useful horizontal transport found");
    success:
      /* next row */;
    }
}

void
UZRect2x1::water_column (const GeometryRect& geo, const Soil& soil,
                         const SoilHeat& soil_heat, 
                         const Surface& surface, 
                         const Groundwater& groundwater,
                         const size_t top_cell, const size_t bottom_cell,
                         const std::vector<double>& S,
                         std::vector<double>& h_old,
                         const std::vector<double>& Theta_old,
                         const std::vector<double>& h_ice,
                         std::vector<double>& h,
                         std::vector<double>& Theta,
                         const size_t q_offset,
                         std::vector<double>& q,
                         std::vector<double>& q_p,
                         const double dt,
                         Treelog& msg)
{
  // Find top edge.
  const size_t top_edge = top_cell + q_offset;
  daisy_assert (geo.edge_to (top_edge) == Geometry::cell_above);
  daisy_assert (q.size () > bottom_cell + 1);

  // Limit for ridging.
  const size_t first = top_cell +
    (surface.top_type (geo, top_edge) == Surface::soil
     ?  surface.last_cell (geo, top_edge) : 0);

  // Limit for groundwater table.
  size_t last = bottom_cell;
  if (groundwater.bottom_type () == Groundwater::pressure)
    {
      if (groundwater.table () <= geo.zminus (bottom_cell))
        throw ("Groundwater table in or below lowest cell.");

      while (groundwater.table () > geo.zminus (last) && last > first)
        last--;

      // Pressure at the last cell is equal to the water above it.
      for (size_t i = last + 1; i <= bottom_cell; i++)
        h_old[i] = h[i] = groundwater.table () - geo.z (i);
    }

  // Calculate matrix flow next.
  for (size_t i = 0; i < vertical.size (); i++)
    {
      Treelog::Open nest (msg, vertical[i]->name);
      try
        {
          vertical[i]->tick (msg, geo, soil, soil_heat,
                             first, surface, top_edge, last, groundwater,
                             S, h_old, Theta_old, h_ice, h, Theta, 
                             q_offset, q, dt);
          for (size_t i = last + 2; i <= bottom_cell + 1; i++)
            {
              daisy_assert (q.size () > i + q_offset);
              q[i + q_offset] = q[i-1 + q_offset];
              q_p[i + q_offset] = q_p[i-1 + q_offset];
            }
          // Update Theta below groundwater table.
          if (groundwater.bottom_type () == Groundwater::pressure)
            {
              for(size_t i = last + 1; i < soil.size (); i++)
                Theta[i] = soil.Theta (i, h[i], h_ice[i]);
            }
          return;
        }
      catch (const char* error)
        {
          msg.warning (std::string ("UZ problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.warning (std::string ("UZ trouble: ") + error);
        }
    }
  throw "Vertical transport failed";
}

void 
UZRect2x1::has_macropores (const bool has_them)
{
  for (size_t i = 0; i < vertical.size (); i++)
    vertical[i]->has_macropores (has_them);
}

void 
UZRect2x1::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add ("vertical", Librarian<UZmodel>::library (), 
              Syntax::Const, Syntax::Sequence,
              "Vertical matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
  std::vector<AttributeList*> vertical_models;
  AttributeList vertical_default (UZmodel::default_model ());
  vertical_models.push_back (&vertical_default);
  AttributeList vertical_reserve (UZmodel::reserve_model ());
  vertical_models.push_back (&vertical_reserve);
  alist.add ("vertical", vertical_models);
  syntax.add ("horizontal", Librarian<UZmodel>::library (),
              Syntax::Const, Syntax::Sequence,
              "Horizontal matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends."); 
  std::vector<AttributeList*> horizontal_models;
  AttributeList horizontal_default (UZ1D::default_model ());
  horizontal_models.push_back (&horizontal_default);
  AttributeList horizontal_none (UZ1D::none_model ());
  horizontal_models.push_back (&horizontal_none);
  alist.add ("horizontal", horizontal_models);
}

UZRect2x1::UZRect2x1 (Block& al)
  : UZRect (al),
    vertical (Librarian<UZmodel>::build_vector (al, "vertical")),
    horizontal (Librarian<UZ1D>::build_vector (al, "horizontal"))
{ }

UZRect2x1::~UZRect2x1 ()
{ 
  sequence_delete (vertical.begin (), vertical.end ());
  sequence_delete (horizontal.begin (), horizontal.end ());
}

const AttributeList& 
UZRect::reserve_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax dummy;
      UZRect2x1::load_syntax (dummy, alist);
      alist.add ("type", "v+h");

    }
  return alist;
}

static struct UZRect2x1Syntax
{
  static UZRect& make (Block& al)
  { return *new UZRect2x1 (al); }
  UZRect2x1Syntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "\
Transport water in the matrix in two phases, first vertical, then\n\
horizontal.");
    UZRect2x1::load_syntax (syntax, alist);
    Librarian<UZRect>::add_type ("v+h", alist, syntax, &make);
  }
} UZRect2x1_syntax;

// uzrect_2x1.C ends here.
