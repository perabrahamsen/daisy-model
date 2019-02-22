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

#define BUILD_DLL

#include "uzrect.h"
#include "uzmodel.h"
#include "uz1d.h"
#include "soil_water.h"
#include "groundwater.h"
#include "surface.h"
#include "frame.h"
#include "mathlib.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "treelog.h"
#include "block_model.h"
#include <sstream>

class UZRect2x1 : public UZRect
{
public:
  // Failure.
  std::vector<size_t> vertical_fail;
  std::vector<size_t> vertical_total;
  std::vector<size_t> horizontal_fail;
  std::vector<size_t> horizontal_total;
  void vertical_attempt (size_t level);
  void vertical_failure (size_t level);
  void horizontal_attempt (size_t level);
  void horizontal_failure (size_t level);
  void summarize (Treelog&) const;

  // Parameters.
  const std::vector<UZmodel*> vertical;
  const std::vector<UZ1D*> horizontal;

  // Interface.
  void tick (const GeometryRect&, 
             const std::vector<size_t>& drain_cell,
	     const double drain_water_level,
	     const Soil&, SoilWater&, 
             const SoilHeat&, const Surface&, const Groundwater&, 
             double dt, Treelog&);
  void output (Log&) const;

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
  void initialize (const Geometry& geo, const bool has_macropores);
  UZRect2x1 (const BlockModel& al);
  ~UZRect2x1 ();
};

void 
UZRect2x1::vertical_attempt (const size_t level)
{
  while (vertical_total.size () <= level)
    vertical_total.push_back (0);
  vertical_total[level]++;
}

void 
UZRect2x1::vertical_failure (const size_t level)
{
  while (vertical_fail.size () <= level)
    vertical_fail.push_back (0);
  vertical_fail[level]++;
}

void 
UZRect2x1::horizontal_attempt (const size_t level)
{
  while (horizontal_total.size () <= level)
    horizontal_total.push_back (0);
  horizontal_total[level]++;
}

void 
UZRect2x1::horizontal_failure (const size_t level)
{
  while (horizontal_fail.size () <= level)
    horizontal_fail.push_back (0);
  horizontal_fail[level]++;
}

void 
UZRect2x1::summarize (Treelog& msg) const
{
  TREELOG_MODEL (msg);
  bool found = false;
  for (size_t i = 0; i < vertical_fail.size (); i++)
    if (vertical_fail[i] > 0)
      {
        found = true;
        daisy_assert (vertical.size () > i);
        Treelog::Open nest (msg, "vertical", i, vertical[i]->name);
        daisy_assert (vertical_total[i] > 0);
        std::ostringstream tmp;
        tmp << "Matrix vertical transport model " << i << " failed " 
            << vertical_fail[i] << " times out of "
            << vertical_total[i] << ", or "
            << (100.0 * vertical_fail[i] / (vertical_total[i] + 0.0)) << "%";
        msg.warning (tmp.str ());
      }
  for (size_t i = 0; i < horizontal_fail.size (); i++)
    if (horizontal_fail[i] > 0)
      {
        found = true;
        daisy_assert (horizontal.size () > i);
        Treelog::Open nest (msg, "horizontal", i, horizontal[i]->name);
        daisy_assert (horizontal_total[i] > 0);
        std::ostringstream tmp;
        tmp << "Matrix horizontal transport model " << i << " failed " 
            << horizontal_fail[i] << " times out of " 
            << horizontal_total[i] << ", or "
            << (100.0 * horizontal_fail[i] / (horizontal_total[i] + 0.0))
            << "%";
        msg.warning (tmp.str ());
      }
  if (found)
    msg.message ("See 'daisy.log' for details.");
}

void 
UZRect2x1::output (Log&) const
{ }

void 
UZRect2x1::tick (const GeometryRect& geo, const std::vector<size_t>&,
		 const double, const Soil& soil, 
                 SoilWater& soil_water, const SoilHeat& soil_heat,
                 const Surface& surface, const Groundwater& groundwater, 
                 double dt, Treelog& msg)
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
                    col, soil_water.q_matrix_, soil_water.q_tertiary_,
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
                        || approximate (geo.cell_z (cells[col-1]),
                                        geo.cell_z (cells[col])));
          edges.push_back (edge);
          from = to;
        }

      SMM1D smm (geo, soil, soil_water, soil_heat, cells, edges);

      for (size_t i = 0; i < horizontal.size (); i++)
        {
          horizontal_attempt (i);
          Treelog::Open nest (msg, horizontal[i]->name);
          try 
            {
              horizontal[i]->tick (smm, 0.0, dt, msg);
              if (i > 0)
                msg.debug ("Reserve model succeeded");
              goto success;
            }
          catch (const char* error)
            {
              msg.debug (std::string ("UZhor problem: ") + error);
            }
          catch (const std::string& error)
            {
              msg.debug (std::string ("UZhor trouble: ") + error);
            }
          horizontal_failure (i);
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
  daisy_assert (geo.edge_from (top_edge) == top_cell);
  daisy_assert (geo.edge_to (top_edge) == Geometry::cell_above);
  daisy_assert (q.size () > bottom_cell + 1);

  // Limit for top.
  const size_t first = top_cell;

  // Limit for groundwater table.
  const size_t bottom_edge = bottom_cell + q_offset + 1U;
  daisy_assert (geo.edge_to (bottom_edge) == bottom_cell);
  daisy_assert (geo.edge_from (bottom_edge) == Geometry::cell_below);
  size_t last = bottom_cell;

  // Calculate matrix flow next.
  for (size_t m = 0; m < vertical.size (); m++)
    {
      vertical_attempt (m);
      Treelog::Open nest (msg, vertical[m]->name);
      try
        {
          vertical[m]->tick (msg, geo, soil, soil_heat,
                             first, surface, top_edge, 
                             last, groundwater, bottom_edge, 
                             S, h_old, Theta_old, h_ice, h, Theta, 
                             q_offset, q, dt);
          for (size_t i = last + 2; i <= bottom_cell + 1; i++)
            {
              daisy_assert (q.size () > i + q_offset);
              q[i + q_offset] = q[i-1 + q_offset];
              q_p[i + q_offset] = q_p[i-1 + q_offset];
            }
          if (m > 0)
            msg.debug ("Reserve model succeeded");
          return;
        }
      catch (const char* error)
        {
          msg.debug (std::string ("UZ problem: ") + error);
        }
      catch (const std::string& error)
        {
          msg.debug (std::string ("UZ trouble: ") + error);
        }
      vertical_failure (m);
    }
  throw "Vertical transport failed";
}

void 
UZRect2x1::initialize (const Geometry&, const bool has_macropores)
{
  for (size_t i = 0; i < vertical.size (); i++)
    vertical[i]->has_macropores (has_macropores);
}

UZRect2x1::UZRect2x1 (const BlockModel& al)
  : UZRect (al),
    vertical (Librarian::build_vector<UZmodel> (al, "vertical")),
    horizontal (Librarian::build_vector<UZ1D> (al, "horizontal"))
{ }

UZRect2x1::~UZRect2x1 ()
{ 
  sequence_delete (vertical.begin (), vertical.end ());
  sequence_delete (horizontal.begin (), horizontal.end ());
}

static struct UZRect2x1Syntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new UZRect2x1 (al); }
  UZRect2x1Syntax ()
    : DeclareModel (UZRect::component, "v+h", "\
Transport water in the matrix in two phases, first vertical, then\n\
horizontal.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.declare_object ("vertical", UZmodel::component, 
                      Attribute::Const, Attribute::Variable,
                      "Vertical matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends.");
    frame.set_strings ("vertical", "richards", "none");
    frame.declare_object ("horizontal", UZ1D::component,
                      Attribute::Const, Attribute::Variable,
                      "Horizontal matrix water transport models.\n\
Each model will be tried in turn, until one succeeds.\n\
If none succeeds, the simulation ends."); 
    // The 'richards' model doesn't work :-(
    frame.set_strings ("horizontal", "none");
  }
} UZRect2x1_syntax;

// uzrect_2x1.C ends here.
