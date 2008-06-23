// tertiary.C --- Transport of water and solute outside the matrix.
// 
// Copyright 2008 Per Abrahamsen and KU.
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

#include "tertiary.h"
#include "geometry.h"
#include "soil_water.h"
#include "block.h"
#include "alist.h"
#include "librarian.h"

// The 'tertiary' component.

const char *const Tertiary::component = "tertiary";

symbol
Tertiary::library_id () const
{
  static const symbol id (component);
  return id;
}

Tertiary::Content::Content ()
{ }

Tertiary::Content::~Content ()
{ }

const Tertiary::Content& 
Tertiary::State::inspect () const
{ return *content; }

Tertiary::State& 
Tertiary::State::operator= (const State& other)
{
  daisy_assert (this != &other);
  content = other.content->clone (); 
  return *this;
}

Tertiary::State::State (const Tertiary::State& other)
{
  daisy_assert (this != &other);
  content = other.content->clone ();
}

Tertiary::State::State (const std::auto_ptr<Content> other)
{ 
  daisy_assert (content.get () != other.get ());
  content = other->clone (); 
}

Tertiary::State::~State ()
{ }

Tertiary::State 
Tertiary::get_state () const
// By default, we have no state.
{
  struct ContentNone : public Content
  {
    std::auto_ptr<Content> clone () const
    { 
      std::auto_ptr<Content> copy (new ContentNone ());
      return copy; 
    }
  };
  
  std::auto_ptr<Content> copy (new ContentNone ());
  return State (copy);
}

void
Tertiary::set_state (const State&)
{ }

bool 
Tertiary::converge (const State&)
{ return true; }

void
Tertiary::tick (const Geometry& geo, const Soil& soil, const double dt, 
                SoilWater& soil_water, Surface& surface, Treelog& msg)
{
  Treelog::Open nest (msg, component + std::string (":") + name);
  const size_t cell_size = geo.cell_size ();
  std::vector<double> S_drain (cell_size, 0.0);
  std::vector<double> S_matrix (cell_size, 0.0);
  const size_t edge_size = geo.edge_size ();
  std::vector<double> q_tertiary (edge_size, 0.0);
  this->tick_water (geo, soil, soil_water, dt, surface,
                    S_drain, S_matrix, q_tertiary, msg);
  soil_water.drain (S_drain);
  soil_water.set_tertiary (S_matrix, q_tertiary);
}

void 
Tertiary::matrix_sink (const Geometry& geo, const Soil& soil,  
                       const SoilHeat& soil_heat, 
                       const std::vector<double>& h,
                       std::vector<double>& S_matrix,
                       std::vector<double>& S_drain) const
{ }

void 
Tertiary::update_biopores (const Geometry& geo, 
                           const Soil& soil,  
                           const SoilHeat& soil_heat, 
                           const std::vector<double>& h,
                           const double dt)
{ }

Tertiary::Tertiary (Block& al)
  : ModelAListed (al.alist ())
{ }

Tertiary::~Tertiary ()
{ }

static Librarian Tertiary_init (Tertiary::component, "\
Transport of water and solute outside the matrix.");

// The 'none' model.

class TertiaryNone : public Tertiary
{
  // Identity.
  bool has_macropores ()
  { return false; }

  // Simulation.
  void tick_water (const Geometry&, const Soil&, const SoilWater&,
                   const double /* dt */,
                   Surface& /* surface */,
                   std::vector<double>& /* S_drain */,
                   std::vector<double>& /* S_matrix */,
                   std::vector<double>& /* q_tertiary */, 
                   Treelog&)
  { }
  void update_water (const Geometry&, const Soil&, 
                     const std::vector<double>& h_matrix,
                     const double dt,
                     std::vector<double>& S_drain,
                     std::vector<double>& S_matrix, 
                     std::vector<double>& q_tertiary, 
                     Treelog& msg)
  { }
  void solute (const Geometry&, const SoilWater&, 
               const std::map<size_t, double>& J_tertiary,
               const double /* dt */,
               Chemical&, Treelog&)
  { }
  void output (Log&) const
  { }

  // Create and Destroy.
  bool initialize (const Geometry&, const Soil&, const Scope&,  
                   const double /* pipe_position */, Treelog&)
  { return true; }
  bool check (const Geometry&, Treelog&) const
  { return true; }
public:
  TertiaryNone (Block& al)
    : Tertiary (al)
  { }
};

static struct TertiaryNoneSyntax
{
  static Model& make (Block& al)
  { return *new TertiaryNone (al); }
  TertiaryNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "No tertiary transport.");
    Librarian::add_type (Tertiary::component, "none", alist, syntax, &make);
  }
} TertiaryNone_syntax;

const AttributeList& 
Tertiary::none_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    alist.add ("type", "none");

  return alist;
}

// tertiary.C ends here.
