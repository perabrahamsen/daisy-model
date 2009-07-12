// ponddamp.C --- Dampening effect of ponding on randrop momentum.
// 
// Copyright 2009 Per Abrahamsen and KVL.
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

#include "ponddamp.h"
#include "mathlib.h"
#include "librarian.h"
#include "block_model.h"
#include "frame.h"

// The 'ponddamp' component.

const char *const Ponddamp::component = "ponddamp";

symbol 
Ponddamp::library_id () const
{
  static const symbol id (component);
  return id;
}

double                          // Median droplet size [mm].
Ponddamp::dds (const double P)
{ return 1.238 * std::pow (P, 0.182); }

Ponddamp::Ponddamp ()
{ }

Ponddamp::~Ponddamp ()
{ }

static struct PonddampInit : public DeclareComponent 
{
  PonddampInit ()
    : DeclareComponent (Ponddamp::component, "\
Dampening affect of ponding on soil erosion from rain.")
  { }
  void load_frame (Frame& frame) const
  { Model::load_model (frame); }
} Ponddamp_init;

// The 'EUROSEM' model.

struct PonddampEUROSEM : public Ponddamp
{
  // Parameters.
  const double b;               // [mm^-1]

  // Simulation.
  double value (const double h, const double /* P */) const
  { return std::min (1.0, std::exp (- b * h)); }
  // Create and Destroy.
  PonddampEUROSEM (const BlockModel& al)
    : b (al.number ("b"))
  { }
  ~PonddampEUROSEM ()
  { }
};

static struct PonddampEUROSEMSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PonddampEUROSEM (al); }
  PonddampEUROSEMSyntax ()
    : DeclareModel (Ponddamp::component, "EUROSEM", "\
KH = exp (-b h)")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "EUROSEM");
    frame.declare ("b", "mm^-1", Attribute::Const, "\
Exponential degradation coefficient.\n\
The range of 'b' is from 0.9 to 3.1, a default value of 2 is proposed\n\
by the EUROSEM project.");
    frame.set ("b", 2.0);
  }
} PonddampEUROSEM_syntax;

// The 'Park82' model.

struct PonddampPark82 : public Ponddamp
{
  // Simulation.
  double value (const double h, const double P) const
  {
    const double dds = this->dds (P);
    daisy_assert (dds > 0.9);
    return std::min (1.0, 2.7183 * std::exp (- h / dds)); 
  }

  // Create and Destroy.
  PonddampPark82 (const BlockModel&)
  { }
  ~PonddampPark82 ()
  { }
};

static struct PonddampPark82Syntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PonddampPark82 (al); }
  PonddampPark82Syntax ()
    : DeclareModel (Ponddamp::component, "Park82", "\
KH = 2.7183 * exp (-h / dds)")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "park82");
  }
} PonddampPark82_syntax;

// The 'Hairsine91' model.

struct PonddampHairsine91 : public Ponddamp
{
  // Simulation.
  double value (const double h, const double P) const
  { 
    if (h < 0.01)
      return 1.0;
    
    return std::min (1.0, std::pow (h / dds (P), -0.8)); 
  }

  // Create and Destroy.
  PonddampHairsine91 (const BlockModel&)
  { }
  ~PonddampHairsine91 ()
  { }
};

static struct PonddampHairsine91Syntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PonddampHairsine91 (al); }
  PonddampHairsine91Syntax ()
    : DeclareModel (Ponddamp::component, "Hairsine91", "\
KH = (h / dds)^-0.8")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.set_strings ("cite", "hairsine91");
  }
} PonddampHairsine91_syntax;

// The 'none' model.

struct PonddampNone : public Ponddamp
{
  // Simulation.
  double value (const double h, const double P) const
  { return 1.0; }

  // Create and Destroy.
  PonddampNone (const BlockModel&)
  { }
  ~PonddampNone ()
  { }
};

static struct PonddampNoneSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new PonddampNone (al); }
  PonddampNoneSyntax ()
    : DeclareModel (Ponddamp::component, "none", "\
KH = 1.0")
  { }
  void load_frame (Frame& frame) const
  { }
} PonddampNone_syntax;

// ponddamp.C ends here.
