// average.C --- Find the average of two numbers.
// 
// Copyright 1996-2001, 2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001, 2005 KVL.
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

#include "average.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"

// average component.

const char *const Average::component = "average";

symbol 
Average::library_id () const
{
  static const symbol id (component);
  return id;
}

Average::Average ()
{ }

Average::~Average ()
{ }

// arithmetic model.

struct AverageArithmetic : public Average
{
  // Simulation.
  double operator()(double a, double b) const
  { return (a + b) / 2.0; }
  // Create and Destroy.
  AverageArithmetic (const BlockModel&)
  { }
  AverageArithmetic (const char *const)
  { }
  ~AverageArithmetic ()
  { }
};

static struct AverageArithmeticSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new AverageArithmetic (al); }
  AverageArithmeticSyntax ()
    : DeclareModel (Average::component, "arithmetic", "\
Arithmetic average '(a+b)/2'.")
  { }
  void load_frame (Frame& frame) const
  { }
} AverageArithmetic_syntax;


std::unique_ptr<const Average>
Average::build_arithmetic ()
{ return std::unique_ptr<const Average> (new AverageArithmetic (__FUNCTION__)); }

// harmonic model.

struct AverageHarmonic : public Average
{
  // Simulation.
  double operator()(double a, double b) const
  { return 2.0 * a * b / (a + b); }
  // Create and Destroy.
  AverageHarmonic (const BlockModel&)
  { }
  ~AverageHarmonic ()
  { }
};

static struct AverageHarmonicSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new AverageHarmonic (al); }
  AverageHarmonicSyntax ()
    : DeclareModel (Average::component, "harmonic", "\
Harmonic average '2ab/(a+b)'.")
  { }
  void load_frame (Frame& frame) const
  { }
} AverageHarmonic_syntax;

// geometric model.

struct AverageGeometric : public Average
{
  // Simulation.
  double operator()(double a, double b) const
  { return sqrt (a * b); }
  // Create and Destroy.
  AverageGeometric (const BlockModel&)
  { }
  AverageGeometric (const char *const)
  { }
  ~AverageGeometric ()
  { }
};

static struct AverageGeometricSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new AverageGeometric (al); }
  AverageGeometricSyntax ()
    : DeclareModel (Average::component, "geometric", "\
Geometric average 'sqrt(a*b)'.")
  { }
  void load_frame (Frame& frame) const
  { }
} AverageGeometric_syntax;

std::unique_ptr<const Average>
Average::build_geometric ()
{ return std::unique_ptr<const Average> (new AverageGeometric (__FUNCTION__)); }

static struct AverageInit : public DeclareComponent 
{
  AverageInit ()
    : DeclareComponent (Average::component, "\
Find the average of two numbers.")
  { }
} Average_init;

// average.C ends here.
