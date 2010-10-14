// draineqd.C --- Find the equivalent drain depth for Hooghoudt model.
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

#include "draineqd.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"

// draineqd component.

const char *const Draineqd::component = "draineqd";

symbol 
Draineqd::library_id () const
{
  static const symbol id (component);
  return id;
}

Draineqd::Draineqd ()
{ }

Draineqd::~Draineqd ()
{ }

static struct DraineqdInit : public DeclareComponent 
{
  DraineqdInit ()
    : DeclareComponent (Draineqd::component, "\
Find the equilibrium drain depth for the Hooghoudt drainage model.")
  { }
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame); 
    frame.set_strings ("cite", "hooghoudt");
  }
} Draineqd_init;

// 'none' model.

struct DraineqdNone : public Draineqd
{
  // Simulation.
  double value (double L, double rad, double Hb) const
  { return Hb;}
  // Create and Destroy.
  DraineqdNone (const BlockModel&)
  { }
  ~DraineqdNone ()
  { }
};

static struct DraineqdNoneSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DraineqdNone (al); }
  DraineqdNoneSyntax ()
    : DeclareModel (Draineqd::component, "none", "\
No modifications.")
  { }
  void load_frame (Frame& frame) const
  { }
} DraineqdNone_syntax;



// Wesseling
struct DraineqdWesseling : public Draineqd
{
  // Simulation.
  double value (double L, double rad, double Hb) const
  {
    double part1 = (pow((L-sqrt(2.0)*Hb),2.0))/(8.0*Hb*L);
    double part2 = (1/M_PI)*log(Hb/(sqrt(2)*rad));
    return L / (8.0*(part1+part2));
  }
  // Create and Destroy.
  DraineqdWesseling (const BlockModel&)
  { }
  ~DraineqdWesseling ()
  { }
};

static struct DraineqdWesselingSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DraineqdWesseling (al); }
  DraineqdWesselingSyntax ()
    : DeclareModel (Draineqd::component, "Wesseling", "\
Equivalent depth calculated with model by Wesseling.")
  { }
  void load_frame (Frame& frame) const
  {frame.set_strings ("cite", "wesseling");}
} DraineqdWesseling_syntax;


// Moody
struct DraineqdMoody : public Draineqd
{
  // Simulation.
  double value (double L, double rad, double Hb) const
  { 
    if (Hb/L <= 0.3)
      return Hb / (1+(Hb/L)*(8.0/M_PI*log(Hb/rad)-3.4));
    else  
      return L / (8.0/M_PI*(log(L/rad)-1.15));
  }
  // Create and Destroy.
  DraineqdMoody (const BlockModel&)
  { }
  ~DraineqdMoody ()
  { }
};

static struct DraineqdMoodySyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DraineqdMoody (al); }
  DraineqdMoodySyntax ()
    : DeclareModel (Draineqd::component, "Moody", "\
Equivalent depth calculated with model by Moody.")
  { }
  void load_frame (Frame& frame) const
  {frame.set_strings ("cite", "moody");}
} DraineqdMoody_syntax;

// van der Molen and Wesseling
struct DraineqdMolenWesseling : public Draineqd
{
  // Simulation.
  double value (double L, double rad, double Hb) const
  { 
    double y = 2*M_PI * Hb / L;
    double F = 0.0;
    if (y < 0.5)
      F = (M_PI*M_PI)/(4*y)+log(y/(2*M_PI));
    else 
      {
        for (int j = 0; j < 100; j++)
          {
            int n = 2*j-1;
            F += 4.0*exp(-2.0*n*y) / (n*(1-exp(-2.0*n*y))); 
          }
      }
    return (M_PI*L/8.0) / (log(L/(M_PI*rad))+F);
  }
  // Create and Destroy.
  DraineqdMolenWesseling (const BlockModel&)
  { }
  ~DraineqdMolenWesseling ()
  { }
};

static struct DraineqdMolenWesselingSyntax : DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new DraineqdMolenWesseling (al); }
  DraineqdMolenWesselingSyntax ()
    : DeclareModel (Draineqd::component, "MolenWesseling", "\
Equivalent depth calculated with model by van der Molen and Wesseling.")
  { }
  void load_frame (Frame& frame) const
  {frame.set_strings ("cite", "molenwesseling");}
} DraineqdMolenWesseling_syntax;


// draineqd.C ends here.
