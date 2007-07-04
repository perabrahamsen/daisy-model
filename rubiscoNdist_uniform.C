// rubiscoNdist_Uniform.C -- Rubisco crop N distribution model of De Pury & Farquhar (1997)
// 
// Copyright 2006 Birgitte Gjettermann and KVL
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL
#include "rubiscoNdist.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block.h"
#include "librarian.h"

static const double Mw = 14.0; //The molecular weight for N [g mol¯1]

struct rubiscoNdistUniform : public RubiscoNdist
{
  // Parameters.
private:
  const double f_photo; //Fraction of photosynthetically active N in canopy
  
  // Simulation.
  void tick (std::vector <double>& , std::vector <double>& , 
	     const double , Treelog&)
  { }
  void rubiscoN_distribution (const std::vector <double>& PAR_height,
			      const double LAI, const double DS,
			      std::vector <double>& rubiscoNdist, 
			      const double cropN/*[g]*/, Treelog& msg);
  void output (Log&) const
  { }
  const AttributeList& default_model ();

  // Create.
  public:
  rubiscoNdistUniform (Block& al)
    : RubiscoNdist (al),
       f_photo (al.number ("f_photo"))
  { }
};

void
rubiscoNdistUniform::rubiscoN_distribution (const std::vector <double>& PAR_height,
					    const double LAI, const double DS,
					    std::vector <double>& rubiscoNdist, 
					    const double cropN/*[g]*/, Treelog& msg)
{
  daisy_assert (std::isfinite (cropN));
  daisy_assert (cropN >= 0.0);
  
  // Crop N in top of the canopy:
  double cropN0 = cropN; // [g/m² area]
  cropN0 = cropN0 / Mw;  // [mol/m² area]
  cropN0 = cropN0 / LAI; // [mol/m² leaf] 
  daisy_assert (cropN0 >= 0.0);

  // Fill photosynthetically active N (cummulative) for each canopy layer in vector
  const int No = rubiscoNdist.size ();

  for (int i = 0; i < No; i++)
     rubiscoNdist[i] = f_photo * cropN0; //[mol/m² leaf]

}

static struct rubiscoNdistUniformSyntax
{
  static Model& make (Block& al)
  { return *new rubiscoNdistUniform (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("f_photo", Syntax::None (), Check::positive (), Syntax::Const,
                "Fraction of photosynthetically active N in canopy, f_photo = 0.75 (Boegh et al., 2002). However, non-functional N is already substracted from leaf-N in the cropN_std module, therefore f_photo = 1.0 as default.");
    alist.add ("f_photo", 1.0);

  }  
  rubiscoNdistUniformSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Uniform  rubisco N-distribution model in the canopy for photosynthesis.");

    load_syntax (syntax, alist);

    Librarian::add_type (RubiscoNdist::component, "uniform", alist, syntax, &make);
  }
} rubiscoNdistUniform_syntax;


