// cropNdist_Uniform.C -- Crop N distribution model of De Pury & Farquhar (1997)
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
#include "cropNdist.h"
#include "mathlib.h"
#include <sstream>
#include "check.h"
#include "block.h"
#include "librarian.h"

static const double Mw = 14.0; //The molecular weight for N [g mol¯1]

struct cropNdistUniform : public CropNdist
{
  // Parameters.
private:
  const double f_photo; //Fraction of photosynthetically active N in canopy
  const double Xn; //Slope of relationship between leaf N and Vm [mmol/mol/s]
  
  // Simulation.
  void tick (std::vector <double>& , std::vector <double>& , 
	     const double , Treelog&)
  { }
  void cropN_distribution (const double LAI, 
			   std::vector <double>& cropNdist, 
			   std::vector <double>& cropVm, 
			   const double cropN/*[g]*/, Treelog& msg);
  void crop_Vmax_total (const double LAI, 
			std::vector <double>& cropNdist, 
			std::vector <double>& cropVm);
  void output (Log&) const
  { }
  const AttributeList& default_model ();

  // Create.
  public:
  cropNdistUniform (Block& al)
    : CropNdist (al),
       f_photo (al.number ("f_photo")),
       Xn (al.number ("Xn"))
  { }
};

void
cropNdistUniform::cropN_distribution (const double LAI, 
				  std::vector <double>& cropNdist /*[mol/m²]*/,  
				  std::vector <double>& cropVm_total,  
				  const double cropN /*[g/m²area]*/, Treelog&)
{
  daisy_assert (std::isfinite (cropN));
  daisy_assert (cropN >= 0.0);
  
  // Crop N in top of the canopy:
  double cropN0 = cropN; // [g/m² area]
  cropN0 = cropN0 / Mw;  // [mol/m² area]
  cropN0 = cropN0 / LAI; // [mol/m² leaf] 
  daisy_assert (cropN0 >= 0.0);

  // Fill photosynthetically active N (cummulative) for each canopy layer in vector
  const int No = cropVm_total.size ();
  daisy_assert (cropNdist.size () == No);

  for (int i = 0; i < No; i++)
     cropNdist[i] = f_photo * cropN0; //[mol/m² leaf]
  crop_Vmax_total (LAI, cropNdist, cropVm_total);  
}

void
cropNdistUniform::crop_Vmax_total (const double, 
		       std::vector <double>& cropNdist /*[mol/m² leaf]*/,  
		       std::vector <double>& cropVm)
{
  const int No = cropVm.size ();
  daisy_assert (cropNdist.size () == No);
  // Fill photosynthetic capacity Vm for each canopy layer in vector
  for (int i = 0; i < No; i++)
     cropVm[i] = Xn * cropNdist[i]; //[mol/m² leaf/s]
}

static struct cropNdistUniformSyntax
{
  static Model& make (Block& al)
  { return *new cropNdistUniform (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("f_photo", Syntax::None (), Check::positive (), Syntax::Const,
                "Fraction of photosynthetically active N in canopy, f_photo = 0.75 (Boegh et al., 2002)");
    alist.add ("f_photo", 0.75);

    syntax.add ("Xn", "mol/mol/s", Check::positive (), Syntax::Const,
                "Slope of relationship between leaf nitrogen and Vmax, Xn = 1.16E-3 mol/mol/s for wheat (de Pury & Farquhar, 1997)");
   alist.add ("Xn", 1.16e-3);
  }  
  cropNdistUniformSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Uniform  crop N-distribution model in the canopy for photosynthesis and distribution of photosynthetical capacity.");

    load_syntax (syntax, alist);

    Librarian::add_type (CropNdist::component, "N-uniform", alist, syntax, &make);
  }
} cropNdistUniform_syntax;


