// cropNdist_DPF.C -- Crop N distribution model of De Pury & Farquhar (1997)
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

#include "cropNdist.h"
#include "mathlib.h"
#include "block.h"
#include "syntax.h"
#include <sstream>
#include "check.h"

static const double Mw = 14.0; //The molecular weight for N [g mol¯1]

struct cropNdistDPF : public CropNdist
{
  // Parameters.
private:
  const double kn;  //Extinction coefficient of nitrogen in the canopy
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
  cropNdistDPF (Block& al)
    : CropNdist (al),
       kn (al.number ("kn")),
       f_photo (al.number ("f_photo")),
       Xn (al.number ("Xn"))
  { }
};

void
cropNdistDPF::cropN_distribution (const double LAI, 
				  std::vector <double>& cropNdist /*[mol/m²]*/,  
				  std::vector <double>& cropVm_total,  
				  const double cropN /*[g/m²area]*/, Treelog&)
{
  daisy_assert (std::isfinite (cropN));
  daisy_assert (cropN >= 0.0);
  
  // Crop N in top of the canopy:
  const double divisor = 1. - exp(-LAI * kn);
  daisy_assert(divisor > 0.0);
  daisy_assert (std::isnormal(divisor));

  double cropN0 = kn * cropN / divisor; // [g/m² area]
  cropN0 = cropN0 / Mw;  // [mol/m² area]
  daisy_assert (cropN0 >= 0.0);

  // Fill photosynthetically active N (cummulative) for each canopy layer in vector
  const int No = cropVm_total.size ();
  daisy_assert (cropNdist.size () == No);

  const double dLAI = (LAI /(No + 0.0));
  for (int i = 0; i < No; i++)
     cropNdist[i] = f_photo * cropN0 * (exp(-kn * dLAI *(i+0.5))); //[mol/m² leaf]

  crop_Vmax_total (LAI, cropNdist, cropVm_total);  
}

void
cropNdistDPF::crop_Vmax_total (const double, 
		       std::vector <double>& cropNdist /*[mol/m² leaf]*/,  
		       std::vector <double>& cropVm)
{
  const int No = cropVm.size ();
  daisy_assert (cropNdist.size () == No);
  // Fill photosynthetic capacity Vm for each canopy layer in vector
  for (int i = 0; i < No; i++)
     cropVm[i] = Xn * cropNdist[i]; //[mol/m² leaf/s]
}

static struct cropNdistDPFSyntax
{
  static Model& make (Block& al)
  { return *new cropNdistDPF (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("kn", Syntax::None (), Check::positive (), Syntax::Const,
                "Extinction coefficient of nitrogen in the canopy, kn = 0.713 (De Pury &Farquhar, 1997)");
    alist.add ("kn", 0.713);

    syntax.add ("f_photo", Syntax::None (), Check::positive (), Syntax::Const,
                "Fraction of photosynthetically active N in canopy. According to (Boegh et al., 2002) f_photo = 0.75. However, non-functional N is already substracted from leaf-N in the cropN_std module, therefore f_photo = 1.0 as default.");
    alist.add ("f_photo", 1.0);

    syntax.add ("Xn", "mol/mol/s", Check::positive (), Syntax::Const,
                "Slope of relationship between leaf nitrogen and Vmax, Xn = 1.16E-3 mol/mol/s for wheat (de Pury & Farquhar, 1997)");
   alist.add ("Xn", 1.16e-3);
  }  

  cropNdistDPFSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Boegh et al.(2002) crop N-distribution model in the canopy for photosynthesis and distribution of photosynthetical capacity.");

    load_syntax (syntax, alist);
    Librarian<CropNdist>::add_type ("N-exp", alist, syntax, &make);
  }
} cropNdistDPF_syntax;


const AttributeList& 
CropNdist::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      Syntax syntax;
      cropNdistDPFSyntax::load_syntax (syntax, alist);
      alist.add ("type", "N-exp");
    }
  return alist;
}
