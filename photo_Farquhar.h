// photo_Farquhar.h -- Leaf photosynthesis based on Farquhar et al., 1980 and Ball et al. 1987.
// 
// Copyright 1996-2001,2005 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001,2005-2006 KVL.
// Copyright 2006,2007 Birgitte Gjettermann.
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

#ifndef PHOTO_FARQUHAR_H
#define PHOTO_FARQUHAR_H

#include "photo.h"
#include <memory>
#include <vector>

class PLF;
class CropNdist;
class ABAEffect;
class Treelog;
class CanopyStandard;
class Phenology;
class Log;
class Syntax;

// Chemical constants affecting the crop.
static const double molWeightCH2O = 30.0; // [gCH2O/mol]
static const double molWeightCO2 = 44.0;  // [gCO2/mol]
static const double molWeightH2O = 18.0;  // [gH2O/mol]
static const double molWeightN = 14.0;    // [gN/mol]

class PhotoFarquhar : public Photo
{
  // Parameters.
protected:
  const double O2_atm;// Oxygen partial pressure of the atmosphere
  const double CO2_atm;//CO2 partial pressure of atmosphere
  const double Gamma25;//CO2 compensation point of photosynthesis
  const double Ea_Gamma; // Activation energy for Gamma
  const double Ptot;  // Atmospheric pressure
  const double m;     // Stomatal slope factor, Ball and Berry model
  const double b;     // Stomatal intercept factor, Ball and Berry model
  const double gbw;   // Leaf boundary conductance of water 
  std::auto_ptr<CropNdist> cropNdist;// Crop N distribution model.
  std::auto_ptr<ABAEffect> ABAeffect;// ABA-xylem effect on photosynthesis.

  // Log variable.
  std::vector<double> ci_vector; // Stomata CO2 pressure
  std::vector<double> Vm_vector; // Photosynthetic capacity  
  std::vector<double> Nleaf_vector; // Distribution of photosynthetic N-leaf  
  std::vector<double> Ass_vector; // Brutto assimilate  
  std::vector<double> Jm_vector; // Potential rate of electron transport
  std::vector<double> gs_vector; // Stomata cunductance
  std::vector<double> sun_LAI_vector; // sunlit LAI
  double ci_middel;              // Average stomata CO2 pressure per LAI units
  double Ass;                    // 'Netto' assimilate of CO2 
  double Res;                    // Leaf respiration of CO2 
  double LAI;                    // Leaf Area index for the canopy
  double sun_LAI;                // Leaf Area index for the sunlit fraction
  double PAR_;                   // Photosynthetic active radiation
  double gs;                     // Stomata conductance
  double Vmax;                   // Photosynthetic Rubisco capacity
  double jm;                     // Potential rate of electron transport 
  double leafPhotN;              // Content of photosynthetic active leaf N
  double fraction_sun;           // fraction of sunlit in the canopy.
  double fraction_total;         // total fraction of leaf active in photosynthesis.
    
  // Simulation.
public:
  double Arrhenius (const double k25, const double Ea, double Tl) const;  
  virtual double V_m (const double Vm_25, double Tl) const = 0;
  double Sat_vapor_pressure (const double T) const;
  double GSTModel(double ABA, double pn, double vp_ref, const double LA, 
		  const double fraction, const double Ta, const double Tl,  
		  double gb, Treelog& msg);
private:
  virtual double respiration_rate (const double Vm_25, const double Tl) const = 0;
  virtual double J_m(const double Vm_25, const double Tl) const = 0;
  virtual void CxModel(double& pn, double& ci, const double dPAR,
		       const double gsw, const double Tl, const double Vm_25, 
		       const double rd, Treelog& msg) const = 0;
public:
  double assimilate (const double ABA_xylem, const double rel_hum, 
		     double Ta, double Tl, const double cropN,
		     const std::vector<double>& PAR,
		     const std::vector<double>& PAR_Height,
		     const double PAR_LAI, const std::vector<double>& fraction,
                     double dt, CanopyStandard& canopy,
		     Phenology& development, Treelog&);
  void clear ();
  void output (Log& log) const;
  
  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  PhotoFarquhar (Block& al);
  virtual ~PhotoFarquhar ();
};

inline double pow2 (double x)
{ return x * x; }

double first_root_of_square_equation (double a, double b, double c);
double second_root_of_square_equation (double a, double b, double c);



#endif // PHOTO_FARQUHAR_H
