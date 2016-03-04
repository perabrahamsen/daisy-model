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
class RubiscoNdist;
class ABAEffect;
class StomataCon;
class Treelog;
class CanopyStandard;
class Phenology;
class Log;

// Chemical constants affecting the crop.
static const double molWeightCH2O = 30.0; // [gCH2O/mol]
static const double molWeightCO2 = 44.0;  // [gCO2/mol]
static const double molWeightH2O = 18.0;  // [gH2O/mol]
static const double molWeightN = 14.0;    // [gN/mol]

class PhotoFarquhar : public Photo
{
  // Parameters.
protected:
  const double Xn; //Slope of relationship between leaf N and Vm [mmol/mol/s]
  const double Gamma25;//CO2 compensation point of photosynthesis
  const double Ea_Gamma; // Activation energy for Gamma
  std::unique_ptr<RubiscoNdist> rubiscoNdist;// Crop N distribution model.
  std::unique_ptr<StomataCon> Stomatacon;// Stomata conductance.

  // Log variable.
  std::vector<double> ci_vector; // Stomata CO2 pressure
  std::vector<double> Vm_vector; // Photosynthetic capacity  
  std::vector<double> Nleaf_vector; // Distribution of photosynthetic N-leaf  
  std::vector<double> Ass_vector; // Brutto assimilate  
  std::vector<double> Jm_vector; // Potential rate of electron transport
  std::vector<double> pn_vector; // Net photosynthesis
  std::vector<double> cs_vector; // Leaf surface CO2
  std::vector<double> hs_vector; // Relative humidity at leaf surface
  std::vector<double> gs_vector; // Stomata cunductance
  
  std::vector<double> LAI_vector; // LAI
  double ci_middel;              // Average stomata CO2 pressure per LAI units
  double Ass;                    // 'Netto' assimilate of CO2 
  double Res;                    // Leaf respiration of CO2 
  double LAI;                    // Leaf Area index for the canopy
  double PAR_;                   // Photosynthetic active radiation
  double Gamma;
  double gbw;                     // Boundary conductance [mol m^-2 L s^-1]
  double gs;                     // Stomata conductance [mol m^-2 F s^-1]
  double gs_ms;                  // Stomata conductance [m s^-1 F]
  double Vmax;                   // Photosynthetic Rubisco capacity
  double jm;                     // Potential rate of electron transport 
  double leafPhotN;              // Content of photosynthetic active leaf N
  double fraction_total;         // total fraction of leaf active in photosynthesis.
  double ABA_effect;
    
  // Simulation.
public:
  void crop_Vmax_total (const std::vector <double>& rubiscoNdist, 
			std::vector <double>& cropVm) const;
  double Arrhenius (const double k25, const double Ea, double Tl) const;  
  virtual double V_m (const double Vm_25, double Tl) const = 0;
  double Sat_vapor_pressure (const double T) const;
  double stomata_conductance() const; // [m s^-1]
private:
  virtual double respiration_rate (const double Vm_25, const double Tl) const = 0;
  virtual double J_m(const double Vm_25, const double Tl) const = 0;
  virtual void CxModel(const double CO2_atm, const double O2_atm, 
                       const double Ptot, 
                       double& pn, double& ci, const double dPAR,
		       const double gsw, const double gbw, 
                       const double Tl, const double Vm_25, 
		       const double rd, Treelog& msg) const = 0;
public:
  double assimilate (const Units&, 
                     const double ABA_xylem, const double psi_c,
                     const double ec, const double gbw_ms /* [m/s] */, 
		     const double CO2_atm, const double O2_atm, 
                     const double Ptot /* [Pa] */, 
                     double Ta, double Tc, double Tl,
                     const double cropN,
		     const std::vector<double>& PAR,
		     const std::vector<double>& PAR_Height,
		     const double PAR_LAI, const std::vector<double>& fraction,
                     double dt, CanopyStandard& canopy,
		     Phenology& development, Treelog&);
  void clear ();
  void output (Log& log) const;
  
  // Create and Destroy.
  bool handle_N_stress () const;
  bool handle_water_stress () const;
public:
  PhotoFarquhar (const BlockModel& al);
  ~PhotoFarquhar ();
};

inline double pow2 (double x)
{ return x * x; }

double first_root_of_square_equation (double a, double b, double c);
double second_root_of_square_equation (double a, double b, double c);



#endif // PHOTO_FARQUHAR_H
