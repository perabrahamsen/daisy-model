// soil_water.h
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


#ifndef SOIL_WATER_H
#define SOIL_WATER_H

#include <vector>

class AttributeList;
class Surface;
class Groundwater;
class Log;
class Soil;
class Syntax;
class Geometry;
class SoilHeat;
class Volume;
class Treelog;
class Block;

class SoilWater
{
  friend class Movement1D;
  friend class MovementRect;
  friend class UZRect2x1;

  // Types.
public:
  enum mobile_solute_t { mobile, immobile, mixed };

  // Content.
private:
  std::vector<double> h_;
  std::vector<double> h_old_;
  std::vector<double> Theta_;
  std::vector<double> Theta_old_;
  std::vector<double> S_sum_;
  std::vector<double> S_root_;
  std::vector<double> S_drain_;
  std::vector<double> S_incorp_;
  std::vector<double> S_p_;
  std::vector<double> S_permanent_;
  std::vector<double> S_ice_;
  std::vector<double> tillage_;
  std::vector<double> X_ice_;
  std::vector<double> X_ice_buffer_;
  std::vector<double> h_ice_;
  std::vector<mobile_solute_t> mobile_solute_; 
  std::vector<double> Theta_mobile_;
  std::vector<double> Theta_immobile_;
  std::vector<double> q_;
  std::vector<double> q_p_;
  std::vector<double> K_;

  // Sink.
public:
  void clear ();
  void freeze (const Soil&, size_t c, double rate /* [h^-1] */);
  void drain (const std::vector<double>&);
  void root_uptake (const std::vector<double>&);
  
  // Queries
public:
  double h (size_t i) const
  { return h_[i]; }
  double h_old (size_t i) const
  { return h_old_[i]; }
  double Theta (size_t i) const
  { return Theta_[i]; }
  double Theta_left (size_t i, const double dt) const
  { return Theta_[i] - S_sum_[i] * dt; }
  double Theta_old (size_t i) const
  { return Theta_old_[i]; }
  double content_surface (const Geometry&, 
                          double from, double to) const; // [cm]
  double S_root (size_t i) const
  { return S_root_[i]; }
  double S_drain (size_t i) const
  { return S_drain_[i]; }
  double S_ice (size_t i) const
  { return S_ice_[i]; }
  double S_sum (size_t i) const
  { return S_sum_[i]; }
  double h_ice (size_t i) const
  { return h_ice_[i]; }
  double X_ice (size_t i) const
  { return X_ice_[i]; }
  double X_ice_total (size_t i) const
  { return X_ice_[i] + X_ice_buffer_[i]; }
  mobile_solute_t mobile_solute (size_t i) const
  { return mobile_solute_[i]; }
  double Theta_mobile (size_t i) const
  { return Theta_mobile_[i]; }
  double Theta_immobile (size_t i) const
  { return Theta_immobile_[i]; }
  double q (size_t i) const
  { return q_[i]; }
  double q_p (size_t i) const
  { return q_p_[i]; }
  double Theta_ice (const Soil&, size_t i, double h) const;
  double K (size_t i) const
  { return K_[i]; }

  // Modify.
public:
  void set_content (size_t i, double h, double Theta);
  void set_flux (size_t i, double q);

  // Simulation.
public:
  void tick (const size_t cell_size, const Soil& soil, 
             double dt, Treelog& msg);
  void tick_after (const size_t cell_size, 
                   const Soil& soil, const SoilHeat& soil_heat, Treelog& msg);
  void incorporate (const Geometry&, double amount, double from, double to);
  void incorporate (const Geometry&, double amount, const Volume&);
  void mix (const Geometry& geo, const Soil&, const SoilHeat&, double from, 
            double to, double dt, Treelog&);
  void swap (const Geometry& geo, const Soil&, const SoilHeat&, 
             double from, double middle, double to, double dt, Treelog&);
  bool check (size_t n, Treelog& err) const;
  void output (Log& log) const;

  // Communication with surface.
public:
  double MaxExfiltration (const Geometry&, size_t edge, 
                          const Soil&, double T) const;
  double MaxExfiltration (const Geometry&, const Soil&, double T) const;
  double infiltration (const Geometry&) const; // [mm/h]

  // Creation.
public:
  static void load_syntax (Syntax&, AttributeList&);
  void initialize (const AttributeList&, const Geometry&,
                   const Soil&, const SoilHeat&, const Groundwater&, Treelog&);
  SoilWater (Block&);
  virtual ~SoilWater ();
};

#endif // SOIL_WATER_H
