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

class Surface;
class Groundwater;
class Log;
class Soil;
class Frame;
class FrameSubmodel;
class Geometry;
class SoilHeat;
class Volume;
class Treelog;
class Block;

class SoilWater
{
  friend class UZRect2x1;

  // Parameters.
private:
  const double max_exfiltration_gradient; // [cm/cm]

  // Content.
private:
  std::vector<double> h_;
  std::vector<double> h_old_;
  std::vector<double> Theta_;
  std::vector<double> Theta_old_;
  std::vector<double> Theta_primary_;
  std::vector<double> Theta_primary_old_;
  std::vector<double> Theta_secondary_;
  std::vector<double> Theta_secondary_old_;
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
  std::vector<double> q_matrix_;
  std::vector<double> q_primary_;
  std::vector<double> q_secondary_;
  std::vector<double> q_tertiary_;
  std::vector<double> K_cell_;
  std::vector<double> K_edge_;

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
  double Theta_old (size_t i) const
  { return Theta_old_[i]; }
  double Theta_primary (size_t i) const
  { return Theta_primary_[i]; }
  double Theta_primary_old (size_t i) const
  { return Theta_primary_old_[i]; }
  double Theta_secondary (size_t i) const
  { return Theta_secondary_[i]; }
  double Theta_secondary_old (size_t i) const
  { return Theta_secondary_old_[i]; }
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
  double q_matrix (size_t i) const
  { return q_matrix_[i]; }
  double q_primary (size_t i) const
  { return q_primary_[i]; }
  double q_secondary (size_t i) const
  { return q_secondary_[i]; }
  double q_tertiary (size_t i) const
  { return q_tertiary_[i]; } 
  double velocity_cell_primary (const Geometry& geo, size_t i) const;
  double velocity_cell_secondary (const Geometry& geo, size_t i) const;
  double Theta_ice (const Soil&, size_t i, double h) const;
  double K_cell (size_t i) const
  { return K_cell_[i]; }

  // Modify.
public:
  void set_content (size_t cell, double h, double Theta);
  void set_flux (size_t edge, double q);
  void set_matrix (const std::vector<double>& h,
                   const std::vector<double>& Theta,
                   const std::vector<double>& q);
  void add_tertiary_sink (const std::vector<double>& S_p);
  void set_tertiary_flux (const std::vector<double>& q_p);

  // Simulation.
public:
  // Before water movement.
  void tick_before (const Geometry&, const Soil& soil, 
                    double dt, Treelog& msg);
  // After water movement.
  void tick_after (const Geometry&, 
                   const Soil& soil, const SoilHeat& soil_heat, 
                   bool initial, Treelog& msg);
  void mass_balance (const Geometry& geo, double dt, Treelog& msg);
  void incorporate (const Geometry&, double amount, double from, double to);
  void incorporate (const Geometry&, double amount, const Volume&);
  double mix (const Geometry& geo, const Soil&, const SoilHeat&, double from, 
              double to, double dt, Treelog&);
  double swap (const Geometry& geo, const Soil&, const SoilHeat&, 
               double from, double middle, double to, double dt, Treelog&);
  double overflow (const Geometry&, const Soil&, const SoilHeat&, double dt, 
                   Treelog& msg);
  bool check (size_t n, Treelog& err) const;
  void output (Log& log) const;

  // Communication with surface.
public:
  double MaxExfiltration (const Geometry&, size_t edge, 
                          const Soil&, double T) const;
  double infiltration (const Geometry&) const; // [mm/h]

  // Creation.
public:
  static void load_syntax (Frame&);
  void initialize (const FrameSubmodel&, const Geometry&,
                   const Soil&, const SoilHeat&, const Groundwater&, Treelog&);
  SoilWater (const Block&);
  ~SoilWater ();
};

#endif // SOIL_WATER_H
