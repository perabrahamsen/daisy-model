// groundwater_pipe.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#include "groundwater.h"
#include "log.h"
#include "soil.h"
#include "soil_heat.h"
#include "tmpstream.h"
#include "treelog.h"
#include "mathlib.h"
#include "check.h"
#ifndef USE_UPDATE_WATER
#include "soil_water.h"
#endif // !USE_UPDATE_WATER

class GroundwaterPipe : public Groundwater
{
  // Parameters
  const double L;		// Distance between pipes. [cm]
  const double x;		// Distance to nearest pipe. [cm]
  const double pipe_position;	// Height pipes are placed above surface. [cm]
  const double K_aquitard_;	// Conductivity of the aquitard [cm h^-1]
  /*const*/ double Z_aquitard_;	// Vertical length of the aquitard [cm]
  const double h_aquifer;	// Pressure potential in the aquifer [cm]
  int i_bottom;			// Node, bottom layer
  int i_drain;			// Node, drain

  // Accessors.
  double Z_aquitard () const
  { return Z_aquitard_; }
  double K_aquitard () const
  { return K_aquitard_; }
  void set_Z_aquitard (double value)
  { Z_aquitard_ = value; }

  // Data.
  double height;		// Groundwater table height above surface. [cm]
  double EqDrnFlow;
  double GWT_new;		// Updated GWT
  double DrainFlow;		// Drain flow [cm/h]
  enum GWT_State_Type {Raising, Falling} GWT_State;
  vector<double> S;		// Pipe drainage. [cm^3/cm^3/h]
  vector<double> Percolation;	// [cm^3/cm^2/h]
#ifndef USE_UPDATE_WATER
  double deep_percolation;	// [cm^3/cm^2/h]
#endif // USE_UPDATE_WATER
  double extra_water_to_surface; // [cm^3/cm^2/h]
  bool flooding;
  vector<double> ThetaOld;	// [cm^3/cm^3]


  // UZbottom.
public:
  type_t type () const
  { 
#ifdef USE_UPDATE_WATER
    return pressure; 
#else // !USE_UPDATE_WATER
    return forced_flux;
#endif // !USE_UPDATE_WATER
  }
  double q_bottom () const
  { return -deep_percolation; }

  bool accept_bottom (double)
  { return true; }

  // Identity
  bool is_pipe () const
  { return true; }
  double pipe_height () const
  { return pipe_position; }

  // Simulation.
public:
  void tick (const Soil&, SoilWater&, const SoilHeat&, const Time&, Treelog&);
  void update_water (const Soil&, const SoilHeat&,
		     UZtop& top,
  		     vector<double>& S_sum,
  		     vector<double>& S_drain,
		     vector<double>& h,
		     const vector<double>& h_ice,
		     vector<double>& Theta,
		     vector<double>& q,
		     const vector<double>& q_p,
		     Treelog& msg);
  void output (Log& log) const;

private:
  double DeepPercolation (const Soil&);
  double EquilibriumDrainFlow (const Soil&, const SoilHeat&);
  void RaisingGWT  (const Soil&,
                    vector<double>& h, const vector<double>& h_ice,
                    vector<double>& Theta, const double deficit);
  void FallingGWT1 (const Soil&,
                    vector<double>& h, const vector<double>& h_ice,
                    vector<double>& Theta, const double deficit);
  void FallingGWT2 (const Soil&,
                    vector<double>& h, const vector<double>& h_ice,
                    vector<double>& Theta, const double deficit);
  void Update_GWT  (const Soil&,
                    vector<double>& h, const vector<double>& h_ice,
                    vector<double>& Theta, const vector<double>& q,
                    const vector<double>& q_p);
  double EquilibriumDrainage (const int i_drainage, const Soil& soil,
                    const vector<double>& h_ice);
  double EqlDeficit (const int node, const Soil& soil, const double Theta,
                    const double h_ice, const double GWT);
  double InternalGWTLocation (const Soil& soil, const double theta,
                    const double h_ice, const int node);
  double table () const
  { return height; }

  // Create and Destroy.
public:
  void initialize (const Soil& soil, const Time&, Treelog& treelog)
  {
    unsigned int size = soil.size ();
    double largest = 0.0;
    for (unsigned int i = 0; i < size; i++)
      if (soil.dz (i) > largest)
	largest = soil.dz (i);
    if (largest > 10.0)
      {
	Treelog::Open nest (treelog, "Groundwater pipe");
	TmpStream tmp;
	tmp () << "WARNING: drained soil needs soil intervals < 10.0 cm; "
	       << "largest is " << largest << "";
	treelog.warning (tmp.str ());
      }

    i_bottom = size - 1;
    i_drain = soil.interval_plus (pipe_position);

    S.insert (S.end (), size, 0.0);
    Percolation.insert (Percolation.end (), size, 0.0);
    ThetaOld.insert (ThetaOld.end (), size, 0.0);
  }
  GroundwaterPipe (const AttributeList& al)
    : Groundwater (al),
      L (al.number ("L")),
      x (al.check ("x") ? al.number ("x") : L / 2.0),
      pipe_position (al.number ("pipe_position")),
      K_aquitard_ (al.number ("K_aquitard")),
      Z_aquitard_ (al.number ("Z_aquitard")),
      h_aquifer (al.check ("h_aquifer") 
		 ? al.number ("h_aquifer") 
		 : Z_aquitard_),
      height (al.check ("height") ? al.number ("height") : pipe_position),
      extra_water_to_surface (-42.42e42),
      flooding (false)
  { }
  ~GroundwaterPipe ()
  { }
};

void 
GroundwaterPipe::tick (const Soil& soil, SoilWater& soil_water, 
		       const SoilHeat& soil_heat, const Time&, Treelog& msg)
{
#ifndef USE_UPDATE_WATER
  Treelog::Open nest (msg, "Groundwater " + name);

  // Empty source.
  fill (S.begin (), S.end (), 0.0);
  
  // Find groundwater height.
  height = 0.0;
  for (int i = soil.size () - 1; i >= 0; i--)
    if (soil_water.h (i) < 0)
      {
	height = soil.zplus (i);
	break;
      }

  // Find sink term.
  EqDrnFlow = EquilibriumDrainFlow (soil, soil_heat);
  DrainFlow= soil.total (S);
  soil_water.drain (S);

  // Find deep percolation.
  deep_percolation = DeepPercolation (soil);
#endif // !USE_UPDATE_WATER
}

void
GroundwaterPipe::update_water (const Soil& soil,
			       const SoilHeat& soil_heat,
			       UZtop& top,
			       vector<double>& S_sum,
			       vector<double>& S_drain,
			       vector<double>& h,
			       const vector<double>& h_ice,
			       vector<double>& Theta,
			       vector<double>& q,
			       const vector<double>& q_p,
			       Treelog& msg)
{
#ifdef USE_UPDATE_WATER
  Treelog::Open nest (msg, "Groundwater " + name);
  fill (S.begin (), S.end (), 0.0);
  for (unsigned int i = 1; i <= i_bottom+1; i++)
    {
       Percolation[i-1] = - q[i] - q_p[i];
       ThetaOld[i-1]    = Theta[i-1];
    }
  extra_water_to_surface = 0.0;
  int i_sat = i_bottom;
  for (unsigned int i = i_bottom; ; i--)
    {
       if (h[i]<=0.0) break;
       i_sat = i;
    }
  daisy_assert (i_sat > 0);
  const double GWT_UZ = soil.zplus (i_sat-1);
  if (height<GWT_UZ) height = GWT_UZ;
  Percolation[i_bottom] = DeepPercolation(soil);
  EqDrnFlow = EquilibriumDrainFlow (soil, soil_heat);
  Update_GWT (soil, h, h_ice, Theta, q, q_p);
  const int i_GWT_old = soil.interval_plus (height) + 1;
  const int i_GWT = soil.interval_plus (GWT_new) + 1;
  for (unsigned int i = 1; i <= i_bottom; i++)
    {
       const double w = (Theta[i]-ThetaOld[i]) -
                        (((Percolation[i-1]+q[i]+q_p[i]) -
                          (Percolation[i]+q[i+1]+q_p[i+1]))
                         /soil.dz(i) -
                         S[i])*dt;
       if (fabs(w)>0.0001)
	 {
	   TmpStream tmp;
	   tmp () << "GWT " << int(GWT_State)
		  << " i_GWT = " << int(i_GWT) << " i_GWT_old = " 
		  << int(i_GWT_old) << " BUG: W = " << w << " i = " << i;
	   daisy_bug (tmp.str ());
	 }
    }
  DrainFlow = 0.0;
  for (unsigned int i = 0; i <= i_bottom; i++)
    {
       S_sum[i] += S[i];
       if (i==i_GWT && S[i_drain]<=0.0 && S[i_drain-1]>0.0)
	 {
	   TmpStream tmp;
	   tmp () << " i_GWT = " << i_GWT
		  << " Percolation[i_GWT-1] = " << Percolation[i_GWT-1]
		  << " Eq-Drainage = " << EqDrnFlow;
	   daisy_bug (tmp.str ());
	 }

       DrainFlow += S[i] * soil.dz (i);
    }
  for (unsigned int i = 1; i <= i_bottom+1; i++)
    {
      q[i] = -Percolation[i-1] - q_p[i];
      daisy_assert (isfinite (q[i]));
    }
  if (isnormal (extra_water_to_surface))
    {
      if (extra_water_to_surface > 0)
	{
	  const bool ok = top.accept_top (msg, extra_water_to_surface);
	  daisy_assert (ok);
	  q[0] += extra_water_to_surface;
	  if (!flooding)
	    {
	      msg.message ("Flooding begins");
	      flooding = true;
	    }
	}
      else
	{
	  TmpStream tmp;
	  tmp () << "Extra water " << extra_water_to_surface 
		 << " should be >= 0.0";
	  msg.error (tmp.str ());
	}
    }
  else if (flooding)
    {
      msg.message ("Flooding ends");
      flooding = false;
    }
  
  daisy_assert(DrainFlow>=0.0);
  height = GWT_new;
  for (unsigned int i = i_GWT; i <= i_bottom; i++)
    h[i] = height - soil.z (i);
  S_drain = S;
#endif // USE_UPDATE_WATER
}

double
GroundwaterPipe::DeepPercolation(const Soil& soil)
{
  const double hb = height - soil.zplus(i_bottom);
  if (hb > 0)
    return K_aquitard_ * (1.0 + (hb - h_aquifer) / Z_aquitard_);
  else
    return 0;
}

double
GroundwaterPipe::EquilibriumDrainFlow (const Soil& soil, 
				       const SoilHeat& soil_heat)
{
  const int i_GWT = soil.interval_plus (height) + 1;
  if (height >= soil.zplus(i_drain-1))
    {
      // GWT located above drain
      double Ha = 0;
      double Ka = 0;
      for (unsigned int i = i_GWT; i <= i_drain; i++)
	{
	  Ha += soil.dz (i);
	  Ka += soil.dz (i) 
	    * soil.K (i, 0.0, 0.0, soil_heat.T (i))
	    * soil.anisotropy (i);
	}
      Ka /= Ha;

      // GWT located below drain
      double Hb = 0;
      double Kb = 0;
      for (unsigned int i = i_drain+1; i <= i_bottom; i++)
	{
	  Hb += soil.dz (i);
	  Kb += soil.dz (i) 
	    * soil.K (i, 0.0, 0.0, soil_heat.T (i))
	    * soil.anisotropy (i);
	}
      Kb /= Hb;
      const double Flow = (4*Ka*Ha*Ha + 2*Kb*Hb*Ha) / (L*x - x*x);

      // Distribution of drain flow among numeric soil layers
      const double a = Flow / (Ka*Ha + Kb*Hb);
      for (unsigned int i = i_GWT; i <= i_bottom; i++)
	{
          S[i] = a * soil.K (i, 0.0, 0.0, soil_heat.T (i))
	    * soil.anisotropy (i);
	}
      daisy_assert (isfinite (Flow));
#ifdef USE_UPDATE_WATER
      for (unsigned int i = i_bottom; i >= i_GWT; i--)
	{
	  Percolation[i-1] = Percolation[i] + S[i] * soil.dz (i);
	}
#endif // USE_UPDATE_WATER
      return Flow;
    }
#ifdef USE_UPDATE_WATER
  else
    {
      for (unsigned int i = i_bottom; i >= i_GWT; i--)
	{
	  daisy_assert (S[i] == 0.0);
	  Percolation[i-1] = Percolation[i] + S[i] * soil.dz (i);
	}
    }
#endif // USE_UPDATE_WATER
  return 0.0;
}

void
GroundwaterPipe::Update_GWT (const Soil& soil,
                             vector<double>& h, const vector<double>& h_ice,
                             vector<double>& Theta, const vector<double>& q,
                             const vector<double>& q_p)
{
  const int i_GWT = soil.interval_plus (height) + 1;
  const double z_drain = soil.zplus (i_drain);
  const double dPerc = (q[i_GWT+1]+q_p[i_GWT+1]) - (q[i_GWT]+q_p[i_GWT]);
  Percolation[i_GWT-1] += dPerc;
  const double deficit = (Percolation[i_GWT-1] + (q[i_GWT]+q_p[i_GWT])) * dt;
  if (deficit < 0)
    {
      RaisingGWT (soil, h, h_ice, Theta, deficit);
      GWT_State = Raising;
    }
  else if (height>z_drain)
    {
      Percolation[i_GWT-1] =  -(q[i_GWT]+q_p[i_GWT]) - dPerc;
      FallingGWT1 (soil, h, h_ice, Theta, deficit);
      Percolation[i_GWT-1] += dPerc;
      GWT_State = Falling;
    }
  else
    {
      Percolation[i_GWT-1] =  -(q[i_GWT]+q_p[i_GWT]) - dPerc;
      FallingGWT2 (soil, h, h_ice, Theta, deficit);
      Percolation[i_GWT-1] += dPerc;
      GWT_State = Falling;
    }
  const int i_UZ = soil.interval_plus (GWT_new);
  GWT_new += InternalGWTLocation (soil, Theta[i_UZ], h_ice[i_UZ], i_UZ);
}

void
GroundwaterPipe::RaisingGWT (const Soil& soil,
                             vector<double>& h, const vector<double>& h_ice,
                             vector<double>& Theta, const double deficit)
{
  vector<double> WaterDef;
  const int i_UZ = soil.interval_plus (height);
  WaterDef.insert (WaterDef.end (), i_UZ+1, 0.0);
  GWT_new = height;
  double dw = -deficit;
  for (int i = i_UZ; i >= 0; i--)
    {
       const double GWT = soil.zplus (i-1);
       double def = 0.0;
       int i_Eql = i_bottom;
       for (int j = i; j >= 0; j--)
         {
           WaterDef[j] = EqlDeficit (j, soil, Theta[j], h_ice[j], GWT);
           if (WaterDef[j] <= 0.0) break;
           def += WaterDef[j];
           i_Eql = j;
         }
       if (dw < def)
         {
           def = dw;
           for (int j = i-1; j >= i_Eql; j--)
             {
               if (def>WaterDef[j])
                 {
                   Theta[j] += WaterDef[j] / soil.dz (j);
                   h[j] = soil.h(j,Theta[j]);
                   def -= WaterDef[j];
                 }
               else
                 {
                   WaterDef[j] = def;
                   if (def>0.0)
                     {
                       Theta[j] += WaterDef[j] / soil.dz (j);
                       h[j] = soil.h(j,Theta[j]);
                     }
                   def = 0.0;
                 }
             }
           if (def>0.0)
             {
               WaterDef[i] = def;
               Theta[i] += def / soil.dz (i);
             }
           else
             WaterDef[i] = 0.0;
           def = dw;
           for (int j = i; j >= i_Eql; j--)
             {
	       daisy_assert (j >= 0);
	       daisy_assert (j < WaterDef.size ());
               def -= WaterDef[j];
	       if (j > 0)
		 {
		   daisy_assert (j <= Percolation.size ());
		   Percolation[j-1] = Percolation[j-1] - def / dt;
		 }
	       else
		 {
#if 0
		   TmpStream tmp;
		   tmp () << "percolation[" << (j - 1) 
			  << "] = " << (def / dt) << " (dw < def)";
		   daisy_bug (tmp.str ());
#endif
		   extra_water_to_surface += def / dt;
		 }
               if (def<=0.0) break;
             }
           break;
         }
       else if (i <= 0)
	 {
	   extra_water_to_surface += def/dt;
	   def = 0.0;
#if 0
	   daisy_bug ("Tile drain system inadequate. Soil profile saturated");
#endif
	 }
       else
         {
           def = 0.0;
           for (int j = i; j >= i_Eql; j--)
             {
               Theta[j] += WaterDef[j] / soil.dz (j);
               h[j] = soil.h(j,Theta[j]);
               def += WaterDef[j];
             }
           dw -= def;
           for (int j = i; j >= i_Eql; j--)
             {
	       daisy_assert (j >= 0);
	       daisy_assert (j < WaterDef.size ());
               def -= WaterDef[j];
	       if (j > 0)
		 {
		   daisy_assert (j <= Percolation.size ());
		   Percolation[j-1] -= def / dt;
		 }
	       else
		 {
#if 0
		   TmpStream tmp;
		   tmp () << "percolation[" << (j - 1) 
			  << "] = " << (def / dt) << " (dw < def)";
		   daisy_bug (tmp.str ());
#endif
		   extra_water_to_surface += def / dt;
		 }
               if (def<=0.0) break;
             }
	   daisy_assert (i > 0);
           GWT_new = soil.zplus (i-1);
           Theta[i] = soil.Theta(i,0.0,h_ice[i]);
           Percolation[i-1] -= dw / dt;
         }
    }
}

double
GroundwaterPipe::EqlDeficit (const int node, const Soil& soil,
                             const double Theta, const double h_ice,
                             const double GWT)
{
  const double h = 0.90 * (GWT - soil.z (node));
  const double Theta_Eql = soil.Theta (node, h, h_ice);
  if (Theta_Eql>Theta)
    return (Theta_Eql - Theta) * soil.dz (node);
  else
    return 0.0;
}

//GWT is above the drain depth
void
GroundwaterPipe::FallingGWT1 (const Soil& soil,
                              vector<double>& h, const vector<double>& h_ice,
                              vector<double>& Theta,
                              const double deficit)
{
  int i_drainage = -42;		// Shut up gcc.
  GWT_new = height;
  const int i_GWT = soil.interval_plus (height) + 1;
  daisy_assert (i_GWT <= i_bottom);
  for (unsigned int i = i_GWT; i <= i_bottom; i++)
    {
       i_drainage = i;
       const double def = EquilibriumDrainage(i_drainage, soil, h_ice);
       if (def >= deficit) break;
    }
  daisy_assert (i_drainage >= 0);
  double def = deficit - S[i_GWT] * soil.dz (i_GWT) * dt;
  S[i_GWT] = 0.0;
  for (unsigned int i = i_drainage; i > i_GWT; i--)
    {
       def -= S[i] * soil.dz (i) * dt;
       Percolation[i-1] = Percolation[i];
       S[i] = 0.0;
    }
  if (i_drainage > i_drain)
    {
      for (unsigned int i = i_bottom-1; i >= i_GWT; i--)
        {
           def -= S[i+1] * soil.dz (i+1) * dt;
           S[i+1] = 0.0;
           Percolation[i] = Percolation[i+1];
        }
      for (unsigned int i = i_GWT; i <= i_bottom; i++)
        {
           i_drainage = i;
           def = EquilibriumDrainage(i_drainage, soil, h_ice);
           if (def >= deficit) break;
        }
      if (def <= 0.0)
        {
           const double DrainSink = -def /
                                (soil.zplus (i_GWT-1) - soil.zplus (i_bottom));
           for (unsigned int i = i_bottom-1; i >= i_GWT; i--)
             {
               S[i] = DrainSink;
               Percolation[i] = Percolation[i+1] + S[i] * soil.dz (i);
             }
           const double w = (Percolation[i_GWT-1] - Percolation[i_GWT]) * dt -
                            S[i_GWT] * soil.dz (i_GWT) * dt;
           if (fabs(w)>0.0001)
	     {
	       TmpStream tmp;
	       tmp () << "Falling GWT: W = " << w << " i_GWT = " << i_GWT
		      << " deficit = " << deficit;
	       daisy_bug (tmp.str ());
	     }
           return;
      }
    }
  GWT_new = soil.zplus (i_drainage);
  for (unsigned int i = i_drainage; i > i_GWT; i--)
    {
       const double ThetaS = soil.Theta(i,0.0,h_ice[i]);
       h[i] = -(soil.z(i) - GWT_new);
       Theta[i] = soil.Theta(i,h[i],h_ice[i]);
       Percolation[i-1] = Percolation[i] -
                          (ThetaS - Theta[i]) * soil.dz (i) / dt;
    }
  const double dTheta = (Percolation[i_GWT-1] - Percolation[i_GWT])
                        / soil.dz (i_GWT) * dt;
  if (dTheta >= 0.0)
    S[i_GWT] = dTheta / dt;
  else
    Theta[i_GWT] += dTheta;
  if (Theta[i_GWT] > soil.Theta(i_GWT,0.0,h_ice[i_GWT]))
    { 
      TmpStream tmp;
      tmp () << "Theta = " << Theta[i_GWT] << ", Theta_sat = "
	     << soil.Theta(i_GWT,0.0,h_ice[i_GWT]);
      daisy_bug (tmp.str ());
    }
  h[i_GWT] = soil.h (i_GWT, Theta[i_GWT]);
}

void
GroundwaterPipe::FallingGWT2 (const Soil& soil,
                              vector<double>& h, const vector<double>& h_ice,
                              vector<double>& Theta,
                              const double deficit)
{
  int i_drainage = -42;		// Shut up GCC.
  GWT_new = height;
  const int i_GWT = soil.interval_plus (height) + 1;
  for (unsigned int i = i_GWT; i <= i_bottom; i++)
    {
       i_drainage = i;
       const double def = EquilibriumDrainage(i_drainage, soil, h_ice);
       if (def >= deficit) 
	 goto found;
    }
  throw ("Groundwater falling below last node, need aquitard horizon");
 found:
  GWT_new = soil.zplus (i_drainage);
  for (unsigned int i = i_drainage; i > i_GWT; i--)
    {
       const double ThetaS = soil.Theta(i,0.0,h_ice[i]);
       h[i] = -(soil.z(i) - GWT_new);
       Theta[i] = soil.Theta(i,h[i],h_ice[i]);
       Percolation[i-1] = Percolation[i] -
                          (ThetaS - Theta[i]) * soil.dz (i) / dt;
    }
  const double dTheta = (Percolation[i_GWT-1] - Percolation[i_GWT])
                        / soil.dz (i_GWT) * dt;
  Theta[i_GWT] += dTheta;
  if (Theta[i_GWT] > soil.Theta(i_GWT,0.0,h_ice[i_GWT]))
    {
      TmpStream tmp;
      tmp () << "Theta = " << Theta[i_GWT] << ", Theta_sat = "
	     << soil.Theta(i_GWT,0.0,h_ice[i_GWT]);
      daisy_bug (tmp.str ());
    }
  h[i_GWT] = soil.h (i_GWT, Theta[i_GWT]);
}

#if 0
// GWT is below the drain depth
void
GroundwaterPipe::FallingGWT2 (const Soil& soil,
                              vector<double>& h, const vector<double>& h_ice,
                              vector<double>& Theta, treelog&)
{
  GWT_new = height;
  const double z_bottom = soil.zplus(i_bottom);
  const int i_GWT = soil.interval_plus (height) + 1;
  for (unsigned int i = i_GWT; i <= i_bottom; i++)
    {
       const double qi = Percolation[i-1];
       const double qo = Percolation[i];
       const double ThetaS = soil.Theta(i,0.0,h_ice[i]);
       const double h1 = max ( z_bottom - height, -100.0);
       const double Theta1 = soil.Theta(i,h1,h_ice[i]);
       const double h2 = max (-soil.dz (i) / 2.0, h1);
       const double Theta2 = soil.Theta(i,h2,h_ice[i]);
       const double W1 = (ThetaS - Theta1) * soil.dz (i) / dt;
       const double W2 = (ThetaS - Theta2) * soil.dz (i) / dt;
       if (qo-qi < 0)
         {
           S[i] = (qi-qo) / soil.dz (i);
           break;
         }
       else if (qo-qi < W2)
         {
           h[i] = h2;
           Theta[i] = Theta2;
           const double W = (ThetaS - Theta2) * soil.dz (i) / dt + qi - qo;
           S[i] = W / soil.dz (i);
           GWT_new = soil.zplus (i);
           break;
         }
       else if (qo-qi < W1)
         {
           Theta[i] = ThetaS + (qi-qo) / soil.dz (i) * dt;
           h[i] = soil.h(i,Theta[i]);
           GWT_new = soil.zplus (i);
           break;
         }
       else
         {
           Theta[i] = Theta2;
           h[i] = h2;
           Percolation[i] = qi + (ThetaS-Theta2)/dt * soil.dz (i);
         }
    }
}
#endif

double
GroundwaterPipe::EquilibriumDrainage (const int i_drainage,
                                      const Soil& soil,
                                      const vector<double>& h_ice)
{
  const int i_GWT = soil.interval_plus (height) + 1;
  double w = 0;
  for (unsigned int i = i_GWT; i <= i_drainage; i++)
    {
       const double ThetaS = soil.Theta(i,0.0,h_ice[i]);
       const double h = -(soil.z(i) - soil.zplus(i_drainage));
       const double Theta =  soil.Theta(i,h,h_ice[i]);
       double Sd = 0;
       if (i<=i_drain)
         Sd = S[i] * dt * soil.dz(i);
       else if (i==i_drain+1)
         for (unsigned int j = i_drain+1; j <= i_bottom; j++)
           Sd += S[i] * dt * soil.dz(i);
       w += (ThetaS - Theta) * soil.dz(i) + Sd;
    }
  return w;
}

double
GroundwaterPipe::InternalGWTLocation (const Soil& soil, const double theta,
                                      const double h_ice, const int node)
{
  vector<double> IntWater;
  const double dz = soil.dz (node);
  const int n = double2int (bound (5.0, dz / 1.0, 100.0));
  IntWater.insert (IntWater.end (), n, 0.0);
  const double dx = dz / n;
  double h_bottom = 0.0;
  double AccWater = 0.0;
  for (unsigned int i = 0; i < n; i++)
    {
       const double h = h_bottom - (i - 0.5) * dx;
       IntWater[i] = soil.Theta(node,h,h_ice) * dx;
       AccWater += IntWater[i];
    }
  const double WaterCont = theta * dz;
  for (unsigned int i = 0; i < n; i++)
    {
       AccWater += IntWater[0] - IntWater[n-i-1];
       if (AccWater >= WaterCont) break;
       h_bottom += dx;
    }
  return h_bottom;
}

void
GroundwaterPipe::output (Log& log) const
{
  Groundwater::output (log);
  output_variable (DrainFlow, log);
  output_variable (EqDrnFlow, log);
  output_value (deep_percolation, "DeepPercolation", log);
  output_variable (S, log);
}

static struct GroundwaterPipeSyntax
{
  static Groundwater& make (const AttributeList& al)
    {
      return *new GroundwaterPipe (al);
    }
  GroundwaterPipeSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Groundwater for pipe (tile) drained soil.\n\
If you specify this groundwater model, and does not specify the 'zplus' Soil\n\
discretion parameter, an extra aquitard soil horizon approximately a third\n\
of the size of 'Z_aquitart' will be added.  This will allow the grounwater\n\
level to sink into the aquitart.  The model cannot handle groundwater levels\n\
below the last node, or above the soil surface.");
      Groundwater::load_syntax (syntax, alist);

      syntax.add ("L", "cm", Check::positive (), Syntax::Const,
		  "Distance between pipes.");
      alist.add ("L", 1800.0);
      syntax.add ("x", "cm", Check::positive (), Syntax::OptionalConst,
		  "Horizontal distance to nearest pipe.\n\
By default, this is 1/2 L.");
      syntax.add ("pipe_position", "cm", Check::negative (), Syntax::Const,
		  "Height pipes are placed in the soil (a negative number).");
      alist.add ("pipe_position", -110.0);
      syntax.add ("K_aquitard", "cm/h", Check::non_negative (), Syntax::Const,
		  "Conductivity of the aquitard.");
      alist.add ("K_aquitard", 1e-4);
      syntax.add ("Z_aquitard", "cm", Check::positive (), Syntax::Const,
		  "Thickness of the aquitard.\n\
The aquitard begins below the bottommost soil horizon.");
      alist.add ("Z_aquitard", 200.0);
      syntax.add ("h_aquifer", "cm", Check::positive (), Syntax::OptionalConst,
		  "Pressure potential in the aquifer below the aquitard.\n\
By default. this is Z_aquitard.");

      syntax.add ("height", "cm", Check::non_positive (), 
		  Syntax::OptionalState,
		  "Current groundwater level (a negative number).");
      syntax.add ("DrainFlow", "cm/h", Syntax::LogOnly,
		  "Drain flow to pipes.");
      syntax.add ("EqDrnFlow", "cm/h", Syntax::LogOnly,
		  "Equilibrium drain flow to pipes.");
      syntax.add ("deficit", "cm", Syntax::LogOnly,
		  "Deficit.");
      syntax.add ("DeepPercolation", "cm/h", Syntax::LogOnly,
		  "Deep percolation to aquifer.");
      syntax.add ("S", "cm^3/cm^3/h", Syntax::LogOnly, Syntax::Sequence,
		  "Pipe drainage.");
      Librarian<Groundwater>::add_type ("pipe", alist, syntax, &make);
    }
} GroundwaterPipe_syntax;


