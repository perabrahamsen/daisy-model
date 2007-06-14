// svat_PMSW.C --- Advanced SVAT model using hourly weatherdata.
// 
// Copyright 1996-2002 Peter van der Keur, Per Abrahamsen and Søren Hansen.
// Copyright 2000-2002 KVL.
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


// Status: revised October 1999
// important for sensitivity studies: Do not alter !

// description of functions

#define BUILD_DLL

# include "svat.h"
#include "mathlib.h"
#include "block.h"
# include "surface.h"
# include <fstream>
# include <stdio.h>
# include <stdlib.h>
# include "weather.h"
# include "time.h"
# include "soil.h"
# include "soil_water.h"
# include "soil_heat.h"
# include "vegetation.h"
# include "pet.h"
# include "log.h"
#include "fao.h"
#ifndef NRGAUSS
#include "gaussj.h"
#include "librarian.h"
#include <sstream>
#endif
# include "assertion.h"
# define NRANSI // from xgaussj
#define vector _my_vector
# include "nrutil.h"  // from Num Rec
#undef vector
# define SWAP(a,b) {temp=(a);(a)=(b);(b)=temp;} // from gaussj function
# define NP 20 // from xgaussj driver program
# define MP 20 // from xgaussj driver program

using namespace std;

#ifdef _MSC_VER
#pragma warning (disable: 4244)
#endif
// prototypes
int RA(double, double, double, double&);
int RAA(double, double, double, double, double, double, double, double,
	double&, double&, double&, double&);
int RASTAB (double, double, double, double, double, double, double, double, 
	    double, double&, double&, double&, double&, double&, double&);
int RAASTAB_1 (double, double, double, double, double, double, double, double, 
	       double, double&, double&, double&, double&, double&, double&, 
	       double&);
int RAASTAB_2 (double,
	       double, double, double, double, double, double, double, double,
               double, double&, double&, double&, double&, double&, double&,
               double&, double&);
int RAC(double, 
	double, double, double , double&, double&, double&, double&, double&);
int RAS(double,
	double, double, double, double, double, double, double&, double&);
int RSC(double, double, double, double, double, double, double , double,
        double, double, double, double, double, double, double, double,
        double, double, double, double, double, double, double&, double&,
	double&, double&, double&, double&, double&, double&, double&, double&,
	double&, double&, double&, double&, double&, double&, double&, double&,
	double&, double&, double&, double&, double&);
int RSCSTAR (double, double, double, double, double, double, double, double,
             double, double, double, double, double, double, double, double,
             double, double, double, double, double, double, double, double&,
             double&, double&, double&, double&, double&, double&, double&);
int EPA2ABS(double, double, double&);
int EABS2PA(double, double, double&);
int NETRAD(double, double, double, double, double , double, double, double,
           double, double&, double&, double&, double&, double&);
int AVENER(double ,double ,double ,double ,double&, double&);
int GFLUX(double, double, double, double&);
int LEHFLUX(double, double, double, double, double, double, double, double,
            double, double, double, double, double, double, double, double&,
            double&, double&, double&,double&, double&, double&, double&, double&,
            double&, double&, double&);
int LEHFLUXSTAR(double, double, double, double, double, double, double, double,
                double, double, double, double, double&, double&, double&, double&,
                double&, double&, double&, double&, double&, double&, double&);
int VAPOR(double, double&, double&, double&, double&);
int ACOEFF(double, double, double, double, double, double, double,
           double, double, double, double, double, double&, double&, double&,
           double&, double&, double&, double&, double&, double&, double&, double&,
           double&, double&, double&, double&);
double RTSAFE_DT(void *, double, double, double, double, double, double, double,
                 double, double, double, double&, double, double, double);
void EBAL_NR(double, double, double, double, double, double, double,
             double, double, double, double, double&, double *, double *);
int EBAL_DT(double, double, double, double, double, double, double, double,
            double, double, double, double&, double&, double&, double&,
            double&, double&, double&, double&, double&, double&, double&, double&,
            double&, double&, double&, double&, double&, double&, double&, double&,
            double&, double&, double&, double&, double&);
int EBAL_PM(double, double, double, double, double, double, double, double,
            double, double, double, double&, double&, double&, double&, double&,
            double&, double&, double&, double&, double&, double);

// Declaration of functions

// compute aerodynamic resistance r_a under various conditions:
// RA() simple 'standard' model for neutral conditions
// RAA() aerodynamic resistance from mean source to reference height without
// stability correction
// RASTAB(): 'big-leaf' r_a under neutral or unstable conditions:
// from Brutsaert (1982), cited by Zhang et al. (1995)
// RAASTAB_1(): 'sparse-canopy' r_aa based on Mahrt & Ek (1984) for neutral or
// unstable conditions, cited in Seen et al. (1997)
// RAASTAB_2(): 'sparse-canopy' r_aa based on Dolman & Wallace (1993) using the
// Businger-Dyer formulations as stability correction functions

// RA() simple 'standard' model for neutral conditions

int RA(double z_ref, double u, double h, double &rr_a)
{
  // z0: roughness length, d: zero plane displacement
  // z0=0.1*h (rule of thumb) and d=0.67*h
  double z0,d;
  // u: measured windspeed
  // von Karmans constant & reference height
  const double kappa=0.41;

  if (h==0.0)
    {
      z0=0.005; // between 0.01 and 0.001 (Oke, p.57)
      d=0.0;  // no crop
    } else {
      z0=0.1*h; // rule of thumb
      d=0.67*h;
    }
  rr_a=pow(log((z_ref-d)/z0),2)/(u*pow(kappa,2));

  return 0;
}

// aerodynamic resistance from mean source to reference height without
// stability correction
int RAA (double z_ref, double u, double h, double LAI, double ndif, double c_d, double z_0s,
         double z0_def, double &rd, double &rz0, double &ru_f, double &rr_aa)
{
  double d_p,Z0_p,Kh,X; // _p = 'preferred' in SG
  const double kappa=0.41;  // von Karmans constant
  // const double ndif=2.5: eddy diffusivity decay constant in crop (SG,p.505)
  // const double c_d=0.05: Daamen, p.211 used 0.2
  // const double z_0s=0.01; z_0* for bare soil SW p.847

  if (h==0.0 || LAI==0.0)
    {
      rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
      rd=0.0;
      rr_aa=pow(log((z_ref-rd)/rz0),2)/(u*pow(kappa,2)); // like in RA(u,h)
    } else {  // else_2
      Z0_p=0.13*h; // 'preferred value of z0' SG p.506
      d_p=0.63*h; // 'preferred value of d'
      X=c_d*LAI;
      rd=1.1*h*log(1+pow(X,0.25)); // Shaw & pereira (1982)
      // SG eq.43a and Daamen eq.16
      if (X>0.0 && X<0.2) rz0=z_0s+0.3*h*sqrt(X);
      else if (X>0.2 && X<1.5) rz0=0.3*h*(1-rd/h); // SG eq. 43b
      else rz0=0.13*h; // otherwise..
      // SG (1990) formulation
      ru_f=kappa*u/log(fabs((z_ref-rd)/rz0)); // friction velocity
      Kh=kappa*ru_f*(h-rd);

      rr_aa=(1/(kappa*ru_f))*log((z_ref-rd)/(h-rd))+(h/(ndif*Kh))*
        (exp(ndif*(1-(Z0_p+d_p)/h))-1); // SG eq.46

    } // end else_2


  return 0;
}

// returns r_a for EBAL_DT()
int RASTAB (double z_ref, double u, double tair, double tsurf_prev, double h, double LAI,
            double c_d, double z_0s, double z0_def, double &rd, double &rz0,
            double &ry, double &rL_s, double &rR_i_s, double &rr_astab)
{
  double psi_m,psi_h; // stability correction functions
  const double kappa=0.41;  // von Karmans constant
  const double g=9.81; // gravitational accelleration

  // ndif=2.5: eddy diffusivity decay constant in crop (SG,p.505)
  // c_d=0.05: Daamen, p.211 used 0.2
  // z_0s=0.01; z_0 for bare soil SW p.847
  if (h==0.0 || LAI==0.0)
    {
      rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
      rd=0.0;
      rr_astab=pow(log((z_ref-rd)/rz0),2)/(u*pow(kappa,2)); //like in RA(u,h)
    } else {  // else_1
      rd=1.1*h*log(1+pow(c_d*LAI,0.25)); // Shaw & pereira (1982)
      // SG eq.43a and Daamen eq.16
      if (c_d*LAI>0.0 && c_d*LAI<0.2) rz0=z_0s+0.3*h*sqrt(c_d*LAI);
      // SG eq. 43b
      else if (c_d*LAI>0.2 && c_d*LAI<1.5) rz0=0.3*h*(1-rd/h);
      else rz0=0.13*h; // otherwise..
      // Check for thermal stability, i.e. stable Ri > 0 and unstable Ri < 0
      // Air temperature must not be 0 (no division by 0 in rR_i_s)
      if (tair == 0.0) tair=tair+0.1;
      rR_i_s=g*(tair-tsurf_prev)*(z_ref-rd)/(tair*pow(u,2.0));
      if (rR_i_s < 0.0)  // unstable
        {
          // x in Businger-Dyer equations, e.g. Zhang et al.(1995),p.247
          ry=pow(1.0-16.0*rR_i_s,0.25);
          //                cout << "UNSTABLE ! \n";
          // calculate Businger-Dyer correction functions psi_m and psi_h
          psi_m=2*log((1.0+ry)/2)+log((1.0+pow(ry,2))/2)-
            2*atan(ry)+3.14/2.0;
          psi_h=2*log((1.0+pow(ry,2))/2);
          // calculate Obukhov stability length (Zhang et al.,1993 p.247)
          rL_s=(z_ref-rd)/rR_i_s;
        } else  {  // stable
          //                cout << "STABLE ! \n";
          psi_m=-5.0*rR_i_s/(1.0-5.0*rR_i_s);
          psi_h=-5.0*rR_i_s/(1.0-5.0*rR_i_s);
          ry=-9999; // no value when R_i > 0
          if (tair != tsurf_prev)
            {  // Zhang p.247
              rL_s=(1.0-5*rR_i_s)*(rd-z_ref)/rR_i_s;
            } else rL_s=-9999;
        }
      // Finally the aerodynamic resistance is calculated, Brutsaert (1982), cited
      // in Zhang et al. (1995), p.247 and z0h=z0m
      rr_astab=(log((z_ref-rd)/rz0)-psi_h)*(log((z_ref-rd)/rz0)-psi_m)/
        (kappa*kappa*u);
      // cout << "r_a is:\t" << rr_astab << "\n";
    } // end else_1

  return 0;
}

// From SG(1990): Aerodyn. resistance between canopy source heigth and ref.level
// Mahrt & Ek (1984), cited by Seen et al. (1997)
int RAASTAB_1 (double z_ref, double u, double tair, double tcan_prev, double h,
               double LAI, double c_d, double z_0s, double z0_def, double &rd,
               double &rz0, double &rC, double &rC_q, double &rR_i, 
	       double &rr_aa, double &ru_f)
{
  const double kappa=0.41;  // von Karmans constant
  const double g=9.81; // gravitation constant

  // Stability corrected aerodynamic resistance following Mahrt & Ek (1984)

#define Z_REF z_ref // SH BUG: These were 2.0, I believe they should be z_ref
  if (h==0.0 || LAI==0.0)
    {
      rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
      rd=0.0;
      ru_f=kappa*u/log((Z_REF-rd)/rz0);
    }
  else
    { // else 1
      rd=1.1*h*log(1+pow(c_d*LAI,0.25)); // Shaw & pereira (1982)
      // SG eq.43a and Daamen eq.16
      if (c_d*LAI>0.0 && c_d*LAI<0.2) 
	{
	  rz0=z_0s+0.3*h*sqrt(c_d*LAI);
	  ru_f=kappa*u/log((Z_REF-rd)/rz0);
	}
      // SG eq. 43b
      else if (c_d*LAI>0.2 && c_d*LAI<1.5) {
        rz0=0.3*h*(1-rd/h);
        ru_f=kappa*u/log((Z_REF-rd)/rz0);
      }
      else
	{  // otherwise...
	  rz0=0.13*h;
	  ru_f=kappa*u/log((Z_REF-rd)/rz0);
	}
      // Calculate Richardson number Ri (Seen et al., 1997)...
      rR_i=g*(tair-tcan_prev)*(z_ref-rd)/(tair*pow(u,2.0));
      // ... and auxiliary variable C
      rC=75.0*pow(kappa,2.0)*sqrt((z_ref-rd+rz0)/rz0)/
        pow(log((z_ref-rd+rz0)/rz0),2.0);
    }
  // Stable case: Ri > 0, unstable case Ri < 0
  if (rR_i < 0.0)  // unstable: Ts > Ta
    {
      rC_q=pow(kappa/(log((z_ref-rd+rz0)/rz0)),2.0)*
        (1.0-15.0*rR_i/(1.0+rC*sqrt(-rR_i)));
    } 
  else  // stable: Ts < Ta
    {
      rC_q=pow(kappa/(log((z_ref-rd+rz0)/rz0)),2.0)*
	(1.0/((1.0+15*rR_i)*sqrt(1.0+5.0*rR_i)));
    }
  rr_aa=1.0/(rC_q*u);

  return 0;
}


// Dolman (1993) and Zhang et al. (1995)
// stressed conditions
int RAASTAB_2 (double z_ref,
	       double u, double tair, double tcan_prev, double h, double LAI,
               double ndif, double c_d, double z_0s, double z0_def, double &rd,
               double &rz0, double &ru_f, double &ry, double &rL, double &rR_i,
               double &rr_aa)
{
  double Kh;
  double psi_m,psi_mm,psi_h,psi_hm; // stability correction functions
  const double kappa=0.41;  // von Karmans constant
  const double g=9.81;
  // ndif=2.5: eddy diffusivity decay constant in crop (SG,p.505)
  // c_d=0.05: Daamen, p.211 used 0.2
  // z_0s=0.01; z_0 for bare soil SW p.847
  if (h==0.0 || LAI==0.0)
    {
      rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
      rd=0.0;
      rr_aa=pow(log((z_ref-rd)/rz0),2)/(u*pow(kappa,2)); // like in RA(u,h)
    } else {  // else_1
      rd=1.1*h*log(1+pow(c_d*LAI,0.25)); // Shaw & pereira (1982)
      // SG eq.43a and Daamen eq.16
      if (c_d*LAI>0.0 && c_d*LAI<0.2) rz0=z_0s+0.3*h*sqrt(c_d*LAI);
      // SG eq. 43b
      else if (c_d*LAI>0.2 && c_d*LAI<1.5) rz0=0.3*h*(1-rd/h);
      else rz0=0.13*h; // otherwise..
      // Calculate friction velocity u_f, SG (1990) / SW (1985) formulation ...
      ru_f=kappa*u/log(fabs((z_ref-rd)/rz0));
      // ... and eddy diffusion coefficient at height h
      Kh=kappa*ru_f*(h-rd);
      // Check for thermal stability, i.e. stable Ri > 0 and unstable Ri < 0
      // Air temperature must not be 0 (no division by 0 in rR_i_s)
      if (tair == 0.0) tair=tair+0.1;
      rR_i=g*(tair-tcan_prev)*(z_ref-rd)/(tair*pow(u,2.0));
      if (rR_i < 0.0)  // unstable
        {
          // x in Businger-Dyer equations, e.g. Zhang et al.(1995),p.247
          ry=pow(1.0-16.0*rR_i,0.25);
          // calculate Businger-Dyer correction functions psi_m and psi_h
          psi_m=2*log((1.0+ry)/2)+log((1.0+pow(ry,2))/2)-
            2*atan(ry)+3.14/2.0;
          psi_h=2*log((1.0+pow(ry,2))/2);
          //                cout << "at Businger-Dyer equations-line 351\n";
          //                cout << "pow argument is:\t" << ry;
          // calculate Obukhov stability length (Zhang et al.,1993 p.247)
          rL=(z_ref-rd)/rR_i;
          psi_mm=psi_m*(z_ref-h)/rL; // psi'_m in Dolman (1993) p.26
          psi_hm=psi_h*(z_ref-h)/rL; // psi'_h in Dolman (1993) p.26
        } else  {  // stable
          //                cout << "at Businger-Dyer equations-line 359\n";
          //                cout << "pow argument is:\t" << (1.0-(16*rR_i/(5*rR_i-1.0)));
          psi_m=-5.0*rR_i/(1.0-5.0*rR_i);
          psi_h=-5.0*rR_i/(1.0-5.0*rR_i);
          ry=-9999; // no value when R_i > 0
          rL=(1.0-5.0*rR_i)*(rd-z_ref)/rR_i; // Zhang p.247
          psi_mm=psi_m*(z_ref-h)/rL; // psi'_m in Dolman p.26
          psi_hm=psi_h*(z_ref-h)/rL; // psi'_h in Dolman p.26
        }
      // Finally the aerodynamic resistance is calculated, Dolman (1993), p.26
      rr_aa=(1/(kappa*ru_f))*log((z_ref-rd)/(h-rd))+psi_mm-psi_hm+
        (h/(ndif*Kh))*(exp(ndif*(1-(rz0+rd)/h))-1); // SG eq.46

    } // end else_1
  return 0;
}

// Dolman (1993) and Zhang et al. (1995)
// unstressed conditions

// leaf boundary-layer resistance between leaves and canopy air space as
// formulated in Daamen (1997) p.212 (r_b) based on Choudhury & Monteith (1988)
// and Jones (1983)
int RAC(double z_ref, 
	double u, double h, double LAI, double c_d, double w, double z_0s,
        double alpha_u, double arac, double &rd, double &rX, double &rz_0,
        double &ru_h, double &rr_ac)
{
  daisy_assert (isfinite (u));
  daisy_assert (u >= 0.0);
  daisy_assert (isnormal (LAI));
  daisy_assert (LAI > 0.0);
  daisy_assert (isnormal (h));
  daisy_assert (h > 0.0);
  daisy_assert (isnormal (arac));

  // ru_h: windspeed at cropheight h, i.e. above crop [m/s]
  // z_0, d roughness and zero displacement height [m]
  // w: average leaf width, from SG(1990), p.512 [m]
  // c_d: used in Shaw & Pereira (1982)
  // r_b leaf boundary layer resistance, Daamen eq.21
  // attenuation coefficient for wind speed u
  // z_0s=0.01: z_0* for substrate SW p.847
  // alpha_u=3.0: alpha_u=3 (Choudhury and Monteith, 1988)
  // constant, CM (1988) used 0.01, calibration is important (Daamen, 1997), p.212
  // arac (a)=0.00662: used by Jones (1983)
  double Z; // Auxiliary variable denoting h-rd

  rX=c_d*LAI; // Shaw & Pereira (1982)
  rd=1.1*h*log(1.0+pow(rX,0.25)); // Shaw&Pereira(1982)SG,p.507
  daisy_assert (rd < z_ref);
  Z=h-rd;
  if (rX>0.0 && rX<0.2)
    rz_0=z_0s+0.3*h*sqrt(rX); // SG eq.43a
  else if (rX>0.2 && rX<1.5) 
    {
      daisy_assert (isnormal (h));
      rz_0=0.3*h*(1-rd/h); // SG eq.43b
    }
  else 
    rz_0=0.13*h; // otherwise
  daisy_assert (isnormal (rz_0));

  if (Z < 2.0*rz_0)
    {
      Z=2.0*rz_0; // h-rd at least twice z_0
      if (z_ref-rd > 0.0)
	ru_h=u*log(Z/rz_0)/log((z_ref-rd)/rz_0);
      else
        {
          rd=0.67*h;
          ru_h=u*log(Z/rz_0)/log((z_ref-rd)/rz_0);
        }
    }
  else
    {
      if (z_ref-rd > 0.0) 
	ru_h=u*log(Z/rz_0)/log((z_ref-rd)/rz_0);
      else
	{
	  rd=0.67*h;
	  ru_h=u*log(Z/rz_0)/log((z_ref-rd)/rz_0);
	}
    }
  daisy_assert (isnormal (ru_h));
  rr_ac=(alpha_u/(2.0*LAI*arac))*sqrt((w/ru_h)*1/(1-exp(-alpha_u/2)));

  return 0;
}

// Resistance between soil surface and canopy air, r_scan: defined by
// CM (1988), neutral conditions are assumed (Daamen, 1997,eq.22 & 23)

int RAS(double z_ref, double u, double h, double LAI, double z_0s, 
	double alpha_k, double c_d, double &rk_h, double &rr_as)
{
  // rk_h and rr_as as in Daamen, eq.22 &23 (r_scan)
  double z_0, d; // roughness and zero displacement height [m]
  double X; // used in Shaw & Pereira (1982)
  // attenuation coefficient for eddy diffusions coefficient k_h (SG, 1990)
  // const double alpha_k=2.0: (Choudhury and Monteith, 1988)
  // const double z_0s=0.01: z_0 substrate SW p.847
  // c_d=0.05: SG p.507
  const double kappa=0.41;  // von Karmans constant
  X=c_d*LAI; // Shaw & Pereira (1982)
  d=1.1*h*log(1+pow(X,0.25)); // Shaw & Pereira (1982)
  if (X>0.0 && X<0.2) z_0=z_0s+0.3*h*sqrt(X); // SG eq.43a
  else if (X>0.2 && X<1.5) z_0=0.3*h*(1-d/h); // SG eq. 43b
  else z_0=0.13*h; // otherwise

  rk_h=kappa*u*(h-d)/log((z_ref-d)/z_0);

  rr_as=h*exp(alpha_k)/(alpha_k*rk_h)*(exp(-alpha_k*z_0s/h)-
                                       exp(-alpha_k*(d+z_0)/h));

  return 0;
}

// mean stomatal resistance r_sc_1 is formulated according to a defined rcmin
// and 4 constraint functions F_i following Jacquemin & Noilhan (1990).
// r_sc_2 is calculated following Verma et al. (1993)
// rcmin could be passed as a parameter through RS measurements

int RSC (double LAI, double tair, double srad, double e_pa, double theta_0_20,
         double esta, double theta_w, double theta_c, double /*rcmin*/,
         double rcmax, double zeta, double f3const, double tref, double spar,
         double tmin, double tmax, double nu_1, double nu_2, double nu_3,
         double crop_ea_w, double crop_ep_w, double rcmin_star,
         double &rfpar, double &rf_1,double &rf_2, double &rf_3, double &rf_4,
         double &rr_sc_1, double &/*rr_tot_1*/,double &rbf_temp, double &rf_temp,
         double &rf_def, double &rf_theta, double &rf1_dolman, double &rr_sc_2,
         double &/*rr_tot_2*/,double &rf_etep, double &rr_sc_3,
         double &/*rr_tot_3*/, double &rr_sc_4,double &/*rr_tot_4*/,
         double &rr_sc_5, double &/*rr_tot_5*/, double &rr_sc_min,
         double &rr_sc_js)
{
  daisy_assert (LAI > 0.0);
  double tairk,def;
  const double a4=700.0; // parameter in f1_dolman (for oats)

  esta=611.0*exp((17.27*tair)/(tair+237.3)); // saturated vapor pressure
  def=0.001*(esta-e_pa); // vapor deficit in kPa
  tairk=tair+273.15; // air temperature in K
  daisy_assert (spar > 0.0);
  rfpar=0.55*2*srad/(spar*LAI); // cpar coefficient in rf_1

  // constraint functions used in Dickinson (1984) / Noilhan et al. (1991)
  daisy_assert (rcmax > 0.0);
  daisy_assert (1+rfpar != 0.0);
  rf_1=(rcmin_star/rcmax+rfpar)/(1+rfpar); // related to solar radiation
  rf_2=1-zeta*(esta-e_pa); // related to vapour pressure deficit
  // tref-tairk > 0
  if (tref-tairk <= 0.0) tref=tairk+1.0;
  daisy_assert (tref > tairk);
  rf_3=1-f3const*pow(tref-tairk,2.0); // related to air temperature
  daisy_assert (theta_c != theta_w);
  rf_4=(theta_0_20-theta_w)/(theta_c-theta_w); // related to soil moisture

  // rf_4 should not be zero
  if (rf_4==0.0) rf_4=0.01;

  // f1 function Dolman (1991) referenced in Dolman (1993)
  if (srad==0.0) srad=srad+1.0;
  daisy_assert (a4 + srad != 0.0);
  daisy_assert (1000.0 + a4 != 0.0);
  rf1_dolman=(srad/(a4+srad))/(1000.0/(1000.0+a4));
  // constraint functions used in Verma et al.(1993)
  daisy_assert (nu_1 != tmin);
  rbf_temp=(tmax-nu_1)/(nu_1-tmin); // used in f_temp
  if (tair==tmin) tair=tmin+0.1;
  daisy_assert (tmax > tair);
  daisy_assert (tmax > nu_1);
  daisy_assert (nu_1 != tmin);
  rf_temp=(tair-tmin)*pow(tmax-tair,rbf_temp)/
    ((nu_1-tmin)*pow(tmax-nu_1,rbf_temp));  // Jarvis (1976)
  daisy_assert (1.0 + nu_2 * def != 0.0);
  rf_def=1.0/(1.0+nu_2*def); // Lohammar (1980)

  // Stewart (1988), Kim & Verma (1991) as referenced in Verma et al. (1993)
  rf_theta=1.0-exp(-nu_3*100.0*theta_0_20);

  // calculate contraint function F4 as canopy_ea/canopy_ep
  daisy_assert (crop_ep_w > 0.0);
  rf_etep=crop_ea_w/crop_ep_w;

  // canopy resistance using Noilhan et al.(1991)...: r_sc_1
  daisy_assert (rf_1 != 0.0);
  daisy_assert (rf_2 != 0.0);
  daisy_assert (rf_3 != 0.0);
  daisy_assert (rf_4 != 0.0);
  rr_sc_1=(rcmin_star/LAI)/(rf_1*rf_2*rf_3*rf_4);

  // ... or using Verma et al. (1993): r_sc_2
  daisy_assert (rf_temp != 0.0);
  daisy_assert (rf_def != 0.0);
  daisy_assert (rf_theta != 0.0);
  rr_sc_2=(rcmin_star/LAI)/(rf_1*rf_temp*rf_def*rf_theta);

  // ... or using f_etep: eact/epotc: r_sc_3
  daisy_assert (rf_etep != 0.0);
  rr_sc_3=(rcmin_star/LAI)/(rf_1*rf_2*rf_3*rf_etep);
  /*
    // combine f_etep with f_temp, f_def and f_theta (and f_1): r_sc_4
    rr_sc_4=(rcmin_LAI/LAI)/(rf1_dolman*rf_temp*rf_def*rf_etep);
  */
  // calculate rcmin with f2=f3=f4=1
  rr_sc_4=(rcmin_star/LAI)/rf_1;
  // replace f_2 by f_def: r_sc_5
  rr_sc_5=rcmin_star/(rf_1*rf_def*rf_3*rf_4); // No dision by LAI
  /*
    // Use Jarvis (1976) & Steward (1988) as referenced in Dolman (1993):
    daisy_assert (rf1_dolman != 0.0);
    rr_sc_js=(rcmin_LAI/LAI)/(rf1_dolman*rf_def*rf_3*rf_4);
  */
  // Use Jarvis (1976) & Steward (1988) as referenced in Dolman (1993):
  // in combination to f_etep
  daisy_assert (rf1_dolman != 0.0);
  rr_sc_js=(rcmin_star/LAI)/(rf1_dolman*rf_def*rf_3*rf_etep);

  // for unstressed canopy resistance r_sc_min is equal to rcmin_LAI
  rr_sc_min=rcmin_star;

  return 0;
}

// Modified *****************************************************************
int RSCSTAR (double LAI, double tair, double srad, double e_pa, double theta_0_20,
             double esta, double theta_w, double theta_c, double /*rcmin*/,
             double rcmax, double /*zeta*/, double f3const, double tref, double spar,
             double tmin, double tmax, double nu_1, double nu_2, double nu_3,
             double crop_ea_w,double crop_ep_w,double canopy_ea,double r_sc,
             double lel, double &rf1_dolman,double &rf_def,double &rf_3,
             double &renv_lai_factor, double &rf_etep, double &rrcmin_star,
             double &rpstress, double &rr_sc_js)
{
  daisy_assert (LAI > 0.0);
  double tairk,def;
  double fpar,f_theta;
#ifdef UNUSED
  //Commented out unused code. -- abraham 2003-02-03
  double rcmin_LAI;
  double f_1, bf_temp, f_2, f_4, f_temp;
#endif

  const double a4=700.0; // parameter in f1_dolman (for oats)

  esta=611.0*exp((17.27*tair)/(tair+237.3)); // saturated vapor pressure
  def=0.001*(esta-e_pa); // vapor deficit in kPa
  tairk=tair+273.15; // air temperature in K
  daisy_assert (spar > 0.0);
  fpar=0.55*2*srad/(spar*LAI); // cpar coefficient in f_1
#ifdef UNUSED
  rcmin_LAI=rcmin;
#endif

  // constraint functions used in Dickinson (1984) / Noilhan et al. (1991)
  daisy_assert (rcmax > 0.0);
  daisy_assert (1+fpar != 0.0);
#ifdef UNUSED
  f_1=(rcmin_LAI/rcmax+fpar)/(1+fpar); // related to solar radiation
  f_2=1-zeta*(esta-e_pa); // related to vapour pressure deficit
#endif
  // tref-tairk > 0
  if (tref-tairk <= 0.0) tref=tairk+1.0;
  daisy_assert (tref > tairk);
  rf_3=1-f3const*pow(tref-tairk,2.0); // related to air temperature
  daisy_assert (theta_c != theta_w);
#ifdef UNUSED
  f_4=(theta_0_20-theta_w)/(theta_c-theta_w); // related to soil moisture
#endif
  // rf_4 should not be zero
  //if (f_4==0.0) f_4=0.01;

  // f1 function Dolman (1991) referenced in Dolman (1993)
  if (srad==0.0) srad=srad+1.0;
  daisy_assert (a4 + srad != 0.0);
  daisy_assert (1000.0 + a4 != 0.0);
  rf1_dolman=(srad/(a4+srad))/(1000.0/(1000.0+a4));
  // constraint functions used in Verma et al.(1993)
  daisy_assert (nu_1 != tmin);
#ifdef UNUSED
  bf_temp=(tmax-nu_1)/(nu_1-tmin); // used in f_temp
#endif
  if (tair==tmin) tair=tmin+0.1;
  daisy_assert (tmax > tair);
  daisy_assert (tmax > nu_1);
  daisy_assert (nu_1 != tmin);
#ifdef UNUSED
  f_temp=(tair-tmin)*pow(tmax-tair,bf_temp)/
    ((nu_1-tmin)*pow(tmax-nu_1,bf_temp));  // Jarvis (1976)
#endif
  daisy_assert (1.0 + nu_2 * def != 0.0);
  rf_def=1.0/(1.0+nu_2*def); // Lohammar (1980)

  // Stewart (1988), Kim & Verma (1991) as referenced in Verma et al. (1993)
  f_theta=1.0-exp(-nu_3*100.0*theta_0_20);

  // calculate r_sc_js with previous 'F4'
  daisy_assert (rf1_dolman != 0.0);
  rr_sc_js=(200.0/LAI/LAI)/(rf1_dolman*rf_def*rf_3*f_theta);

 // calculate contraint function F4 as canopy_ea/canopy_ep
  daisy_assert (crop_ep_w > 0.0);
  rf_etep=crop_ea_w/crop_ep_w;

  // calculate rcmin_star from r_sc_star
  // it follows that no stress means rrcmin_star=r_sc, i.e. LAI*Fi=1.0
  renv_lai_factor=LAI* /*rf1_dolman*rf_def*rf_3*/ rf_etep;
  rrcmin_star=r_sc*renv_lai_factor;

  if (rrcmin_star > r_sc) rrcmin_star=r_sc;
  // calculate stress factor
  daisy_assert (r_sc != 0.0);
  // Assume 2 criteria: lel > 25.0 & abs(crop_ea-lel) > 50.0 (noise reduction)
  // when incepted water evaporates rpstress = 0 (no stress)
  if (canopy_ea > 0.0 || lel < 25.0 || fabs(crop_ea_w-lel) < 50.0) rpstress = 0.0;
  		else rpstress=1.0-min(1.0,rrcmin_star/r_sc);
/*
	rpstress=0.0;
*/
  return 0;
}
// **************************************************************************


// convert vapor pressure from Pa to kg/m^3: 0.001*e_a [Pa]=e_a [kg/m^3]*Rv*T
int EPA2ABS(double e_pa, double tair, double &re_abs)
{   // for air pressure at 2 m height only
  const double Rv=461.5; // J/kg/K, Oke p.63
  re_abs=e_pa/(Rv*(tair+273.15));
  return 0;
}

// convert from absolute vapor pressure in kg/m^3 to Pa
int EABS2PA(double eabs, double tempc, double &re_pa)
{   // for various conversions
  const double Rv=461.5; // J/kg/K, Oke p.63
  re_pa=eabs*Rv*(tempc+273.15);
  return 0;
}

// calculation of net radiation following Brunt (used) and satterlund
int NETRAD(double srad,double e_pa,double tair, double relsun, double b1,
           double b2, double b3, double b4, double albedo, double &rnetlong_brunt,
           double &rnetlong_satt, double &rnetshort, double &rnetrad_brunt,
           double &rnetrad_satt)
{
  // 1 mb = 0.1 kPa = 100 Pa
  const double sigma=5.67E-8; // Stefan Boltzman constant

  rnetshort=(1.0-albedo)*srad;
  rnetlong_brunt=sigma*pow(tair+273.15,4)*(b1-b2*sqrt(e_pa))*
    (b3+b4*relsun);
  rnetrad_brunt=rnetshort - rnetlong_brunt;
  rnetlong_satt=sigma*pow(tair+273.15,4)*
    1.08*(1-exp(-(pow(0.01*e_pa,tair/2016))))*(b3+b4*relsun);
  rnetrad_satt=rnetshort - rnetlong_satt;

  return 0;
}

// calculation of ground heat flux
int GFLUX(double tskin, double kh, double temp_0, double &rgflux)
{
  double z_sz=0.02; // depth from soil surface to first cell

  rgflux = kh*(tskin-temp_0)/z_sz; // positive directed from soil surface

  return 0;
}

// MODIFIED ******************************************************************
// calculation of energy fluxes using SW and SG: stressed conditions

int LEHFLUX(double tair,double tskin,double tcan,double tleaf,double r_aastab1,
            double r_ac,double r_as,double r_sc,double /*r_sc_js*/, double e_c_abs,
            double e_sl_abs, double e_abs,double les,double crop_ea_w,
            double canopy_ea_w,double &rhl,double &rha, double &rhs,double &rlea,
            double &rlel,double &rhclos,double &rleclos, double &rdtcta,
            double &rdtltc,double &rdtstc,double &rdtlta,double &rr_sc)
{
  const double lambda=2450000.0; // L of vaporization at 20 C [J/kg]
  const double rho_a=1.23;
  const double c_p=1010.0;

  rha=rho_a*c_p*(tcan-tair)/r_aastab1; // H: source height - reference
  rhl=rho_a*c_p*(tleaf-tcan)/r_ac; // H: leaf - source height
  rhs=rho_a*c_p*(tskin-tcan)/r_as; // H: surface - source height
  rlel=lambda*(e_sl_abs-e_c_abs)/(r_sc+r_ac); // LE: leaf - source height
  // LE: source height - reference plus interception (canopy_ea_w)
  rlea=lambda*(e_c_abs-e_abs)/r_aastab1 + canopy_ea_w;

  rdtcta=tcan-tair;
  rdtltc=tleaf-tcan;
  rdtstc=tskin-tcan;

  rdtlta=tleaf-tair;

  rhclos=rha-rhl-rhs; // closure for sensible heat fluxes
  rleclos=rlea-rlel-les; // closure for latent heat fluxes

  // calculate r_sc_star from crop_ea_w = lel
  rr_sc=(lambda*(e_sl_abs-e_c_abs)-crop_ea_w*r_ac)/crop_ea_w;

  // constrain max value for rr_sc
  if (rr_sc > 1000.0) rr_sc = 1000.0;

  // rr_sc must be positive and not less than 20 s/m
  if (rr_sc < 50.0) rr_sc = 50.0;

  return 0;
}
//****************************************************************************

// MODIFIED ******************************************************************
// not used
int LEHFLUXSTAR(double tair,double tskin,double tcan,double tleaf,
                double r_aa,double r_ac,double r_as,double r_sc_star,double e_c_abs,
                double e_sl_abs,double e_abs,double les,double &rhl,double &rha,
                double &rhs,double &rlea,double &rlel,double &rhclos,double &rleclos,
                double &rdtcta,double &rdtltc,double &rdtstc,double &rdtlta)
{
  const double lambda=2450000.0; // L of vaporization at 20 C [J/kg]
  const double rho_a=1.23;
  const double c_p=1010.0;

  rha=rho_a*c_p*(tcan-tair)/r_aa; // H: source height - reference
  rhl=rho_a*c_p*(tleaf-tcan)/r_ac; // H: leaf - source height
  rhs=rho_a*c_p*(tskin-tcan)/r_as; // H: surface - source height
  rlea=lambda*(e_c_abs-e_abs)/r_aa; // LE: source height - reference

// TEMPORARILY
	if (rlea < -1000.0) rlea=-9999.0;

  rlel=lambda*(e_sl_abs-e_c_abs)/(r_sc_star+r_ac); // LE: leaf - source height
  rdtcta=tcan-tair;
  rdtltc=tleaf-tcan;
  rdtstc=tskin-tcan;
  rdtlta=tleaf-tair;

  rhclos=rha-rhl-rhs; // closure for sensible heat fluxes
  rleclos=rlea-rlel-les; // closure for latent heat fluxes

  return 0;
}
//****************************************************************************

// calculation of es(tair),des(tair), es(tair)_abs and des(tair)_abs
int VAPOR(double tair, double &resta, double &resta_abs,
          double &rdesta, double &rdesta_abs)
{
  const double lambda=2450000; // L of vaporization at 20 C [J/kg]
  const double R=8.314; // Universal gas constant [J/mol/K]
  const double M=0.018; // molar mass of water [kg/mol]
  double tairk;  // [K]

  tairk=tair+273.15;
  resta=611*exp((17.27*tair)/(tair+237.3));  // [Pa]
  rdesta=resta*4098.2/pow(237.3+tair,2); // [Pa/C]
  resta_abs=(exp(31.3716-6014.79/tairk-0.00792495*tairk))/(1000.0*tairk);
  rdesta_abs=(resta_abs/tairk)*(lambda*M/(R*tairk)-1); //[kg/m^3/K]

  return 0;
}

// calculation of coefficient needed in gaussj matrix solution:
// use of r_sc_x (combination of different constraint functions
int ACOEFF(double tair, double e_abs, double netrad, double LAI, double les,
           double temp_0, double kh, double r_aa, double r_ac, double r_as,
           double r_sc, double alpha_r, double &ra_11, double &ra_12,
           double &ra_13, double &rb_1, double &ra_24, double &ra_25, double &rb_2,
           double &ra_31, double &ra_33, double &ra_34, double &ra_35,
           double &rb_3, double &ra_41, double &ra_42, double &rb_4)
{
  const double lambda=2450000.0; // L of vaporization at 20 C [J/kg]
  const double rho_a=1.23;
  const double c_p=1010.0;
  const double z_sz=0.02;

  daisy_assert (isnormal (r_aa));
  daisy_assert (isnormal (r_ac));
  daisy_assert (isnormal (r_as));
  ra_11=1.0/r_aa+1.0/r_ac+1.0/r_as;
  ra_12=-1.0/r_as;
  ra_13=-1.0/r_ac;
  rb_1=(tair+273.15)/r_aa;
  ra_24=lambda/r_aa+lambda/(r_sc+r_ac);
  ra_25=-lambda/(r_sc+r_ac);
  rb_2=les+e_abs*lambda/r_aa;
  ra_31=-rho_a*c_p/r_ac;
  ra_33=rho_a*c_p/r_ac;
  ra_34=-lambda/(r_sc+r_ac);
  ra_35=lambda/(r_sc+r_ac);
  rb_3=netrad*(1.0-exp(-alpha_r*LAI));
  ra_41=-rho_a*c_p/r_as;
  ra_42=rho_a*c_p/r_as+kh/z_sz;
  rb_4=netrad*exp(-alpha_r*LAI)+(temp_0+273.15)*kh/z_sz-les;

  return 0;
}

#ifdef NRGAUSS
// Gaussj function from Num. rec. **a,**b are matrices, n and m dimensions
void gaussj(float **a, int n, float **b, int m)
{
  int *indxc,*indxr,*ipiv;
  int i,icol,irow,j,k,l,ll;
  float big,dum,pivinv,temp;
  
  icol = irow = -42000000;	// abraham: supress initialization warning.

  indxc=ivector(1,n);
  indxr=ivector(1,n);
  ipiv=ivector(1,n);
  for (j=1;j<=n;j++) ipiv[j]=0;
  for (i=1;i<=n;i++) {
    big=0.0;
    for (j=1;j<=n;j++)
      if (ipiv[j] != 1)
        for (k=1;k<=n;k++) {
          if (ipiv[k] == 0) {
            if (fabs(a[j][k]) >= big) {
              big=fabs(a[j][k]);
              irow=j;
              icol=k;
            }
          } else if (ipiv[k] > 1){
            throw ("gaussj: Singular Matrix-1");
          }
        }
    ++(ipiv[icol]);
    if (irow != icol) {
      for (l=1;l<=n;l++) SWAP(a[irow][l],a[icol][l])
                           for (l=1;l<=m;l++) SWAP(b[irow][l],b[icol][l])
                                                }
    indxr[i]=irow;
    indxc[i]=icol;
    if (a[icol][icol] == 0.0) throw("gaussj: Singular Matrix-2");
    pivinv=1.0/a[icol][icol];
    a[icol][icol]=1.0;
    for (l=1;l<=n;l++) a[icol][l] *= pivinv;
    for (l=1;l<=m;l++) b[icol][l] *= pivinv;
    for (ll=1;ll<=n;ll++)
      if (ll != icol) {
        dum=a[ll][icol];
        a[ll][icol]=0.0;
        for (l=1;l<=n;l++) a[ll][l] -= a[icol][l]*dum;
        for (l=1;l<=m;l++) b[ll][l] -= b[icol][l]*dum;
      }
  }
  for (l=n;l>=1;l--) {
    if (indxr[l] != indxc[l])
      for (k=1;k<=n;k++)
        SWAP(a[k][indxr[l]],a[k][indxc[l]]);
  }
  free_ivector(ipiv,1,n);
  free_ivector(indxr,1,n);
  free_ivector(indxc,1,n);
}
#endif // NRGAUSS

// solving energy balance with dt (=tsurf-tair) from RT_SAFE (Newton-Raphson)
// EBAL_NR( ) is an argument in RT_SAFE (), which solves for dt. When dt is
// calculated, EBAL_PM (re-)calculates the energy fluxes from this EBAL_NR
void EBAL_NR(double dt,double tair,double e_pa,double srad,double relsun,
             double kh,double r_aa,double r_ac,double temp_0,double z_sz, double LAI,
             double &rrsc_pm,double *fdt,double *dfdt)
{
  // f(Ts)=Rn-H-LE-G
  const double albedo = 0.2;

// Brunt parameters
  const double b1 = 0.53;
  const double b2 = 0.0065;
  const double b3 = 0.1;
  const double b4 = 0.9;

  const double sigma=5.67E-8; // Stefan Boltzman constant
  const double rho_a = 1.23; // air density [kg*m**-3]
  const double c_p = 1010; // specific heat of air [J*kg**-1*K**-1]
  const double gamma=66.1; // psychometric constant
  const double k=0.5; // extinction coefficient in expression for Rn

  // rnetrad: return value of Brunt parameterization
  // rdnetrad: first derivative of netrad
  // rNetShortwave: Net shortwave radiation
  // rNetLongwave: Net Longwave according to Brunt
  // rgflux_pm: ground heat flux
  // rdgflux_pm: first derivative of gts
  // rhflux_pm: sensible heat flux (SG, p.500)
  // rdhflux_pm: first derivative of sensible heat flux
  // rleflux_pm: latent heat flux (SG, p.500)
  // rdleflux_pm: first derivative of latent heat flux
  double netshortwave_pm,netlongwave_pm,netrad_pm,dnetrad_pm;
  double gflux_pm,dgflux_pm,hflux_pm,dhflux_pm,leflux_pm,dleflux_pm;
  double esta,desta; // ests = saturated vapour pressure deficit
  double konstant; // soil temperature at depth z_sz

  konstant=rho_a*c_p/gamma;
  esta=611*exp((17.27*tair)/(tair+237.3)); // from nreb.f
  desta=esta*4096.4/pow(237.3+tair,2); // derivative of esta
  netshortwave_pm=(1-albedo)*srad;
  netlongwave_pm=sigma*pow(tair+273.15,4)*(b1-b2*sqrt(e_pa))*
    (b3+b4*relsun);
  netrad_pm=netshortwave_pm - netlongwave_pm;

// Taylor expansion: 4(tair+dt)^3 is app. 4*tair^3*dt+3*tair^2*dt
  dnetrad_pm=4*sigma*pow(tair+273.15,3); //+12*pow(tair,2)*dt;

  // ground heat flux and derivative
  gflux_pm=kh*(tair+dt-temp_0)/z_sz;
  dgflux_pm=kh/z_sz;

// sensible heat flux and derivative
  hflux_pm=rho_a*c_p*dt/(r_aa+r_ac); // SG (eq.3, p.500)
  dhflux_pm=rho_a*c_p/(r_aa+r_ac);

// define rsc_pm in terms of components of the energy balance and dt,
// assumming that Ac = netrad*exp(k*LAI)-Hc (from SG, eq.1), and
// e*(tsurf)=e*(tair+dt)=e*(tair)+dt*[de*(t)/dt]t=tair
  rrsc_pm=konstant*(esta+desta*dt-e_pa)/(netrad_pm*exp(k*LAI)-hflux_pm-
                                         (rho_a*c_p*dt)/(r_aa+r_ac))-r_aa-r_ac;  // SG (eq.4, p.500)

  // latent heat flux and derivative, SG (eq.4,p.500), and using that
  // e*(tsurf)=e*(tair+dt)=e*(tair)+[de*(tair)/dt]*dt:
  leflux_pm=konstant*(esta+desta*dt-e_pa)/(r_aa+r_ac+rrsc_pm);
  dleflux_pm=konstant*desta/(r_aa+r_ac+rrsc_pm);

  *fdt = netrad_pm-gflux_pm-hflux_pm-leflux_pm;
  *dfdt = dnetrad_pm-dgflux_pm-dhflux_pm-dleflux_pm;

}
// solving for dt=tsurf-tair by closure(dt)=netrad-le(dt)-h(dt)-g(dt)
int EBAL_DT(double tair, double e_pa, double netrad_brunt, double esta,
            double desta, double kh, double temp_0, double r_a, double r_ac,
            double r_sc_js,double r_sc_min,double &ra_dt_dry,double &rb_dt,
            double &rc_dt_dry,double &ra_dt_wet, double &rc_dt_wet,
            double &ra_dt_pot,double &rc_dt_pot,double &rdt_dt_dry,
            double &rdt_dt_wet,double &rdt_dt_pot,double &rtsurf_dt_dry,
            double &rtsurf_dt_wet,double &rtsurf_dt_pot,double &rg_dt_dry,
            double &rh_dt_dry,double &rh_dt_wet,double &rh_dt_pot,double &rg_dt_wet,
            double &rg_dt_pot,double &rle_dt_dry,double &rle_dt_wet,
            double &rle_dt_pot,double &rclosure_dt_dry,double &rclosure_dt_wet,
            double &rclosure_dt_pot)
{
  const double rho_a = 1.23; // air density [kg*m**-3]
  const double c_p = 1010; // specific heat of air [J*kg**-1*K**-1]
  const double gamma=66.1; // psychometric constant
  const double z_sz=0.02;
  // cout << "inside EBAL_DT()\n";
  // closure(dt)=netrad-le(dt)-h(dt)-g(dt) is solved for dt (=tsurf-tair)

  // calculate coefficients a_dt,b_dt and c_dt (dry, wet, potential)
  ra_dt_dry=(rho_a*c_p/gamma)*(e_pa-esta)/(r_a+r_ac+r_sc_js); // dry
  ra_dt_wet=(rho_a*c_p/gamma)*(e_pa-esta)/(r_a+r_ac); // wet: r_sc=0
  ra_dt_pot=(rho_a*c_p/gamma)*(e_pa-esta)/(r_a+r_ac+r_sc_min);
  rb_dt=kh*(temp_0-tair)/z_sz;
  rc_dt_dry=-rho_a*c_p*desta/((r_a+r_ac+r_sc_js)*gamma)-rho_a*c_p/
    (r_a+r_ac)-kh/z_sz;
  rc_dt_wet=-rho_a*c_p*desta/((r_a+r_ac)*gamma)-rho_a*c_p/(r_a+r_ac)-
    kh/z_sz;
  rc_dt_pot=-rho_a*c_p*desta/((r_a+r_ac+r_sc_min)*gamma)-rho_a*c_p/
    (r_a+r_ac)-kh/z_sz;
  rdt_dt_dry=(-netrad_brunt-ra_dt_dry-rb_dt)/rc_dt_dry;
  rdt_dt_wet=(-netrad_brunt-ra_dt_wet-rb_dt)/rc_dt_wet;
  rdt_dt_pot=(-netrad_brunt-ra_dt_pot-rb_dt)/rc_dt_pot;

  // calculate energy balance components for 'dry', 'wet' and 'potential'
  // Ground heat fluxes
  rg_dt_dry=kh*(tair+rdt_dt_dry-temp_0)/z_sz;
  rg_dt_wet=kh*(tair+rdt_dt_wet-temp_0)/z_sz;
  rg_dt_pot=kh*(tair+rdt_dt_pot-temp_0)/z_sz;
  // Sensible heat fluxes
  rh_dt_dry=rho_a*c_p*rdt_dt_dry/(r_a+r_ac);
  rh_dt_wet=rho_a*c_p*rdt_dt_wet/(r_a+r_ac);
  rh_dt_pot=rho_a*c_p*rdt_dt_pot/(r_a+r_ac);
  // Latent heat fluxes
  rle_dt_dry=(rho_a*c_p/gamma)*(esta+desta*rdt_dt_dry-e_pa)/
    (r_a+r_ac+r_sc_js);
  rle_dt_wet=(rho_a*c_p/gamma)*(esta+desta*rdt_dt_wet-e_pa)/
    (r_a+r_ac);
  rle_dt_pot=(rho_a*c_p/gamma)*(esta+desta*rdt_dt_pot-e_pa)/
    (r_a+r_ac+r_sc_min);
  // Energy balance closure
  rclosure_dt_dry=netrad_brunt-rg_dt_dry-rh_dt_dry-rle_dt_dry;
  rclosure_dt_wet=netrad_brunt-rg_dt_wet-rh_dt_wet-rle_dt_wet;
  rclosure_dt_pot=netrad_brunt-rg_dt_pot-rh_dt_pot-rle_dt_pot;
  // 'Bulk surface' temperatures
  rtsurf_dt_dry=tair+rdt_dt_dry;
  rtsurf_dt_wet=tair+rdt_dt_wet;
  rtsurf_dt_pot=tair+rdt_dt_pot;

  return 0;
}

// Newton-Raphson method: solves for dt (=tsurf-tair)
double RTSAFE_DT(void (*funcd)(double tsurf,double tair,double e_pa,double srad,
                               double relsun,double kh,double r_aa,double r_ac,double temp_0,
                               double z_sz,double LAI,double &rrsc_pm,double *, double *), double tair,
                 double e_pa,double srad,double relsun,double kh,double r_aa,double r_ac,
                 double temp_0,double z_sz,double LAI,double &rrsc_pm, double x1,
                 double x2, double xacc)

  // Uses a combination of Newton-Raphson and bisection, find the root of a
  // function bracketed between x1 and x2. The root, returned as a function
  // value rtsafe, will be refined until its accuracy is known within +/-
  // xacc. Funcd is a user supplied routine that returns both the function
  // value and the first derivative of the function
{
  int j;
  double df,dx,dxold,f,fh,fl;
  double temp,xh,xl,rts;

  (*funcd)(x1,tair,e_pa,srad,relsun,kh,r_aa,r_ac,temp_0,z_sz,LAI,rrsc_pm,
           &fl,&df);
  (*funcd)(x2,tair,e_pa,srad,relsun,kh,r_aa,r_ac,temp_0,z_sz,LAI,rrsc_pm,
           &fh,&df);
  if((fl>0.0 && fh>0.0)||(fl<0.0 && fh<0.0))
    perror("root must be bracketed in rtsafe");

  if (fl==0.0) return x1;
  if (fh==0.0) return x2;
  if (fl<0.0)  // orient the search so that f(xl)<0
    {
      xl=x1;
      xh=x2;
    } else
      {
        xh=x1;
        xl=x2;
      }
  rts=0.5*(x1+x2); // initialize the guess for root,
  dxold=fabs(x2-x1); // the 'stepsize before last',
  dx=dxold; // and the last step
  (*funcd)(rts,tair,e_pa,srad,relsun,kh,r_aa,r_ac,temp_0,z_sz,LAI,rrsc_pm,
           &f,&df);
  for(j=1;j<=100;j++) // loop over allowed iterations
    {
      if ((((rts-xh)*df-f)*((rts-xl)*df-f) >= 0.0) // bisect if NR out of range
          || (fabs(2.0*f) > fabs(dxold*df))) // or not decreasing fast enough
        {
          dxold=dx;
          dx=0.5*(xh-xl);
          rts=xl+dx;
          if (xl==rts) return rts; // change in root is negligible
        } else  // Newton step acceptable. Take it
          {
            dxold=dx;
            dx=f/df;
            temp=rts;
            rts -= dx;
            if (temp == rts) return rts;
          }
      if (fabs(dx) < xacc) return rts; // convergence criterion
      (*funcd)(rts,tair,e_pa,srad,relsun,kh,r_aa,r_ac,temp_0,z_sz,LAI,
               rrsc_pm,&f,&df);
      // the one new function evaluation per iteration
      if (f < 0.0) // maintain the bracket on the root
        xl=rts;
      else
        xh=rts;
    } // end for
  perror("maximum number of iterations exceeded in rtsafe");

  return 0.0;

} // end rtsafe function

int EBAL_PM(double tair,double tsurf_pm,double srad,double e_pa,double relsun,
            double kh,double temp_0,double z_sz,double r_aa,double r_ac,
            double rsc_pm,double &rnetshortwave_pm,double &rnetlongwave_pm,
            double &rnetrad_pm,double &rdnetrad_pm,double &rgflux_pm,
            double &rdgflux_pm,double &rhflux_pm,double &rdhflux_pm,
            double &rleflux_pm,double &rdleflux_pm,
            const double dt)
{
  const double albedo = 0.2;

  // Brunt parameters
  const double b1 = 0.53;
  const double b2 = 0.0065;
  const double b3 = 0.1;
  const double b4 = 0.9;

  const double sigma=5.67E-8; // Stefan Boltzman constant
  const double rho_a = 1.23; // air density [kg*m**-3]
  const double c_p = 1010; // specific heat of air [J*kg**-1*K**-1]
  const double gamma=66.1; // psychometric constant
  // rnetrad: return value of Brunt parameterization
  // rdnetrad: first derivative of netrad
  // rNetShortwave: Net shortwave radiation
  // rNetLongwave: Net Longwave according to Brunt
  // rgflux_pm: ground heat flux
  // rdgflux_pm: first derivative of gts
  // rhflux_pm: sensible heat flux (SG, p.500)
  // rdhflux_pm: first derivative of sensible heat flux
  // rleflux_pm: latent heat flux (SG, p.500)
  // rdleflux_pm: first derivative of latent heat flux
  double esta,desta; // esta = saturated vapour pressure deficit
  double konstant; // soil temperature at depth z_sz

  konstant=rho_a*c_p/gamma;
  esta=611*exp((17.27*tair)/(tair+237.3)); // from nreb.f
  desta=esta*4096.4/pow(237.3+tair,2); // derivative of esta
  rnetshortwave_pm=(1-albedo)*srad;

  // Brunts formula (from Jansson(1996))
  rnetlongwave_pm=sigma*pow(tsurf_pm+273.15,4)-
    sigma*pow(tair+273.15,4)*(b1-b2*sqrt(e_pa))*(b3+b4*relsun);
  rnetrad_pm=rnetshortwave_pm - rnetlongwave_pm;

  // Taylor expansion: 4(tair+dt)^3 is app. 4*tair^3*dt+3*tair^2*dt
  rdnetrad_pm=4*sigma*pow(tair+273.15,3); //+12*pow(tair,2)*dt;

  // ground heat flux and derivative
  rgflux_pm=kh*(tsurf_pm-temp_0)/z_sz;
  rdgflux_pm=kh/z_sz;

  // sensible heat flux and derivative
  rhflux_pm=rho_a*c_p*dt/(r_aa+r_ac); // SG (eq.3, p.500)
  rdhflux_pm=rho_a*c_p/r_aa;

  // latent heat flux and derivative, SG (eq.4,p.500), and using that
  // e*(tsurf)=e*(tair+dt)=e*(tair)+[de*(tair)/dt]*dt
  rleflux_pm=konstant*(esta+desta*dt-e_pa)/(r_aa+r_ac+rsc_pm);
  rdleflux_pm=konstant*desta/(r_aa+r_ac+rsc_pm);

  return 0;
} // end EBAL_BL()


class SVAT_PMSW : public SVAT
{
  // read meteorological forcings from file

public:

// *******************TEMPORARY*********************
#ifdef USE_FILES
  FILE *fp_rcmin, *fp_rcminsb, *fp_rcminww;
#endif
  double rcmin;
#ifdef USE_FILES
  double rcmin_sb_ndvi,rcmin_sb_savi;
  double rcmin_ww_ndvi,rcmin_ww_savi;
  double pgtime;
  int teller;
#endif

  // **************************************************

// meteorological- and derived variables
  double srad,tair,e_abs,e_pa,u,u_ref,relsun,prec; // metinput
  double relsun_day,relsun_last; // daytime- and last daytime value
  double relsun_cld;
  double esta,esta_abs,desta,desta_abs,ests; // saturated vapor pressures
  double netrad_brunt,netlong_brunt,netrad_satt,netlong_satt,netshort;
  double albedo,b1,b2,b3,b4;

// resistances and related variables r_sc_1 (Noilhan et al. (1991)
// r_sc_2 (Verma et al. (1993): different constraint functions
  double r_a,r_astab,r_aa,r_aastab1,r_aastab2;
  double r_ac,r_as,r_sc_1,r_sc_2;

//in RAA(),RAC(),RAS(),RSC() and others
  double d_a,d_aa,d_ac,z0_a,z0_aa,z0_ac,u_f,uh,X_ac,k_h,alpha_r;
  double y,C,C_q,R_i,L; // in RAASTAB_1/2()
  double y_s,R_i_s,L_s; // in RASTAB()
  double ndif,c_d,z_0s,z0_def,w,alpha_u,arac,alpha_k; // from daisy.par
  double theta_w,theta_c,fpar,f_1,f_2,f_3,f_4,r_tot_1; //Noilhan e.a.(1991)
  double bf_temp,f_temp,f_def,f_theta,r_tot_2; // in Verma et al. (1993)
  double tmin,tmax,nu_1,nu_2,nu_3; // in Verma et al. (1993)
  double f_etep,r_sc_3,r_tot_3; // using f_etep=ea/epot
  double r_sc_4,r_tot_4; // using f_temp, f_def, f_etep (and f_1)
  double r_sc_5,r_tot_5; // replacing f_2 by f_def
  double rcmin_LAI,rcmin_const,rcmax,tref,zeta,f3const,spar; // from daisy.par
  double r_sc_min; // unstressed canopy resistance in RSC()
  double r_sc_js,f1_dolman; // Jarvis & Steward & Dolman (1993) F1
  double r_tot; // as r_tot_x, x=1..5
  double r_sc,r_sc_star;
  double env_lai_factor; // LAI*F_i in RSCSTAR()

  // friction velocities
  double ustar_raa,ustar_raastab1,ustar_raastab2;

  // matrix elements in ACOEFF for stressed conditions
  double a_11,a_12,a_13,a_14,a_15,a_21,a_22,a_23,a_24,a_25;
  double a_31,a_32,a_33,a_34,a_35,a_41,a_42,a_43,a_44,a_45;
  double a_51,a_52,a_53,a_54,a_55,b_1,b_2,b_3,b_4,b_5;
  double tskin,tcan,tleaf,e_c,e_sl; // elements to be solved by gaussj
  double tcan_prev; // tcan from previous timestep
  double tcan_init; // initial value for tcan
  double e_c_abs,e_sl_abs; // in kg/m^3: e_c and e_sl in Pa
  double rcmin_star;
  double pstress; // to be returned from svat_pmsw

// matrix elements in ACOEFF with r_sc_star as argument
  double astar_11,astar_12,astar_13,astar_14,astar_15,astar_21,astar_22;
  double astar_23,astar_24,astar_25,astar_31,astar_32,astar_33,astar_34;
  double astar_35,astar_41,astar_42,astar_43,astar_44,astar_45,astar_51;
  double astar_52,astar_53,astar_54,astar_55,bstar_1,bstar_2,bstar_3;
  double bstar_4,bstar_5;
  double tskin_star,tcan_star,tleaf_star,e_c_star,e_sl_star;
  double tcan_prev_star; // tcan from previous timestep
  double tcan_init_star; // initial value for tcan
  double e_abs_star;
  double e_c_abs_star,e_sl_abs_star; // in kg/m^3: e_c and e_sl in Pa

// energy fluxes etc using the SW/SG sparse crop approach:
  double ha,hl,hs,lea,lel; // sensible and latent heat fluxes in LEHFLUX()
  double hclos,leclos; // ebal closure for H and LE in LEHFLUX()
  double dtcta,dtltc,dtstc,dtlta; //tc-ta, tl-tc, ts-tc, tl-ta
  float **a,**ai,**b,**x; // gausjj()
  int j,k,l,m,n; // as used in driver program for gaussj (xgaussj.c)

  // energy fluxes etc using the SW/SG sparse crop approach:
  double ha_star,hl_star,hs_star,lea_star,lel_star,les_star;
  double gflux,z_sz; // ground heat flux in GFLUX()
  double hclos_star,leclos_star; // ebal closure for H and LE in LEHFLUX()
  double dtcta_star,dtltc_star,dtstc_star,dtlta_star; //tc-ta, tl-tc etc.
  float **astar,**aistar,**bstar,**xstar; // gausjj()

  // variables retrieved from other parts of DAISY (by tick())
  int year,month,day,hour;
  double kh; // thermal conductivity from soil_heat.h
  double theta_0; // soil water content at z(0) from soil_water.h
  double theta_5,theta_10,theta_15,theta_20,theta_25,theta_30,theta_35;
  double theta_40,theta_45;// smc
  double theta_50,theta_60,theta_70,theta_80,theta_90,theta_110,theta_130;
  double theta_0_20,theta_0_50,theta_0_100; // vertically averaged
  double theta_0_20_mm,theta_0_50_mm,theta_0_100_mm;
  double les,les_q; // bare soil evaporation = water flow q upwards
  double les_tmp; // not in use
  double epotc,epots; // PotET from canopy / soil surface
  double evaps,evaps_w;
  double eact; // actual evapotranspiration from tick()
  double epotc_w,epots_w,eact_w; // fluxes from cm/hr to W/m^2
  double pond_ea_w,soil_ea_w,pond_ep_w,canopy_ep_w,canopy_ea_w;// in tick
  double crop_ea_w,crop_ep_w; // actual & potential transpiration (f_etep)
  double lat_s; // latent heat at soil surface , e.g. Nichols eq.7
  double temp_0; // soil temperature T(0) from soil_heat.h
  double LAI; // equals LAI_ in PM_bioclimate.C
  double h; //  MxH in PM_bioclimate.C Converted from cm to m

// variables in PM big leaf approach using Newton-Raphson for finding tsurf
  double netrad_pm,dnetrad_pm,netshortwave_pm,netlongwave_pm,gflux_pm;
  double dgflux_pm,hflux_pm,dhflux_pm,leflux_pm,dleflux_pm,rsc_pm,Ac_pm;
  double *fdt,*dfdt; // pointers to functions used in RT_SAFE
  double dt,tsurf_pm;

// dt1,dt2: boundaries for valid solution within NR iteration, acc is accuracy
  double dt1,dt2,acc;

  // variables for solving dt directly, closure(dt)=netrad-le(dt)-h(dt)-g(dt)
  // in EBAL_dt()
  double a_dt_dry,c_dt_dry,dt_dt_dry,tsurf_dt_dry,le_dt_dry,h_dt_dry;
  double g_dt_dry,closure_dt_dry; // 'dry' components
  double a_dt_wet,c_dt_wet,dt_dt_wet,tsurf_dt_wet,le_dt_wet,h_dt_wet;
  double g_dt_wet,closure_dt_wet; // 'wet' components
  double a_dt_pot,c_dt_pot,dt_dt_pot,tsurf_dt_pot,le_dt_pot,h_dt_pot;
  double g_dt_pot,closure_dt_pot; // 'pot' components
  double b_dt; // same for 'dry', 'wet' and 'pot'
  double tsurf_dt_dry_init; // in RASTAB()

  int n_hr; // auxiliary variable for facilitating PG
  // initial value for rcmin_star


  // Simulation.
  double production_stress () const;
  void tick (const Weather& weather, const Vegetation& crops,
             const Surface& surface, const Soil& soil,
             const SoilHeat& soil_heat, const SoilWater& soil_water,
             const Pet& pet, double canopy_ea, double snow_ea, double pond_ea,
             double soil_ea, double crop_ea, double crop_ep);
  void output (Log& log) const;

  // Create and Destroy.
  SVAT_PMSW (Block& al);
  ~SVAT_PMSW();
}; // end class SVAT_PMSW


// return Lel in mm/hr, i.e. (1/680) 1 W/m^2 = 0.001471 mm/hr
double
SVAT_PMSW::production_stress () const
{ return pstress; }

void
SVAT_PMSW::tick (const Weather& weather, const Vegetation& crops,
                 const Surface& , const Soil& soil, const SoilHeat& soil_heat,
                 const SoilWater& soil_water, const Pet& pet, double canopy_ea ,
                 double snow_ea, double pond_ea, double soil_ea, double crop_ea,
                 double crop_ep)
{
  const double divide_ep = pet.wet () - snow_ea;
  const double canopy_ep = divide_ep * crops.cover ();
  const double pond_ep = divide_ep - canopy_ep;

  LAI =1.0*crops.LAI (); // Leaf Areal Index
  h   =0.01*crops.height (); // max crop height [m]
  const double z_ref = weather.screen_height ();

  if (h > z_ref)
    {
      std::ostringstream tmp;
      tmp << "Current crop height is " << h 
	     << " [m].\nMax for svat model PMSW is screen height ("
	     << z_ref << " [m])";
      throw (string (tmp.str ()));
    }

  //cout << "LAI is\t" << LAI << "\n";

  // READ FROM TEMPORARY RCMIN_WW.DAT OR RCMIN_SB.DAT FILE
#ifdef USE_FILES
  fscanf(fp_rcmin,"%lf%lf%lf%lf%lf\n",
         &pgtime,&rcmin_ww_ndvi,&rcmin_ww_savi,&rcmin_sb_ndvi,&rcmin_sb_savi);
  /*
    // ............WHEAT..................
    // test for missing value and for LAI > 0
    if (LAI > 0.0)
    {
    if (rcmin_ww_ndvi==9999.0) rcmin=200.0/LAI;
    else rcmin=rcmin_ww_ndvi; // for wheat
    fprintf(fp_rcminww,"%lf%10.2lf%10.2lf\n",pgtime,200.0/LAI,rcmin_ww_ndvi);
    } else rcmin=rcmin_ww_ndvi; // or another variable=9999

  */
  // ............BARLEY..................
  // test for missing value and for LAI > 0
  if (LAI > 0.0)
    {
      if (rcmin_sb_ndvi==9999.0) rcmin_star=200.0/LAI;
      else rcmin_star=rcmin_sb_ndvi; // for barley
      fprintf(fp_rcminsb,"%lf%10.2lf%10.2lf\n",pgtime,200.0/LAI,rcmin_sb_ndvi);
    } else rcmin_star=rcmin_sb_ndvi; // or another variable=9999
#else
  if (LAI > 0.0)
    rcmin_star=200.0/LAI;
#endif
  // potential evapotranspiration from surface and canopy, from tick()
  // pot.evap.above crop canopy [cm/hr]
  epotc=0.1*canopy_ep;
  // pot evaporation from surface [cm/hr]
  epots=0.1*pond_ep;
  // surface evaporation
  evaps=0.1*(pond_ea + soil_ea); // soil evaporation  ???? CHECK

  // actual evapotranspiration, from soil and canopy, from tick()
  eact=0.1*soil_ea;

  // convert epotc, epots and eact etc. from cm/hr to W/m^2
  epotc_w=6800.0*epotc;
  epots_w=6800.0*epots;
  eact_w =6800.0*eact;
  evaps_w=6800.0*evaps;
  pond_ea_w=680.0*pond_ea; // 0.1*6800 mm/hr -> cm/hr
  soil_ea_w=680.0*soil_ea;
  pond_ep_w=680.0*pond_ep;
  canopy_ep_w=680.0*canopy_ep;
  canopy_ea_w=680.0*canopy_ea;
  crop_ep_w=680.0*crop_ep;
  crop_ea_w=680.0*crop_ea;

  // no division by 0 in fprintf (fp_etep,..) and in RSC()
  if (crop_ea_w==0.0) crop_ea_w=crop_ea_w+1.0;
  if (crop_ep_w==0.0) crop_ep_w=crop_ep_w+1.0;

  // Check crop development: if either h or LAI (or both) are zero, do nothing
  // otherwise: calculate resistances and then energy balance
  if (LAI > 0.0)
    {
#ifdef USE_FILES
      teller++;
#endif
      // communication with time.C
#if 0
      year = time.year();
      month = time.month();
      day = time.yday();
      hour = time.hour();
#endif
      // communication with weather_hourly.C
      e_pa = weather.vapor_pressure (); // [Pa]
      tair = weather.hourly_air_temperature (); // [C]
      srad = weather.hourly_global_radiation (); // [W/m^2]
      u_ref = weather.wind (); // u_ref from reference plane [m/s]
      daisy_assert (isfinite (u_ref));
      daisy_assert (u_ref >= 0.0);
      relsun_day = weather.hourly_cloudiness ();  // [-]
      prec = 1.10*weather.rain(); // [mm] corrected by 10 %

      // cout << "past met variables\n";
      // use daytime values for relsun, otherwise use the last daytime value
      if (srad > 20.0)
        {
          relsun=relsun_day; // from weather()
        } else relsun=relsun_cld;

      // convert from u_ref_2m at reference plane (h=0.12 m, FAO) to u_2m_ww at
      // winter wheat field: use eq.26 FAO, p.10, and d=0.27*h and z0=0.123*h
      u=0.205*u_ref*log((z_ref-0.67*h)/(0.123*h));
      daisy_assert (isfinite (u));
      daisy_assert (u >= 0.0);
      //  cout << "u and u_ref are:\t" << u << "\t" << u_ref << "\n";

      // cout << "past u\n";
      // windspeed should not be zero
      if (u==0.0) u=0.1;

      // calculate esta,esta_abs,desta,desta_abs from tair
      VAPOR(tair, esta, esta_abs, desta, desta_abs);
      // cout << "past VAPOR()\n";

      // communication with soil_water.h
      // Access simulated soil water content and soil temperature for validation
      theta_0 = soil_water.Theta(0); //  [0,-2.5]
      theta_5 = soil_water.Theta(1); //  [-2.5,-7.5]
      theta_10 = soil_water.Theta(2); //  [-7.5,-12.5]
      theta_15 = soil_water.Theta(3); //  [-12.5,-17.5]
      theta_20 = soil_water.Theta(4); //  [-17.5,-22.5]
      theta_25 = soil_water.Theta(5); //  [-22.5,-27.5]
      theta_30 = soil_water.Theta(6); //  [-27.5,-32.5]
      theta_35 = soil_water.Theta(7); // [-32.5,-37.5]
      theta_40 = soil_water.Theta(8); // [-37.5,-42.5]
      theta_45 = soil_water.Theta(9); // [-42.5,-47.5]
      theta_50 = soil_water.Theta(10); // [-47.5,-52.5]
      theta_60 = soil_water.Theta(11); // [-52.5,-62.5]
      theta_70 = soil_water.Theta(12); // [-62.5,-72.5]
      theta_80 = soil_water.Theta(13); // [-72.5,-85.0]
      theta_90 = soil_water.Theta(14); // [-85.0,-100.0]
      theta_110 = soil_water.Theta(15); // [-100.0,-120.0]
      theta_130 = soil_water.Theta(16); // [-120.0,-140.0]

      // calculate averages for validation with TDR_0-20, TDR_0-50 and TDR_0-100
      theta_0_20 = (theta_0+theta_5+theta_10+theta_15+theta_20)/5.0;
      theta_0_50 = (theta_0+theta_5+theta_10+theta_15+theta_20+theta_25+
                    theta_30+theta_35+theta_40+theta_45+theta_50)/11.0;
      theta_0_100 = (theta_0+theta_5+theta_10+theta_15+theta_20+theta_25+
                     theta_30+theta_35+theta_40+theta_45+theta_50+theta_60+theta_70+
                     theta_80+theta_90)/15.0;

      // convert to mm water
      theta_0_20_mm=200.0*theta_0_20;
      theta_0_50_mm=500.0*theta_0_50;
      theta_0_50_mm=1000.0*theta_0_100;

      // communication with soil_heat.h
      temp_0  =soil_heat.T(0); // at depth z(0) = -1.25 cm [degrees C]

      // communication with soil_water.h
      // water flow positive to soil surface (LEs)
      les_q=6800.0*soil_water.q(0); // in W/m^2
      // cout << "past les_q\n";

      // soil evap. is min[abs(-q0),epots]
      // when water infiltrates, i.e. les_q < 0, soil evaporation equals E_PM_wet
      // convert [cm/hr] to [W/m^2]: 1 cm/hr = 6800 W/m^2
      if (les_q < 0.0) les=evaps_w;
      else if (les_q <= evaps_w) les=les_q;  // [W/m^2]
      else les=evaps_w;  // [W/m^2]

      // communication with soil.h
      kh=soil.heat_conductivity(0, theta_0, soil_water.X_ice (0))
        * 1e-7 * 3600.0 / 100.0; // [erg/cm/h/dg C] -> [W/m/dg C]

      // convert from e_pa to e_abs
      EPA2ABS(e_pa,tair,e_abs);

      // Like RASTAB, but no stability
      RA(z_ref, u,h,r_a);

      // Like RAASTAB(), but without stability
      RAA (z_ref, u,h,LAI,ndif,c_d,z_0s,z0_def,d_aa,z0_aa,ustar_raa,r_aa);

      // r_a for one-layer approach and correcting for thermal instability

      RASTAB (z_ref, 
	      u,tair,tsurf_dt_dry,h,LAI,c_d,z_0s,z0_def,d_a,z0_a,y_s,L_s,
              R_i_s,r_astab);

      // r_aa with stability correction, following Mahrt & Ek (1984)
      // stressed conditions
      RAASTAB_1 (z_ref, u,tcan_prev,tair,h,LAI,c_d,z_0s,z0_def,d_aa,z0_aa,
                 C,C_q,R_i,r_aastab1,ustar_raastab1);

      // r_aa with stability correction, following Dolman (1993)
      // stressed conditions
      RAASTAB_2 (z_ref, u,tair,tcan_prev,h,LAI,ndif,c_d,z_0s,z0_def,d_aa,z0_aa,
                 ustar_raastab2,y,L,R_i,r_aastab2);

      // aerodynamic resistance from canopy to mean source height
      RAC (z_ref, u,h,LAI,c_d,w,z_0s,alpha_u,arac,d_ac,X_ac,z0_ac,uh,r_ac);

      // aerodynamic resistance between soil surface and mean source height
      RAS (z_ref, u,h,LAI,z_0s,alpha_k,c_d,k_h,r_as);

#if 0
      // mean stomatal resistance following Jacquemin & Noilhan (1990) and
      // Verma et al.(1993), return r_sc_js (from f1_dolman, f_def, f_3 and f_etep
      RSC (z_ref, LAI,tair,srad,e_pa,theta_0_20,esta,theta_w,theta_c,rcmin,
           rcmax,
           zeta,f3const,tref,spar,tmin,tmax,nu_1,nu_2,nu_3,crop_ea_w,crop_ep_w,
           rcmin_star,fpar,f_1,f_2,f_3,f_4,r_sc_1,r_tot_1,bf_temp,
           f_temp,f_def,f_theta,f1_dolman,r_sc_2,r_tot_2,f_etep,r_sc_3,r_tot_3,
           r_sc_4,r_tot_4,r_sc_5,r_tot_5,r_sc_min,r_sc_js);

#endif
      // Net radiation by brunt's equation
      NETRAD(srad,e_pa,tair,relsun,b1,b2,b3,b4,albedo,netlong_brunt,
             netlong_satt,netshort,netrad_brunt,netrad_satt);

      // Compute matrix elements for assigning to A matrix and set all others to zero
      // include r_sc_js as defined in RSC()
/*
      ACOEFF(tair,e_abs,netrad_brunt,LAI,les,temp_0,kh,r_aastab2,r_ac,r_as,
             r_sc_js,alpha_r,a_11,a_12,a_13,b_1,a_24,a_25,b_2,a_31,a_33,a_34,a_35,
             b_3,a_41,a_42,b_4);
*/
      // size n of matrix 5 x 5
      // number of solutions m is 1, i.e. 1 solution vector b for each computation
/*
      n=5;
      m=1;

      // Read in matrix elements for stressed conditions...
      a[1][1]=a_11;
      a[1][2]=a_12;
      a[1][3]=a_13;
      a[1][4]=0.0;
      a[1][5]=0.0;
      a[2][1]=0.0;
      a[2][2]=0.0;
      a[2][3]=0.0;
      a[2][4]=a_24;
      a[2][5]=a_25;
      a[3][1]=a_31;
      a[3][2]=0.0;
      a[3][3]=a_33;
      a[3][4]=a_34;
      a[3][5]=a_35;
      a[4][1]=a_41;
      a[4][2]=a_42;
      a[4][3]=0.0;
      a[4][4]=0.0;
      a[4][5]=0.0;
      a[5][1]=0.0;
      a[5][2]=0.0;
      a[5][3]=desta_abs;
      a[5][4]=0.0;
      a[5][5]=-1.0;
      a_53=a[5][3]; // is desta_abs

      // read in b vector (matrix) ...
      b[1][1]=b_1;
      b[2][1]=b_2;
      b[3][1]=b_3;
      b[4][1]=b_4;
      b[5][1]=(tair+273.15)*desta_abs-esta_abs;
      b_5=b[5][1]; // for the control matrix

      for (l=1;l<=n;l++)
        {
          for (k=1;k<=n;k++)
            {
              ai[k][l]=a[k][l]; // save a matrix in ai
            }
          for (k=1;k<=m;k++)
            {
              x[l][k]=b[l][k];  // save b matrix in x
            }
        }  // end for

      // invert matrix a, a_pot and a_wet
      gaussj(ai,n,x,m);

      // write solution vector for x (stressed conditions)
      tcan=x[1][1]-273.15;  // in degrees C
      tskin=x[2][1]-273.15; // in degrees C
      tleaf=x[3][1]-273.15; // in degrees C
      e_c_abs=x[4][1];  // e_c in kg/m^3
      e_sl_abs=x[5][1]; // e_sl in kg/m^3

      tcan_prev=tcan; // save tcan for use in RAASTAB()
*/
      // calculate energy balance for sparse crops
      GFLUX(tskin,kh,temp_0,gflux);  // ground heat flux

/*
      LEHFLUX(tair,tskin,tcan,tleaf,r_aastab2,r_ac,r_as,r_sc_js,e_c_abs,e_sl_abs,
              e_abs,les,crop_ea_w,hl,ha,hs,lea,lel,hclos,leclos,dtcta,dtltc,dtstc,dtlta,
              r_sc_star);

*/
// in RSCSTAR () have r_sc and lel an initial value
      RSCSTAR (LAI,tair,srad,e_pa,theta_0_20,esta,theta_w,theta_c,rcmin,rcmax,
               zeta,f3const,tref,spar,tmin,tmax,nu_1,nu_2,nu_3,crop_ea_w,crop_ep_w,
               canopy_ea,r_sc,lel,f1_dolman,f_def,f_3,env_lai_factor,f_etep,
               rcmin_star,pstress,r_sc_js);

      ACOEFF(tair,e_abs,netrad_brunt,LAI,les,temp_0,kh,r_aastab2,r_ac,r_as,
             r_sc,alpha_r,a_11,a_12,a_13,b_1,a_24,a_25,b_2,a_31,a_33,a_34,a_35,
             b_3,a_41,a_42,b_4);

      // size n of matrix 5 x 5
      // number of solutions m is 1, i.e. 1 solution vector b for each computation

      n=5;
      m=1;

      // Read in matrix elements for stressed conditions...
      a[1][1]=a_11;
      a[1][2]=a_12;
      a[1][3]=a_13;
      a[1][4]=0.0;
      a[1][5]=0.0;
      a[2][1]=0.0;
      a[2][2]=0.0;
      a[2][3]=0.0;
      a[2][4]=a_24;
      a[2][5]=a_25;
      a[3][1]=a_31;
      a[3][2]=0.0;
      a[3][3]=a_33;
      a[3][4]=a_34;
      a[3][5]=a_35;
      a[4][1]=a_41;
      a[4][2]=a_42;
      a[4][3]=0.0;
      a[4][4]=0.0;
      a[4][5]=0.0;
      a[5][1]=0.0;
      a[5][2]=0.0;
      a[5][3]=desta_abs;
      a[5][4]=0.0;
      a[5][5]=-1.0;
      a_53=a[5][3]; // is desta_abs

      // read in b vector (matrix) ...
      b[1][1]=b_1;
      b[2][1]=b_2;
      b[3][1]=b_3;
      b[4][1]=b_4;
      b[5][1]=(tair+273.15)*desta_abs-esta_abs;
      b_5=b[5][1]; // for the control matrix

#ifdef NRGAUSS
      for (l=1;l<=n;l++)
        {
          for (k=1;k<=n;k++)
            {
              ai[k][l]=a[k][l]; // save a matrix in ai
            }
          for (k=1;k<=m;k++)
            {
              x[l][k]=b[l][k];  // save b matrix in x
            }
        }  // end for

      // invert matrix a, a_pot and a_wet
      gaussj(ai,n,x,m);
#else
      GaussJordan equations (n);
      
      std::ostringstream tmp;
      tmp << "Equations\n";
      for (int l = 1; l <= n; l++)
	{
	  for (k = 1; k <= n; k++)
	    {
	      equations.set_entry (l-1, k-1, a[l][k]);
	      if (k > 1)
		tmp << " + ";
	      tmp << a[l][k];
	    }
	  equations.set_value (l-1, b[l][1]);
	  tmp << " = " << b[l][1] << "\n";
	}
      try
	{
	  equations.solve ();
	}
      catch (const char* error)
	{
	  tmp << "Error: " << error;
	  throw string (tmp.str ());
	}
      for (int l = 1; l <= n; l++)
	x[l][1] = equations.result (l-1);
#endif
      // write solution vector for x (stressed conditions)
      tcan=x[1][1]-273.15;  // in degrees C
      tskin=x[2][1]-273.15; // in degrees C
      tleaf=x[3][1]-273.15; // in degrees C
      e_c_abs=x[4][1];  // e_c in kg/m^3
      e_sl_abs=x[5][1]; // e_sl in kg/m^3

      tcan_prev=tcan; // save tcan for use in RAASTAB()

      LEHFLUX(tair,tskin,tcan,tleaf,r_aastab2,r_ac,r_as,r_sc,r_sc_js,e_c_abs,
      		  e_sl_abs,e_abs,les,crop_ea_w,canopy_ea_w,hl,ha,hs,lea,lel,hclos,
              leclos,dtcta,dtltc,dtstc,dtlta,r_sc);

      RSCSTAR (LAI,tair,srad,e_pa,theta_0_20,esta,theta_w,theta_c,rcmin,rcmax,
               zeta,f3const,tref,spar,tmin,tmax,nu_1,nu_2,nu_3,crop_ea_w,crop_ep_w,
               canopy_ea,r_sc,lel,f1_dolman,f_def,f_3,env_lai_factor,f_etep,
               rcmin_star,pstress,r_sc_js);

      // convert vapor pressure from kg/m^3 to Pa
      EABS2PA(e_c_abs,tcan,e_c);  // at canopy temperature
      EABS2PA(e_sl_abs,tleaf,e_sl); // at leaf temperature
#if 0
      // abraham 11-03-2002:  BUG: uninitialised and unused variables.
      EABS2PA(e_c_abs_star,tcan_star,e_c_star);  // at canopy temperature
      EABS2PA(e_sl_abs_star,tleaf_star,e_sl_star); // at leaf temperature
#endif

      // solve directly for dt by closure(dt)=netrad-le(dt)-h(dt)-g(dt)
      // One layer model: use r_a from RASTAB ()
      EBAL_DT(tair,e_pa,netrad_brunt,esta,desta,kh,temp_0,r_astab,r_ac,
              r_sc_js,r_sc_min,a_dt_dry,b_dt,c_dt_dry,a_dt_wet,c_dt_wet,a_dt_pot,
              c_dt_pot,dt_dt_dry,dt_dt_wet,dt_dt_pot,tsurf_dt_dry,tsurf_dt_wet,
              tsurf_dt_pot,g_dt_dry,h_dt_dry,h_dt_wet,h_dt_pot,g_dt_wet,g_dt_pot,
              le_dt_dry,le_dt_wet,le_dt_pot,closure_dt_dry,closure_dt_wet,
              closure_dt_pot);
    } else  // i.e. LAI = 0
      {
        if (n_hr % 10000 == 0) n_hr++;  // dummy variable
      } // end if
} // end tick()

void
SVAT_PMSW::output (Log& log) const
{                                                 // var 1-4: yy-mm-dd-hh
  SVAT::output (log); // log_only var
  output_variable (netrad_brunt, log);    // var 5
  output_variable (netlong_brunt, log);  // var 6
  output_variable (r_a, log);                      // var 7
  output_variable (r_astab, log);              // var 8
  output_variable (r_aa, log);                    // var 9
  output_variable (r_aastab1, log);          // var 10
  output_variable (r_aastab2, log);          // var 11
  output_variable (r_as, log);                    // var 12
  output_variable (r_ac, log);                    // var 13
  output_variable (r_sc_1, log);                // var 14
  output_variable (r_sc_2, log);                // var 15
  output_variable (tskin, log);                  // var 16
  output_variable (tcan, log);                    // var 17
  output_variable (tleaf, log);                  // var 18
  output_variable (e_c_abs, log);              // var 19
  output_variable (e_sl_abs, log);            // var 20
  output_variable (ha, log);                        // var 21
  output_variable (hl, log);                        // var 22
  output_variable (hs, log);                        // var 23
  output_variable (lea, log);                      // var 24
  output_variable (lel, log);                      // var 25
  output_variable (gflux, log);                  // var 26
  output_variable (dtcta, log);                  // var 27
  output_variable (dtltc, log);                  // var 28
  output_variable (dtstc, log);                  // var 29
  output_variable (theta_0_20, log);        // var 30
  output_variable (theta_0_50, log);        // var 31
  output_variable (theta_0_100, log);      // var 32
  output_variable (f_1, log);                      // var 33
  output_variable (f1_dolman, log);          // var 34
  output_variable (f_2, log);                      // var 35
  output_variable (f_3, log);                      // var 36
  output_variable (f_4, log);                      // var 37
  output_variable (f_temp, log);                // var 38
  output_variable (f_def, log);                  // var 39
  output_variable (f_theta, log);              // var 40
  output_variable (f_etep, log);                // var 41
  output_variable (r_sc_js, log);              // var 42
  output_variable (r_sc, log);                    // var 43
  output_variable (rcmin_star, log);        // var 44
  output_variable (pstress, log);              // var 45
  output_variable (ustar_raa, log);          // var 46
  output_variable (ustar_raastab1, log);// var 47
  output_variable (ustar_raastab2, log);// var 48
  output_variable (env_lai_factor, log);// var 49
  output_variable (e_pa, log);                    // var 50
  output_variable (e_abs, log);                  // var 51
  output_variable (tair, log);                    // var 52
  output_variable (srad, log);                    // var 53
  output_variable (u_ref, log);                  // var 54
  output_variable (prec, log);                    // var 55
}

SVAT_PMSW::SVAT_PMSW (Block& al)
  : SVAT (al),
    rcmin (1.0),		// BUG!  Totally bogus.  
    // We use rcmin in RSCSTAR without ever setting it. abraham 11-03-2002.
    albedo (al.number ("albedo")),
    b1 (al.number ("b1")),
    b2 (al.number ("b2")),
    b3 (al.number ("b3")),
    b4 (al.number ("b4")),
    alpha_r (al.number ("alpha_r")),
    ndif (al.number ("ndif")),
    c_d (al.number ("c_d")),
    z_0s (al.number ("z_0s")),
    z0_def (al.number ("z0_def")),
    w (al.number ("w")),
    alpha_u (al.number ("alpha_u")),
    arac (al.number ("arac")),
    alpha_k (al.number ("alpha_k")),
    theta_w (al.number ("theta_w")),
    theta_c (al.number ("theta_c")),
    tmin (al.number ("tmin")),
    tmax (al.number ("tmax")),
    nu_1 (al.number ("nu_1")),
    nu_2 (al.number ("nu_2")),
    nu_3 (al.number ("nu_3")),
    rcmin_const (al.number ("rcmin_const")),
    rcmax (al.number ("rcmax")),
    tref (al.number ("tref")),
    zeta (al.number ("zeta")),
    f3const (al.number ("f3const")),
    spar (al.number ("spar")),
    r_sc_min (0.0),		// BUG: Init by abraham, 2002.
    r_sc (100.0),    // arbitrary (dummy) initial value
    tskin (0.0),		// BUG: Init by abraham, 2002.
    pstress (0.0),		// BUG: Init by abraham, 2002.
    lel (100.0),     // arbitrary (dummy) initial value
    dt1 (al.number ("dt1")),
    dt2 (al.number ("dt2")),
    acc (al.number ("acc"))
{
  // matrix definitions for stressed conditions
  a=matrix(1,NP,1,NP); // a=A in Ax=b
  ai=matrix(1,NP,1,NP); // inverse matrix
  b=matrix(1,NP,1,MP);  // b vector(s) in Ax=b
  x=matrix(1,NP,1,MP);  // aux. matrix

  astar=matrix(1,NP,1,NP); // a=A in Ax=b
  aistar=matrix(1,NP,1,NP); // inverse matrix
  bstar=matrix(1,NP,1,MP);  // b vector(s) in Ax=b
  xstar=matrix(1,NP,1,MP);  // aux. matrix

  n_hr=0;  // counter for output files
  les_tmp=-9999.0; // not in use

  // initialize canopy temperatures (first value)
  tcan_init=10.0; // initial value for tcan stressed
  tsurf_dt_dry_init=10.0;
  tcan_prev=tcan_init;
  tsurf_dt_dry=tsurf_dt_dry_init;

// set relsun_cld for cloudy weather: relsun_cld=0.0
  relsun_cld=0.0;

  // initialize pgtime and teller for rcmin_ww.dat and rcmin_sb.dat
#ifdef USE_FILES
  pgtime=0.0;
  teller=0;
#endif
  // temporary input: rcmin_ww.dat or rcmin_sb.dat

#ifdef USE_FILES
  //.............WHEAT..............
  if ((fp_rcmin=fopen("rcmin_ww.dat", "r"))==NULL)
    {
      printf("cannot open input file\n");
      exit(1);
    }
  // Auxiliary output
  if ((fp_rcminww=fopen("rcminww.dat", "w"))==NULL)
    {
      printf("cannot open output file\n");
      exit(1);
    }

  //.............BARLEY...........
  if ((fp_rcmin=fopen("rcmin_sb.dat", "r"))==NULL)
    {
      printf("cannot open input file\n");
      exit(1);
    }
  // Auxiliary output
  if ((fp_rcminsb=fopen("rcminsb.dat", "w"))==NULL)
    {
      printf("cannot open output file\n");
      exit(1);
    }
#endif


} // end PM_svat() implementation


SVAT_PMSW::~SVAT_PMSW() // destructor
{
#ifdef USE_FILES
  fclose(fp_rcmin);
  fclose(fp_rcminsb);
  fclose(fp_rcminww);
#endif
  free_matrix(x,1,NP,1,MP);
  free_matrix(b,1,NP,1,MP);
  free_matrix(ai,1,NP,1,NP);
  free_matrix(a,1,NP,1,NP);

  free_matrix(xstar,1,NP,1,MP);
  free_matrix(bstar,1,NP,1,MP);
  free_matrix(aistar,1,NP,1,NP);
  free_matrix(astar,1,NP,1,NP);
}

static struct SVAT_PMSWSyntax
{
  static Model& make (Block& al)
  { return *new SVAT_PMSW (al); }
  SVAT_PMSWSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    SVAT::load_syntax (syntax, alist);
    syntax.add ("netrad_brunt", "W/m^2", Syntax::LogOnly,
                "Net radiation by Brunt");
    syntax.add ("netlong_brunt", "W/m^2", Syntax::LogOnly,
                "Net long radiation by Brunt");
    syntax.add ("r_a", "s/m", Syntax::LogOnly,
                "bulk aerodynamic resistance, neutral conditions");
    syntax.add ("r_astab", "s/m", Syntax::LogOnly,
                "bulk aerodynamic resistance, stability corrected");
    syntax.add ("r_aa", "s/m", Syntax::LogOnly,
                "aerodynamic resistance mean source-ref, uncorrected");
    syntax.add ("r_aastab1", "s/m", Syntax::LogOnly,
                "aerodynamic resistance mean source-ref, corrected 'method 1'");
    syntax.add ("r_aastab2", "s/m", Syntax::LogOnly,
                "aerodynamic resistance mean source-ref, corrected,'method 2'");
    syntax.add ("r_as", "s/m", Syntax::LogOnly,
                "aerodynamic resistance from soil to mean source");
    syntax.add ("r_ac", "s/m", Syntax::LogOnly,
                "aerodynamic resistance from leaf to mean source");
    syntax.add ("r_sc_1", "s/m", Syntax::LogOnly,
                "Bulk canopy resistance (Noilhan et al., 1991)");
    syntax.add ("r_sc_2", "s/m", Syntax::LogOnly,
                "Bulk canopy resistance (Verma et al., 1993)");
    syntax.add ("tskin", "dg C", Syntax::LogOnly,
                "soil/skin temperature");
    syntax.add ("tcan", "dg C", Syntax::LogOnly,
                "canopy temperature at mean source");
    syntax.add ("tleaf", "dg C", Syntax::LogOnly,
                "Leaf temperature");
    syntax.add ("e_c_abs", "Pa", Syntax::LogOnly,
                "vapor pressure at mean source height");
    syntax.add ("e_sl_abs", "Pa", Syntax::LogOnly,
                "saturated vapor pressure at leaf surface");
    syntax.add ("ha", "W/m^2", Syntax::LogOnly,
                "Sensible heat flux from source- to screen height");
    syntax.add ("hl", "W/m^2", Syntax::LogOnly,
                "Sensible heat flux from leaf to mean source");
    syntax.add ("hs", "W/m^2", Syntax::LogOnly,
                "Sensible heat flux from soil to mean source");
    syntax.add ("lea", "W/m^2", Syntax::LogOnly,
                "Latent heat flux from source- to screen height");
    syntax.add ("lel", "W/m^2", Syntax::LogOnly,
                "Latent heat flux from leaf to mean source");
    syntax.add ("gflux", "W/m^2", Syntax::LogOnly,
                "Ground heat flux");
    syntax.add ("dtcta", "dg C", Syntax::LogOnly,
                "Temperature gradient between mean source og screen height");
    syntax.add ("dtltc", "dg C", Syntax::LogOnly,
                "Temperature gradient between leaf and mean source");
    syntax.add ("dtstc", "dg C", Syntax::LogOnly,
                "Temperature gradient between soil and mean source");
    syntax.add ("dtcta_star", "dg C", Syntax::LogOnly,
                "corrected temp gradient between mean source og screen height");
    syntax.add ("dtltc_star", "dg C", Syntax::LogOnly,
                "corrected temperature gradient between leaf and mean source");
    syntax.add ("dtstc_star", "dg C", Syntax::LogOnly,
                "corrected temperature gradient between soil and mean source");
    syntax.add ("theta_0_20", "cm^3/cm^3", Syntax::LogOnly,
                "Averaged soil water content in upper 20 cm");
    syntax.add ("theta_0_50", "cm^3/cm^3", Syntax::LogOnly,
                "Averaged soil water content in upper 50 cm");
    syntax.add ("theta_0_100", "cm^3/cm^3", Syntax::LogOnly,
                "Averaged soil water content in upper 100 cm");
    syntax.add ("f_1", Syntax::None (), Syntax::LogOnly,
                "Constraint function (Noilhan) related to solar radiation");
    syntax.add ("f1_dolman", Syntax::None (), Syntax::LogOnly,
                "???");
    syntax.add ("f_2", Syntax::None (), Syntax::LogOnly,
                "Constraint function (Noilhan) related to vapor pressure");
    syntax.add ("f_3", Syntax::None (), Syntax::LogOnly,
                "Constraint function (Noilhan) related to air temperature");
    syntax.add ("f_4", Syntax::None (), Syntax::LogOnly,
                "Constraint function (Noilhan) related to soil water content");
    syntax.add ("f_temp", Syntax::None (), Syntax::LogOnly,
                "Constraint function (Verma) related to air temperature");
    syntax.add ("f_def", Syntax::None (), Syntax::LogOnly,
                "Constraint function (Verma) related to vapor pressure");
    syntax.add ("f_theta", Syntax::None (), Syntax::LogOnly,
                "Constraint function (Steward) related to soil water content");
    syntax.add ("f_etep", Syntax::None (), Syntax::LogOnly,
                "Constraint function defined by crop_ea/crop_ep");
    syntax.add ("r_sc_js", "s/m", Syntax::LogOnly,
                "Bulk canopy resistance: f1_dolman*f_def*f3*f4");
    syntax.add ("r_sc", "s/m", Syntax::LogOnly,
                "Bulk canopy resistance: f1_dolman*f_def*f3*f_etep");
    syntax.add ("rcmin_star", "s/m", Syntax::LogOnly,
                "minimum canopy resistance");
    syntax.add ("pstress", Syntax::None (), Syntax::LogOnly,
                "crop production stress");
    syntax.add ("ustar_raa", Syntax::None (), Syntax::LogOnly,
                "friction velocity from RAA()");
    syntax.add ("ustar_raastab1", Syntax::None (), Syntax::LogOnly,
                "friction velocity from RAASTAB1()");
    syntax.add ("ustar_raastab2", Syntax::None (), Syntax::LogOnly,
                "friction velocity from RAASTAB2()");
    syntax.add ("env_lai_factor", Syntax::None (), Syntax::LogOnly,
                "LAI*F_i");
    syntax.add ("e_pa","Pa", Syntax::LogOnly,
                "vapor pressure at 2 m");
    syntax.add ("e_abs", "kg/m^3", Syntax::LogOnly,
                "absolute vapor pressure");
    syntax.add ("tair", "degr.C", Syntax::LogOnly,
                "air temperature");
    syntax.add ("srad", "W/m^2", Syntax::LogOnly,
                "global radiation");
    syntax.add ("u_ref", "m/s", Syntax::LogOnly,
                "friction velocity from ??");
    syntax.add ("prec", "mm", Syntax::LogOnly,
                "precipitation");
    syntax.add ("albedo", Syntax::None (), Syntax::Const,
                "Bulk albedo");
    alist.add ("albedo", 0.2);
    syntax.add ("b1", Syntax::None (), Syntax::Const,
                "Brunt coefficient 1");
    alist.add ("b1", 0.53);
    syntax.add ("b2", Syntax::None (), Syntax::Const,
                "Brunt coefficient 2");
    alist.add ("b2", 0.0065);
    syntax.add ("b3", Syntax::None (), Syntax::Const,
                "Brunt coefficient 3");
    alist.add ("b3", 0.1);
    syntax.add ("b4", Syntax::None (), Syntax::Const,
                "Brunt coefficient 4");
    alist.add ("b4", 0.9);
    syntax.add ("ndif", Syntax::None (), Syntax::Const,
                "Eddy diffusivity decay constant in crop");
    alist.add ("ndif", 2.5);
    syntax.add ("c_d", Syntax::None (), Syntax::Const,
                "Mean drag coefficient for a leaf");
    alist.add ("c_d", 0.05);
    syntax.add ("z_0s", "m", Syntax::Const,
                "Roughness length for soil surface, SG (1990)");
    alist.add ("z_0s", 0.01);
    syntax.add ("z0_def", "m", Syntax::Const,
                "Roughness length for soil surface, Oke");
    alist.add ("z0_def", 0.005);
    syntax.add ("w", "m", Syntax::Const,
                "average leaf width");
    alist.add ("w", 0.0025);
    syntax.add ("alpha_u", Syntax::None (), Syntax::Const,
                "attenuation coefficient for wind speed");
    alist.add ("alpha_u", 3.0);
    syntax.add ("arac", Syntax::None (), Syntax::Const,
                "leaf boundary layer resistance coefficient");
    alist.add ("arac", 0.00662);
    syntax.add ("alpha_k", Syntax::None (), Syntax::Const,
                "Att. coefficient of eddy diffusivity through sparse canopy");
    alist.add ("alpha_k", 2.0);

    syntax.add ("alpha_r", Syntax::None (), Syntax::Const,
                "Att. coefficient for vegetation in ACOEFF()");
    alist.add ("alpha_r", 0.5);
    syntax.add ("theta_w", "cm^3/cm^3", Syntax::Const,
                "Soil water content at 'wilting point'");
    alist.add ("theta_w", 0.05);
    syntax.add ("theta_c", "cm^3/cm^3", Syntax::Const,
                "Soil water content at 'field capacity'");
    alist.add ("theta_c", 0.25);
    syntax.add ("rcmin_const", "s/m", Syntax::Const,
                "Constant minimum canopy resistance");
    alist.add ("rcmin_const", 30.0);
    syntax.add ("rcmax", Syntax::None (), Syntax::Const,
                "Maximum canopy resistance");
    alist.add ("rcmax", 1000.0);
    syntax.add ("tref", Syntax::None (), Syntax::Const,
                "Reference/optimum temperature in temperature dependent "
                "constraint function");
    alist.add ("tref", 298.0);
    syntax.add ("zeta", Syntax::None (), Syntax::Const,
                "Coefficient in vapor pressure dependent constraint function");
    alist.add ("zeta", 0.0002);
    syntax.add ("f3const", Syntax::None (), Syntax::Const,
                "Coefficient in temperature dependent constraint function");
    alist.add ("f3const", 0.0016);
    syntax.add ("spar", Syntax::None (), Syntax::Const,
                "Reference value of photosynthetically active part of Si");
    alist.add ("spar", 100.0);
    syntax.add ("tmin", "dg C", Syntax::Const,
                "Minimum temperature for canopy conductance");
    alist.add ("tmin", 0.0);
    syntax.add ("tmax", "dg C", Syntax::Const,
                "Maximum temperature for canopy conductance");
    alist.add ("tmax", 55.0);
    syntax.add ("nu_1", Syntax::None (), Syntax::Const,
                "coefficient in Jarvis (1976) constraint function f_temp");
    alist.add ("nu_1", 26.5);
    syntax.add ("nu_2", Syntax::None (), Syntax::Const,
                "coefficient in Lohammar (1980) constraint function f_def");
    alist.add ("nu_2", 0.57);
    syntax.add ("nu_3", Syntax::None (), Syntax::Const,
                "coefficient in Steward (1988) constraint function f_theta");
    alist.add ("nu_3", 0.008);
    syntax.add ("dt1", "dg C", Syntax::Const,
                "lower solution limit in Newton-Raphson method");
    alist.add ("dt1", -5.0);
    syntax.add ("dt2", "dg C", Syntax::Const,
                "upper solution limit in Newton-Raphson method");
    alist.add ("dt2", 5.0);
    syntax.add ("acc", Syntax::None (), Syntax::Const,
                "iteration accuracy in Newton-Raphson method");
    alist.add ("acc", 0.01);
    Librarian::add_type (SVAT::component, "PMSW", alist, syntax, &make);
  }
} SVAT_PMSW_syntax;
