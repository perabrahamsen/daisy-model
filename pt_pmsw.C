// Status: revised October 1999
// important for sensitivity studies: Do not alter !

// description of functions
# include "surface.h"
# include <iostream.h>
# include <math.h>
# include <fstream.h>
# include <stdio.h>
# include <stdlib.h>
# include "weather.h"
# include "time.h"
# include "soil.h"
# include "soil_water.h"
# include "soil_heat.h"
# include "vegetation.h"
# include "pet.h"
# include "pt.h"
# include "log.h"
# define NRANSI // from xgaussj
#define vector _my_vector
# include "nrutil.h"  // from Num Rec
#undef vector
# define SWAP(a,b) {temp=(a);(a)=(b);(b)=temp;} // from gaussj function
# define NP 20 // from xgaussj driver program
# define MP 20 // from xgaussj driver program

// prototypes
int RA(double, double, double&);
int RAA(double, double, double, double, double, double, double, double&,
	double&, double&, double&);
int RASTAB (double, double, double, double, double, double, double, double,
	double&, double&, double&, double&, double&, double&);
int RAASTAB_1 (double, double, double, double, double, double, double, double,
	double&, double&, double&, double&, double&, double&);
int RAASTABPOT_1 (double, double, double, double, double, double, double,
	double, double&, double&, double&, double&, double&, double&);
int RAASTABWET_1 (double, double, double, double, double, double, double,
	double, double&, double&, double&, double&, double&, double&);
int RAASTAB_2 (double, double, double, double, double, double, double, double,
	double, double&, double&, double&, double&, double&, double&, double&);
int RAASTABPOT_2 (double, double, double, double, double, double, double,
	double, double, double&, double&, double&, double&, double&, double&,
        double&);
int RAASTABWET_2 (double, double, double, double, double, double, double,
	double, double, double&, double&, double&, double&, double&, double&,
        double&);
int RAS(double, double, double, double, double, double, double&, double&);
int RAC(double, double, double , double&, double&, double&, double&, double&);
int RSC(double , double, double, double, double, double, double , double,
	double, double, double, double, double, double, double, double,
        double, double, double, double, double, double&, double&, double&,
        double&, double&, double&, double&, double&, double&, double&, double&,
        double&, double&, double&, double&, double&, double&, double&, double&,
        double&, double&, double&, double&);
int EPA2ABS(double, double, double&);
int EABS2PA(double, double, double&);
int NETRAD(double, double, double, double, double , double, double, double,
	double, double&, double&, double&, double&, double&);
int AVENER(double ,double ,double ,double ,double&, double&);
int GFLUX(double, double, double, double&);
int LEHFLUX(double, double, double, double, double, double, double, double,
	double, double, double, double, double&, double&, double&, double&,
   	double&, double&, double&, double&, double&, double&, double&);
int LEHFLUXPOT(double, double, double, double, double, double, double, double,
	double, double, double, double, double&, double&, double&, double&,
        double&, double&, double&, double&, double&, double&, double&);
int LEHFLUXWET(double, double, double, double, double, double, double, double,
	double, double, double, double&, double&, double&, double&, double&,
        double&, double&, double&, double&, double&, double&);
int VAPOR(double, double&, double&, double&, double&);
int ACOEFF(double, double, double, double, double, double, double,
	double, double, double, double, double, double&, double&, double&,
        double&, double&, double&, double&, double&, double&, double&, double&,
        double&, double&, double&, double&);
int ACOEFFPOT(double, double, double, double, double, double, double,
	double, double, double, double, double, double&, double&, double&,
        double&, double&, double&, double&, double&, double&, double&, double&,
        double&, double&, double&, double&);
int ACOEFFWET(double, double, double, double, double, double, double,
	double, double, double, double, double&, double&, double&, double&,
        double&, double&, double&, double&, double&, double&, double&, double&,
        double&, double&, double&);
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
        double&, double&, double&, double&, double&);

// Declaration of functions

// compute aerodynamic resistance r_a under various conditions:
// RA() simple 'standard' model for neutral conditions
// RASTAB(): 'big-leaf' r_a under neutral or unstable conditions:
// from Brutsaert (1982), cited by Zhang et al. (1995)
// RAASTAB_1(): 'sparse-canopy' r_aa based on Mahrt & Ek (1984) for neutral or
// unstable conditions, cited in Seen et al. (1997)
// RAASTAB_2(): 'sparse-canopy' r_aa based on Dolman & Wallace (1993) using the
// Businger-Dyer formulations as stability correction functions

int RA(double u, double h, double &rr_a)
	{
// z0: roughness length, d: zero plane displacement
// z0=0.1*h (rule of thumb) and d=0.67*h
	double z0,d;
// u: measured windspeed
// von Karmans constant & reference height
	const double kappa=0.41;
	const double z_ref=2.0;

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
int RAA (double u, double h, double LAI, double ndif, double c_d, double z_0s,
	double z0_def, double &rd, double &rz0, double &ru_f, double &rr_aa)
	{
   double d_p,Z0_p,Kh,X; // _p = 'preferred' in SG
   const double kappa=0.41;  // von Karmans constant
   const double z_ref=2.0;
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
int RASTAB (double u, double tair, double tsurf_prev, double h, double LAI,
	double c_d, double z_0s, double z0_def, double &rd, double &rz0,
        double &ry, double &rL_s, double &rR_i_s, double &rr_astab)
	{
        double psi_m,psi_h; // stability correction functions
   	const double kappa=0.41;  // von Karmans constant
   	const double z_ref=2.0;
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
int RAASTAB_1 (double u, double tair, double tcan_prev, double h,
	double LAI, double c_d, double z_0s, double z0_def, double &rd,
        double &rz0, double &rC, double &rC_q, double &rR_i, double &rr_a)
	{
   	const double kappa=0.41;  // von Karmans constant
   	const double z_ref=2.0; // screen height
        const double g=9.81; // gravitation constant

// Stability corrected aerodynamic resistance following Mahrt & Ek (1984)

	if (h==0.0 || LAI==0.0)
      	{
         rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
         rd=0.0;
        } else { // else 1
 	       rd=1.1*h*log(1+pow(c_d*LAI,0.25)); // Shaw & pereira (1982)
// SG eq.43a and Daamen eq.16
	       	if (c_d*LAI>0.0 && c_d*LAI<0.2) rz0=z_0s+0.3*h*sqrt(c_d*LAI);
// SG eq. 43b
       		else if (c_d*LAI>0.2 && c_d*LAI<1.5) rz0=0.3*h*(1-rd/h);
    	   	else rz0=0.13*h; // otherwise..
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
                } else  // stable: Ts < Ta
                	{
                        rC_q=pow(kappa/(log((z_ref-rd+rz0)/rz0)),2.0)*
                        (1.0/((1.0+15*rR_i)*sqrt(1.0+5.0*rR_i)));
                        }
        rr_a=1.0/(rC_q*u);

   	return 0;
        }

int RAASTABPOT_1 (double u, double tcan_prev_pot, double tair, double h,
	double LAI, double c_d, double z_0s, double z0_def, double &rd,
        double &rz0, double &rC, double &rC_q_pot, double &rR_i_pot,
        double &rr_aa_pot)
	{
   	const double kappa=0.41;  // von Karmans constant
   	const double z_ref=2.0; // screen height
        const double g=9.81; // gravitation constant

// Stability corrected aerodynamic resistance following Mahrt & Ek (1984)

	if (h==0.0 || LAI==0.0)
      	{
         rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
         rd=0.0;
        } else { // else 1
 	       rd=1.1*h*log(1+pow(c_d*LAI,0.25)); // Shaw & pereira (1982)
// SG eq.43a and Daamen eq.16
	       	if (c_d*LAI>0.0 && c_d*LAI<0.2) rz0=z_0s+0.3*h*sqrt(c_d*LAI);
// SG eq. 43b
       		else if (c_d*LAI>0.2 && c_d*LAI<1.5) rz0=0.3*h*(1-rd/h);
    	   	else rz0=0.13*h; // otherwise..
// Calculate Richardson number Ri (Seen et al., 1997)...
// Air temperature must not be 0 (no division by 0 in rR_i_pot)
	if (tair == 0.0) tair=tair+0.1;
	rR_i_pot=g*(tair-tcan_prev_pot)*(z_ref-rd)/(tair*pow(u,2.0));
// ... and auxiliary variable C
	rC=75.0*pow(kappa,2.0)*sqrt((z_ref-rd+rz0)/rz0)/
        	pow(log((z_ref-rd+rz0)/rz0),2.0);
                }
// Stable case: Ri > 0, unstable case Ri < 0
	if (rR_i_pot < 0.0)
		{
                rC_q_pot=pow(kappa/(log((z_ref-rd+rz0)/rz0)),2.0)*
                (1.0-15*rR_i_pot/(1.0+rC*sqrt(-rR_i_pot)));
                } else
                	{
                        rC_q_pot=pow(kappa/(log((z_ref-rd+rz0)/rz0)),2.0)*
                        (1.0/((1.0+15*rR_i_pot)*sqrt(1.0+5.0*rR_i_pot)));
                        }
        rr_aa_pot=1.0/(rC_q_pot*u);

   	return 0;
        }

int RAASTABWET_1 (double u, double tcan_prev_wet, double tair, double h,
	double LAI, double c_d, double z_0s, double z0_def, double &rd,
        double &rz0, double &rC, double &rC_q_wet, double &rR_i_wet,
        double &rr_aa_wet)
	{
   	const double kappa=0.41;  // von Karmans constant
   	const double z_ref=2.0; // screen height
        const double g=9.81; // gravitation constant

// Stability corrected aerodynamic resistance following Mahrt & Ek (1984)

	if (h==0.0 || LAI==0.0)
      	{
         rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
         rd=0.0;
        } else { // else 1
 	       rd=1.1*h*log(1+pow(c_d*LAI,0.25)); // Shaw & pereira (1982)
// SG eq.43a and Daamen eq.16
	       	if (c_d*LAI>0.0 && c_d*LAI<0.2) rz0=z_0s+0.3*h*sqrt(c_d*LAI);
// SG eq. 43b
       		else if (c_d*LAI>0.2 && c_d*LAI<1.5) rz0=0.3*h*(1-rd/h);
    	   	else rz0=0.13*h; // otherwise..
// Calculate Richardson number Ri (Seen et al., 1997)...
// Air temperature must not be 0 (no division by 0 in rR_i_wet)
	if (tair == 0.0) tair=tair+0.1;
	rR_i_wet=g*(tair-tcan_prev_wet)*(z_ref-rd)/(tair*pow(u,2.0));
// ... and auxiliary variable C
	rC=75.0*pow(kappa,2.0)*sqrt((z_ref-rd+rz0)/rz0)/
        	pow(log((z_ref-rd+rz0)/rz0),2.0);
               }
// Stable case: Ri > 0, unstable case Ri < 0
	if (rR_i_wet < 0.0)
		{
                rC_q_wet=pow(kappa/(log((z_ref-rd+rz0)/rz0)),2.0)*
                (1.0-15*rR_i_wet/(1.0+rC*sqrt(-rR_i_wet)));
                } else
                	{
                        rC_q_wet=pow(kappa/(log((z_ref-rd+rz0)/rz0)),2.0)*
                        (1.0/((1.0+15*rR_i_wet)*sqrt(1.0+5*rR_i_wet)));
                        }
        rr_aa_wet=1.0/(rC_q_wet*u);

   	return 0;
        }

// Dolman (1993) and Zhang et al. (1995)
// stressed conditions
int RAASTAB_2 (double u, double tair, double tcan_prev, double h, double LAI,
	double ndif, double c_d, double z_0s, double z0_def, double &rd,
        double &rz0, double &ru_f, double &ry, double &rL, double &rR_i,
        double &rr_aa_dry)
	{
   	double Kh;
        double psi_m,psi_mm,psi_h,psi_hm; // stability correction functions
   	const double kappa=0.41;  // von Karmans constant
   	const double z_ref=2.0;
        const double g=9.81;
// ndif=2.5: eddy diffusivity decay constant in crop (SG,p.505)
// c_d=0.05: Daamen, p.211 used 0.2
// z_0s=0.01; z_0 for bare soil SW p.847
	if (h==0.0 || LAI==0.0)
      	{
         rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
         rd=0.0;
         rr_aa_dry=pow(log((z_ref-rd)/rz0),2)/(u*pow(kappa,2)); // like in RA(u,h)
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
	rr_aa_dry=(1/(kappa*ru_f))*log((z_ref-rd)/(h-rd))+psi_mm-psi_hm+
        	(h/(ndif*Kh))*(exp(ndif*(1-(rz0+rd)/h))-1); // SG eq.46

                } // end else_1
   	return 0;
   	}

// Dolman (1993) and Zhang et al. (1995)
// unstressed conditions
int RAASTABPOT_2 (double u, double tair, double tcan_prev, double h, double LAI,
	double ndif, double c_d, double z_0s, double z0_def, double &rd,
        double &rz0, double &ru_f, double &ry, double &rL, double &rR_i,
        double &rr_aa_pot)
	{
   	double Kh;
        double psi_m,psi_mm,psi_h,psi_hm; // stability correction functions
   	const double kappa=0.41;  // von Karmans constant
   	const double z_ref=2.0;
        const double g=9.81;
// ndif=2.5: eddy diffusivity decay constant in crop (SG,p.505)
// c_d=0.05: Daamen, p.211 used 0.2
// z_0s=0.01; z_0 for bare soil SW p.847
	if (h==0.0 || LAI==0.0)
      	{
         rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
         rd=0.0;
         rr_aa_pot=pow(log((z_ref-rd)/rz0),2)/(u*pow(kappa,2)); // like in RA(u,h)
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
	rr_aa_pot=(1/(kappa*ru_f))*log((z_ref-rd)/(h-rd))+psi_mm-psi_hm+
        	(h/(ndif*Kh))*(exp(ndif*(1-(rz0+rd)/h))-1); // SG eq.46

                } // end else_1
   	return 0;
   	}

// Dolman (1993) and Zhang et al. (1995)
// wet conditions
int RAASTABWET_2 (double u, double tair, double tcan_prev, double h, double LAI,
	double ndif, double c_d, double z_0s, double z0_def, double &rd,
        double &rz0, double &ru_f, double &ry, double &rL, double &rR_i,
        double &rr_aa_wet)
	{
   	double Kh;
        double psi_m,psi_mm,psi_h,psi_hm; // stability correction functions
   	const double kappa=0.41;  // von Karmans constant
   	const double z_ref=2.0;
        const double g=9.81;
// ndif=2.5: eddy diffusivity decay constant in crop (SG,p.505)
// c_d=0.05: Daamen, p.211 used 0.2
// z_0s=0.01; z_0 for bare soil SW p.847
	if (h==0.0 || LAI==0.0)
      	{
         rz0=z0_def; // between 0.01 and 0.001 (Oke, p.57)
         rd=0.0;
         rr_aa_wet=pow(log((z_ref-rd)/rz0),2)/(u*pow(kappa,2)); // like in RA(u,h)
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
	rr_aa_wet=(1/(kappa*ru_f))*log((z_ref-rd)/(h-rd))+psi_mm-psi_hm+
        	(h/(ndif*Kh))*(exp(ndif*(1-(rz0+rd)/h))-1); // SG eq.46

                } // end else_1
   	return 0;
   	}

// leaf boundary-layer resistance between leaves and canopy air space as
// formulated in Daamen (1997) p.212 (r_b) based on Choudhury & Monteith (1988)
// and Jones (1983)
int RAC(double u, double h, double LAI, double c_d, double w, double z_0s,
	double alpha_u, double arac, double &rd, double &rX, double &rz_0,
	double &ru_h, double &rr_ac)
	{
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
   	const double z_ref=2.0; // [m]
      double Z; // Auxiliary variable denoting h-rd

   	rX=c_d*LAI; // Shaw & Pereira (1982)
   	rd=1.1*h*log(1.0+pow(rX,0.25)); // Shaw&Pereira(1982)SG,p.507
		Z=h-rd;
   	if (rX>0.0 && rX<0.2) rz_0=z_0s+0.3*h*sqrt(rX); // SG eq.43a
		else if (rX>0.2 && rX<1.5) rz_0=0.3*h*(1-rd/h); // SG eq.43b
         		else rz_0=0.13*h; // otherwise

        if (Z < 2.0*rz_0)
        	{
            Z=2.0*rz_0; // h-rd at least twice z_0
      		if (z_ref-rd > 0.0) ru_h=u*log(Z/rz_0)/log((z_ref-rd)/rz_0);
     		   else
           	   {
           	   rd=0.67*h;
           	   ru_h=u*log(Z/rz_0)/log((z_ref-rd)/rz_0);
               }
         } else
         	   {
              	if (z_ref-rd > 0.0) ru_h=u*log(Z/rz_0)/log((z_ref-rd)/rz_0);
     		 	  		else
           	   		{
           	   		rd=0.67*h;
           	   		ru_h=u*log(Z/rz_0)/log((z_ref-rd)/rz_0);
                     }
         		}
         rr_ac=(alpha_u/(2.0*LAI*arac))*sqrt((w/ru_h)*1/(1-exp(-alpha_u/2)));

   	return 0;
   	}

// Resistance between soil surface and canopy air, r_scan: defined by
// CM (1988), neutral conditions are assumed (Daamen, 1997,eq.22 & 23)

int RAS(double u, double h, double LAI, double z_0s, double alpha_k, double c_d,
	double &rk_h, double &rr_as)
	{
// rk_h and rr_as as in Daamen, eq.22 &23 (r_scan)
   	double z_0, d; // roughness and zero displacement height [m]
   	double X; // used in Shaw & Pereira (1982)
// attenuation coefficient for eddy diffusions coefficient k_h (SG, 1990)
// const double alpha_k=2.0: (Choudhury and Monteith, 1988)
// const double z_0s=0.01: z_0 substrate SW p.847
// c_d=0.05: SG p.507
   	const double kappa=0.41;  // von Karmans constant
   	const double z_ref=2.0; // [m]
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
	double esta, double theta_w, double theta_c, double rcmin,
   double rcmax, double zeta, double f3const, double tref, double spar,
	double tmin, double tmax, double nu_1, double nu_2, double nu_3,
   double crop_ea_w, double crop_ep_w, double &rfpar, double &rf_1,
   double &rf_2, double &rf_3, double &rf_4, double &rr_sc_1,
	double &/*rr_tot_1*/,double &rbf_temp, double &rf_temp, double &rf_def,
	double &rf_theta, double &rf1_dolman, double &rr_sc_2, double &/*rr_tot_2*/,
	double &rf_etep, double &rr_sc_3, double &/*rr_tot_3*/, double &rr_sc_4,
	double &/*rr_tot_4*/, double &rr_sc_5, double &/*rr_tot_5*/, double &rr_sc_min,
	double &rr_sc_js)
	{
	  assert (rcmin > 0.0);
	  assert (LAI > 0.0);
   	double tairk,def;
	double rcmin_LAI;
	const double a4=700.0; // parameter in f1_dolman (for oats)

   	esta=611.0*exp((17.27*tair)/(tair+237.3)); // saturated vapor pressure
   	def=0.001*(esta-e_pa); // vapor deficit in kPa
	tairk=tair+273.15; // air temperature in K
	assert (spar > 0.0);
	rfpar=0.55*2*srad/(spar*LAI); // cpar coefficient in rf_1
	rcmin_LAI=rcmin;   // read from file or calculate from 200/LAI

// constraint functions used in Dickinson (1984) / Noilhan et al. (1991)
        assert (rcmax > 0.0);
	assert (1+rfpar != 0.0);
	rf_1=(rcmin_LAI/rcmax+rfpar)/(1+rfpar); // related to solar radiation
   	rf_2=1-zeta*(esta-e_pa); // related to vapour pressure deficit
// tref-tairk > 0
	if (tref-tairk <= 0.0) tref=tairk+1.0;
	assert (tref > tairk);
   	rf_3=1-f3const*pow(tref-tairk,2.0); // related to air temperature
	assert (theta_c != theta_w);
   	rf_4=(theta_0_20-theta_w)/(theta_c-theta_w); // related to soil moisture

// rf_4 should not be zero
	if (rf_4==0.0) rf_4=0.01;

// f1 function Dolman (1991) referenced in Dolman (1993)
	if (srad==0.0) srad=srad+1.0;
	assert (a4 + srad != 0.0);
	assert (1000.0 + a4 != 0.0);
	rf1_dolman=(srad/(a4+srad))/(1000.0/(1000.0+a4));
// constraint functions used in Verma et al.(1993)
	assert (nu_1 != tmin);
   	rbf_temp=(tmax-nu_1)/(nu_1-tmin); // used in f_temp
	if (tair==tmin) tair=tmin+0.1;
	assert (tmax > tair);
	assert (tmax > nu_1);
	assert (nu_1 != tmin);
   	rf_temp=(tair-tmin)*pow(tmax-tair,rbf_temp)/
   	((nu_1-tmin)*pow(tmax-nu_1,rbf_temp));  // Jarvis (1976)
	assert (1.0 + nu_2 * def != 0.0);
	rf_def=1.0/(1.0+nu_2*def); // Lohammar (1980)

// Stewart (1988), Kim & Verma (1991) as referenced in Verma et al. (1993)
	rf_theta=1.0-exp(-nu_3*100.0*theta_0_20);

// calculate contraint function F4 as canopy_ea/canopy_ep
	assert (crop_ep_w > 0.0);
	rf_etep=crop_ea_w/crop_ep_w;

// canopy resistance using Noilhan et al.(1991)...: r_sc_1
	assert (rf_1 != 0.0);
	assert (rf_2 != 0.0);
	assert (rf_3 != 0.0);
	assert (rf_4 != 0.0);
	rr_sc_1=(rcmin_LAI/LAI)/(rf_1*rf_2*rf_3*rf_4);

// ... or using Verma et al. (1993): r_sc_2
	assert (rf_temp != 0.0);
	assert (rf_def != 0.0);
	assert (rf_theta != 0.0);
	rr_sc_2=(rcmin_LAI/LAI)/(rf_1*rf_temp*rf_def*rf_theta);

// ... or using f_etep: eact/epotc: r_sc_3
	assert (rf_etep != 0.0);
	rr_sc_3=(rcmin_LAI/LAI)/(rf_1*rf_2*rf_3*rf_etep);
/*
// combine f_etep with f_temp, f_def and f_theta (and f_1): r_sc_4
	rr_sc_4=(rcmin_LAI/LAI)/(rf1_dolman*rf_temp*rf_def*rf_etep);
*/
// calculate rcmin with f2=f3=f4=1
	rr_sc_4=(rcmin_LAI/LAI)/rf_1;
// replace f_2 by f_def: r_sc_5
	rr_sc_5=rcmin_LAI/(rf_1*rf_def*rf_3*rf_4); // No dision by LAI

// Use Jarvis (1976) & Steward (1988) as referenced in Dolman (1993):
	assert (rf1_dolman != 0.0);
	rr_sc_js=(rcmin_LAI/LAI)/(rf1_dolman*rf_def*rf_3*rf_4);

// for unstressed canopy resistance r_sc_min is equal to rcmin_LAI
	rr_sc_min=rcmin_LAI;

   	return 0;
   	}

// convert vapor pressure from Pa to kg/m**3: 0.001*e_a [Pa]=e_a [kg/m**3]*Rv*T
int EPA2ABS(double e_pa, double tair, double &re_abs)
   	{   // for air pressure at 2 m height only
   	const double Rv=461.5; // J/kg/K, Oke p.63
   	re_abs=e_pa/(Rv*(tair+273.15));
   	return 0;
   	}

// convert from absolute vapor pressure in kg/m**3 to Pa
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

      	rnetshort=(1-albedo)*srad;
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
        double z_sz=0.02; // depth from soil surface to first node

	rgflux = kh*(tskin-temp_0)/z_sz; // positive directed from soil surface

   	return 0;
	}

// calculation of available energy at leaf and soil level (f(LAI))
int AVENER(double netrad_satt,double LAI,double alpha_r,double gflux,
	double &rav_s,double &rav_l)
   	{
   	rav_l=netrad_satt*(1-exp(-alpha_r*LAI));
   	rav_s=netrad_satt*exp(-alpha_r*LAI)-gflux;

	return 0;
   	}

// calculation of energy fluxes using SW and SG: stressed conditions
int LEHFLUX(double tair,double tskin,double tcan,double tleaf,
	double r_aa_dry,double r_ac,double r_as,double r_sc_js,double e_c_abs,
        double e_sl_abs,double e_abs,double les,double &rhl,double &rha,
        double &rhs,double &rlea,double &rlel,double &rhclos,double &rleclos,
        double &rdtcta,double &rdtltc,double &rdtstc,double &rdtlta)
   	{
   	const double lambda=2450000.0; // L of vaporization at 20 C [J/kg]
   	const double rho_a=1.23;
   	const double c_p=1010.0;

   	rha=rho_a*c_p*(tcan-tair)/r_aa_dry; // H: source height - reference
   	rhl=rho_a*c_p*(tleaf-tcan)/r_ac; // H: leaf - source height
   	rhs=rho_a*c_p*(tskin-tcan)/r_as; // H: surface - source height
   	rlea=lambda*(e_c_abs-e_abs)/r_aa_dry; // LE: source height - reference
   	rlel=lambda*(e_sl_abs-e_c_abs)/(r_sc_js+r_ac); // LE: leaf - source height
   	rdtcta=tcan-tair;
   	rdtltc=tleaf-tcan;
   	rdtstc=tskin-tcan;
   	rdtlta=tleaf-tair;

   	rhclos=rha-rhl-rhs; // closure for sensible heat fluxes
   	rleclos=rlea-rlel-les; // closure for latent heat fluxes

   	return 0;
   	}

// calculation of energy fluxes using SW and SG: unstressed conditions
// r_sc = r_scmin
int LEHFLUXPOT(double tair, double tskin_pot, double tcan_pot, double tleaf_pot,
	double r_aa_pot, double r_ac, double r_as, double r_sc_4,
        double e_c_abs_pot, double e_sl_abs_pot, double e_abs, double les,
        double &rhl_pot, double &rha_pot, double &rhs_pot, double &rlea_pot,
        double &rlel_pot, double &rhclos_pot, double &rleclos_pot,
        double &rdtcta_pot, double &rdtltc_pot, double &rdtstc_pot,
        double &rdtlta_pot)
   	{
   	const double lambda=2450000; // L of vaporization at 20 C [J/kg]
   	const double rho_a=1.23;
   	const double c_p=1010.0;

   	rha_pot=rho_a*c_p*(tcan_pot-tair)/r_aa_pot;
   	rhl_pot=rho_a*c_p*(tleaf_pot-tcan_pot)/r_ac;
   	rhs_pot=rho_a*c_p*(tskin_pot-tcan_pot)/r_as;
   	rlea_pot=lambda*(e_c_abs_pot-e_abs)/r_aa_pot;
   	rlel_pot=lambda*(e_sl_abs_pot-e_c_abs_pot)/(r_sc_4+r_ac);
   	rdtcta_pot=tcan_pot-tair;
   	rdtltc_pot=tleaf_pot-tcan_pot;
   	rdtstc_pot=tskin_pot-tcan_pot;
   	rdtlta_pot=tleaf_pot-tair;

   	rhclos_pot=rha_pot-rhl_pot-rhs_pot; // closure for sensible heat fluxes
   	rleclos_pot=rlea_pot-rlel_pot-les; // closure for latent heat fluxes

   	return 0;
   	}

// calculation of energy fluxes using SW and SG: wet conditions (r_sc=0)
int LEHFLUXWET(double tair, double tskin_wet, double tcan_wet, double tleaf_wet,
	double r_aa_wet, double r_ac, double r_as,double e_c_abs_wet,
        double e_sl_abs_wet, double e_abs, double les, double &rhl_wet,
        double &rha_wet, double &rhs_wet, double &rlea_wet, double &rlel_wet,
        double &rhclos_wet, double &rleclos_wet, double &rdtcta_wet,
        double &rdtltc_wet, double &rdtstc_wet, double &rdtlta_wet)
   	{
   	const double lambda=2450000; // L of vaporization at 20 C [J/kg]
   	const double rho_a=1.23;
   	const double c_p=1010.0;

   	rha_wet=rho_a*c_p*(tcan_wet-tair)/r_aa_wet;
   	rhl_wet=rho_a*c_p*(tleaf_wet-tcan_wet)/r_ac;
   	rhs_wet=rho_a*c_p*(tskin_wet-tcan_wet)/r_as;
   	rlea_wet=lambda*(e_c_abs_wet-e_abs)/r_aa_wet;
   	rlel_wet=lambda*(e_sl_abs_wet-e_c_abs_wet)/r_ac;
   	rdtcta_wet=tcan_wet-tair;
   	rdtltc_wet=tleaf_wet-tcan_wet;
   	rdtstc_wet=tskin_wet-tcan_wet;
   	rdtlta_wet=tleaf_wet-tair;

   	rhclos_wet=rha_wet-rhl_wet-rhs_wet; // closure for sensible heat fluxes
   	rleclos_wet=rlea_wet-rlel_wet-les; // closure for latent heat fluxes

   	return 0;
   	}

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
   	rdesta_abs=(resta_abs/tairk)*(lambda*M/(R*tairk)-1); //[kg/m**3/K]

   	return 0;
   	}

// calculation of coefficient needed in gaussj matrix solution:
// use of r_sc_x (combination of different constraint functions
int ACOEFF(double tair, double e_abs, double netrad, double LAI, double les,
	double temp_0, double kh, double r_aa, double r_ac, double r_as,
   	double r_sc_js, double alpha_r, double &ra_11, double &ra_12,
        double &ra_13, double &rb_1, double &ra_24, double &ra_25, double &rb_2,
   	double &ra_31, double &ra_33, double &ra_34, double &ra_35,
        double &rb_3, double &ra_41, double &ra_42, double &rb_4)
	{
   	const double lambda=2450000.0; // L of vaporization at 20 C [J/kg]
   	const double rho_a=1.23;
   	const double c_p=1010.0;
        const double z_sz=0.02;

	ra_11=1.0/r_aa+1.0/r_ac+1.0/r_as;
	ra_12=-1.0/r_as;
   	ra_13=-1.0/r_ac;
   	rb_1=(tair+273.15)/r_aa;
   	ra_24=lambda/r_aa+lambda/(r_sc_js+r_ac);
   	ra_25=-lambda/(r_sc_js+r_ac);
   	rb_2=les+e_abs*lambda/r_aa;
   	ra_31=-rho_a*c_p/r_ac;
   	ra_33=rho_a*c_p/r_ac;
   	ra_34=-lambda/(r_sc_js+r_ac);
   	ra_35=lambda/(r_sc_js+r_ac);
   	rb_3=netrad*(1.0-exp(-alpha_r*LAI));
   	ra_41=-rho_a*c_p/r_as;
   	ra_42=rho_a*c_p/r_as+kh/z_sz;
   	rb_4=netrad*exp(-alpha_r*LAI)+(temp_0+273.15)*kh/z_sz-les;

   	return 0;
   	}

// calculation of coefficient needed in gaussj matrix solution:
// use of r_sc_4 for potential evapotranspiration for DAISY feedback
int ACOEFFPOT(double tair, double e_abs, double netrad, double LAI, double les,
	double temp_0, double kh, double r_aa_pot, double r_ac, double r_as,
   	double r_sc_4, double alpha_r, double &ra_11, double &ra_12,
        double &ra_13, double &rb_1, double &ra_24_pot, double &ra_25_pot,
        double &rb_2, double &ra_31, double &ra_33, double &ra_34_pot,
        double &ra_35_pot, double &rb_3, double &ra_41, double &ra_42,
        double &rb_4)
	{
   	const double lambda=2450000.0; // L of vaporization at 20 C [J/kg]
   	const double rho_a=1.23;
   	const double c_p=1010.0;
        const double z_sz=0.02;

	ra_11=1.0/r_aa_pot+1.0/r_ac+1.0/r_as;
	ra_12=-1.0/r_as;
   	ra_13=-1.0/r_ac;
   	rb_1=(tair+273.15)/r_aa_pot;
   	ra_24_pot=lambda/r_aa_pot+lambda/(r_sc_4+r_ac);
   	ra_25_pot=-lambda/(r_sc_4+r_ac);
   	rb_2=les+e_abs*lambda/r_aa_pot;
   	ra_31=-rho_a*c_p/r_ac;
   	ra_33=rho_a*c_p/r_ac;
   	ra_34_pot=-lambda/(r_sc_4+r_ac);
   	ra_35_pot=lambda/(r_sc_4+r_ac);
   	rb_3=netrad*(1.0-exp(-alpha_r*LAI));
   	ra_41=-rho_a*c_p/r_as;
   	ra_42=rho_a*c_p/r_as+kh/z_sz;
   	rb_4=netrad*exp(-alpha_r*LAI)+(temp_0+273.15)*kh/z_sz-les;

   	return 0;
   	}

// calculation of coefficient needed in gaussj matrix solution:
// use of r_sc_min for 'wet' processes (r_sc=0)
int ACOEFFWET(double tair, double e_abs, double netrad, double LAI, double les,
	double temp_0, double kh, double r_aa_wet, double r_ac, double r_as,
   	double alpha_r, double &ra_11, double &ra_12,double &ra_13,
        double &rb_1, double &ra_24_wet, double &ra_25_wet, double &rb_2,
        double &ra_31, double &ra_33, double &ra_34_wet, double &ra_35_wet,
        double &rb_3, double &ra_41, double &ra_42, double &rb_4)
	{
   	const double lambda=2450000.0; // L of vaporization at 20 C [J/kg]
   	const double rho_a=1.23;
   	const double c_p=1010.0;
        const double z_sz=0.02;

	ra_11=1.0/r_aa_wet+1.0/r_ac+1.0/r_as;
	ra_12=-1.0/r_as;
   	ra_13=-1.0/r_ac;
   	rb_1=(tair+273.15)/r_aa_wet;
   	ra_24_wet=lambda/r_aa_wet+lambda/r_ac;
   	ra_25_wet=-lambda/r_ac;
   	rb_2=les+e_abs*lambda/r_aa_wet;
   	ra_31=-rho_a*c_p/r_ac;
   	ra_33=rho_a*c_p/r_ac;
   	ra_34_wet=-lambda/r_ac;
   	ra_35_wet=lambda/r_ac;
   	rb_3=netrad*(1.0-exp(-alpha_r*LAI));
   	ra_41=-rho_a*c_p/r_as;
   	ra_42=rho_a*c_p/r_as+kh/z_sz;
   	rb_4=netrad*exp(-alpha_r*LAI)+(temp_0+273.15)*kh/z_sz-les;

   	return 0;
   	}

// Gaussj function from Num. rec. **a,**b are matrices, n and m dimensions
void gaussj(float **a, int n, float **b, int m)
	{
	int *indxc,*indxr,*ipiv;
	int i,icol,irow,j,k,l,ll;
	float big,dum,pivinv,temp;

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
                                        nrerror("gaussj: Singular Matrix-1");
                                            }
				}
		++(ipiv[icol]);
		if (irow != icol) {
			for (l=1;l<=n;l++) SWAP(a[irow][l],a[icol][l])
			for (l=1;l<=m;l++) SWAP(b[irow][l],b[icol][l])
		}
		indxr[i]=irow;
		indxc[i]=icol;
		if (a[icol][icol] == 0.0) nrerror("gaussj: Singular Matrix-2");
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
        double &rleflux_pm,double &rdleflux_pm)
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
	double esta,desta; // ests = saturated vapour pressure deficit
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


class PT_PMSW : public PT
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
   	double r_a,r_astab,r_aa,r_aa_dry,r_ac,r_as,r_sc_1,r_sc_2;
        double r_aa_pot,r_aa_wet; // returned from RAASTABPOT() and ..WET()

//in RAA(),RAC(),RAS(),RSC() and others
   	double d_a,d_aa,d_ac,z0_a,z0_aa,z0_ac,u_f,uh,X_ac,k_h,alpha_r;
        double y,C,C_q,R_i,L; // in RAASTAB_1/2()
        double C_q_pot,R_i_pot; // in RAASTABPOT_1()
        double C_q_wet,R_i_wet; // in RAASTABWET_1()
        double y_pot,L_pot; // in RAASTABPOT_2()
        double y_wet,L_wet; // in RAASTABWET_2()
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

// matrix elements in ACOEFF for stressed conditions
   	double a_11,a_12,a_13,a_14,a_15,a_21,a_22,a_23,a_24,a_25;
   	double a_31,a_32,a_33,a_34,a_35,a_41,a_42,a_43,a_44,a_45;
   	double a_51,a_52,a_53,a_54,a_55,b_1,b_2,b_3,b_4,b_5;
   	double av_l,av_s; // available energy at leaf surface and soil surface
   	double tskin,tcan,tleaf,e_c,e_sl; // elements to be solved by gaussj
	double tcan_prev; // tcan from previous timestep
        double tcan_init; // initial value for tcan
   	double e_c_abs,e_sl_abs; // in kg/m**3: e_c and e_sl in Pa

// matrix element and variables for unstressed condition used in ACOEFFPOT()
   	double a_11_pot,a_12_pot,a_13_pot,a_14_pot,a_15_pot,a_21_pot,a_22_pot;
        double a_23_pot,a_24_pot,a_25_pot,a_31_pot,a_32_pot,a_33_pot,a_34_pot;
        double a_35_pot,a_41_pot,a_42_pot,a_43_pot,a_44_pot,a_45_pot,a_51_pot;
        double a_52_pot,a_53_pot,a_54_pot,a_55_pot,b_1_pot,b_2_pot,b_3_pot;
        double b_4_pot,b_5_pot;
   	double tskin_pot,tcan_pot,tleaf_pot,e_c_pot,e_sl_pot;
        double tcan_prev_pot; // tcan_pot from previous timestep
        double tcan_pot_init; // initial value for tcan_pot
   	double e_c_abs_pot,e_sl_abs_pot;

// matrix element and variables for wet condition used in ACOEFFWET()
   	double a_11_wet,a_12_wet,a_13_wet,a_14_wet,a_15_wet,a_21_wet,a_22_wet;
        double a_23_wet,a_24_wet,a_25_wet,a_31_wet,a_32_wet,a_33_wet,a_34_wet;
        double a_35_wet,a_41_wet,a_42_wet,a_43_wet,a_44_wet,a_45_wet,a_51_wet;
        double a_52_wet,a_53_wet,a_54_wet,a_55_wet,b_1_wet,b_2_wet,b_3_wet;
        double b_4_wet,b_5_wet;
   	double tskin_wet,tcan_wet,tleaf_wet,e_c_wet,e_sl_wet;
        double tcan_prev_wet; // tcan_pot from previous timestep
        double tcan_wet_init; // initial value for tcan_pot
   	double e_c_abs_wet,e_sl_abs_wet;

// energy fluxes etc using the SW/SG sparse crop approach:
	double ha,hl,hs,lea,lel; // sensible and latent heat fluxes in LEHFLUX()
	double ha_pot,hl_pot,hs_pot,lea_pot,lel_pot; // idem LEHFLUXPOT()
	double ha_wet,hl_wet,hs_wet,lea_wet,lel_wet; // idem LEHFLUXWET()
	double gflux,z_sz; // ground heat flux in GFLUX()
   	double hclos,leclos; // ebal closure for H and LE in LEHFLUX()
   	double hclos_pot,leclos_pot; // ebal closure for H and LE in LEHFLUXPOT()
   	double hclos_wet,leclos_wet; // ebal closure for H and LE in LEHFLUXWET()
   	double dtcta,dtltc,dtstc,dtlta; //tc-ta, tl-tc, ts-tc, tl-ta
   	double dtcta_pot,dtltc_pot,dtstc_pot,dtlta_pot; // idem for LEHFLUXPOT()
	double dtcta_wet,dtltc_wet,dtstc_wet,dtlta_wet; // idem for LEHFLUXWET()
	float **a,**ai,**um,**b,**x,**t; // gausjj()
   	float **a_pot,**ai_pot,**um_pot,**b_pot,**x_pot,**t_pot; // gausjj()
   	float **a_wet,**ai_wet,**um_wet,**b_wet,**x_wet,**t_wet; // gausjj()
   	int j,k,l,m,n; // as used in driver program for gaussj (xgaussj.c)

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
   	double epotc_w,epots_w,eact_w; // fluxes from cm/hr to W/m**2
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

  PT_PMSW (const AttributeList& al)
     : PT (al)
#if 1
       ,
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
       dt1 (al.number ("dt1")),
       dt2 (al.number ("dt2")),
       acc (al.number ("acc"))
#endif
   	{
// matrix definitions for stressed conditions
	a=matrix(1,NP,1,NP); // a=A in Ax=b
	ai=matrix(1,NP,1,NP); // inverse matrix
	um=matrix(1,NP,1,NP); // unitary matrix
	b=matrix(1,NP,1,MP);  // b vector(s) in Ax=b
	x=matrix(1,NP,1,MP);  // aux. matrix
	t=matrix(1,NP,1,MP);  // aux matrix
// cout << "past matrix_1\n";

// matrix definitions for unstressed conditions
	a_pot=matrix(1,NP,1,NP); // a=A in Ax=b
	ai_pot=matrix(1,NP,1,NP); // inverse matrix
	um_pot=matrix(1,NP,1,NP); // unitary matrix
	b_pot=matrix(1,NP,1,MP);  // b vector(s) in Ax=b
	x_pot=matrix(1,NP,1,MP);  // aux. matrix
	t_pot=matrix(1,NP,1,MP);  // aux matrix
// cout << "past matrix_2\n";

// matrix definitions for wet conditions
	a_wet=matrix(1,NP,1,NP); // a=A in Ax=b
	ai_wet=matrix(1,NP,1,NP); // inverse matrix
	um_wet=matrix(1,NP,1,NP); // unitary matrix
	b_wet=matrix(1,NP,1,MP);  // b vector(s) in Ax=b
	x_wet=matrix(1,NP,1,MP);  // aux. matrix
	t_wet=matrix(1,NP,1,MP);  // aux matrix

      	n_hr=0;  // counter for output files
      	les_tmp=-9999.0; // not in use

// initialize canopy temperatures (first value)
      	tcan_init=10.0; // initial value for tcan stressed
      	tcan_pot_init=10.0; // initial value for tcan unstressed
      	tcan_wet_init=10.0; // initial value for tcan wet
      	tsurf_dt_dry_init=10.0;
      	tcan_prev=tcan_init;
      	tcan_prev_pot=tcan_pot_init;
      	tcan_prev_wet=tcan_wet_init;
      	tsurf_dt_dry=tsurf_dt_dry_init;

// set relsun_cld for cloudy weather: relsun_cld=0.0
	relsun_cld=0.0;

// initialize some parameters in RAASTAB_2 not used when RAASTAB_1() is active
	L_pot=L_wet=0.0;
        y_pot=y_wet=0.0;

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


	double  potential_transpiration_ ;
// return Lel in mm/hr, i.e. (1/680) 1 W/m**2 = 0.001471 mm/hr
  	double potential_transpiration () const
  { return potential_transpiration_ /* 0.001471*lel_pot */; }

void tick (const Weather& weather, const Vegetation& crops,
	const Surface& surface, const Soil& soil, const SoilHeat& soil_heat,
	const SoilWater& soil_water, const Pet& pet, double canopy_ea ,
        double snow_ea, double pond_ea, double soil_ea, double crop_ea,
        double crop_ep)
       	{
      	const double divide_ep = pet.wet () - snow_ea;
      	const double canopy_ep = divide_ep * crops.cover ();
      	const double pond_ep = divide_ep - canopy_ep;

	double potential_crop_transpiration = canopy_ep - canopy_ea;
	double potential_soil_transpiration
	= (pond_ep - pond_ea - soil_ea) * surface.EpInterchange ();
      	potential_transpiration_ =
        	min (max (0.0, potential_crop_transpiration
		   + potential_soil_transpiration),pet.dry ());


	LAI =crops.LAI (); // Leaf Areal Index
    	h   =0.01*crops.height (); // max crop height [m]

cout << "LAI is\t" << LAI << "\n";

cout << "LAI is\t" << LAI << "\n";

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
	if (rcmin_sb_ndvi==9999.0) rcmin=200.0/LAI;
   	else rcmin=rcmin_sb_ndvi; // for barley
	fprintf(fp_rcminsb,"%lf%10.2lf%10.2lf\n",pgtime,200.0/LAI,rcmin_sb_ndvi);
        } else rcmin=rcmin_sb_ndvi; // or another variable=9999
#else
        rcmin=200.0/LAI;
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

// convert epotc, epots and eact etc. from cm/hr to W/m**2
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
        srad = weather.hourly_global_radiation (); // [W/m**2]
        u_ref = weather.wind (); // u_ref from reference plane [m/s]
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
	u=0.205*u_ref*log((2.0-0.67*h)/(0.123*h));
//  cout << "u and u_ref are:\t" << u << "\t" << u_ref << "\n";

// cout << "past u\n";
// windspeed should not be zero
      	if (u==0.0) u=0.1;

// convert e_pa [Pa] to e_abs [kg/m**3], Oke p.63
   	VAPOR(tair, esta, esta_abs, desta, desta_abs);
// cout << "past VAPOR()\n";

// communication with soil_water.h
// Access simulated soil water content and soil temperature for validation
	theta_0	= soil_water.Theta(0); //  [0,-2.5]
	theta_5	= soil_water.Theta(1); //  [-2.5,-7.5]
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
	temp_0	=soil_heat.T(0); // at depth z(0) = -1.25 cm [degrees C]

// communication with soil_water.h
// water flow positive to soil surface (LEs)
        les_q=6800.0*soil_water.q(0); // in W/m**2
// cout << "past les_q\n";

// soil evap. is min[abs(-q0),epots]
// when water infiltrates, i.e. les_q < 0, soil evaporation equals E_PM_wet
// convert [cm/hr] to [W/m**2]: 1 cm/hr = 6800 W/m**2
        if (les_q < 0.0) les=evaps_w;
        		 else if (les_q <= evaps_w) les=les_q;  // [W/m**2]
   			      	else les=evaps_w;  // [W/m**2]

// communication with soil.h
	kh=soil.heat_conductivity(0, theta_0, soil_water.X_ice (0))
	  * 1e-7 * 3600.0 / 100.0; // [erg/cm/h/dg C] -> [W/m/dg C]

// convert from e_pa to e_abs
 	EPA2ABS(e_pa,tair,e_abs);

// Like RASTAB, but no stability
	RA(u,h,r_a);

// Like RAASTAB(), but without stability
	RAA (u,h,LAI,ndif,c_d,z_0s,z0_def,d_aa,z0_aa,u_f,r_aa);
// cout << "past RAA()\n";

// OPTIONS: RASTAB(), RAASTABXXX_1 or RAASTABXXX_2
// r_a for one-layer approach and correcting for thermal instability

  	RASTAB (u,tair,tsurf_dt_dry,h,LAI,c_d,z_0s,z0_def,d_a,z0_a,y_s,L_s,
        R_i_s,r_astab);
// cout << "past RASTAB()\n";
/*
// r_aa with stability correction, following Mahrt & Ek (1984)
// stressed conditions
	RAASTAB_1 (u,tcan_prev,tair,h,LAI,c_d,z_0s,z0_def,d_aa,z0_aa,
        C,C_q,R_i,r_aa_dry);
cout << "past RAASTAB_1()\n";

// r_aa with stability correction, following Mahrt & Ek (1984)
// unstressed conditions
	RAASTABPOT_1 (u,tcan_prev_pot,tair,h,LAI,c_d,z_0s,z0_def,d_aa,z0_aa,
        C,C_q_pot,R_i_pot,r_aa_pot);
cout << "past RAASTABPOT_1()\n";

// r_aa with stability correction, following Mahrt & Ek (1984)
// wet conditions
	RAASTABWET_1 (u,tcan_prev_wet,tair,h,LAI,c_d,z_0s,z0_def,d_aa,z0_aa,
        C,C_q_wet,R_i_wet,r_aa_wet);
cout << "past RAASTABWET_1()\n";
*/
// r_aa with stability correction, following Dolman (1993)
// stressed conditions
	RAASTAB_2 (u,tair,tcan_prev,h,LAI,ndif,c_d,z_0s,z0_def,d_aa,z0_aa,u_f,
        y,L,R_i,r_aa_dry);

// r_aa with stability correction, following Dolman (1993)
// unstressed conditions
	RAASTABPOT_2 (u,tair,tcan_prev_pot,h,LAI,ndif,c_d,z_0s,z0_def,d_aa,
        z0_aa,u_f,y_pot,L_pot,R_i_pot,r_aa_pot);

// r_aa with stability correction, following Dolman (1993)
// wet conditions
	RAASTABWET_2 (u,tair,tcan_prev_wet,h,LAI,ndif,c_d,z_0s,z0_def,d_aa,
        z0_aa,u_f,y_wet,L_wet,R_i_wet,r_aa_wet);

// aerodynamic resistance from canopy to mean source height
	RAC (u,h,LAI,c_d,w,z_0s,alpha_u,arac,d_ac,X_ac,z0_ac,uh,r_ac);
// cout << "past RAC()\n";

// aerodynamic resistance between soil surface and mean source height
	RAS(u,h,LAI,z_0s,alpha_k,c_d,k_h,r_as);
// cout << "past RAS()\n";

// mean stomatal resistance following Jacquemin & Noilhan (1990) and
// Verma et al.(1993)
	RSC (LAI,tair,srad,e_pa,theta_0_20,esta,theta_w,theta_c,rcmin,rcmax,
        zeta,f3const,tref,spar,tmin,tmax,nu_1,nu_2,nu_3,crop_ea_w,crop_ep_w,
        fpar,f_1,f_2,f_3,f_4,r_sc_1,r_tot_1,bf_temp,f_temp,f_def,f_theta,
        f1_dolman,r_sc_2,r_tot_2,f_etep,r_sc_3,r_tot_3,r_sc_4,r_tot_4,r_sc_5,
        r_tot_5,r_sc_min,r_sc_js);

// Net radiation by brunt's equation
      	NETRAD(srad,e_pa,tair,relsun,b1,b2,b3,b4,albedo,netlong_brunt,
        netlong_satt,netshort,netrad_brunt,netrad_satt);
// cout << "past NETRAD()\n";

// Compute matrix elements for assigning to A matrix and set all others to zero
// include r_sc_xx as defined in RSC()
	ACOEFF(tair,e_abs,netrad_brunt,LAI,les,temp_0,kh,r_aa_dry,r_ac,r_as,
        r_sc_js,alpha_r,a_11,a_12,a_13,b_1,a_24,a_25,b_2,a_31,a_33,a_34,a_35,
        b_3,a_41,a_42,b_4);

// cout << "past ACOEFF()\n";

// idem for unstressed conditions, i.e. r_sc=r_sc_4 with f2=f3=f4=1
	ACOEFFPOT(tair,e_abs,netrad_brunt,LAI,les,temp_0,kh,r_aa_pot,r_ac,r_as,
        r_sc_4,alpha_r,a_11_pot,a_12_pot,a_13_pot,b_1_pot,a_24_pot,a_25_pot,
        b_2_pot,a_31_pot,a_33_pot,a_34_pot,a_35_pot,b_3_pot,a_41_pot,a_42_pot,
        b_4_pot);
// cout << "past ACOEFFPOT()\n";

// idem for wet conditions, i.e. r_sc=0
	ACOEFFWET(tair,e_abs,netrad_brunt,LAI,les,temp_0,kh,r_aa_wet,r_ac,r_as,
        alpha_r,a_11_wet,a_12_wet,a_13_wet,b_1_wet,a_24_wet,a_25_wet,b_2_wet,
        a_31_wet,a_33_wet,a_34_wet,a_35_wet,b_3_wet,a_41_wet,a_42_wet,b_4_wet);
// cout << "past ACOEFFWET()\n";

// cout << "past matrix_3\n";

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
// cout << "past matrix_1 coefficients\n";

// ... and for unstressed conditions...
	a_pot[1][1]=a_11_pot;
        a_pot[1][2]=a_12_pot;
        a_pot[1][3]=a_13_pot;
        a_pot[1][4]=0.0;
        a_pot[1][5]=0.0;
        a_pot[2][1]=0.0;
        a_pot[2][2]=0.0;
        a_pot[2][3]=0.0;
        a_pot[2][4]=a_24_pot;
        a_pot[2][5]=a_25_pot;
        a_pot[3][1]=a_31_pot;
        a_pot[3][2]=0.0;
        a_pot[3][3]=a_33_pot;
        a_pot[3][4]=a_34_pot;
        a_pot[3][5]=a_35_pot;
        a_pot[4][1]=a_41_pot;
        a_pot[4][2]=a_42_pot;
        a_pot[4][3]=0.0;
        a_pot[4][4]=0.0;
        a_pot[4][5]=0.0;
        a_pot[5][1]=0.0;
        a_pot[5][2]=0.0;
        a_pot[5][3]=desta_abs;
        a_pot[5][4]=0.0;
        a_pot[5][5]=-1.0;
        a_53_pot=a_pot[5][3];
// cout << "past matrix_2 coefficients\n";

// ... and for wet conditions...
	a_wet[1][1]=a_11_wet;
        a_wet[1][2]=a_12_wet;
        a_wet[1][3]=a_13_wet;
        a_wet[1][4]=0.0;
        a_wet[1][5]=0.0;
        a_wet[2][1]=0.0;
        a_wet[2][2]=0.0;
        a_wet[2][3]=0.0;
        a_wet[2][4]=a_24_wet;
        a_wet[2][5]=a_25_wet;
        a_wet[3][1]=a_31_wet;
        a_wet[3][2]=0.0;
        a_wet[3][3]=a_33_wet;
        a_wet[3][4]=a_34_wet;
        a_wet[3][5]=a_35_wet;
        a_wet[4][1]=a_41_wet;
        a_wet[4][2]=a_42_wet;
        a_wet[4][3]=0.0;
        a_wet[4][4]=0.0;
        a_wet[4][5]=0.0;
        a_wet[5][1]=0.0;
        a_wet[5][2]=0.0;
        a_wet[5][3]=desta_abs;
        a_wet[5][4]=0.0;
        a_wet[5][5]=-1.0;
        a_53_wet=a_wet[5][3];

// read in b vector (matrix) ...
        b[1][1]=b_1;
        b[2][1]=b_2;
        b[3][1]=b_3;
        b[4][1]=b_4;
        b[5][1]=(tair+273.15)*desta_abs-esta_abs;
	b_5=b[5][1]; // for the control matrix

// ... and read b_pot vector
        b_pot[1][1]=b_1_pot;
        b_pot[2][1]=b_2_pot;
        b_pot[3][1]=b_3_pot;
        b_pot[4][1]=b_4_pot;
        b_pot[5][1]=(tair+273.15)*desta_abs-esta_abs;
	b_5_pot=b_pot[5][1];  // for the control matrix

// ... and read b_pot vector
        b_wet[1][1]=b_1_wet;
        b_wet[2][1]=b_2_wet;
        b_wet[3][1]=b_3_wet;
        b_wet[4][1]=b_4_wet;
        b_wet[5][1]=(tair+273.15)*desta_abs-esta_abs;
	b_5_wet=b_wet[5][1]; // for the control matrix

	for (l=1;l<=n;l++)
        {
		for (k=1;k<=n;k++)
         	{
        	ai[k][l]=a[k][l]; // save a matrix in ai
            	ai_pot[k][l]=a_pot[k][l]; // idem for a_pot
            	ai_wet[k][l]=a_wet[k][l]; // idem for a_wet

           	}
		for (k=1;k<=m;k++)
         	{
         	x[l][k]=b[l][k];  // save b matrix in x
            	x_pot[l][k]=b_pot[l][k]; // idem for x_pot and b_pot
            	x_wet[l][k]=b_wet[l][k]; // idem for x_wet and b_wet

            	}
	}  // end for

// invert matrix a, a_pot and a_wet
	gaussj(ai,n,x,m);
      	gaussj(ai_pot,n,x_pot,m);
      	gaussj(ai_wet,n,x_wet,m);

// write solution vector for x (stressed conditions)
	tcan=x[1][1]-273.15;  // in degrees C
	tskin=x[2][1]-273.15; // in degrees C
	tleaf=x[3][1]-273.15; // in degrees C
 	e_c_abs=x[4][1];  // e_c in kg/m**3
	e_sl_abs=x[5][1]; // e_sl in kg/m**3

        tcan_prev=tcan; // save tcan for use in RAASTAB()

// write solution vector for x_pot (unstressed conditions, i.e. F_i=1.0)
	tcan_pot=x_pot[1][1]-273.15;  // in degrees C
	tskin_pot=x_pot[2][1]-273.15; // in degrees C
	tleaf_pot=x_pot[3][1]-273.15; // in degrees C
 	e_c_abs_pot=x_pot[4][1];  // e_c in kg/m**3
	e_sl_abs_pot=x_pot[5][1]; // e_sl in kg/m**3

        tcan_prev_pot=tcan_pot; // save tcan_pot for use in RAASTABPOT()

// write solution vector for x_wet (wet conditions, i.e. r_sc=0)
	tcan_wet=x_wet[1][1]-273.15;  // in degrees C
	tskin_wet=x_wet[2][1]-273.15; // in degrees C
	tleaf_wet=x_wet[3][1]-273.15; // in degrees C
 	e_c_abs_wet=x_wet[4][1];  // e_c in kg/m**3
	e_sl_abs_wet=x_wet[5][1]; // e_sl in kg/m**3

        tcan_prev_wet=tcan_wet; // save tcan_wet for use in RAASTABWET()

// calculate energy balance for sparse crops
	GFLUX(tskin,kh,temp_0,gflux);  // ground heat flux
	LEHFLUX(tair,tskin,tcan,tleaf,r_aa_dry,r_ac,r_as,r_sc_js,e_c_abs,e_sl_abs,
        e_abs,les,hl,ha,hs,lea,lel,hclos,leclos,dtcta,dtltc,dtstc,dtlta);
	LEHFLUXPOT(tair,tskin_pot,tcan_pot,tleaf_pot,r_aa_pot,r_ac,r_as,r_sc_4,
      	e_c_abs_pot,e_sl_abs_pot,e_abs,les,hl_pot,ha_pot,hs_pot,lea_pot,lel_pot,
      	hclos_pot,leclos_pot,dtcta_pot,dtltc_pot,dtstc_pot,dtlta_pot);
	LEHFLUXWET(tair,tskin_wet,tcan_wet,tleaf_wet,r_aa_wet,r_ac,r_as,
        e_c_abs_wet,e_sl_abs_wet,e_abs,les,hl_wet,ha_wet,hs_wet,lea_wet,lel_wet,
        hclos_wet,leclos_wet,dtcta_wet,dtltc_wet,dtstc_wet,dtlta_wet);

      	AVENER(netrad_brunt,LAI,alpha_r,gflux,av_s,av_l);  // available energy

// convert vapor pressure from kg/m**3 to Pa
  	EABS2PA(e_c_abs,tcan,e_c);  // at canopy temperature
     	EABS2PA(e_sl_abs,tleaf,e_sl); // at leaf temperature
  	EABS2PA(e_c_abs_pot,tcan_pot,e_c_pot);  // at canopy temperature
     	EABS2PA(e_sl_abs_pot,tleaf_pot,e_sl_pot); // at leaf temperature
  	EABS2PA(e_c_abs_wet,tcan_wet,e_c_wet);  // at canopy temperature
     	EABS2PA(e_sl_abs_wet,tleaf_wet,e_sl_wet); // at leaf temperature

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

  void output (Log& log) const
  {                                                 // var 1-4: yy-mm-dd-hh
    PT::output (log); // log_only var
    log.output ("netrad_brunt", netrad_brunt);  	// var 5
    log.output ("netlong_brunt", netlong_brunt);  // var 6
    log.output ("r_a", r_a);                      // var 7
    log.output ("r_astab", r_astab);              // var 8
    log.output ("r_aa", r_aa);                    // var 9
    log.output ("r_aa_dry", r_aa_dry);            // var 10
    log.output ("r_aa_pot", r_aa_pot);            // var 11
    log.output ("r_as", r_as);                    // var 12
    log.output ("r_ac", r_ac);                    // var 13
    log.output ("r_sc_1", r_sc_1);                // var 14
    log.output ("r_sc_2", r_sc_2);                // var 15
    log.output ("tskin", tskin);                  // var 16
    log.output ("tcan", tcan);                    // var 17
    log.output ("tleaf", tleaf);                  // var 18
    log.output ("e_c", e_c);                      // var 19
    log.output ("e_sl", e_sl);                    // var 20
    log.output ("tskin_pot", tskin_pot);          // var 21
    log.output ("tcan_pot", tcan_pot);            // var 22
    log.output ("tleaf_pot", tleaf_pot);          // var 23
    log.output ("e_c_pot", e_c_pot);              // var 24
    log.output ("e_sl_pot", e_sl_pot);            // var 25
    log.output ("ha", ha);                        // var 26
    log.output ("hl", hl);                        // var 27
    log.output ("hs", hs);                        // var 28
    log.output ("lea", lea);                      // var 29
    log.output ("lel", lel);                      // var 30
    log.output ("ha_pot", ha_pot);                // var 31
    log.output ("hl_pot", hl_pot);                // var 32
    log.output ("hs_pot", hs_pot);                // var 33
    log.output ("lea_pot", lea_pot);              // var 34
    log.output ("lel_pot", lel_pot);              // var 35
    log.output ("gflux", gflux);                  // var 36
    log.output ("dtcta", dtcta);                  // var 37
    log.output ("dtltc", dtltc);                  // var 38
    log.output ("dtstc", dtstc);                  // var 39
    log.output ("dtcta_pot", dtcta_pot);          // var 40
    log.output ("dtltc_pot", dtltc_pot);          // var 41
    log.output ("dtstc_pot", dtstc_pot);          // var 42
    log.output ("theta_0_20", theta_0_20);        // var 43
    log.output ("theta_0_50", theta_0_50);        // var 44
    log.output ("theta_0_100", theta_0_100);      // var 45
    log.output ("f_1", f_1);                      // var 46
    log.output ("f_2", f_2);                      // var 47
    log.output ("f_3", f_3);                      // var 48
    log.output ("f_4", f_4);                      // var 49
    log.output ("f_temp", f_temp);                // var 50
    log.output ("f_def", f_def);                  // var 51
    log.output ("f_theta", f_theta);              // var 52
    log.output ("f_etep", f_etep);                // var 53
    log.output ("r_sc_js", r_sc_js);              // var 54
    log.output ("r_sc_min", r_sc_min);            // var 55

  }

   ~PT_PMSW() // destructor
   	{
#ifdef USE_FILES
   fclose(fp_rcmin);
   fclose(fp_rcminsb);
   fclose(fp_rcminww);
#endif
  	free_matrix(t,1,NP,1,MP);
  	free_matrix(x,1,NP,1,MP);
  	free_matrix(b,1,NP,1,MP);
  	free_matrix(um,1,NP,1,NP);
  	free_matrix(ai,1,NP,1,NP);
  	free_matrix(a,1,NP,1,NP);
	free_matrix(a_pot,1,NP,1,NP);
	free_matrix(ai_pot,1,NP,1,NP);
	free_matrix(um_pot,1,NP,1,NP);
	free_matrix(b_pot,1,NP,1,MP);
	free_matrix(x_pot,1,NP,1,MP);
	free_matrix(t_pot,1,NP,1,MP);
	free_matrix(a_wet,1,NP,1,NP);
	free_matrix(ai_wet,1,NP,1,NP);
	free_matrix(um_wet,1,NP,1,NP);
	free_matrix(b_wet,1,NP,1,MP);
	free_matrix(x_wet,1,NP,1,MP);
	free_matrix(t_wet,1,NP,1,MP);
      }

}; // end class PT_PMSW

static struct PT_PMSWSyntax
{
  static PT&
  make (const AttributeList& al)
    { return *new PT_PMSW (al); }
  PT_PMSWSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      PT::load_syntax (syntax, alist);
#if 1
syntax.add ("netrad_brunt", "W/m**2", Syntax::LogOnly,
		"Net radiation by Brunt");
syntax.add ("netlong_brunt", "W/m**2", Syntax::LogOnly,
		"Net long radiation by Brunt");
syntax.add ("r_a", "s/m", Syntax::LogOnly,
		"bulk aerodynamic resistance, neutral conditions");
syntax.add ("r_astab", "s/m", Syntax::LogOnly,
		"bulk aerodynamic resistance, stability corrected");
syntax.add ("r_aa", "s/m", Syntax::LogOnly,
		"aerodynamic resistance mean source-ref, uncorrected");
syntax.add ("r_aa_dry", "s/m", Syntax::LogOnly,
		"aerodynamic resistance mean source-ref, corrected 'dry'");
syntax.add ("r_aa_pot", "s/m", Syntax::LogOnly,
		"aerodynamic resistance mean source-ref, corrected,'pot'");
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
syntax.add ("e_c", "Pa", Syntax::LogOnly,
		"vapor pressure at mean source height");
syntax.add ("e_sl", "Pa", Syntax::LogOnly,
		"saturated vapor pressure at leaf surface");
syntax.add ("tskin_pot", "dg C", Syntax::LogOnly,
		"potential soil/skin temperature");
syntax.add ("tcan_pot", "dg C", Syntax::LogOnly,
		"potential canopy temperature at mean source");
syntax.add ("tleaf_pot", "dg C", Syntax::LogOnly,
		"potential leaf temperature");
syntax.add ("e_c_pot", "Pa", Syntax::LogOnly,
		"potential vapor pressure at mean source height");
syntax.add ("e_sl_pot", "Pa", Syntax::LogOnly,
		"potential saturated vapor pressure at leaf surface");
syntax.add ("ha", "W/m**2", Syntax::LogOnly,
		"Sensible heat flux from source- to screen height");
syntax.add ("hl", "W/m**2", Syntax::LogOnly,
		"Sensible heat flux from leaf to mean source");
syntax.add ("hs", "W/m**2", Syntax::LogOnly,
		"Sensible heat flux from soil to mean source");
syntax.add ("lea", "W/m**2", Syntax::LogOnly,
		"Latent heat flux from source- to screen height");
syntax.add ("lel", "W/m**2", Syntax::LogOnly,
		"Latent heat flux from leaf to mean source");
syntax.add ("ha_pot", "W/m**2", Syntax::LogOnly,
		"Potential sensible heat flux from source- to screen height");
syntax.add ("hl_pot", "W/m**2", Syntax::LogOnly,
		"Potential sensible heat flux from leaf to mean source");
syntax.add ("hs_pot", "W/m**2", Syntax::LogOnly,
		"Potential sensible heat flux from soil to mean source");
syntax.add ("lea_pot", "W/m**2", Syntax::LogOnly,
		"Potential latent heat flux from source- to screen height");
syntax.add ("lel_pot", "W/m**2", Syntax::LogOnly,
		"Potential latent heat flux from leaf to mean source");
syntax.add ("gflux", "W/m**2", Syntax::LogOnly,
		"Ground heat flux");
syntax.add ("dtcta", "dg C", Syntax::LogOnly,
		"Temperature gradient between mean source og screen height");
syntax.add ("dtltc", "dg C", Syntax::LogOnly,
		"Temperature gradient between leaf and mean source");
syntax.add ("dtstc", "dg C", Syntax::LogOnly,
		"Temperature gradient between soil and mean source");
syntax.add ("dtcta_pot", "dg C", Syntax::LogOnly,
		"Potential temp gradient between mean source og screen height");
syntax.add ("dtltc_pot", "dg C", Syntax::LogOnly,
		"Potential temperature gradient between leaf and mean source");
syntax.add ("dtstc_pot", "dg C", Syntax::LogOnly,
		"Potential temperature gradient between soil and mean source");
syntax.add ("theta_0_20", "cm3/cm3", Syntax::LogOnly,
		"Averaged soil water content in upper 20 cm");
syntax.add ("theta_0_50", "cm3/cm3", Syntax::LogOnly,
		"Averaged soil water content in upper 50 cm");
syntax.add ("theta_0_100", "cm3/cm3", Syntax::LogOnly,
		"Averaged soil water content in upper 100 cm");
syntax.add ("f_1", "NA", Syntax::LogOnly,
		"Constraint function (Noilhan) related to solar radiation");
syntax.add ("f_2", "NA", Syntax::LogOnly,
		"Constraint function (Noilhan) related to vapor pressure");
syntax.add ("f_3", "NA", Syntax::LogOnly,
		"Constraint function (Noilhan) related to air temperature");
syntax.add ("f_4", "NA", Syntax::LogOnly,
		"Constraint function (Noilhan) related to soil water content");
syntax.add ("f_temp", "NA", Syntax::LogOnly,
		"Constraint function (Verma) related to air temperature");
syntax.add ("f_def", "NA", Syntax::LogOnly,
		"Constraint function (Verma) related to vapor pressure");
syntax.add ("f_etep", "NA", Syntax::LogOnly,
		"Constraint function defined by crop_ea/crop_ep");
syntax.add ("r_sc_js", "s/m", Syntax::LogOnly,
		"Bulk canopy resistance: f1_dolman*f_def*f3*f4");
syntax.add ("r_sc_min", "s/m", Syntax::LogOnly,
		"minimum canopy resistance");
syntax.add ("albedo", "NA", Syntax::Const,
		"Bulk albedo");
alist.add ("albedo", 0.2);
syntax.add ("b1", "NA", Syntax::Const,
		"Brunt coefficient 1");
alist.add ("b1", 0.53);
syntax.add ("b2", "NA", Syntax::Const,
		"Brunt coefficient 2");
alist.add ("b2", 0.0065);
syntax.add ("b3", "NA", Syntax::Const,
		"Brunt coefficient 3");
alist.add ("b3", 0.1);
syntax.add ("b4", "NA", Syntax::Const,
		"Brunt coefficient 4");
alist.add ("b4", 0.9);
syntax.add ("ndif", "NA", Syntax::Const,
		"Eddy diffusivity decay constant in crop");
alist.add ("ndif", 2.5);
syntax.add ("c_d", "NA", Syntax::Const,
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
syntax.add ("alpha_u", "NA", Syntax::Const,
		"attenuation coefficient for wind speed");
alist.add ("alpha_u", 3.0);
syntax.add ("arac", "NA", Syntax::Const,
		"leaf boundary layer resistance coefficient");
alist.add ("arac", 0.00662);
syntax.add ("alpha_k", "NA", Syntax::Const,
		"Att. coefficient of eddy diffusivity through sparse canopy");
alist.add ("alpha_k", 2.0);

syntax.add ("alpha_r", "NA", Syntax::Const,
		"Att. coefficient for vegetation in ACOEFF()");
alist.add ("alpha_r", 0.5);
syntax.add ("theta_w", "cm3/cm3", Syntax::Const,
		"Soil water content at 'wilting point'");
alist.add ("theta_w", 0.05);
syntax.add ("theta_c", "cm3/cm3", Syntax::Const,
		"Soil water content at 'field capacity'");
alist.add ("theta_c", 0.25);
syntax.add ("rcmin_const", "s/m", Syntax::Const,
		"Constant minimum canopy resistance");
alist.add ("rcmin_const", 30.0);
syntax.add ("rcmax", "NA", Syntax::Const,
		"Maximum canopy resistance");
alist.add ("rcmax", 1000.0);
syntax.add ("tref", "NA", Syntax::Const,
		"Reference/optimum temperature in temperature dependent "
                "constraint function");
alist.add ("tref", 298.0);
syntax.add ("zeta", "NA", Syntax::Const,
		"Coefficient in vapor pressure dependent constraint function");
alist.add ("zeta", 0.0002);
syntax.add ("f3const", "NA", Syntax::Const,
		"Coefficient in temperature dependent constraint function");
alist.add ("f3const", 0.0016);
syntax.add ("spar", "NA", Syntax::Const,
		"Reference value of photosynthetically active part of Si");
alist.add ("spar", 100.0);
syntax.add ("tmin", "dg C", Syntax::Const,
		"Minimum temperature for canopy conductance");
alist.add ("tmin", 0.0);
syntax.add ("tmax", "dg C", Syntax::Const,
		"Maximum temperature for canopy conductance");
alist.add ("tmax", 55.0);
syntax.add ("nu_1", "NA", Syntax::Const,
		"coefficient in Jarvis (1976) constraint function f_temp");
alist.add ("nu_1", 26.5);
syntax.add ("nu_2", "NA", Syntax::Const,
		"coefficient in Lohammar (1980) constraint function f_def");
alist.add ("nu_2", 0.57);
syntax.add ("nu_3", "NA", Syntax::Const,
		"coefficient in Steward (1988) constraint function f_theta");
alist.add ("nu_3", 0.008);
syntax.add ("dt1", "dg C", Syntax::Const,
		"lower solution limit in Newton-Raphson method");
alist.add ("dt1", -5.0);
syntax.add ("dt2", "dg C", Syntax::Const,
		"upper solution limit in Newton-Raphson method");
alist.add ("dt2", 5.0);
syntax.add ("acc", "NA", Syntax::Const,
		"iteration accuracy in Newton-Raphson method");
alist.add ("acc", 0.01);
#endif
      Librarian<PT>::add_type ("PMSW", alist, syntax, &make);
    }
} PT_PMSW_syntax;
