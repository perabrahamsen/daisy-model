// Status: revised 17-12-1998

// description of functions

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
int RAS(double, double, double, double, double, double, double&, double&);
int RAC(double, double, double , double&, double&, double&, double&, double&);
int RSC(double , double, double, double, double, double, double , double,
	double, double, double, double, double, double, double, double, double&,
   double&, double&,double&, double&, double&, double&, double&, double&,
   double&, double&, double&, double&, double&, double&, double&, double&,
   double&, double&, double&);
int EPA2ABS(double, double, double&);
int EABS2PA(double, double, double&);
int NETRAD(double, double, double, double, double , double, double, double,
	double, double&, double&, double&, double&, double&);
int AVENER(double ,double ,double ,double ,double&, double&);
int GFLUX(double, double, double, double, double&);
int LEHFLUX(double, double, double, double, double, double, double, double,
	double, double, double, double, double&, double&, double&, double&,
   double&, double&, double&, double&, double&, double&, double&);
int VAPOR(double, double&, double&, double&, double&);
int ACOEFF(double, double, double, double, double,	double, double, double,
	double, double, double, double, double, double&, double&, double&, double&,
   double&, double&, double&, double&, double&, double&, double&, double&,
   double&, double&, double&);
double RTSAFE_DT(void *, double, double, double, double, double, double, double,
	double, double, double, double&, double, double, double);
void EBAL_NR(double, double, double, double, double, double, double,
	double, double, double, double, double&, double *, double *);
int EBAL_DT(double, double, double, double, double, double, double, double,
	double, double, double, double&, double&, double&, double&, double&, double&,
   double&, double&, double&, double&, double&, double&, double&);
int EBAL_PM(double, double, double, double, double, double, double, double,
	double, double, double, double&, double&, double&, double&, double&, double&,
   double&, double&, double&, double&);



// Declaration of functions

// compute aerodynamic resistance r_a under neutral conditions

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

// From SG(1990): Aerodyn. resistance between canopy source heigth and ref.level

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

// leaf boundary-layer resistance between leaves and canopy air space as
// formulated in Daamen (1997) p.212 (r_b) based on Choudhury & Monteith (1988)
// and Jones (1983)

int RAC (double u, double h, double LAI, double c_d, double w, double z_0s,
	double alpha_u, double arac, double &rd, double &rX, double &rz_0,
	double &ru_h, double &rr_ac)
	{
   // ru_h: windspeed at cropheight h, i.e. above crop [m/s]
   // z_0, d roughness and zero displacement height [m]
   // double w: average leaf width, from SG(1990), p.512 [m]
	// double c_d: used in Shaw & Pereira (1982)
   // rr_b leaf boundary layer resistance, Daamen eq.21
   // attenuation coefficient for wind speed u
   // const double z_0s=0.01: z_0* for substrate SW p.847
   // const double alpha_u=3.0: alpha_u=3 (Choudhury and Monteith, 1988)
   // constant, CM (1988) used 0.01, calibration is
   // important (Daamen, 1997), p.212
   // const double arac (a)=0.00662: used by Jones (1983)
   const double z_ref=2.0; // [m]

   rX=c_d*LAI; // Shaw & Pereira (1982)
     		rd=1.1*h*log(1+pow(rX,0.25)); // Shaw&Pereira(1982)SG,p.507
		   if (rX>0.0 && rX<0.2) rz_0=z_0s+0.3*h*sqrt(rX); // SG eq.43a
		  		else if (rX>0.2 && rX<1.5) rz_0=0.3*h*(1-rd/h); // SG eq.43b
         		else rz_0=0.13*h; // otherwise
      	   		if (z_ref-rd > 0.0)
            		{
            		ru_h=u*log((h-rd)/rz_0)/log((z_ref-rd)/rz_0);
            		} else
           				{
           				rd=0.67*h;
           				ru_h=u*log((h-rd)/rz_0)/log((z_ref-rd)/rz_0);
                     }
         rr_ac=(alpha_u/(2*LAI*arac))*sqrt((w/ru_h)*1/(1-exp(-alpha_u/2)));

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
	//	c_d=0.05: SG p.507
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
	double esta, double theta_w, double theta_c, double rcmin_LAI, double rcmax,
   double zeta, double f3const, double tref, double spar, double eact_w,
   double epotc_w, double &rfpar, double &rf_1, double &rf_2, double &rf_3,
   double &rf_4, double &rr_sc_1, double &rr_tot_1,double &rbf_temp,
   double &rf_temp, double &rf_def, double &rf_theta, double &rr_sc_2,
   double &rr_tot_2, double &rf_etep, double &rr_sc_3, double &rr_tot_3,
   double &rr_sc_4, double &rr_tot_4, double &rr_sc_5, double &rr_tot_5)
	{
   double tairk,def;

   const double a_3=26.5; // from Verma et al. (1993)
   const double a_4=0.570;
   const double a_5=0.008;
   const double tmin=0.0;
   const double tmax=55.0;

   esta=611*exp((17.27*tair)/(tair+237.3)); // saturated vapor pressure
   def=0.001*(esta-e_pa); // vapor deficit in kPa

	tairk=tair+273.15;
	rfpar=0.55*2*srad/(spar*LAI);
   rcmin_LAI=200/LAI;   // FAO

// constraint functions used in Noilhan et al.(1991)
	rf_1=(rcmin_LAI/rcmax+rfpar)/(1+rfpar); // resp.func. related to solar radiation
   rf_2=1-zeta*(esta-e_pa); // resp.func. related to vapour pressure deficit
   rf_3=1-f3const*pow(tref-tairk,2); // resp. func. related to air temperature
   rf_4=(theta_0_20-theta_w)/(theta_c-theta_w); // idem related to soil moisture

// rf_4 should not be zero
	if (rf_4==0.0) rf_4=0.01;
// constraint functions used in Verma et al.(1993)
   rbf_temp=(tmax-a_3)/(a_3-tmin);
	if (tair==tmin) tair=tmin+0.1;
   rf_temp=(tair-tmin)*pow(tmax-tair,rbf_temp)/
   	((a_3-tmin)*pow(tmax-a_3,rbf_temp));  // Jarvis (1976)
//	cout << "past f_temp\n";
   rf_def=1.0/(1.0+a_4*def); // Lohammar (1980)
//	cout << "past f_def\n";
   rf_theta=1.0-exp(-a_5*100.0*theta_0_20); // Stewart (1988), Kim & Verma (1991)
//	cout << "past f_theta\n";
	if (epotc_w==0.0) epotc_w=epotc_w+1.0; // no division by 0
   if (eact_w==0.0) eact_w=eact_w+1.0; // no division by 0 in rr_sc_3
	rf_etep=eact_w/epotc_w;
// canopy resistance using Noilhan et al.(1991)...
	if (LAI*rf_1*rf_2*rf_3*rf_4 <= 1.0)
   	{
   	rr_sc_1=(rcmin_LAI/LAI)/(rf_1*rf_2*rf_3*rf_4);
      } else rr_sc_1=rcmin_LAI;
//   cout << "past rsc_1\n";
   rr_tot_1=rf_1*rf_2*rf_3*rf_4;
//   cout << "past r_tot_1\n";
// ... or using Verma et al. (1993), modified with rcmin/LAI and theta_0_20...
	if (LAI*rf_1*rf_temp*rf_def*rf_theta <= 1.0)
   	{      // still using f_1...
		rr_sc_2=(rcmin_LAI/LAI)/(rf_1*rf_temp*rf_def*rf_theta);
      } else rr_sc_2=rcmin_LAI;
//  cout << "past r_sc_2\n";
   rr_tot_2=rf_1*rf_temp*rf_def*rf_theta; // still using f_1...
//   cout << "past r_tot_2\n";
//	... or using f_etep: eact/epotc
	if (LAI*rf_1*rf_2*rf_3*rf_etep <= 1.0)
   	{
		rr_sc_3=(rcmin_LAI/LAI)/(rf_1*rf_2*rf_3*rf_etep);
      } else rr_sc_3=rcmin_LAI;
   rr_tot_3=rf_1*rf_2*rf_3*rf_etep;
// combine f_etep with f_temp, f_def and f_theta (and f_1):
	if (LAI*rf_1*rf_temp*rf_def*rf_etep <= 1.0)
     	{
		rr_sc_4=(rcmin_LAI/LAI)/(rf_1*rf_temp*rf_def*rf_etep);
      } else rr_sc_4=rcmin_LAI;
   rr_tot_4=rf_1*rf_temp*rf_def*rf_etep;
// replace f_2 by f_def
	if (LAI*rf_1*rf_def*rf_3*rf_4 <= 1.0)
     	{
		rr_sc_5=(rcmin_LAI/LAI)/(rf_1*rf_def*rf_3*rf_4);
      } else rr_sc_5=rcmin_LAI;
   rr_tot_5=rf_1*rf_def*rf_3*rf_4;

   return 0;
   }

// Net radiation is calculated from measured global radiation
// and estimated longwave downwards radiation and returns netrad

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
   	rnetlong_brunt=sigma*pow(tair+273.15,4)*(b1-b2*sqrt(e_pa))*(b3+b4*relsun);
		rnetrad_brunt=rnetshort - rnetlong_brunt;
      rnetlong_satt=sigma*pow(tair+273.15,4)*
      1.08*(1-exp(-(pow(0.01*e_pa,tair/2016))))*(b3+b4*relsun);
      rnetrad_satt=rnetshort - rnetlong_satt;

		return 0;
	}

// calculation of ground heat flux
int GFLUX(double tsurf, double kh, double temp_sz, double z_sz, double &rgflux)
	{
		rgflux = kh*(tsurf-temp_sz)/z_sz; // positive directed from soil surface

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

// calculation of energy fluxes using SW and SG
int LEHFLUX(double tair, double tsurf, double tcan, double tleaf, double r_aa,
	double r_ac, double r_as, double r_sc_5, double e_c_abs, double e_sl_abs,
   double e_abs,double les, double &rhl, double &rha, double &rhs, double &rlea,
   double &rlel, double &rhclos, double &rleclos, double &rdtcta,
   double &rdtltc, double &rdtstc, double &rdtlta)
   {
   const double lambda=2450000; // latent heat of vaporization at 20 C [J/kg]
   const double rho_a=1.23;
   const double c_p=1010.0;

   rha=rho_a*c_p*(tcan-tair)/r_aa; // sensible heat: source height - reference
   rhl=rho_a*c_p*(tleaf-tcan)/r_ac; // sensible heat: leaf - source height
   rhs=rho_a*c_p*(tsurf-tcan)/r_as; // sensible heat: surface - source height
   rlea=lambda*(e_c_abs-e_abs)/r_aa; // latent heat: source height - reference
   rlel=lambda*(e_sl_abs-e_c_abs)/(r_sc_5+r_ac); // LE: leaf - source height
   rdtcta=tcan-tair;
   rdtltc=tleaf-tcan;
   rdtstc=tsurf-tcan;
   rdtlta=tleaf-tair;

   rhclos=rha-rhl-rhs; // closure for sensible heat fluxes
   rleclos=rlea-rlel-les; // closure for latent heat fluxes

   return 0;
   }

// calculation of es(tair),des(tair), es(tair)_abs and des(tair)_abs
int VAPOR(double tair, double &resta, double &resta_abs,
	double &rdesta, double &rdesta_abs)
	{
   const double lambda=2450000; // latent heat of vaporization at 20 C [J/kg]
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

// calculation of coefficient needed in gaussj matrix solution
int ACOEFF(double tair, double e_abs, double netrad, double LAI, double les,
	double temp_sz, double kh, double r_aa, double r_ac, double r_as,
   double z_sz, double r_sc_5, double alpha_r, double &ra_11, double &ra_12,
   double &ra_13, double &rb_1, double &ra_24, double &ra_25, double &rb_2,
   double &ra_31, double &ra_33, double &ra_34, double &ra_35, double &rb_3,
   double &ra_41, double &ra_42, double &rb_4)
	{
   const double lambda=2450000; // latent heat of vaporization at 20 C [J/kg]
   const double rho_a=1.23;
   const double c_p=1010;
	// const double alpha_r=0.5: attenuation/extinction coef.
   // const double z_sz=0.1: 10 cm depth
	ra_11=1/r_aa+1/r_ac+1/r_as;
	ra_12=-1/r_as;
   ra_13=-1/r_ac;
   rb_1=(tair+273.15)/r_aa;
   ra_24=lambda/r_aa+lambda/(r_sc_5+r_ac);
   ra_25=-lambda/(r_sc_5+r_ac);
   rb_2=les+e_abs*lambda/r_aa;
   ra_31=-rho_a*c_p/r_ac;
   ra_33=rho_a*c_p/r_ac;
   ra_34=-lambda/(r_sc_5+r_ac);
   ra_35=lambda/(r_sc_5+r_ac);
   rb_3=netrad*(1-exp(-alpha_r*LAI));
   ra_41=-rho_a*c_p/r_as;
   ra_42=rho_a*c_p/r_as+kh/z_sz;
   rb_4=netrad*exp(-alpha_r*LAI)+(temp_sz+273.15)*kh/z_sz-les;

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
					} else if (ipiv[k] > 1) nrerror("gaussj: Singular Matrix-1");
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

	  	netlongwave_pm=sigma*pow(tair+273.15,4)*(b1-b2*sqrt(e_pa))*(b3+b4*relsun);

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
	double desta, double kh, double temp_0, double z_sz, double r_aa,
   double r_ac,double r_sc_5, double &ra_dt, double &rb_dt, double &rc_dt,
   double &ra_dt_wet, double &rc_dt_wet,double &rdt_dt, double &rdt_dt_wet,
   double &rtsurf_dt,double &rg_dt, double &rh_dt,double &rle_dt,
   double &rle_dt_wet,double &rclosure_dt)
   {
	const double rho_a = 1.23; // air density [kg*m**-3]
	const double c_p = 1010; // specific heat of air [J*kg**-1*K**-1]
   const double gamma=66.1; // psychometric constant

   // closure(dt)=netrad-le(dt)-h(dt)-g(dt) is solved for dt (=tsurf-tair)

   // calculate coefficients a_dt,b_dt and c_dt
   ra_dt=(rho_a*c_p/gamma)*(e_pa-esta)/(r_aa+r_ac+r_sc_5); // dry
   ra_dt_wet=(rho_a*c_p/gamma)*(e_pa-esta)/(r_aa+r_ac);
   rb_dt=kh*(temp_0-tair)/z_sz;
   rc_dt=-rho_a*c_p*desta/((r_aa+r_ac+r_sc_5)*gamma)-rho_a*c_p/(r_aa+r_ac)-
   kh/z_sz;
	rc_dt_wet=-rho_a*c_p*desta/((r_aa+r_ac)*gamma)-rho_a*c_p/(r_aa+r_ac)-kh/z_sz;
   rdt_dt=(-netrad_brunt-ra_dt-rb_dt)/rc_dt;
   rdt_dt_wet=(-netrad_brunt-ra_dt_wet-rb_dt)/rc_dt_wet;

	rg_dt=kh*(tair+rdt_dt-temp_0)/z_sz;
	rh_dt=rho_a*c_p*rdt_dt/(r_aa+r_ac);
	rle_dt=(rho_a*c_p/gamma)*(esta+desta*rdt_dt-e_pa)/(r_aa+r_ac+r_sc_5);
   rle_dt_wet=(rho_a*c_p/gamma)*(esta+desta*rdt_dt_wet-e_pa)/(r_aa+r_ac);
   rclosure_dt=netrad_brunt-rg_dt-rh_dt-rle_dt;
  	rtsurf_dt=tair+rdt_dt;
   return 0;
   }

// Newton-Raphson method: solves for dt (=tsurf-tair)
double RTSAFE_DT(void (*funcd)(double tsurf,double tair,double e_pa,double srad,
	double relsun,double kh,double r_aa,double r_ac,double temp_0,double z_sz,
   double LAI,double &rrsc_pm,double *, double *), double tair,double e_pa,
   double srad,double relsun,double kh,double r_aa,double r_ac,double temp_0,
   double z_sz,double LAI,double &rrsc_pm, double x1,
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
//   printf("inside rtsafe!\n");

 	(*funcd)(x1,tair,e_pa,srad,relsun,kh,r_aa,r_ac,temp_0,z_sz,LAI,rrsc_pm,&fl,&df);
	(*funcd)(x2,tair,e_pa,srad,relsun,kh,r_aa,r_ac,temp_0,z_sz,LAI,rrsc_pm,&fh,&df);
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
	rts=0.5*(x1+x2); 		// initialize the guess for root,
	dxold=fabs(x2-x1); 	// the 'stepsize before last',
	dx=dxold;          	// and the last step
   (*funcd)(rts,tair,e_pa,srad,relsun,kh,r_aa,r_ac,temp_0,z_sz,LAI,rrsc_pm,&f,&df);
   for(j=1;j<=100;j++) // loop over allowed iterations
   	{
      if ((((rts-xh)*df-f)*((rts-xl)*df-f) >= 0.0) // bisect if NR out of range
      || (fabs(2.0*f) > fabs(dxold*df)))        // or not decreasing fast enough
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
double kh,double temp_0,double z_sz,double r_aa,double r_ac, double rsc_pm,
double &rnetshortwave_pm,double &rnetlongwave_pm,double &rnetrad_pm,
double &rdnetrad_pm,double &rgflux_pm,double &rdgflux_pm,double &rhflux_pm,
double &rdhflux_pm,double &rleflux_pm,double &rdleflux_pm)
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

	// I/O files
// FILE *fp_ebal_pm;
	FILE *fp_daisypar, *fp_parcheck; // i/o parameter file
	FILE *fp_daisy,*fp_rescheck;
   FILE *fp_solcheck, *fp_axainv, *fp_mat_a, *fp_mat_b; // for gaussj procedure

	// meteorological- and derived variables
   double srad,tair,e_abs,e_pa,u,relsun,prec; // metinput
   double esta,esta_abs,desta,desta_abs,ests; // saturated vapor pressures
   double netrad_brunt,netlong_brunt,netrad_satt,netlong_satt,netshort;
	double albedo,b1,b2,b3,b4;

	// resistances and related variables r_sc_1 (Noilhan et al. (1991)
   // r_sc_2 (Verma et al. (1993): different constraint functions
   double r_a,r_aa,r_ac,r_as,r_sc_1,r_sc_2,r_sc_dt;

	//in RAA(),RAC(),RAS() and RST()
   double d_aa,d_ac,z0_aa,z0_ac,uf_aa,uh,X_ac,r_b,k_h,alpha_r;
   double ndif,c_d,z_0s,z0_def,w,alpha_u,arac,alpha_k; // from daisy.par
   double theta_w,theta_c,fpar,f_1,f_2,f_3,f_4,r_tot_1; // in Noilhan e.a.(1991)
   double bf_temp,f_temp,f_def,f_theta,r_tot_2; // in Verma et al. (1993)
   double f_etep,r_sc_3,r_tot_3; // using f_etep=eact/epotc
   double r_sc_4,r_tot_4; // using f_temp, f_def, f_etep (and f_1)
   double r_sc_5,r_tot_5; // replacing f_2 by f_def
	double rcmin_LAI,rcmin_const,rcmax,tref,zeta,f3const,spar; // from daisy.par

	// parameters and variables used in SG(1990) approach
   double av_l,av_s; // available energy at leaf surface and soil surface
   double tsurf,tcan,tleaf,e_c,e_sl; // elements to be solved by gaussj
   double e_c_abs,e_sl_abs; // in kg/m**3: e_c and e_sl in Pa
   double a_11,a_12,a_13,a_14,a_15,a_21,a_22,a_23,a_24,a_25; // matrix elements
   double a_31,a_32,a_33,a_34,a_35,a_41,a_42,a_43,a_44,a_45; // matrix elements
   double a_51,a_52,a_53,a_54,a_55,b_1,b_2,b_3,b_4,b_5; // matrix elements

	// energy fluxes etc using the SW/SG sparse crop approach:
	double ha,hl,hs,lea,lel; // sensible and latent heat fluxes in LEHFLUX()
	double gflux,z_sz; // ground heat flux in GFLUX()
   double hclos,leclos; // ebal closure for H and LE in LEHFLUX()
   double dtcta,dtltc,dtstc,dtlta; //tc-ta, tl-tc, ts-tc, tl-ta
	float **a,**ai,**um,**b,**x,**t; // arrays for matrix solution
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
	double epotc,epots; // PotEvapo(transpi)ration from canopy / soil surface
   double eact; // actual evapotranspiration from tick()
   double epotc_w,epots_w,eact_w; // fluxes from cm/hr to W/m**2
	double lat_s; // latent heat at soil surface according to e.g. Nichols eq.7
   double temp_0; // soil temperature T(0) from soil_heat.h
   double LAI; // equals LAI_ in PM_bioclimate.C
   double h; // equals max height MxH in PM_bioclimate.C Converted from cm to m

	// variables in PM big leaf approach using Newton-Raphson for finding tsurf
   double netrad_pm,dnetrad_pm,netshortwave_pm,netlongwave_pm,gflux_pm;
   double dgflux_pm,hflux_pm,dhflux_pm,leflux_pm,dleflux_pm,rsc_pm,Ac_pm;
   double *fdt,*dfdt; // pointers to functions used in RT_SAFE
   double dt,tsurf_pm;
   double dt1,dt2,acc; // dt1 and dt2 are boundaries for valid solution within
   						  // NR iteration, acc is accuracy (read in daisy.par)

	// variables for solving dt directly, closure(dt)=netrad-le(dt)-h(dt)-g(dt)
   double a_dt,b_dt,c_dt,a_dt_wet,c_dt_wet,dt_dt,dt_dt_wet,tsurf_dt,le_dt;
   double le_dt_wet,h_dt,g_dt,closure_dt;

   int n_hr; // auxiliary variable for facilitating PG

  PT_PMSW (const AttributeList& al)
     : PT (al)
   	{
      n_hr=0;  // counter for output files
      les_tmp=-9999.0; // not in use


// open parameter file
	if ((fp_daisypar=fopen("daisy.par", "r"))==NULL)
   	{
   	printf("cannot open parameter file !!\n");
      exit(1);
      }

// open files for output
  	if ((fp_daisy=fopen("daisy.out", "w"))==NULL)
   	{
   	printf("cannot open output file\n");
      exit(1);
      }
  	if ((fp_rescheck=fopen("rescheck.out", "w"))==NULL)
   	{
   	printf("cannot open output file\n");
      exit(1);
      }
/*
  	if ((fp_ebal_pm=fopen("ebal_pm.out", "w"))==NULL)
   	{
   	printf("cannot open output file\n");
      exit(1);
      }
*/
// control files:
  	if ((fp_solcheck=fopen("solcheck.out", "w"))==NULL)
   	{
   	printf("cannot open output file\n");
      exit(1);
      }
  	if ((fp_axainv=fopen("axainv.out", "w"))==NULL)
   	{
   	printf("cannot open output file\n");
      exit(1);
      }
  	if ((fp_mat_a=fopen("mat_a.out", "w"))==NULL)
   	{
   	printf("cannot open output file\n");
      exit(1);
      }
      if ((fp_mat_b=fopen("mat_b.out", "w"))==NULL)
   	{
   	printf("cannot open output file\n");
      exit(1);
      }

      } // end PM_svat() implementation


// PM is called
//	pm.tick (time, weather, MxH, LAI_, soil, soil_water, soil_heat);


  double potential_transpiration () const
    { return /* potential_transpiration_ */ lel; }

  void output (Log& log, Filter& filter) const
    {
      PT::output (log, filter);
//      log.output ("potential_crop_transpiration",
//		  filter, potential_crop_transpiration, true);
//      log.output ("potential_soil_transpiration",
//  		  filter, potential_soil_transpiration, true);
    }
  void tick (const Weather& weather, const Vegetation& crops,
	     const Surface&, const Soil& soil, const SoilHeat& soil_heat,
	     const SoilWater& soil_water, const Pet& pet,
	     double /* canopy_ea */, double snow_ea, double pond_ea, double soil_ea)
       	{
      const double divide_ep = pet.wet () - snow_ea;
      const double canopy_ep = divide_ep * crops.cover ();
      const double pond_ep = divide_ep - canopy_ep;

      // Crop information
//	 printf("Crop information\n");
	 LAI =crops.LAI (); // Leaf Areal Index
         h   =0.01*crops.height (); // max crop height

// potential evapotranspiration from surface and canopy, from tick()
// pot.evap.above crop canopy [cm/hr]
			epotc=0.1*canopy_ep;
// pot evaporation from surface [cm/hr]
         epots=0.1*pond_ep;
// surface evaporation
         double evaps=0.1* (pond_ea + soil_ea); // soil evaporation

// actual evapotranspiration from tick()
			eact=0.1*soil_ea;

// convert epotc, epots and eact from cm/hr to W/m**2
			epotc_w=6800*epotc;
         epots_w=6800*epots;
         eact_w =6800*eact;
	 double evaps_w=6800*evaps;

// Check crop development: if either h or LAI (or both) are zero, do nothing
// otherwise: calculate resistances and then energy balance
	if (LAI > 0.0)
   	{
// communication with time.C
#if 0
			year = time.year();
         month = time.month();
         day = time.yday();
         hour = time.hour();
#endif
// communication with weather_hourly.C
//	printf("communication with weather_hourly.C\n");
         e_pa = weather.vapor_pressure (); // [Pa]
         tair = weather.hourly_air_temperature (); // [C]
         srad = weather.hourly_global_radiation (); // [W/m**2]
         u = weather.wind (); // [m/s]
         relsun = weather.cloudiness();  // [-]
         prec = 1.10*weather.rain(); // [mm] corrected by 10 %

// windspeed should not be zero
	   	if (u==0.0) u=0.1;

// convert e_pa [Pa] to e_abs [kg/m**3], Oke p.63
			VAPOR(tair, esta, esta_abs, desta, desta_abs);

// communication with soil_water.h
// Access simulated soil water content and soil temperature for validation
//	printf("communication with soil_water.h\n");
			theta_0	= soil_water.Theta(0); //  [0,-2.5]
			theta_5	= soil_water.Theta(1); //  [-2.5,-7.5]
			theta_10	= soil_water.Theta(2); //  [-7.5,-12.5]
			theta_15	= soil_water.Theta(3); //  [-12.5,-17.5]
			theta_20	= soil_water.Theta(4); //  [-17.5,-22.5]
			theta_25	= soil_water.Theta(5); //  [-22.5,-27.5]
			theta_30	= soil_water.Theta(6); //  [-27.5,-32.5]
			theta_35	= soil_water.Theta(7); // [-32.5,-37.5]
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
//	 printf("communication with soil_heat.h\n");
			temp_0	=soil_heat.T(0); // at depth z(0) = -1.25 cm [degrees C]

// communication with soil_water.h
// water flow positive to soil surface (LEs)
         les_q=6800.0*soil_water.q(0); // in W/m**2

// soil evap. is min[abs(-q0),epots]
// when water infiltrates, i.e. les_q < 0, soil evaporation equals E_PM_wet
// convert [cm/hr] to [W/m**2]: 1 cm/hr = 6800 W/m**2
         if (les_q < 0.0) les=evaps_w;
        		 else	if (les_q <= evaps_w) les=les_q;  // [W/m**2]
   					else les=evaps_w;  // [W/m**2]

// communication with soil.h
//	printf("communication with soil.h\n");
			kh=0.01*soil.heat_conductivity(0,theta_0); // should be around 1 W/m/C

// read parameterfile
			fscanf(fp_daisypar,"%lf%lf%lf%lf\n" 					// in RAA()
         						 "%lf%lf%lf\n"                   // in RAC()
                            "%lf\n"                         // in RAS()
                            "%lf%lf%lf%lf%lf%lf%lf%lf\n" 	// in RST()
                            "%lf%lf%lf%lf%lf\n"            // in NETRADBRUNT()
                            "%lf\n"                         // in GFLUX()
                            "%lf\n"                        // in ACOEFF()
                            "%lf%lf%lf\n",                 // in RT_SAFE()
                &ndif,&c_d,&z_0s,&z0_def,
                &w,&alpha_u,&arac,
                &alpha_k,
                &theta_w,&theta_c,&rcmin_const,&rcmax,&tref,&zeta,&f3const,&spar,
                &albedo,&b1,&b2,&b3,&b4,
                &z_sz,
                &alpha_r,
                &dt1,&dt2,&acc);

// convert from e_pa to e_abs
 			EPA2ABS(e_pa,tair,e_abs);

//      printf("past EPA2ABS() ");
// Calculate resistances

// aerodynamic resistance from mean source to reference height
   	RA(u,h,r_a);
//	   printf("past RA\n");
// r_aa following Daamen (1997)

        RAA (u,h,LAI,ndif,c_d,z_0s,z0_def,d_aa,z0_aa,uf_aa,r_aa);
//	   printf("past RAA\n");

			RAC (u,h,LAI,c_d,w,z_0s,alpha_u,arac,d_ac,X_ac,z0_ac,uh,r_ac);
//	   printf("past RAC\n");
// aerodynamic resistance between soil surface and mean source height
// r_scan in Daamen
			RAS(u,h,LAI,z_0s,alpha_k,c_d,k_h,r_as);
//	   printf("past RAS\n");
// mean stomatal resistance following Jacquemin & Noilhan (1990) and
// Verma et al.(1993)
			RSC (LAI,tair,srad,e_pa,theta_0_20,esta,theta_w,theta_c,rcmin_LAI,rcmax,
         zeta,f3const,tref,spar,eact_w,epotc_w,fpar,f_1,f_2,f_3,f_4,r_sc_1,
         r_tot_1,bf_temp,f_temp,f_def,f_theta,r_sc_2,r_tot_2,f_etep,r_sc_3,
         r_tot_3,r_sc_4,r_tot_4,r_sc_5,r_tot_5);
//	   printf("past RST\n");
// Net radiation by brunt's equation
      	NETRAD(srad,e_pa,tair,relsun,b1,b2,b3,b4,albedo,netlong_brunt,
         netlong_satt,netshort,netrad_brunt,netrad_satt);
//	   printf("past NETRADBRUNT\n");

// Compute matrix elements for assigning to A matrix and set all others to zero
			ACOEFF(tair,e_abs,netrad_brunt,LAI,les,temp_0,kh,r_aa,r_ac,r_as,z_sz,
         r_sc_5,alpha_r,a_11,a_12,a_13,b_1,a_24,a_25,b_2,a_31,a_33,a_34,a_35,b_3,
         a_41,a_42,b_4);

//	   printf("past ACOEFF\n");
// matrix definitions
//   printf("inside: matrix definitions");
			a=matrix(1,NP,1,NP); // a=A in Ax=b
			ai=matrix(1,NP,1,NP); // inverse matrix
			um=matrix(1,NP,1,NP); // unitary matrix
			b=matrix(1,NP,1,MP);  // b vector(s) in Ax=b
			x=matrix(1,NP,1,MP);  // aux. matrix
			t=matrix(1,NP,1,MP);  // aux matrix

   		n=5; // size of matrix 5 x 5
   		m=1; // number of solutions is 1, i.e. 1 solution vector b for
         	  // each computation

// read in A matrix values in :
// a[1][1]=a_11;
// a[1][2]=a_12 .... a[5][5]=a_55;

			a[1][1]=a_11;   // a_11 a_12 ... a_15
         a[1][2]=a_12;   // a_21 a_22 ... a_25
         a[1][3]=a_13;   //  ..
         a[1][4]=0;   // a_51 a_52 ... a_55
         a[1][5]=0;
         a[2][1]=0;
         a[2][2]=0;
         a[2][3]=0;
         a[2][4]=a_24;
         a[2][5]=a_25;
         a[3][1]=a_31;
         a[3][2]=0;
         a[3][3]=a_33;
         a[3][4]=a_34;
         a[3][5]=a_35;
         a[4][1]=a_41;
         a[4][2]=a_42;
         a[4][3]=0;
         a[4][4]=0;
         a[4][5]=0;
         a[5][1]=0;
         a[5][2]=0;
         a[5][3]=desta_abs;
         a[5][4]=0;
         a[5][5]=-1;
         a_53=a[5][3]; // is desta_abs

fprintf(fp_mat_a,"%5d\t%5d\t%5d\n"   				// write A matrix
	"%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\n"
	"%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\n"
   "%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\n"
   "%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\n"
   "%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\n\n",
   month,day,hour,a_11,a_12,a_13,a_14,a_15,a_21,a_22,a_23,a_24,a_25,a_31,a_32,
   a_33,a_34,a_35,a_41,a_42,a_43,a_44,a_45,a_51,a_52,a_53,a_54,a_55);

// read in b vector (matrix)
// b[1][1]=b_11 .. b[5][1]=b_51

         b[1][1]=b_1;
         b[2][1]=b_2;
         b[3][1]=b_3;
         b[4][1]=b_4;
         b[5][1]=(tair+273.15)*desta_abs-esta_abs;
			b_5=b[5][1];

                                                // write B vector
fprintf(fp_mat_b,"%5d\t%5d\t%5d\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\n",
   month,day,hour,b_1,b_2,b_3,b_4,b_5);

// save matrices for later testing of results
// printf("save matrices for later testing of results");
		for (l=1;l<=n;l++) {
			for (k=1;k<=n;k++) ai[k][l]=a[k][l]; // save a matrix in ai
			for (k=1;k<=m;k++) x[l][k]=b[l][k];  // save b matrix in x
		}  // end for

// invert matrix a
		gaussj(ai,n,x,m);

// check inverse
		for (k=1;k<=n;k++) {  // for_1
			for (l=1;l<=n;l++) {  // for_2
				um[k][l]=0.0;
				for (j=1;j<=n;j++)  // for_3
					um[k][l] += (a[k][j]*ai[j][l]);
			}  // end for_2
   		for (l=1;l<=n;l++) fprintf(fp_axainv,"%12.6f",um[k][l]);
			fprintf(fp_axainv,"\n");
			if (k%5==0) fprintf(fp_axainv,"\n");
		} // end for_1

// check vector solutions */
		for (l=1;l<=m;l++) {  // for_1
			for (k=1;k<=n;k++) {  // for_2
				t[k][l]=0.0;
				for (j=1;j<=n;j++)
					t[k][l] += (a[k][j]*x[j][l]);
               fprintf(fp_solcheck,"%d\t%d\t%d\t%10.2f\t%10.2f\t%10.2f\n",
               month,day,hour,b[k][l],t[k][l],b[k][l]-t[k][l]);
               if (k%5==0) fprintf(fp_solcheck,"\n");
         } // end for_2
        } // end for_1

// write solution vector
		tcan=x[1][1]-273.15;  // in degrees C
		tsurf=x[2][1]-273.15; // in degrees C
		tleaf=x[3][1]-273.15; // in degrees C
 	 	e_c_abs=x[4][1];  // e_c in kg/m**3
		e_sl_abs=x[5][1]; // e_sl in kg/m**3

// calculate energy balance for sparse crops
		GFLUX(tsurf,kh,temp_0,z_sz,gflux);  // ground heat flux
		LEHFLUX(tair,tsurf,tcan,tleaf,r_aa,r_ac,r_as,r_sc_5,e_c_abs,e_sl_abs,e_abs,
      les,hl,ha,hs,lea,lel,hclos,leclos,dtcta,dtltc,dtstc,dtlta);
      AVENER(netrad_brunt,LAI,alpha_r,gflux,av_s,av_l);  // available energy

// convert vapor pressure from kg/m**3 to Pa
  	 	EABS2PA(e_c_abs,tcan,e_c);  // at canopy temperature
     	EABS2PA(e_sl_abs,tleaf,e_sl); // at leaf temperature

// solve directly for dt by closure(dt)=netrad-le(dt)-h(dt)-g(dt)
 		EBAL_DT(tair,e_pa,netrad_brunt,esta,desta,kh,temp_0,z_sz,r_aa,r_ac,r_sc_5,
      a_dt,b_dt,c_dt,a_dt_wet,c_dt_wet,dt_dt,dt_dt_wet,tsurf_dt,g_dt,h_dt,le_dt,
      le_dt_wet,closure_dt);

// print results
      n_hr++;

fprintf(fp_daisy,"%5d\t%5d\t%5d\t%5d\t"     // 69 var
	"%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
	"%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
	"%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
   "%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
 	"%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
   "%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
   "%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
   "%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
   "%7.2g\n",
 	n_hr,month,day,hour,e_pa,relsun,tair,srad,u,prec,netrad_brunt,kh,les,h,LAI,
   r_aa,r_as,r_ac,r_sc_5,fpar,f_1,f_2,f_3,f_4,r_tot_1,tcan,tsurf,tleaf,e_c,e_sl,
   ha,hl,hs,lea,lel,gflux,hclos,leclos,dtcta,dtltc,dtstc,dtlta,esta,desta,ests,
   av_s,av_l,dt_dt,tsurf_dt,r_sc_dt,le_dt,h_dt,g_dt,theta_0,theta_5,theta_10,
   theta_15,theta_20,theta_30,theta_40,theta_50,theta_0_20,theta_0_50,
   theta_0_100,temp_0,les_tmp,les_q,epotc_w,eact_w);

fprintf(fp_rescheck,"%5d\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
	"%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\t"
   "%7.2g\t%7.2g\t%7.2g\t%7.2g\t%7.2g\n",
	n_hr,f_1,f_2,f_3,f_4,f_temp,f_def,f_theta,r_tot_1,r_tot_2,r_sc_1,r_sc_2,
   f_etep,r_sc_3,r_tot_3,r_sc_4,r_tot_4,r_a,r_aa,r_sc_5,r_tot_5);  // 21 var

	} else
   		{
      	if (n_hr % 10000 == 0) printf("no crop development yet....\n");
      	} // end if

         } // end tick()

   ~PT_PMSW() // destructor
   	{
      fclose(fp_daisypar);
      fclose(fp_rescheck);
      fclose(fp_daisy);
      fclose(fp_solcheck);
      fclose(fp_axainv);
      fclose(fp_mat_a);
      fclose(fp_mat_b);
//    fclose(fp_ebal_pm);
     	free_matrix(t,1,NP,1,MP);
		free_matrix(x,1,NP,1,MP);
		free_matrix(b,1,NP,1,MP);
		free_matrix(um,1,NP,1,NP);
		free_matrix(ai,1,NP,1,NP);
		free_matrix(a,1,NP,1,NP);
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
#if 0
      syntax.add ("potential_crop_transpiration", "mm/h", Syntax::LogOnly,
		  "Potential canopy evapotranspiration not satified by \
intercepted water");
      syntax.add ("potential_soil_transpiration", "mm/h", Syntax::LogOnly,
		  "Unsatisfied potential soil evaporation transmogriffed \
into transpiration");
#endif
      Librarian<PT>::add_type ("PMSW", alist, syntax, &make);
    }
} PT_PMSW_syntax;
