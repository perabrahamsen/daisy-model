// mike_she.C
//
//    EXCHANGE DATA BETWEEN WM AND DAISY
//    1. RECEIVE DATA FROM WM TO DAISY
//    2. DAISY ACTION
//    3. SEND DATA FROM DAISY TO WM

#include "mike_she.h"
#include "syntax.h"
#include "alist.h"
#include "time.h"
#include "common.h"
extern "C"
{
#include <f2c.h>
#include "mshe/ff_write_read.P"
#include "mshe/mshedaisycoup.P"
#include "mshe/prdebug.P"
extern int atoi(const char*);
}

#include <assert.h>


// FORTRAN constants.
/* const */ logical c_true = TRUE_;
/* const */ logical c_false = FALSE_;

static /* const */ integer nine = 9;

MikeSHE* mike_she = NULL;

struct MikeSHE::Implementation
{ 
  // Parameters.
  /* const */ integer nlay;		
  /* const */ integer ncol;		
  
  int lay (int i)
    // I hope this is right.
  { return column * nlay + i; } 

  // Data.
  real time;			// hours after start [h]
  real *const cevapo;		// evaporation from canopy water [m/s]
  real *const olconc;		// Overland concentration[g/m**2]
  real *const precip;		// precipitation [m/s]
  real *const cansto;		// canopy storage [m]
  real *const pevapo;		// evaporation from ponded water [m/s]
  real *const sevapo;		// soil evaporation [m/s]
  real *const uzconc;		// uz concentrations [g/m**3]
  real *const doc;		// overland water depth [m]
  real *const phi;		// UZ potential [m]
  real *const netprec;		// net precipitation [m/s]
  real *const airtemp;			// air temperature [C]
  real *const ruptake;		// root uptake of water [m/s]
  real *const snowsto;		// snow storage [m]
  real *const evapo;		// potential evapotranspiration [m/s]

  // Communication.
  logical lstopsig;		// stop signal from fifo exchange
  integer iuerr;
  integer ifpdaisyad;		// fifo file pointer for ??
  integer ifpaddaisy;		// fifo file pointer for ??
  integer ifpdaisywm;		// fifo file pointer for ??
  integer ifpwmdaisy;		// fifo file pointer for ??
  logical lok;
  logical ldatamode;
  logical ldtransmode;

  // State.
  int column;			// Current working column.
  bool mshe_stop;		// End of simulation.

  // Actions.
  void receive ();
  void send ();
  void select (const string& name);

  // Create and destroy.
  Implementation (int nlay_, int ncol_, 
		  const char* name, int runnumber, const Time& start);
  ~Implementation ();
};

MikeSHE::Implementation::Implementation (int nlay_, int ncol_, 
					 const char* name,
					 const int runnumber,
					 const Time& start)
  : nlay (nlay_),
    ncol (ncol_),
    cevapo (new real[ncol]),
    olconc (new real[ncol]),
    precip (new real[ncol]),
    cansto (new real[ncol]),
    pevapo (new real[ncol]),
    sevapo (new real[ncol]),
    uzconc (new real[ncol * nlay]),
    doc (new real[ncol]),
    phi (new real[ncol * nlay]),
    netprec (new real[ncol]),
    airtemp (new real[ncol]),
    ruptake (new real[ncol * nlay]),
    snowsto (new real[ncol]),
    evapo (new real[ncol]),
    iuerr (6),
    lok (FALSE_),
    ldatamode (TRUE_),
    column (-1),
    mshe_stop (false)
{ 
  // Create a space padded array of length 200, starting with name.
  char setupname[200];
  for (int i = 0; i < 200; i++)
    setupname[i] = ' ';
  strncpy (setupname, name, strlen (name));
  
  // Initialization of fifofiles
  if (!read_init0__(&iuerr, &ifpwmdaisy, &ifpdaisywm, "daisy", "wm",
		    &lstopsig, setupname, &nine, 5L, 2L, 200L)) 
    {
      THROW ("Couldn't init wm FIFO.");
    }
  if (!read_init0__(&iuerr, &ifpaddaisy, &ifpdaisyad, "daisy", "ad",
		    &lstopsig, setupname, &nine, 5L, 2L, 200L)) 
    {
      THROW ("Couldn't init ad FIFO.");
    }

  integer runnumber_she;
  integer starttime[5];

  if (!daisywmcoupreceive_init2__(&iuerr, &ifpwmdaisy, &ifpdaisywm, 
				  starttime, &runnumber_she,
				  &lstopsig, &nine)) 
    {
      THROW ("Couldn't init2 wm.");
    }
  if (runnumber_she != runnumber)
    {
      cerr << "Runnumber mismatch: " << runnumber
	   << " != " << runnumber_she << "\n";
    }
  if (starttime[0] != start.year ()
      || starttime[1] != start.month ()
      || starttime[2] != start.mday ()
      || starttime[3] != start.hour ()
      || starttime[4] != 0)
    {
      cerr << "Date mismatch: " << starttime[0] << "-" << starttime[1] << "-" 
	   << starttime[2] << " " << starttime[3] << ":" << starttime[4] 
	   << " != " << start.year () << "-" << start.month () << "-" 
	   << start.mday () << " " << start.hour () << ":00\n";
    }
}

void
MikeSHE::Implementation::receive ()
{
  // Receive data from WM to Daisy.
  if (!wmdaisycoup_dreceive__(&iuerr, &ncol, &nlay,
			      &ifpwmdaisy, &ifpdaisywm, &lstopsig, 
			      &time, doc, phi, precip, evapo, airtemp,
			      &ldtransmode, &nine))
    {
      THROW ("Couldn't receive data from WM.");
    }

  if (!ldtransmode)
    {
      mshe_stop = true;
      return;
    }

  // Receive data from AD to Daisy.
  if (!addaisycoup_dreceive__(&iuerr, &ncol, &nlay, 
			      &ifpaddaisy, &ifpdaisyad, &lstopsig, 
			      uzconc, olconc, 
			      &nine)) 
    {
      THROW ("Couldn't receive data from WM.");
    }

  column = -1;
}

void
MikeSHE::Implementation::select (const string& name)
{
  const char* s = name.c_str ();
  column = atoi (s + 1) - 1;
  delete s;
}

void
MikeSHE::Implementation::send ()
{
  // Send data from DAISY to WM.
  if (!daisywmcoup_dsend__(&iuerr, &ncol, &nlay,
			   &ifpwmdaisy, &ifpdaisywm, &lstopsig, 
			   ruptake, sevapo, pevapo, 
			   cevapo, cansto, snowsto, netprec, 
			   &nine)) 
    {
      THROW ("Couldn't send data to WM.");
    }

  // Send data from DAISY to AD
  if (!daisyadcoup_dsend__(&iuerr, &ncol, &nlay,
			   &ifpaddaisy, &ifpdaisyad, &lstopsig, 
			   uzconc, olconc,
			   &nine))
    {
      THROW ("Couldn't send data to AD.");
    }
}


MikeSHE::Implementation::~Implementation ()
{
  delete [] cevapo;
  delete [] olconc;
  delete [] precip;
  delete [] cansto;
  delete [] pevapo;
  delete [] sevapo;
  delete [] uzconc;
  delete [] doc;
  delete [] phi;
  delete [] netprec;
  delete [] airtemp;
  delete [] ruptake;
  delete [] snowsto;
  delete [] evapo;
}

void 
MikeSHE::load_syntax (Syntax& syntax, AttributeList&)
{
  syntax.add ("ncol", Syntax::Integer, Syntax::Const);
  syntax.add ("nlay", Syntax::Integer, Syntax::Const);
  syntax.add ("setupname", Syntax::String, Syntax::Const);
  syntax.add ("runnumber", Syntax::Integer, Syntax::Const);
}

MikeSHE::MikeSHE (const AttributeList& al, const Time& time)
  : impl (*new Implementation (al.integer ("nlay"), 
			       al.integer ("ncol"),
			       al.name ("setupname").c_str (),
			       al.integer ("runnumber"),
			       time))
{ }

void
MikeSHE::receive ()
{ impl.receive (); }

void
MikeSHE::send ()
{ impl.send (); }

void
MikeSHE::select (const string& name)
{ impl.select (name); }

// Communication with SoilWater.
void 
MikeSHE::get_water_pressure (vector<double>& h) const
{ 
  assert (h.size () == impl.nlay + 0UL);
  for (int i = 0; i < impl.nlay; i++)
    h[i] = impl.phi[impl.lay (i)] * 100.0;	// m -> cm
}

void 
MikeSHE::put_water_sink (const vector<double>& S)
{   
  assert (S.size () == impl.nlay + 0UL);
  for (int i = 0; i < impl.nlay; i++)
    {
      assert (S[i] == 0.0);
      impl.ruptake[impl.lay (i)] = S[i] / 100.0; // cm/h -> m/h
    }
}

// Communication with SoilNO3.
void 
MikeSHE::get_no3_m (vector<double>& M) const
{ 
  assert (M.size () == impl.nlay + 0UL);
  for (int i = 0; i < impl.nlay; i++)
    M[i]
      = impl.uzconc[impl.lay (i)] * (100.0 * 100.0 * 100.0); // g/m³ -> g/cm³
}

void 
MikeSHE::put_no3_m (const vector<double>& M)
{ 
  assert (M.size () == impl.nlay + 0UL);
  for (int i = 0; i < impl.nlay; i++)
    impl.uzconc[impl.lay (i)]
      = M[i] / (100.0 * 100.0 * 100.0); // g/cm³ -> g/m³
}

// Communication with Snow.
void 
MikeSHE::put_snow_height (double mm)
{ 
  impl.snowsto[impl.column] = mm / 1000.0; // mm -> m
}

// Communication with Weather.
double 
MikeSHE::get_precipitation () const
{ 
  return impl.precip[impl.column] * 1000.0 / (3600.0 * 24.0); // m/s -> mm/d
}
double 
MikeSHE::get_air_temperature () const
{ 
  assert (impl.airtemp[impl.column] < 50.0);

  return impl.airtemp[impl.column];
}
double 
MikeSHE::get_reference_evapotranspiration () const
{
  const double e =  impl.evapo[impl.column] * 1000.0 / (3600.0 * 24.0); 
  assert (e < 1000.0);
  return e; // m/s -> mm/d
}

// Communication with Bioclimate.
void
MikeSHE::put_evap_interception (double EvapInterception)
{ 
  assert (EvapInterception >= 0.0);
  impl.cevapo[impl.column] = EvapInterception * 3600.0 / 1000.0; // mm/h -> m/s
}
void 
MikeSHE::put_intercepted_water (double intercepted_water)
{ 
  assert (intercepted_water >= 0.0);
  impl.cansto[impl.column] = intercepted_water / 1000.0; // mm -> m
}

void
MikeSHE::put_net_precipitation (double NetPrecipitation)
{
  assert (NetPrecipitation >= 0.0);
  impl.netprec[impl.column] 
    = NetPrecipitation * 3600.0 / 1000.0; // mm/h -> m/s
}

// Communication with Surface.
void 
MikeSHE::put_evap_soil_surface (double EvapSoilSurface)
{ 
  assert (EvapSoilSurface >= 0.0);
  impl.sevapo[impl.column] = EvapSoilSurface * 3600.0 / 1000.0;	// mm/h -> m/s
}

void 
MikeSHE::put_evap_pond (double EvapPond)
{ 
  assert (EvapPond >= 0.0);
  impl.pevapo[impl.column] = EvapPond * 3600.0 / 1000.0;	// mm/h -> m/s
}

double
MikeSHE::get_ponding () const
{
  return impl.doc[impl.column] * 1000.0; // m -> mm
}

double
MikeSHE::get_surface_no3 () const
{
  return impl.olconc[impl.column] * (100.0 * 100.0); // g/m² -> g/cm²
}

void
MikeSHE::put_surface_no3 (double NO3)
{
  impl.olconc[impl.column] = NO3 / (100.0 * 100.0); // g/cm² -> g/m²
}

// Done

bool 
MikeSHE::done ()
{
  return impl.mshe_stop;
}

// BUG: olconc not included

MikeSHE::~MikeSHE ()
{
  delete &impl;
}

