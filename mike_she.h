// mike_she.h

#ifndef MIKE_SHE_H
#define MIKE_SHE_H

#include <string>
#include <vector>

struct Syntax;
struct AttributeList;
struct Time;
struct Geometry;

class MikeSHE
{
  // Parameters.
private:
  struct Implementation;
  Implementation& impl;

  // Action.
public:
  void receive ();		// Receive data from AD and WM.
  void select (const string& name); // Select a column to work on.
  void send ();			// Send data to AD and WM.

  // Communication with SoilWater.
  void get_water_pressure (vector<double>& h) const;
  void put_water_sink (const Geometry&, const vector<double>& S);

  // Communication with SoilNO3.
  void get_no3_m (vector<double>& M) const;
  void put_no3_m (const vector<double>& M);

  // Communication with Snow.
  void put_snow_height (double mm);

  // Communication with Weather.
  double get_precipitation () const;
  double get_air_temperature () const;
  double get_reference_evapotranspiration () const;

  // Communication with Bioclimate.
  void put_evap_interception (double EvapInterception);
  void put_intercepted_water (double intercepted_water);
  void put_net_precipitation (double NetPrecipitation);

  // Communication with Surface.
  void put_evap_soil_surface (double EvapSoilSurface);
  void put_evap_pond (double);
  double get_ponding () const;
  double get_surface_no3 () const;
  void put_surface_no3 (double No3);
  
  // Generic
  bool done ();

  // Create and destroy.
public:
  static void load_syntax (Syntax&, AttributeList&);
  MikeSHE (const AttributeList&, const Time&);
  ~MikeSHE ();
};

extern MikeSHE* mike_she;

#endif
