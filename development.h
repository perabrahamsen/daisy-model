// development.h -- Default crop development submodel.
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

#ifndef DEVELOPMENT_H
#define DEVELOPMENT_H

#include <string>
using namespace std;

class AttributeList;
class Syntax;
class Log;
class PLF;
class CrpAux;
class Vernalization;

class Development 
{
  // Parameters.
private:
    double EmrTSum;	   // Soil temp sum at emergence
    const PLF& EmrSMF;     // Soil moisture effect on emergence
    double DS_Emr;	   // Development stage (DS) emergence
    double DSRate1;	   // Development rate [C-1 or d-1],
    			   // the vegetative stage
    double DSRate2;	   // Development rate [C-1 or d-1],
			   // the reproductive stage
    const PLF& TempEff1;   // Temperature effect, vegetative stage
    const PLF& TempEff2;   // Temperature effect, reproductive stage
    const PLF& PhotEff1;   // Ptotoperiode effect, vegetative stage defi. limit

    double DSMature;	   // DS at maturation
    double DSRepeat;	   // DS where DS is set back (perennial crops)
    double DSSetBack; 	   // DS set back at DSRepeat
    double defined_until_ds; // Model invalid after this DS.

  // State.
public:
    double DS;	        	// Development Stage
    double partial_day_length;	// Light hours this day until now [0-24 h]
    double day_length;		// Light hours previous day. [0-24 h]
    double partial_soil_temperature; // Accumaleted soil temperature. [°C]
    double soil_temperature;	// Soil temperature previous day. [°C]
    double soil_h;		// Soil potential [cm]

  // Simulation.
public:
  void light_hour ();
  void tick_daily (const string& name, double Ta, double WLeaf, 
		   CrpAux&, Vernalization&);
  void emergence ();
  void output (Log& log) const;

  // Create and Destroy.
public:
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  Development (const AttributeList&);
  ~Development ();
};

#endif // DEVELOPMENT_H
