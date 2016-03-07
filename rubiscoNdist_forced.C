// rubiscoNdist_forced.C -- Forced expression of rubisco Capacity model
// 
// Copyright 2006,2007 Birgitte Gjettermann and KVL
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
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#define BUILD_DLL
#include "rubiscoNdist.h"
#include "mathlib.h"
#include "block_model.h"
#include <sstream>
#include "check.h"
#include "librarian.h"
#include "number.h"
#include "scope_exchange.h"
#include "frame.h"

// static const double Mw = 14.0; //The molecular weight for N [g mol¯1]
static const symbol LAI_symbol = symbol("LAI");
static const symbol distance_from_top_symbol = symbol("distance_from_top"); // [cm]
static const symbol relative_LAI_symbol = symbol("relative_LAI");
static const symbol relative_distance_from_top_symbol = symbol("relative_distance_from_top");
static const symbol DS_symbol = symbol("DS");

struct rubiscoNdist_forced : public RubiscoNdist
{
  static const symbol mol_per_area;

  // Parameters.
private:
  const std::unique_ptr<Number> expr;
  ScopeExchange scope;
  
  // Simulation.
  double function (const Units&, const double distance_from_top, 
		   const double LAI, const double relative_LAI, 
		   const double relative_distance_from_top, const double DS,
		   Treelog& msg);
  void rubiscoN_distribution (const Units& units,
                              const std::vector <double>& PAR_height,
			      const double LAI, const double DS, 
			      std::vector <double>& rubiscoNdist, 
			      const double cropN/*[g]*/, Treelog& msg);
  void output (Log&) const
  { }

  // Create.
public:
  rubiscoNdist_forced (const BlockModel& al)
    : RubiscoNdist (al),
      expr (Librarian::build_item<Number> (al, "value")),
      scope (__FUNCTION__)
  {
    scope.add_item (new ExchangeNumber (LAI_symbol, Attribute::None(),
					"Leaf area index"));
    scope.add_item (new ExchangeNumber (distance_from_top_symbol, "cm",
					"Distance_From_Top of canopy"));
    scope.add_item (new ExchangeNumber (relative_LAI_symbol, Attribute::None(),
					"Relative leaf area index"));
    scope.add_item (new ExchangeNumber (relative_distance_from_top_symbol, Attribute::None(),
					"Relative distance_from_top of canopy"));
    scope.add_item (new ExchangeNumber (DS_symbol, Attribute::None(),
					"Development stage"));
    scope.done ();
    expr->initialize (al.units (), scope, al.msg());
    if (!expr->check_dim (al.units (), scope, mol_per_area, al.msg ()))
      al.error("Invalid expression of rubisco expr");
  }
};

const symbol rubiscoNdist_forced::mol_per_area ("mol/m^2");

double
rubiscoNdist_forced::function (const Units& units,
                               const double distance_from_top,
			       const double LAI, const double relative_LAI, 
			       const double relative_distance_from_top,
			       const double DS, Treelog& msg)
{
  scope.set (distance_from_top_symbol, distance_from_top);
  scope.set (relative_distance_from_top_symbol, relative_distance_from_top);
  scope.set (LAI_symbol, LAI);
  scope.set (relative_LAI_symbol, relative_LAI);
  scope.set (DS_symbol, DS);
  double value = -1.0;
  if (!expr->tick_value (units, value, mol_per_area, scope, msg))
    throw "Missing value in rubisco forced expr";
  return  value; 
}

void
rubiscoNdist_forced
/**/ ::rubiscoN_distribution (const Units& units,
                              const std::vector <double>& PAR_height, 
                              const double LAI, const double DS,
                              std::vector <double>& rubiscoNdist/*[mol/m²]*/,  
                              const double cropN /*[g/m²area]*/, 
                              Treelog& msg)
{
  daisy_assert (std::isfinite (cropN));
  daisy_assert (cropN >= 0.0);

  // Number of layers
  const int No = rubiscoNdist.size ();
  const double total_height = PAR_height[0];

  // Fill rubisco capacity for each canopy layer in vector
  for (int i = 0; i < No; i++)
    {
      const double relative_LAI = (i + 0.5)/(No + 0.0);  
      const double distance_from_top_i = 
	total_height-((PAR_height [i]+ PAR_height[i+1])/2.0);
      const double relative_distance_from_top = distance_from_top_i/total_height;
      const double LAI_i = LAI * (i + 0.5)/(No + 0.0);
      rubiscoNdist[i] = function(units,
                                 distance_from_top_i, LAI_i, relative_LAI,
				 relative_distance_from_top, DS, msg); //[mol/m² leaf]
    }
}

static struct rubiscoNdist_forcedSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new rubiscoNdist_forced (al); }

  rubiscoNdist_forcedSyntax ()
    : DeclareModel (RubiscoNdist::component, "forced", 
	       "Forced rubisco capacity distribution model in the canopy.")
  { }
  void load_frame (Frame& frame) const
  {

    frame.declare_object ("value", Number::component, 
                       Attribute::Const, Attribute::Singleton, "\
Expression that evaluates to the relative rubisco capacity where 1 is the value in top of the canopy.");
    frame.order ("value");

  }
} rubiscoNdist_forcedsyntax;


