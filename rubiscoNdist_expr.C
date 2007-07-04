// rubiscoNdist_expr.C -- Individual expression of rubisco N distribution model
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
#include "block.h"
#include "syntax.h"
#include <sstream>
#include "check.h"
#include "librarian.h"
#include "number.h"
#include "scope_exchange.h"

static const double Mw = 14.0; //The molecular weight for N [g mol¯1]
static const symbol LAI_symbol = symbol("LAI");
static const symbol height_symbol = symbol("height"); // [cm]
static const symbol relative_LAI_symbol = symbol("relative_LAI");
static const symbol relative_height_symbol = symbol("relative_height");
static const symbol DS_symbol = symbol("DS");

struct rubiscoNdist_expr : public RubiscoNdist
{
  // Parameters.
private:
  const double f_photo; //Fraction of photosynthetically active N in canopy
  const std::auto_ptr<Number> expr;
  ScopeExchange scope;
  
  // Simulation.
  void tick (std::vector <double>& , std::vector <double>& , 
	     const double , Treelog& msg)
  { 
    expr->tick (scope, msg);
  }

  double function (const double height, const double LAI, 
		       const double relative_LAI, const double relative_height, 
		       const double DS);
  double integral (const double total_LAI, const std::vector <double>& PAR_height, 
		   const double DS, const int No);
  void rubiscoN_distribution (const std::vector <double>& PAR_height,
			      const double LAI, const double DS, 
			      std::vector <double>& rubiscoNdist, 
			      const double cropN/*[g]*/, Treelog& msg);
  void output (Log&) const
  { }
  const AttributeList& default_model ();

  // Create.
  public:
  rubiscoNdist_expr (Block& al)
    : RubiscoNdist (al),
      f_photo (al.number ("f_photo")),
      expr (Librarian::build_item<Number> (al, "value"))
  {
    scope.add_item (new ExchangeNumber (LAI_symbol, Syntax::None(),
					"Leaf area index"));
    scope.add_item (new ExchangeNumber (height_symbol, "cm",
					"Height of canopy"));
    scope.add_item (new ExchangeNumber (relative_LAI_symbol, Syntax::None(),
					"Relative leaf area index"));
    scope.add_item (new ExchangeNumber (relative_height_symbol, Syntax::None(),
					"Relative height of canopy"));
    scope.add_item (new ExchangeNumber (DS_symbol, Syntax::None(),
					"Development stage"));
    scope.done ();
    expr->initialize (al.msg());
    if (!expr->check (scope, al.msg()))
      al.error("Invalid expression of rubisco expr");
  }
};

double
rubiscoNdist_expr::function (const double height, const double LAI, 
			     const double relative_LAI, 
			     const double relative_height, const double DS)
{
  scope.set_number (height_symbol, height);
  scope.set_number (relative_height_symbol, relative_height);
  scope.set_number (LAI_symbol, LAI);
  scope.set_number (relative_LAI_symbol, relative_LAI);
  scope.set_number (DS_symbol, DS);
  if(expr->missing (scope))
    throw "Missing value in rubisco expr";
  return  expr->value (scope); 
}

double
rubiscoNdist_expr::integral (const double total_LAI, 
			     const std::vector <double>& PAR_height, 
			     const double DS, const int No)
{
  const double total_height = PAR_height[0];
  const double dLAI = total_LAI / No;
  double sum = 0.0;

  for (int i = 0; i < No; i++)
    {
      const double relative_LAI = (i + 0.5)/(No + 0.0);  
      const double height_i = (PAR_height [i]- PAR_height[i+1])/2.0;
      const double relative_height = height_i/total_height;
      const double LAI_i = total_LAI * (i + 0.5)/(No + 0.0);
      sum+= function(height_i, LAI_i, relative_LAI, relative_height, DS) * dLAI; //[leaf area per soil area]
    }
  return sum; 
}

void
rubiscoNdist_expr::rubiscoN_distribution (const std::vector <double>& PAR_height, 
					  const double LAI, const double DS,
					  std::vector <double>& rubiscoNdist/*[mol/m²]*/,  
					  const double cropN /*[g/m²area]*/, Treelog&)
{
  daisy_assert (std::isfinite (cropN));
  daisy_assert (cropN >= 0.0);

  // Rubisco N in top of the canopy:
  const int No = rubiscoNdist.size ();
  const double divisor = integral(LAI, PAR_height, DS, No);

  daisy_assert(divisor > 0.0);
  daisy_assert (std::isnormal(divisor));

  const double total_height = PAR_height[0];
  double cropN0 = cropN / divisor; // [g/m² leaf]
  cropN0 = cropN0 / Mw;  // [mol/m² leaf]
  daisy_assert (cropN0 >= 0.0);

  // Fill photosynthetically active rubisco N (cummulative) for each canopy layer in vector

  for (int i = 0; i < No; i++)
    {
      const double relative_LAI = (i + 0.5)/(No + 0.0);  
      const double height_i = (PAR_height [i]- PAR_height[i+1])/2.0;
      const double relative_height = height_i/total_height;
      const double LAI_i = LAI * (i + 0.5)/(No + 0.0);
      rubiscoNdist[i] = f_photo * cropN0 * 
	function(height_i, LAI_i, relative_LAI, relative_height, DS); //[mol/m² leaf]
    }
}

static struct rubiscoNdist_exprSyntax
{
  static Model& make (Block& al)
  { return *new rubiscoNdist_expr (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add ("f_photo", Syntax::None (), Check::positive (), Syntax::Const,
                "Fraction of photosynthetically active N in canopy. According to (Boegh et al., 2002) f_photo = 0.75. However, non-functional N is already substracted from leaf-N in the cropN_std module, therefore f_photo = 1.0 as default.");
    alist.add ("f_photo", 1.0);

    syntax.add_object ("value", Number::component, 
                       Syntax::Const, Syntax::Singleton, "\
Expression that evaluates to the relative rubisco N intesity where 1 is the value in top of the canopy.");
    syntax.order ("value");
  }  

  rubiscoNdist_exprSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "expr rubisco N-distribution model in the canopy.");

    load_syntax (syntax, alist);
    Librarian::add_type (RubiscoNdist::component, "expr", alist, syntax, &make);
  }
} rubiscoNdist_exprsyntax;


