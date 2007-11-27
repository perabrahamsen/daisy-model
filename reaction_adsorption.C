// reaction_adsorption.C -- Equilibrium between sorbed and solute.
// 
// Copyright 2004, 2007 Per Abrahamsen and KVL.
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

#define BUILD_DLL

#include "reaction.h"
#include "block.h"
#include "number.h"
#include "adsorption.h"
#include "chemistry.h"
#include "chemical.h"
#include "geometry.h"
#include "soil.h"
#include "soil_water.h"
#include "scope_soil.h"
#include "units.h"
#include "log.h"
#include "assertion.h"
#include "librarian.h"
#include "mathlib.h"
#include <memory>

struct ReactionAdsorption : public Reaction
{
  // Parameters.
  const symbol name_solute;
  const symbol name_sorbed;
  const std::auto_ptr<Adsorption> equilibrium;
  const std::auto_ptr<Number> adsorption_rate;
  const std::auto_ptr<Number> desorption_rate;
  
  // Output.
  std::vector<double> adsorption_source;
  void output (Log& log) const
  { output_variable (adsorption_source, log); }

  // Simulation.
  void tick (const Geometry& geo, const Soil& soil,
	     const SoilWater& soil_water, const SoilHeat& soil_heat, 
	     const OrganicMatter&, Chemistry& chemistry,
	     const double dt, Treelog& msg)
  { 
    const size_t cell_size = geo.cell_size ();
    Chemical& solute = chemistry.find (name_solute);
    Chemical& sorbed = chemistry.find (name_sorbed);
    
    ScopeSoil scope (soil, soil_water, soil_heat);
    for (size_t c = 0; c < cell_size; c++)
      { 
	scope.set_cell (c);
	const double Theta = soil_water.Theta (c);
	const double has_solute = solute.M (c);
	const double has_sorbed = sorbed.M (c);
	const double has_M = has_solute + has_sorbed;
	const double want_C = equilibrium->M_to_C (soil, Theta, c, has_M);
	const double want_solute = want_C * Theta;
	const double want_sorbed = has_M - want_solute;

	daisy_assert (approximate (has_solute + has_sorbed, 
				   want_solute + want_sorbed));
	
	double convert = 0.0;

	if (has_solute > want_solute)
	  {
	    if (!adsorption_rate->tick_value (convert,
					      Units::per_h (), scope, msg))
	      msg.error ("Could not evaluate 'adsorption_rate'");
	    
	    
	    if (convert >= 1.0)
	      convert = (has_solute - want_solute);
	    else
	      convert *= (has_solute - want_solute);
	    
	    const double left = solute.M_left (c, dt);
	    if (convert > left)
	      convert = left * 0.99;
	  }
	else
	  {
	    if (!desorption_rate->tick_value (convert,
					      Units::per_h (), scope, msg))
	      msg.error ("Could not evaluate 'desorption_rate'");
	    
	    if (convert >= 1.0)
	      convert = (has_sorbed - want_sorbed);
	    else
	      convert *= (has_sorbed - want_sorbed);
	    
	    const double left = sorbed.M_left (c, dt);
	    if (convert > left)
	      convert = left * 0.99;
	    
	    convert *= -1.0;
	  }
      
      adsorption_source[c] = convert;
	
    }
    solute.add_to_sorption_sink (adsorption_source, dt);
    sorbed.add_to_sorption_source (adsorption_source, dt);
  }

  // Create.
  bool check (const Soil& soil, const SoilWater& soil_water, 
	      const SoilHeat& soil_heat,
	      const Chemistry& chemistry, Treelog& msg) const
  { 
    bool ok = true;
    if (!chemistry.know (name_solute))
      {
        msg.error ("'" + name_solute.name () + "' not traced");
        ok = false;
      }
    if (!chemistry.know (name_sorbed))
      {
        msg.error ("'" + name_sorbed.name () + "' not traced");
        ok = false;
      }
    ScopeSoil scope (soil, soil_water, soil_heat);
    if (!adsorption_rate->check_dim (scope, Units::per_h (), msg))
      ok = false;
    if (!desorption_rate->check_dim (scope, Units::per_h (), msg))
      ok = false;

    return ok;
  }
  void initialize (const Soil& soil, Treelog& msg)
  { 
    adsorption_source.insert (adsorption_source.begin (), soil.size (), 0.0);
    daisy_assert (adsorption_source.size () == soil.size ());
    adsorption_rate->initialize (msg); 
    desorption_rate->initialize (msg); 
  }
  explicit ReactionAdsorption (Block& al)
    : Reaction (al),
      name_solute (al.identifier ("solute")),
      name_sorbed (al.identifier ("sorbed")),
      equilibrium (Librarian::build_item<Adsorption> (al, "equilibrium")),
      adsorption_rate (Librarian::build_item<Number> (al, "adsorption_rate")),
      desorption_rate (al.check ("desorption_rate")
	    ? Librarian::build_item<Number> (al, "desorption_rate")
	    : Librarian::build_item<Number> (al, "adsorption_rate"))
  { }
};

static struct ReactionAdsorptionSyntax
{
  static Model& make (Block& al)
  { return *new ReactionAdsorption (al); }
  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    alist.add ("description", 
	       "Maintain equilibrium between solute and sorbed from.");
    syntax.add ("solute", Syntax::String, Syntax::Const,
		"Name of solute form of chemical.");
    syntax.add ("sorbed", Syntax::String, Syntax::Const,
		"Name of sorbed form of chemical.");
    syntax.add_object ("equilibrium", Adsorption::component, "\
Function for calculating equilibrium between solute and sorbed form.");
    syntax.add_object ("adsorption_rate", Number::component,
                       Syntax::Const, Syntax::Singleton, 
                       "Tranformation rate from solute to sorbed form.");
    syntax.add_object ("desorption_rate", Number::component,
                       Syntax::OptionalConst, Syntax::Singleton,
                       "Tranformation rate from sorbed to solute form.\n\
By default, this is identical to 'adsorption_rate'.");
    syntax.add ("adsorption_source", "g/cm^3/h", 
		Syntax::LogOnly, Syntax::Sequence, "\
Converted from solute to sorbed form this timestep (may be negative).");
  }
  static void load_NH4 (Syntax& syntax, AttributeList& alist);
  static void build_adsoption ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    load_syntax (syntax, alist);
    Librarian::add_type (Reaction::component, "adsorption",
			 alist, syntax, &make);
  }
  static void build_NH4 ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    load_NH4 (syntax, alist);
    alist.add ("type", "adsorption");

    Librarian::add_type (Reaction::component, "NH4_sorption",
			 alist, syntax, &make);
  }
  ReactionAdsorptionSyntax ()
  {
    build_adsoption ();
    build_NH4 ();
  }
} ReactionAdsorption_syntax;

void 
ReactionAdsorptionSyntax::load_NH4 (Syntax& syntax, AttributeList& alist)
{
  load_syntax (syntax, alist);
  alist.add ("solute", Chemical::NH4_solute ());
  alist.add ("sorbed", Chemical::NH4_sorbed ());
  AttributeList linear;
  linear.add ("type", "linear");
  linear.add ("K_clay", 117.116);
  alist.add ("equilibrium", linear);
  AttributeList rate;
  rate.add ("type", "const");
  rate.add ("value", 1.0, "s^-1");
  alist.add ("adsorption_rate", rate);
}

const AttributeList& 
Reaction::NH4_sorption_model ()
{ 
  static AttributeList alist;
  if (!alist.check ("type"))
    {
      Syntax dummy;
      ReactionAdsorptionSyntax::load_NH4 (dummy, alist);
      alist.add ("type", "NH4_sorption");
    }
  return alist;
}

// reaction_adsorption.C ends here.
