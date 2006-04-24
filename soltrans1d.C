// soltrans1d.C -- 1D transport of solutes in soil water.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#include "soltrans1d.h"
#include "solute.h"
#include "element.h"
#include "geometry1d.h"
#include "soil_water1d.h"
#include "submodel.h"
#include "mathlib.h"
#include <sstream>

void 
Soltrans1D::solute (const Geometry1D& geo, const Soil& soil, 
                    const SoilWater1D& soil_water, 
                    const double J_in, Solute& solute, 
                    Treelog& msg)
{ 
  solute.tick (geo.node_size (), soil_water);

  // Upper border.
  if (soil_water.q_p (0) < 0.0)
    {
      if (soil_water.q (0) >= 0.0)
        {
          if (soil_water.q (0) > 1.0e-10)
            {
              std::ostringstream tmp;
              tmp << "BUG: q_p[0] = " << soil_water.q_p (0) 
                  << " and q[0] = " << soil_water.q (0);
              msg.error (tmp.str ());
            }
          solute.J_p[0] = J_in;
          solute.J[0] = J_in;
        }
      else
        {
          const double macro_fraction
            = soil_water.q_p (0) / (soil_water.q_p (0) + soil_water.q (0));
          solute.J_p[0] = J_in * macro_fraction;
          solute.J[0] = J_in - solute.J_p[0];
        }
    }
  else
    solute.J[0] = J_in;

  // Flow.
  flow (geo, soil, soil_water, solute.submodel, 
        solute.M_, solute.C_, 
        solute.S, solute.S_p,
        solute.J, solute.J_p, 
        *solute.adsorption, solute.diffusion_coefficient (), msg);
}

void 
Soltrans1D::element (const Geometry1D& geo, 
                     const Soil& soil, 
                     const SoilWater1D& soil_water, 
                     Element& element,
                     Adsorption& adsorption,
                     double diffusion_coefficient,
                     Treelog& msg)
{
  element.tick (geo.node_size (), soil_water);
  flow (geo, soil, soil_water, "DOM", 
        element.M, element.C, element.S, element.S_p, element.J, element.J_p, 
        adsorption, diffusion_coefficient, msg);
}

void 
Soltrans1D::flow (const Geometry1D& geo, 
                  const Soil& soil, 
                  const SoilWater1D& soil_water, 
                  const std::string& name,
                  std::vector<double>& M, 
                  std::vector<double>& C, 
                  std::vector<double>& S, 
                  std::vector<double>& S_p, 
                  std::vector<double>& J, 
                  std::vector<double>& J_p, 
                  Adsorption& adsorption,
                  double diffusion_coefficient,
                  Treelog& msg)
{
  const double old_content = geo.total (M);

  // Flow.
  if (adsorption.full ())
    transport_solid->tick (msg, geo, soil, soil_water, adsorption, 
                          diffusion_coefficient, M, C, S, J);
  else
    {
      mactrans->tick (geo, soil_water, M, C, S, S_p, J_p, msg);

      try
        {
          transport->tick (msg, geo, soil, soil_water, adsorption, 
                           diffusion_coefficient, 
                           M, C, S, J);
        }
      catch (const char* error)
        {
          msg.warning (std::string ("Transport problem: ") + error +
                       ", trying reserve.");
          try
            {
              reserve->tick (msg, geo, soil, soil_water, adsorption, 
                             diffusion_coefficient, M, C, S, J);
            }
          catch (const char* error)
            {
              msg.warning (std::string ("Reserve transport problem: ") + error
                           + ", trying last resort.");
              last_resort->tick (msg, geo, soil, soil_water, adsorption, 
                                 diffusion_coefficient, M, C, S, J);
            }
        }
    }
  const double new_content = geo.total (M);
  const double delta_content = new_content - old_content;
  const double source = geo.total (S);
  const double in = -J[0];	// No preferential transport, it is 
  const double out = -J[geo.edge_size () - 1]; // included in S.
  const double expected = source + in - out;
  if (!approximate (delta_content, expected)
      && new_content < fabs (expected) * 1e10)
    {
      std::ostringstream tmp;
      tmp << __FILE__ << ":" << __LINE__ << ": " << name
	  << ": mass balance new - old != source + in - out\n"
	  << new_content << " - " << old_content << " != " 
	  << source << " + " << in << " - " << out << " (error "
	  << (delta_content - expected) << ")";
      msg.error (tmp.str ());
    }
}

void
Soltrans1D::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  alist.add ("submodel", "Soltrans1D");
  syntax.add ("transport", Librarian<Transport>::library (), 
	      "Solute transport model in matrix.");
  AttributeList cd;
  cd.add ("type", "cd");
  cd.add ("max_time_step_reductions", 20);
  alist.add ("transport", cd);
  syntax.add ("reserve", Librarian<Transport>::library (),
	      "Reserve transport model if the primary model fails.");
  AttributeList convection;
  convection.add ("type", "convection");
  convection.add ("max_time_step_reductions", 10);
  alist.add ("reserve", convection);
  syntax.add ("last_resort", Librarian<Transport>::library (),
	      "Last resort transport model if the reserve model fails.");
  AttributeList none;
  none.add ("type", "none");
  alist.add ("last_resort", none);
  
  syntax.add ("transport_solid", Librarian<Transport>::library (),
	      "Transport model for non-dissolvable chemicals.\n\
Should be 'none'.");
  alist.add ("transport_solid", none);

  syntax.add ("mactrans", Librarian<Mactrans>::library (), 
	      "Solute transport model in macropores.");
  AttributeList mactrans;
  mactrans.add ("type", "default");
  alist.add ("mactrans", mactrans);
}

Soltrans1D::Soltrans1D (Block& al)
  : transport (Librarian<Transport>::build_item (al, "transport")),
    reserve (Librarian<Transport>::build_item (al, "reserve")),
    last_resort (Librarian<Transport>::build_item (al, "last_resort")),
    transport_solid (Librarian<Transport>::build_item (al, "transport_solid")),
    mactrans  (Librarian<Mactrans>::build_item (al, "mactrans"))
{ }

Soltrans1D::~Soltrans1D ()
{ }

static Submodel::Register 
soltrans1d_submodel ("Soltrans1D", Soltrans1D::load_syntax);
