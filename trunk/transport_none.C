// transport_none.C --- No transport.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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
#include "transport.h"
#include "geometry.h"
#include "soil.h"
#include "adsorption.h"
#include "alist.h"
#include "submodeler.h"
#include "memutils.h"
#include "librarian.h"
#include <sstream>

struct TransportNone : public Transport
{
  // Solute.
  void flow (const Geometry& geo, 
             const Soil& soil, 
             const std::vector<double>& Theta_old,
             const std::vector<double>& Theta_new,
             const std::vector<double>& q,
             symbol name,
             const std::vector<double>& S, 
             const std::map<size_t, double>& J_forced,
             const std::map<size_t, double>& C_border,
             std::vector<double>& C, 
             std::vector<double>& J, 
             double diffusion_coefficient, double dt,
             Treelog& msg) const;

  // Create.
  static void load_syntax (Syntax& syntax, AttributeList& alist);
  TransportNone (Block& al);
  ~TransportNone ();
};

void
TransportNone::flow (const Geometry& geo, 
                        const Soil& soil, 
                        const std::vector<double>& Theta_old,
                        const std::vector<double>& Theta_new,
                        const std::vector<double>& q,
                        const symbol /* name */,
                        const std::vector<double>& S, 
                        const std::map<size_t, double>& J_forced,
                        const std::map<size_t, double>& /* C_border */,
                        std::vector<double>& C, 
                        std::vector<double>& J, 
                        double /* diffusion_coefficient */, double dt,
                        Treelog& /* msg */) const
{
  const size_t cell_size = geo.cell_size ();

  // Solute M.
  std::vector<double> M (cell_size);
  for (size_t i = 0; i < M.size (); i++)
    M[i] = Theta_old[i] * C[i];

  // Forced flux.
  for (std::map<size_t, double>::const_iterator i = J_forced.begin ();
       i != J_forced.end ();
       i++)
    {
      const size_t edge = (*i).first;
      const double flow = (*i).second;
      J[edge] = flow;

      const double amount = flow * geo.edge_area (edge) * dt;

      const int from = geo.edge_from (edge);
      if (geo.cell_is_internal (from))
        M[from] -= amount / geo.cell_volume (from);

      const int to = geo.edge_to (edge);
      if (geo.cell_is_internal (to))
        M[to] += amount / geo.cell_volume (to);
    }

  // Cell source.
  for (size_t c = 0; c < cell_size; c++)
    M[c] += S[c] * dt;

  // Update C.
  for (size_t c = 0; c < cell_size; c++)
    C[c] = M[c] / Theta_new[c];
}

TransportNone::TransportNone (Block& al)
  : Transport (al)
{ }

TransportNone::~TransportNone ()
{ }

void 
TransportNone::load_syntax (Syntax&, AttributeList&)
{ }

const AttributeList& 
Transport::none_model ()
{
  static AttributeList alist;

  if (!alist.check ("type"))
    {
      Syntax dummy;
      TransportNone::load_syntax (dummy, alist);
      alist.add ("type", "none");
    }
  return alist;
}

static struct TransportNoneSyntax
{
  static Model& make (Block& al)
  { return *new TransportNone (al); }

  TransportNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
               "Disable all transport except through boundaries.");
    TransportNone::load_syntax (syntax, alist);
 
    Librarian::add_type (Transport::component, "none", alist, syntax, &make);
  }
} TransportNone_syntax;

// transport_none.C ends here.
