// transport_convection.C --- Using convection alone for solute transport.

#include "transport.h"
#include "soil.h"
#include "soil_water.h"
#include "solute.h"
#include "log.h"
#include "mathlib.h"

class TransportConvection : public Transport
{
  // Log variable.
private:
  vector<double> J;
  
  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, const Solute&,
	     vector<double>& M, 
	     vector<double>& C,
	     vector<double>& S,
	     double J_in);
  void output (Log&, Filter&) const;

  // Create.
public:
  TransportConvection (const AttributeList& al)
    : Transport (al.name ("type"))
    { }
};

void
TransportConvection::output (Log& log, Filter& filter) const
{
  log.output ("J", filter, J, true);
}

void 
TransportConvection::tick (const Soil& soil, const SoilWater& soil_water,
		   const Solute& solute, 
		   vector<double>& M, 
		   vector<double>& C,
		   vector<double>& S,
		   double J_in)
{
 // Number of soil layers.
  const unsigned int size = soil.size ();

  // Remember old content
  const double old_total = soil.total (M) + soil.total (S) * dt;

  // Make room make room!
  if (size + 1 > J.size ())
    J.insert (J.begin (), size + 1 - J.size (), 0.0);

  // Upper boundary.
  J[0] = J_in;

  // Middle nodes.
  for (unsigned int i = 1; i < size; i++)
    {
      const double q = soil_water.q (i+1);
      if (q < 0)		// Downward flow, take from water above.
	J[i] = q * C[i-1];
      else			// Upward flow, take from water below.
	J[i] = q * C[i];
    }
  
  // Lower boundary.
  // We assume the same concentration below the lowest node.
  J[size] = soil_water.q (size) * C[size-1];

  // Update content.
  for (unsigned int i = 0; i < size; i++)
    {
      M[i] += (-J[i] * dt +J[i+1]) / soil.dz (i) * dt + S[i] * dt;
      C[i] = solute.M_to_C (soil, soil_water.Theta (i), i, M[i]);
    }

  // Check mass conservation.
  const double new_total = soil.total (M);
  assert (approximate (old_total - J[0] * dt + J[size] * dt, new_total));
}

static struct TransportConvectionSyntax
{
  static Transport& make (const AttributeList& al)
  {
    return *new TransportConvection (al);
  }

  TransportConvectionSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("J", Syntax::Number, Syntax::LogOnly, Syntax::Sequence);
    Librarian<Transport>::add_type ("convection", alist, syntax, &make);
  }
} TransportConvection_syntax;
