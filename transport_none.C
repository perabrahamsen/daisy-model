// transport_none.C

#include "transport.h"
#include "soil.h"
#include "soil_water.h"
#include "solute.h"
#include "log.h"
#include "mathlib.h"

class TransportNone : public Transport
{
  // Simulation.
public:
  void tick (const Soil&, const SoilWater&, const Solute&,
	     vector<double>& M, 
	     vector<double>& C,
	     const vector<double>& S,
	     const double J_in);
  void output (Log&, Filter&) const
    { }

  // Create.
public:
  TransportNone (const AttributeList& al)
    : Transport (al.name ("type"))
    { }
};

void 
TransportNone::tick (const Soil& soil, const SoilWater& soil_water,
		     const Solute& solute, 
		     vector<double>& M, 
		     vector<double>& C,
		     const vector<double>& S,
		     const double J_in)
{
  for (unsigned int i = 0; i < soil.size (); i++)
    {
      M[i] += S[i];

      if (i == 0)
	M[i] -= J_in / soil.dz (0);

      C[i] = solute.M_to_C (soil, soil_water.Theta (i), i, M[i]);
      if (!(M[i] >= 0.0))
	{
	  CERR << "BUG: M[" << i << "] = " << M[i] 
	       << "(J_in = " << J_in << ") S[" << i << "] = " << S[i] << "\n";

	}

      assert (M[i] >= 0.0);
      assert (C[i] >= 0.0);
    }
}

static struct TransportNoneSyntax
{
  static Transport& make (const AttributeList& al)
  {
    return *new TransportNone (al);
  }

  TransportNoneSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", "No solute transport.");
    Librarian<Transport>::add_type ("none", alist, syntax, &make);
  }
} TransportNone_syntax;
