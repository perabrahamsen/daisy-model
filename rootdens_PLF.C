// rootdens_PLF.C -- Use piecewise linear functions for root density.
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


#include "rootdens.h"
#include "geometry.h"
#include "check.h"
#include "message.h"

struct Rootdens_PLF : public Rootdens
{
  // Parameters.
  struct Entry
  {
    // Parameters.
    const double value;
    const PLF& plf;
    
    // Create and Destroy.
    static void load_syntax (Syntax&, AttributeList&);
    Entry (const AttributeList&);
    ~Entry ();
  };
  vector<const Entry*> entries;
  
  // Simulation.
  void set_density (vector<double>& Density,
		    const Geometry& geometry, 
		    double Depth, double PotRtDpt,
		    double WRoot, double DS);

  // Create.
  Rootdens_PLF (const AttributeList&);
  ~Rootdens_PLF ();
};

void
Rootdens_PLF::set_density (vector<double>& Density,
			   const Geometry& geometry, 
			   const double Depth, const double PotRtDpt,
			   const double WRoot, const double)
{
#if 0
  // Dimensional conversion.
  static const double m_per_cm = 0.01;

  const double MinLengthPrArea = (DensRtTip * 1.2) * PotRtDpt;
  const double LengthPrArea
    = max (m_per_cm * SpRtLength * WRoot, MinLengthPrArea); // [cm/cm^2]
  double a = density_distribution_parameter (LengthPrArea / 
					     (PotRtDpt * DensRtTip));
  double L0 = DensRtTip * exp (a);
  a /= PotRtDpt;
  if (Depth < PotRtDpt)
    {
      double Lz = L0 * exp (-a * Depth);
      a = density_distribution_parameter (LengthPrArea / (Depth * Lz)) / Depth;
    }

  // Check minimum density
  double extra = 0.0;
  if (MinDens > 0.0 && WRoot > 0.0)
    {
      assert (L0 > 0.0);
      assert (a > 0.0);
      const double too_low = -log (MinDens / L0) / a; // [cm]

      if (too_low < Depth)
	{
	  // We don't have MinDens all the way down.
	  const double NewLengthPrArea 
	    =  LengthPrArea - MinDens * Depth; // [cm/cm^2]
#if 1
	  CERR << "too_low = " << too_low 
	       << ", NewLengthPrArea = " << NewLengthPrArea
	       << "MinLengthPrArea = " << MinLengthPrArea << "\n";
#endif	    
	  if (too_low > 0.0 && NewLengthPrArea > too_low * DensRtTip * 1.2)
	    {
	      // There is enough to have MinDens all the way, spend
	      // the rest using the standard model until the point
	      // where the standard model would give too little..
	      a = density_distribution_parameter (NewLengthPrArea
						  / (too_low * DensRtTip));
	      L0 = DensRtTip * exp (a);
	      a /= too_low;
	      extra = MinDens;
	    }
	  else
	    {
	      // There is too little, use uniform density all the way.
	      L0 = 0.0;
	      extra = LengthPrArea / Depth;
	    }
	}
    }

  unsigned int i = 0;
  for (; i == 0 || -geometry.zplus (i-1) < Depth; i++)
    Density[i] = extra + L0 * exp (a * geometry.z (i));

  assert (i < geometry.size ());
  for (; i < geometry.size (); i++)
    Density[i] = 0.0;
#endif
}

Rootdens_PLF::Rootdens_PLF (const AttributeList& al)
  : Rootdens (al),
    entries (map_construct_const<Entry> (al.alist_sequence ("entries")))
{ }

static struct Rootdens_PLFSyntax
{
  static Rootdens&
  make_DS_Depth (const AttributeList& al)
  { return *new Rootdens_DS_Depth (al); }
  static Rootdens&
  make_Depth_Depth (const AttributeList& al)
  { return *new Rootdens_Depth_Depth (al); }
  static Rootdens&
  make_DS_Fraction (const AttributeList& al)
  { return *new Rootdens_DS_Fraction (al); }

  Rootdens_PLFSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();

    Rootdens::load_syntax (syntax, alist);

    syntax.add_submodule_sequence("entries", Syntax::Const, "\
PLF Sequence.", Rootdens_PLF::Entry::load_syntax);

    Librarian<Rootdens>::add_type ("DS_Depth", alist, syntax, make_DS_Depth);
    Librarian<Rootdens>::add_type ("Depth_Depth", alist, syntax,
				   make_Depth_Depth);
    Librarian<Rootdens>::add_type ("DS_Fraction", alist, syntax,
				   make_DS_Fraction);
  }
} Rootdens_PLF_syntax;

