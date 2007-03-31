// hydraulic_old.C
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

//
// Read hydraulic parameters from file.

#include "hydraulic.h"
#include "mathlib.h"
#include "plf.h"
#include "librarian.h"
#include <fstream>

class HydraulicOld : public Hydraulic
{
  // We cheat and use h_minus instead of h in all the PLF except M_.
  PLF Thetam_;
  PLF hm_;
  PLF Cw2_;
  PLF K_;
  PLF M_;

public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;

  // Create and Destroy.
private:
  friend class HydraulicOldSyntax;
  static Model& make (Block& al);
  HydraulicOld (Block&);
public:
  virtual ~HydraulicOld ();
};

double 
HydraulicOld::Theta (const double h) const
{
  if (h >= -1.0e-20)
    return Theta_sat;
  else
    return -Thetam_ (-h);
}

double 
HydraulicOld::K (const double h) const
{
  return K_ (-h);
}

double 
HydraulicOld::Cw2 (const double h) const
{
  return Cw2_ (-h);
}

double 
HydraulicOld::h (const double Theta) const
{
  daisy_assert (Theta <= Theta_sat);
  return -hm_ (-Theta);
}

double 
HydraulicOld::M (double h) const
{
  return M_ (h);
}

HydraulicOld::HydraulicOld (Block& al)
  : Hydraulic (al)
{ 
  const int M_intervals (al.integer ("M_intervals"));
  const string name (al.name ("file"));
  
  ifstream file (Options::find_file (name));
  if (!file.good ())
    throw (name + ": read error");
  while (file.good () && file.get () != '\n')
    ;

  int line = 0;
  double pF;
  double Theta;
  double Cw2;
  double K;
  while (file.good ())
    {
      file >> pF >> Theta >> Cw2 >> K;
      line++;

      if (Theta_sat < 0.0)
	Theta_sat = Theta;
      
      const double h_minus = (pF < 1.0e-10) ? 0.0 : - pF2h (pF);
      
      Thetam_.add (h_minus, -Theta);
      Cw2_.add (h_minus, Cw2 * 1.0e-2);
      K_.add (h_minus, K * 3.6e5);
    }
  
  if (!file.eof ())
    {
#if 0
      throw (name + ":" + line + ": file error");
#else
      throw (name + ": file error");
#endif
      
    }
  hm_ = Thetam_.inverse ();
  K_to_M (M_, M_intervals);

  close (file.rdbuf ()->fd ());
}

HydraulicOld::~HydraulicOld ()
{ }

// Add the HydraulicOld syntax to the syntax table.

Model& make (Block& al)
{
  return *new HydraulicOld (al);
}

static struct HydraulicOldSyntax
{
  HydraulicOldSyntax ();
} hydraulicOld_syntax;

HydraulicOldSyntax::HydraulicOldSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  alist.add ("description", "\
Reads a file of lines in the format < pF Theta Cw2 K >, where pF is the\n\
water pressure, Theta is the water content at that  pressure, cw2 is\n\
dTheta/dh at that pressure [m^-1], and K is the water conductivity at\n\
that pressure [m/s].");
  syntax.add ("M_intervals", Syntax::Integer, Syntax::Const,
	      "Number of intervals for numeric integration of K.");
  alist.add ("M_intervals", 500);
  syntax.add ("file", Syntax::String, Syntax::Const, "The file to read.");
  syntax.order ("file");
  Librarian::add_type (Hydraulic::component, "old", alist, syntax, &HydraulicOld::make);
}
