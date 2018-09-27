// hydraulic_old2.C
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

#define BUILD_DLL

#include "hydraulic.h"
#include "mathlib.h"
#include "plf.h"
#include "librarian.h"
#include "frame.h"
#include "block_model.h"
#include "path.h"
#include "check.h"
#include <fstream>
#include <sstream>

class HydraulicOld2 : public Hydraulic
{
  // We cheat and use h_minus instead of h in all the PLF except M_.
  const double inc;
  double Theta_[501];
  PLF hm_;
  double Cw2_[501];
  double K_[501];
  double M_[501];

  inline int pF2pos (const double pF)
  { return double2int (pF / inc + 0.5); }
  inline int h2pos (const double h)
  { return pF2pos (h2pF (h)); }
  inline double lookup (const double *const a, const double h) const
  {
    if (h >= -1e-10)
      return a[0];
    const double pos = h2pF (h) / inc;
    int min = double2int (floor (pos) + 0.5);
    int max = double2int (ceil (pos) + 0.5);

    if (min < 0)
      return a[0];
    if (max > 500)
      return a[500];
    return a[min] + (pos - min) * (a[max] - a[min]);
  }
public:
  double Theta (double h) const;
  double K (double h) const;
  double Cw2 (double h) const;
  double h (double Theta) const;
  double M (double h) const;

  // Create and Destroy.
private:
  friend class HydraulicOld2Syntax;
  static Model& make (BlockModel& al);
  HydraulicOld2 (const BlockModel&);
public:
  virtual ~HydraulicOld2 ();
};

double 
HydraulicOld2::Theta (const double h) const
{
  return lookup (Theta_, h);
}

double 
HydraulicOld2::K (const double h) const
{
  return lookup (K_, h);
}

double 
HydraulicOld2::Cw2 (const double h) const
{
  return lookup (Cw2_, h);
}

double 
HydraulicOld2::h (const double Theta) const
{
  daisy_assert (Theta <= Theta_sat);
  return -hm_ (-Theta);
}

double 
HydraulicOld2::M (double h) const
{
  return lookup (M_, h);
}

HydraulicOld2::HydraulicOld2 (const BlockModel& al)
  : Hydraulic (al),
    inc (al.number ("inc"))
{ 
  const int M_intervals (al.integer ("M_intervals"));
  const symbol name (al.name ("file"));
  
  std::unique_ptr<std::istream> file_own = al.path ().open_file (name);
  daisy_assert (file_own.get ());
  std::istream& file = *file_own.get ();
  
  if (!file.good ())
    {
      throw (name + "read error");
    }
#if 0
  while (file.good () && file.get () != '\n')
    ;
#endif
  
  int line = 0;
  double pF;
  double Theta;
  double Cw2;
  double K;

  PLF Thetam_;

  for (int i = 0; i <= 500; i++)
    {
      if (!file.good ())
	{
	throw name + ": no good";
	}
      file >> pF >> Theta >> Cw2 >> K;
      line++;

      if (Theta_sat < 0.0)
	Theta_sat = Theta;
      
      if (i != pF2pos (pF))
	{
	  std::ostringstream tmp;
	  tmp << name << ":" << line << ": i " << i << " != "
	      << (pF / inc) << "(" << pF2pos (pF) << ")";
	  daisy_bug (tmp.str ());
	}
      
      Theta_[i] = Theta;
      Cw2_[i] = Cw2;
      K_[i] = K;

      const double h_minus = (pF < 1.0e-10) ? 0.0 : - pF2h (pF);
      
      Thetam_.add (h_minus, -Theta);
    }
  
  hm_ = Thetam_.inverse ();

  PLF myM;
  K_to_M (myM, M_intervals);

  for (int i = 0; i <= 500; i++)
    M_[i] = myM (pF2h (i * inc));
}

HydraulicOld2::~HydraulicOld2 ()
{ }

// Add the HydraulicOld2 syntax to the syntax table.

static struct HydraulicOld2Syntax : public DeclareModel
{
  HydraulicOld2Syntax ()
    : DeclareModel (Hydraulic::component, "old2", "\
Reads a file of lines in the format < pF Theta Cw2 K >, where pF is the\n\
water pressure, Theta is the water content at that  pressure, cw2 is\n\
dTheta/dh at that pressure [cm^-1], and K is the water conductivity at\n\
that pressure [cm/h].\n\
\n\
There must be exactly 501 lines, with pF starting at 0 and ending at 'inc' * 500,\n\
increasing with 'inc' on each line.")
  { }
  Model* make (const BlockModel& al) const
  {
    return new HydraulicOld2 (al);
  }

  void load_frame (Frame& frame) const
  {
    frame.declare ("inc", "pF", Check::positive (), Attribute::Const, "\
Increment in pF between each line in the file.");
    frame.set ("inc", 0.01);
    frame.declare_integer ("M_intervals", Attribute::Const,
			   "Number of intervals for numeric integration of K.");
    frame.set ("M_intervals", 500);
    frame.declare_string ("file", Attribute::Const, "The file to read.");
    frame.order ("file");
  }
} hydraulicOld2_syntax;

// hydraulic_old2.C ends here.
