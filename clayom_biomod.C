// clayom_biomod.C -- New clay funtion from BIOMOD project.
// 
// Copyright 2002 KVL and Per Abrahamsen.
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


#include "clayom.h"
#include "block.h"
#include "alist.h"
#include "check.h"
#include "smb.h"
#include "soil.h"
#include "treelog.h"
#include "mathlib.h"
#include <sstream>

using namespace std;

class ClayOMBiomod : public ClayOM
{
  // Content.
  const double a;		// Maintenance ratio parameter.
#ifdef OLD_VERSION
  const double alpha;		// Speed parameter.
#else // !OLD_VERSION
  PLF factor_;
#endif // !OLD_VERSION
  const double E_SMB;		// Efficiency.
  const double f_SMB1;		// AOM fraction going to SMB1.

  // Simulation.
private:
  double find_f (double r_SMB1, double r_SMB2, double clay) const;
public:
  void set_rates (const Soil& soil, const vector<SMB*>& smb) const;
  double factor (const double clay) const ;
  bool smb_use_clay (unsigned int pool) const ;
  bool som_use_clay (unsigned int pool) const ;

  // Create and Destroy.
public:
  bool check (const vector<SMB*>& smb, Treelog& err) const;
  ClayOMBiomod (Block& al);
  ~ClayOMBiomod ();
};

double
ClayOMBiomod::find_f (const double r_SMB1, const double r_SMB2,
		      const double clay) const
{
  // We only count the fraction going to SMB.
  const double f_SMB2 = 1.0 - f_SMB1;

  // Ration between CO2-C and NOM-C production.
  const double R = a * (1.85 + 1.6 * exp (-7.86 * clay));
  const double h = 1.0 / (R + 1.0);

  // The equation.
  if (f_SMB2 > 1e-5)
    {
      const double s = pow (r_SMB1 * f_SMB1, 2) 
	+ 4 * h * r_SMB1 * r_SMB2 * f_SMB2;
      daisy_assert (s >= 0.0);
      return (-r_SMB1 * f_SMB1 + sqrt (s))
	/ (2 * E_SMB * r_SMB1 * r_SMB2 * f_SMB2);
    }
  else
    return h / (E_SMB * r_SMB1);
}

void
ClayOMBiomod::set_rates (const Soil& soil, const vector<SMB*>& smb) const
{ 
  // We always have two SMB pools in BIOMOD.
  daisy_assert (smb.size () == 2);

  // Total death and maintenance.
  const double t_SMB1 = smb[0]->turnover_rate + smb[0]->maintenance;
  const double t_SMB2 = smb[1]->turnover_rate + smb[1]->maintenance;

  // The ratios we want to change.
  const double r_SMB1 = smb[0]->turnover_rate / t_SMB1;
  const double r_SMB2 = smb[1]->turnover_rate / t_SMB2;

  for (unsigned int i  = 0; i < soil.size (); i++)
    {
      // Find modifier.
      const double f = find_f (r_SMB1, r_SMB2, soil.clay (i));
      daisy_assert (f > 0.0);

      // Update turnover rate and maintence.
      const double clay_rate1 = smb[0]->turnover_rate * f;
      daisy_assert (clay_rate1 < 1.0);
      smb[0]->clay_turnover.push_back (clay_rate1);
      daisy_assert (t_SMB1 >= clay_rate1);
      smb[0]->clay_maintenance.push_back (t_SMB1 - clay_rate1);
      const double clay_rate2 = smb[1]->turnover_rate * f;
      daisy_assert (clay_rate2 < 1.0);
      smb[1]->clay_turnover.push_back (clay_rate2);
      daisy_assert (t_SMB2 >= clay_rate2);
      smb[1]->clay_maintenance.push_back (t_SMB2 - clay_rate2);
    }

  for (unsigned int i = 0; i < smb.size (); i++)
    {
      daisy_assert (smb[i]->clay_turnover.size () == soil.size ());
      daisy_assert (smb[i]->clay_maintenance.size () == soil.size ());
    }
}

double
ClayOMBiomod::factor (const double clay) const 
{ 
#ifdef OLD_VERSION
  const double b = (0.1 + alpha) / alpha;
  return alpha * b / (clay + alpha);
#else // !OLD_VERSION
  return factor_ (clay); 
#endif // !OLD_VERSION
}

bool 
ClayOMBiomod::smb_use_clay (unsigned int /*pool*/) const
{
  // BUG? How can this be true when clay effect already incorporated?
  // Maybe because there are two clay effects?
  return true; 
}

bool 
ClayOMBiomod::som_use_clay (unsigned int /*pool*/) const
{ return false; }

// Create and Destroy.

bool
ClayOMBiomod::check (const vector<SMB*>& smb, Treelog& err) const
{
  Treelog::Open nest (err, "ClayOM biomod");
  
  if (smb.size () != 2)
    {
      err.error ("\
You must have exactly two SMB pool with the 'biomod' clay model");
      return false;
    }

  // Total death and maintenance.
  const double t_SMB1 = smb[0]->turnover_rate + smb[0]->maintenance;
  const double t_SMB2 = smb[1]->turnover_rate + smb[1]->maintenance;

  // The ratios we want to change.
  const double r_SMB1 = smb[0]->turnover_rate / t_SMB1;
  const double r_SMB2 = smb[1]->turnover_rate / t_SMB2;

#ifdef OLD_VERSION
  // The ratio modifier at 0.1 clay.
  const double f = find_f (r_SMB1, r_SMB2, 0.1);
  if (!approximate (f, 1.0, 0.01))
    {
      std::ostringstream tmp;
      tmp << "\
The biomod clay model is calibrated so f (0.1) must be 1.0, that is,\n\
the turnover and maintenance parameters are normalized for 10% clay content.\n\
However, f (0.1) is " << f << ", which mean any results produced are bogus";
      err.warning (tmp.str ());
    }
#else // !OLD_VERSION
  // The ratio modifier at 0.0 clay.
  const double f = find_f (r_SMB1, r_SMB2, 0.0);
  if (!approximate (f, 1.0, 0.01))
    {
      std::ostringstream tmp;
      tmp << "\
The biomod clay model is calibrated so f (0.0) must be 1.0, that is,\n\
the turnover and maintenance parameters are normalized zero clay content.\n\
However, f (0.0) is " << f << ", which mean any results produced are bogus";
      err.warning (tmp.str ());
    }
#endif // !OLD_VERSION

#ifdef OLD_VERSION
  // The overall modfier at 0.1 clay.
  const double g = factor (0.1);
  if (!approximate (g, 1.0, 0.01))
    {
      std::ostringstream tmp;
      tmp << "\
The biomod clay model is calibrated so g (0.1) must be 1.0, that is,\n\
the turnover and maintenance parameters are normalized for 10% clay content.\n\
However, g (0.1) is " << g << ", which mean any results produced are bogus";
      err.warning (tmp.str ());
    }
#endif // OLD_VERSION

  // Check efficiencies.
  for (unsigned int pool = 0; pool < smb.size (); pool++)
    for (unsigned int target = 0;
         target < smb[pool]->efficiency.size (); 
         target++)
      if (!approximate (smb[pool]->efficiency[target], E_SMB))
        {
          std::ostringstream tmp;
          tmp << "\
The efficiency specified for the biomod clay model (E_SMB) must be the\n\
same as specified for the SMB pools.  However, E_SMB is " << E_SMB << "\n\
and SMB[" << pool << "].efficiency[" << target << "] is "
                 << smb[pool]->efficiency[target];
          err.warning (tmp.str ());
        }
  
  return true;
}

ClayOMBiomod::ClayOMBiomod (Block& al)
  : ClayOM (al),
    a (al.number ("a")),
#ifdef OLD_VERSION
    alpha (al.number ("alpha")),
#else  // !OLD_VERSION
    factor_ (al.plf ("factor")),
#endif // !OLD_VERSION
    E_SMB (al.number ("E_SMB")),
    f_SMB1 (al.number ("f_SMB1"))
{ }

ClayOMBiomod::~ClayOMBiomod ()
{ }

static struct ClayOMBiomodSyntax
{
  static Model& make (Block& al)
  { return *new ClayOMBiomod (al); }

  ClayOMBiomodSyntax ()
  {
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    alist.add ("description", 
	       "Clay influence on organic matter from BIOMOD project.\n\
All SMB pools are affected, but not the SOM pools.  Additionally, the\n\
ration between maintenance and turnover is also clay dependent.");
    syntax.add ("a", Syntax::None (), Check::positive (), Syntax::Const,
		"Maintenance parameter.");
#ifdef OLD_VERSION
    syntax.add ("alpha", Syntax::None (), Check::positive (), Syntax::Const,
		"Speed parameter.");
#else // !OLD_VERSION
    syntax.add ("factor", Syntax::Fraction (), Syntax::None (),
		Syntax::Const, "\
Function of clay content, multiplied to the maintenance and turnover rates\n\
of the SMB pools.");
    PLF factor;
    factor.add (0.00, 1.0);
    factor.add (0.25, 0.5);
    factor.add (1.00, 0.5);
    alist.add ("factor", factor);
#endif // !OLD_VERSION
    syntax.add_fraction ("E_SMB", Syntax::Const,
			 "SMB efficiency in processing organic matter.\n\
Note that you must set the 'efficiency' parameter for all OM pools to\n\
this number for the BIOMOD clay response model to work correctly.");
    syntax.add_fraction ("f_SMB1", Syntax::Const,
			 "Fraction of AOM pools goind to SMB1.\n\
Only the fraction of AOM going to a SMB pool count, so this is really\n\
a fraction of the fraction coing to the SMB pools.\n\
Note that you must set the 'fraction' parameter of all AOM pools to\n\
reflect this for the BIOMOD clay response model to work correctly.");
    Librarian<ClayOM>::add_type ("biomod", alist, syntax, &make);
  }
} ClayOMBiomod_syntax;
