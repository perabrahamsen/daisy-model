// hydraulic_table.C -- Read hydraulic properties from table.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2019 KU
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
#include "lexer_table.h"
#include "units.h"
#include "check.h"
#include <sstream>

class HydraulicTable : public Hydraulic
{
  double h0;			// h corresponding to lowest pF.
  PLF Theta_pF;
  PLF pF_Theta;
  PLF Cw2_pF;
  PLF K_pF;
  mutable PLF M_;

public:
  double Theta (double h) const
  {
    if (h > h0)
      return Theta_sat;
    return Theta_pF (h2pF (h));
  }
  double K (double h) const
  {
    if (h > h0)
      return K_sat;
    return K_pF (h2pF (h));
  }
      
  double Cw2 (double h) const
  {
    if (h > h0)
      return 0.0;
    return Cw2_pF (h2pF (h));
  }
  double h (double Theta) const
  {
    if (Theta >= Theta_sat)
      return 0.0;
    return pF2h (pF_Theta (Theta));
  }
  double M (double h) const
  {
    if (M_.size () == 0)
      K_to_M (M_, 500);
    
    return M_ (h);
  }
  
// Create and Destroy.
private:
  friend class HydraulicTableSyntax;
  static Model& make (BlockModel& al);
  HydraulicTable (const BlockModel&);
public:
  virtual ~HydraulicTable ();
};

HydraulicTable::HydraulicTable (const BlockModel& al)
  : Hydraulic (al)
{
  Treelog& msg = al.msg ();
  const Units& units = al.units ();
  LexerTable lex (al);
  //  const int M_intervals = al.integer ("M_intervals");
  
  if (!lex.read_header (msg))
    throw "hyd table init err1";

  const int pF_c = lex.find_tag ("pF");
  if (pF_c < 0)
    {
      lex.error ("'pF' not found");
      throw "hyd table init err2";
    }
  const symbol pF_unit = lex.dimension (pF_c);
  if (!units.can_convert (pF_unit, "pF", msg))
    throw "hyd table init err3";
  const int Theta_c = lex.find_tag ("Theta");
  if (Theta_c < 0)
    {
      lex.error ("'Theta' not found");
      throw "hyd table init err4";
    }
  const symbol Theta_unit = lex.dimension (Theta_c);
  if (!units.can_convert (Theta_unit, Attribute::Fraction (), msg))
    throw "hyd table init err5";
  const int Cw2_c = lex.find_tag ("Cw2");
  if (Cw2_c < 0)
    {
      lex.error ("'Cw2' not found");
      throw "hyd table init err6";
    }
  const symbol Cw2_unit = lex.dimension (Cw2_c);
  if (!units.can_convert (Cw2_unit, "cm^-1", msg))
    throw "hyd table init err7";
  const int K_c = lex.find_tag ("K");
  if (K_c < 0)
    {
      lex.error ("'K' not found");
      throw "hyd table init err8";
    }
  const symbol K_unit = lex.dimension (K_c);
  if (!units.can_convert (K_unit, "cm/h", msg))
    throw "hyd table init err9";

  double last_pF = NAN;
  
  while (lex.good ())
    {
      std::vector<std::string> entries;
      if (!lex.get_entries (entries))
	continue;

      double pF = lex.convert_to_double (entries[pF_c]);
      if (units.can_convert (pF_unit, "pF", pF))
	pF = units.convert (pF_unit, "pF", pF);
      else
	{
	  lex.error ("pF convert failed");
	  throw "hyd table init err10";
	}
      double Theta = lex.convert_to_double (entries[Theta_c]);
      if (units.can_convert (Theta_unit, Attribute::Fraction (), Theta))
	Theta = units.convert (Theta_unit, Attribute::Fraction (), Theta);
      else
	{
	  lex.error ("Theta convert failed");
	  throw "hyd table init err11";
	}
      double Cw2 = lex.convert_to_double (entries[Cw2_c]);
      if (units.can_convert (Cw2_unit, "cm^-1", Cw2))
	Cw2 = units.convert (Cw2_unit, "cm^-1", Cw2);
      else
	{
	  lex.error ("Cw2 convert failed");
	  throw "hyd table init err12";
	}
      double K = lex.convert_to_double (entries[K_c]);
      if (units.can_convert (K_unit, "cm/h", K))
	K = units.convert (K_unit, "cm/h", K);
      else
	{
	  lex.error ("K convert failed");
	  throw "hyd table init err13";
	}
      if (pF <= last_pF)
	{
	  lex.error ("pF should be increasing");
	  throw "hyd table init err14";
	}
      last_pF = pF;

      Theta_pF.add (pF, Theta);
      Cw2_pF.add (pF, Cw2);
      K_pF.add (pF, K);
    }
  if (!std::isfinite (last_pF))
    {
      lex.error ("No data");
      throw "hyd table init err15";
    }
  daisy_assert (Theta_pF.size () > 0);
  Theta_sat = Theta_pF.y (0);
  daisy_assert (Theta_sat > 0.0);
  for (int i = Theta_pF.size () - 1; i >= 0; --i)
    {
      const double pF = Theta_pF.x (i);
      const double Theta = Theta_pF.y (i);
      pF_Theta.add (Theta, pF);
    }
  daisy_assert (pF_Theta.size () == Theta_pF.size ());
  {
    const double pF0 = Theta_pF.x (0);
    h0 = pF2h (pF0);
  }

  if (K_sat < 0.0)
    {
      K_sat = K_pF.y (0);
      std::ostringstream tmp;
      tmp << "(K_sat " << K_sat << " [cm/h])";
      msg.message (tmp.str ());
    }
  if (Theta_sat < 0.0)
    {
      Theta_sat = Theta_pF.y (0);
      std::ostringstream tmp;
      tmp << "(Theta_sat " << Theta_sat << " [cm/h])";
      msg.message (tmp.str ());
    }
  return;
}

HydraulicTable::~HydraulicTable ()
{ }

// Add the HydraulicTable syntax to the syntax table.

static struct HydraulicTableSyntax : public DeclareModel
{
  HydraulicTableSyntax ()
    : DeclareModel (Hydraulic::component, "table", "\
Reads a ddf file with column pF Theta Cw2 K.")
  { }
  Model* make (const BlockModel& al) const
  {
    return new HydraulicTable (al);
  }

  void load_frame (Frame& frame) const
  {
    frame.declare_fraction ("Theta_res", Attribute::Const,
			    "Soil residual water.");
    frame.set ("Theta_res", 0.0);
    frame.declare ("K_sat", "cm/h",
		   Check::positive (), Attribute::OptionalConst,
		   "Water conductivity of saturated soil.");
    frame.declare_fraction ("Theta_sat",  Attribute::OptionalState,
			    "Saturation point.");
    LexerTable::load_syntax (frame);
    frame.declare_integer ("M_intervals", Attribute::Const,
			   "Number of intervals for numeric integration of K.");
    frame.set ("M_intervals", 500);
  }
} hydraulicTable_syntax;

// hydraulic_table.C ends here.
