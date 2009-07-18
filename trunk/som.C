// som.C --- A single soil organic matter pool.
// 
// Copyright 2002 Per Abrahamsen and KVL.
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

#include "som.h"
#include "librarian.h"
#include "block_model.h"
#include "frame.h"

const char *const SOM::component = "SOM";

symbol
SOM::library_id () const
{
  static const symbol id (component);
  return id;
}

SOM::SOM (const BlockModel& al)
  : OM (al)
{ }

static struct SOMInit : public DeclareSolo
{
  Model* make (const BlockModel& al) const
  { return new SOM (al); }
  void load_frame (Frame& frame) const
  {
    OM::load_syntax (frame, "\
The first numbers corresponds to each of the SMB pools, the next\n\
numbers corresponds to the SOM pools, and the last numbers to each of\n\
the DOM pools.  The length of the sequence should thus be the number\n\
of SMB pools plus the number of SOM pools plus the number of DOM pools."); 
  }
  SOMInit ()
    : DeclareSolo (SOM::component, "\
A single Soil Organic Matter pool.")
  { }
} SOM_init;

static struct SOMSlowSyntax : public DeclareParam
{
  SOMSlowSyntax ()
    : DeclareParam (SOM::component, "SOM-SLOW", root_name (), "\
Slow SOM pool parameterization by Sander Bruun.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "daisy-somnew");
    frame.set ("turnover_rate", 4.3e-5 / 24.0 /* 1.7916667e-6 */);
    std::vector<double> efficiency;
    efficiency.push_back (0.40); // SMB1
    efficiency.push_back (0.40); // SMB2
    frame.set ("efficiency", efficiency);
    std::vector<double> fractions;
    fractions.push_back (1.0); // SMB1
    fractions.push_back (0.0); // SMB2
    fractions.push_back (0.0); // SOM1
    fractions.push_back (0.0); // SOM2
    fractions.push_back (0.0); // SOM3
    frame.set ("fractions", fractions);
  }
} SOMSlow_syntax;

static struct SOMSlowOldSyntax : public DeclareParam
{
  SOMSlowOldSyntax ()
    : DeclareParam (SOM::component, "SOM-SLOW-OLD", "SOM-SLOW", "\
Original parameterization of the slow SOM pool.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "mueller-smb");
    frame.set ("turnover_rate", 2.7e-6 / 24.0 /* 1.125e-7 */);
  }
} SOMSlowOld_syntax;

static struct SOMFastSyntax : public DeclareParam
{
  SOMFastSyntax ()
    : DeclareParam (SOM::component, "SOM-FAST", root_name (), "\
Fast SOM pool parameterization by Sander Bruun.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "daisy-somnew");
    frame.set ("turnover_rate", 1.4e-4 / 24.0 /* 5.83333333333e-6 */);
    std::vector<double> efficiency;
    efficiency.push_back (0.50); // SMB1
    efficiency.push_back (0.50); // SMB2
    frame.set ("efficiency", efficiency);
    std::vector<double> fractions;
    fractions.push_back (0.7); // SMB1
    fractions.push_back (0.0); // SMB2
    fractions.push_back (0.3); // SOM1
    fractions.push_back (0.0); // SOM2
    fractions.push_back (0.0); // SOM3
    frame.set ("fractions", fractions);
  }
} SOMFast_syntax;

static struct SOMFastOldSyntax : public DeclareParam
{
  SOMFastOldSyntax ()
    : DeclareParam (SOM::component, "SOM-FAST-OLD", "SOM-FAST", "\
Original parameterization of the fast SOM pool.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set_strings ("cite", "mueller-smb");
    std::vector<double> fractions;
    fractions.push_back (0.9); // SMB1
    fractions.push_back (0.0); // SMB2
    fractions.push_back (0.1); // SOM1
    fractions.push_back (0.0); // SOM2
    fractions.push_back (0.0); // SOM3
    frame.set ("fractions", fractions);
  }
} SOMFastOld_syntax;

static struct SOMInertSyntax : public DeclareParam
{
  SOMInertSyntax ()
    : DeclareParam (SOM::component, "SOM-INERT", root_name (), "\
Inert SOM pool parameterization.")
  { }
  void load_frame (Frame& frame) const
  {
    frame.set ("turnover_rate", 0.0);
    std::vector<double> efficiency;
    efficiency.push_back (0.50); // SMB1
    efficiency.push_back (0.50); // SMB2
    frame.set ("efficiency", efficiency);
    std::vector<double> fractions;
    fractions.push_back (0.0); // SMB1
    fractions.push_back (0.0); // SMB2
    fractions.push_back (0.0); // SOM1
    fractions.push_back (0.0); // SOM2
    fractions.push_back (1.0); // SOM3
    frame.set ("fractions", fractions);
  }
} SOMInert_syntax;

// som.C ends here.
