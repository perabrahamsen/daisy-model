// net_radiation.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2021 KU
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

#include "net_radiation.h"
#include "log.h"
#include "weather.h"
#include "block_model.h"
#include "mathlib.h"
#include "librarian.h"
#include "frame.h"

// The 'net_radiation' component.

const char *const NetRadiation::component = "net_radiation";

symbol
NetRadiation::library_id () const
{
  static const symbol id (component);
  return id;
}

NetRadiation::NetRadiation (const BlockModel& al)
  : Model ()
{ }

NetRadiation::~NetRadiation ()
{ }

static struct NetRadiationInit : public DeclareComponent 
{
  NetRadiationInit ()
    : DeclareComponent (NetRadiation::component, "\
Calculate net radiation from meteorological data.")
  { }
  void load_frame (Frame& frame) const
  {
    Model::load_model (frame);
  }
} NetRadiation_init;

// The 'brunt' model.

struct NetRadiationBrunt : public NetRadiation
{
  const double A;
  const double B;

  double find_epsilon_0 (double Ta, double ea) const
  {
    return A + B * sqrt (ea); 
  }

  NetRadiationBrunt (const BlockModel& al)
    : NetRadiation (al),
      A (al.number ("A")),
      B (al.number ("B"))
  { }
};

static struct NetRadiationBruntSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationBrunt (al); }

  NetRadiationBruntSyntax ()
    : DeclareModel (NetRadiation::component, "brunt", "\
Brunt, 1932.  Default parameterization by Jensen et.al., 1990.\n\
FAO recommendation.")
  { }
  void load_frame (Frame& frame) const
  {
    // Brunt.
    frame.set_strings ("cite", "kjaersgaard2007comparison");
    frame.declare ("A", Attribute::None (), Attribute::Const,
		   "Brunt 'A' parameter (offset).");
    frame.set ("A", 0.64);
    frame.declare ("B", "1/sqrt(hPa)", Attribute::Const,
		   "Brunt 'B' parameter (vapor pressure factor).");
    frame.set ("B", 0.044);
  }
} NetRadiationBrunt_syntax;

// The 'idso_jackson' model.

struct NetRadiationIdsoJackson : public NetRadiation
{
  double find_epsilon_0 (double Ta, double ea) const
  {
    const double A = 0.261;     // []
    const double B = -0.000777; // [K^-2]
    return (1 - A * exp(B * sqr (273.15 - Ta)));
  }
  
  NetRadiationIdsoJackson (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationIJSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationIdsoJackson (al); }

  NetRadiationIJSyntax ()
    : DeclareModel (NetRadiation::component, "idso_jackson", "Idso and Jackson, 1969")
  { }
  void load_frame (Frame& frame) const
  { }
} NetRadiationIH_syntax;

// The 'brutsaert' model.

struct NetRadiationBrutsaert : public NetRadiation
{
  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 1.24;     // [K^1/7 hPa^-1/7]
    return (A * pow(ea/Ta, 1./7.));
  }

  NetRadiationBrutsaert (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationBrutsaertSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationBrutsaert (al); }

  NetRadiationBrutsaertSyntax ()
    : DeclareModel (NetRadiation::component, "brutsaert", "Brutsaert, 1975")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationBrutsaert_syntax;

// The 'swinbank' model.

struct NetRadiationSwinbank : public NetRadiation
{
  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 9.2E-6;     // [K^-2]
    return (A * sqr (Ta));
  }

  NetRadiationSwinbank (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationSwinbankSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationSwinbank (al); }

  NetRadiationSwinbankSyntax ()
    : DeclareModel (NetRadiation::component, "swinbank", "Swinbank, 1963")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationSwinbank_syntax;

// The 'satterlund' model.

struct NetRadiationSatterlund : public NetRadiation
{
  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 1.08;      // [?]
    const double B = 2016.;      // [?]
    return (A * (1 - exp(- pow(ea, Ta/B))));
  }

  NetRadiationSatterlund (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationSatterlundSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationSatterlund (al); }

  NetRadiationSatterlundSyntax ()
    : DeclareModel (NetRadiation::component, "satterlund", "Satterlund, 1979")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationSatterlund_syntax;

// The 'prata' model.

struct NetRadiationPrata : public NetRadiation
{
  double find_epsilon_0 (double Ta, double ea /*[hPa] */) const
  {
    const double A = 1.2;      // []
    const double B = 3.0;      // []
    const double w = 46.5 * ea / Ta; // []
    return (1 - (1 + w) * exp ( - sqrt( A + B * w)));
  }

  NetRadiationPrata (const BlockModel& al)
    : NetRadiation (al)
  { }
};

static struct NetRadiationPrataSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new NetRadiationPrata (al); }

  NetRadiationPrataSyntax ()
    : DeclareModel (NetRadiation::component, "prata", "Prata, 1996")
  { }
  void load_frame (Frame&) const
  { }
} NetRadiationPrata_syntax;

// net_radiation.C ends here
