// im.C
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


#include "im.h"
#include "am.h"
#include "log.h"
#include "alist.h"
#include "syntax.h"
#include "submodel.h"

void
IM::output (Log& log) const
{
  output_variable (NO3, log);
  output_variable (NH4, log);
}

void IM::clear ()
{ 
  NO3 = 0.0;
  NH4 = 0.0;
}

void
IM::operator += (const IM& n)
{ 
  NO3 += n.NO3;
  NH4 += n.NH4;
}

void
IM::operator -= (const IM& n)
{ 
  NO3 -= n.NO3;
  NH4 -= n.NH4;
}

void
IM::operator *= (double n)
{ 
  NO3 *= n;
  NH4 *= n;
}

void
IM::operator /= (double n)
{ 
  *this *= (1.0 / n);
}

bool
IM::empty () const
{
  return NO3 < 1e-20 && NH4 < 1e-20;
}

IM
IM::operator* (double flux) const
{
  return IM (*this, flux);
}

IM
IM::operator+ (const IM& im) const
{
  IM result (*this);
  result += im;
  return result;
}

IM::IM ()
  : NH4 (0.0),
    NO3 (0.0)
{ }

IM::IM (const IM& im)
  : NH4 (im.NH4),
    NO3 (im.NO3)
{ }

IM::IM (const AttributeList& al)
  : NH4 (AM::get_NH4 (al)),
    NO3 (AM::get_NO3 (al))
{ }

IM::IM (const IM& n, double flux)
  : NH4 (n.NH4 * flux),
    NO3 (n.NO3 * flux)
{ }

IM::~IM ()
{ }

void 
IM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  alist.add ("submodel", "IM");
  alist.add ("description", "\
Inorganic matter, or more precisely, mineral nitrogen.\n\
The dimensions depend on which model the 'IM' fixed component is used in.");
  syntax.add ("NH4", Syntax::Unknown (), Syntax::State,
	      "Ammonium content.");
  alist.add ("NH4", 0.0);
  syntax.add ("NO3", Syntax::Unknown (), Syntax::State,
	      "Nitrate content.");
  alist.add ("NO3", 0.0);
}

static Submodel::Register im_submodel ("IM", IM::load_syntax);
