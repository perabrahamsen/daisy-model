// rootdens.C
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

EMPTY_TEMPLATE
Librarian<Rootdens>::Content* Librarian<Rootdens>::content = NULL;

const char *const Rootdens::description = "\
Root density calculations.";

double
Rootdens::density_distribution_parameter (double a)
{
  double x, y, z, x1, y1, z1, x2, y2, z2;

  if (1 + a > exp (1.0))
    {
      x1 = 1.0;
      y1 = exp (x1);
      z1 = 1 + a * x1;
      x2 = 20.0;
      y2 = exp (x2);
      z2 = 1 + a * x2;
      while ((z1 - y1) * (z2 - y2) > 0)
	{
	  x1 = x2;
	  y1 = y2;
	  z1 = z2;
	  x2++;
	  y2 = exp (x2);
	  z2 = 1 + a * x2;
	}
    }
  else if (a >= 0.3)
    {
      x1 = 0.3;
      y1 = exp (x1);
 //     z1 = 1 + a * x1;
      x2 = 1.0;
      y2 = exp (x2);
 //     z2 = 1 + a * x2;
    }
  else
    {
      assert (false /* Invalid Root Distribution */);
    }
  x = (y2 * (x2 - 1) - y1 * (x1 - 1)) / (y2 - y1);
  y = exp (x);
  z = 1 + a * x;
  while (fabs (2 * (z - y) / (z + y)) > 1.0e-5)
    {
      if (z - y > 0)
	{
	  x1 = x;
	  y1 = y;
	  // z1 = z;
	}
      else
	{
	  x2 = x;
	  y2 = y;
	  // z2 = z;
	}
      x = (y2 * (x2 - 1) - y1 * (x1 - 1)) / (y2 - y1);
      y = exp (x);
      z = 1 + a * x;
    }
  return x;
}

const AttributeList& 
Rootdens::default_model ()
{
  static AttributeList alist;
  
  if (!alist.check ("type"))
    {
      alist.add ("type", "default");
      alist.add ("description", 
		 "Use exponential function for root density.");
      alist.add ("SpRtLength", 100.0);
      alist.add ("DensRtTip", 0.1);
    }
  return alist;
}

Rootdens::Rootdens (const AttributeList& al)
  : name (al.name ("type"))
{ }

Rootdens::~Rootdens ()
{ }

