// select_content.C --- Select a state variable.
// 
// Copyright 1996-2002 Per Abrahamsen and Søren Hansen
// Copyright 2000-2002 KVL.
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


#include "select_value.h"
#include "soil.h"
#include "check.h"
#include "mathlib.h"

struct SelectContent : public SelectValue
{
  // Content.
  const double height;
  const Soil* old_soil;
  int ia;                       // First node above height.
  double za;                    // Depth of node above height.
  int ib;                       // First node below height.
  double zb;                    // Depth of node above height.
  double rel;                   // Relative distance from za.
  bool use_ia;                  // Always use ia.

  // Output routines.
  void output_array (const std::vector<double>& array, 
		     const Soil* soil, Treelog&)
  { 
    if (soil != old_soil)
      {
        old_soil = soil;

        if (std::fabs (height) < 0.001)
          ia = ib = 0;
        else
          {
            // Find first node below height.
            for (ib = 0; ib < soil->size () && soil->z (ib) > height; ib++)
              /* do nothing */;
            zb = soil->z (ib);
        
            // Find first node above height.
            daisy_assert (soil->size () > 0);
            for (ia = soil->size () - 1; ia >= 0 && soil->z (ia) < height; ia--)
              /* do nothing */;
            za = soil->z (ia);

            // Distance from za relative to zb.
            rel = (height - za) / (zb - za) ;

            // If one is almost right, use that.
            if (approximate (height, za))
              ib = ia;
            else if (approximate (height, zb))
              ia = ib;
          }
        
        // If they are the same, just use ia. 
        use_ia = (ia == ib);
      }
    double result;
    if (use_ia) 
      result = array [ia];
    else
      {
        // Linear interpolation.
        const double va = array[ia];
        const double vb = array[ib];
        result = va + (vb - va) * rel;
      }
    add_result (result); 
  }

  // Create and Destroy.
  SelectContent (Block& al)
    : SelectValue (al),
      height (al.number ("height")),
      old_soil (NULL),
      ia (-42),
      za (42.42e42),
      ib (-42),
      zb (42.42e42),
      rel (42.42e42),
      use_ia (true)
    { }
};

static struct SelectContentSyntax
{
  static Select& make (Block& al)
    { return *new SelectContent (al); }

  SelectContentSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      SelectValue::load_syntax (syntax, alist);

      alist.add ("description", "Extract content at specified height.");
      syntax.add ("height", "cm", Check::non_positive (), Syntax::Const,
		  "Specify height (negative) to measure content.");

      Librarian<Select>::add_type ("content", alist, syntax, &make);
    }
} Select_syntax;
