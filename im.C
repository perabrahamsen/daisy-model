// im.C

#include "im.h"
#include "log.h"
#include "alist.h"
#include "syntax.h"
#include "submodel.h"

void
IM::output (Log& log) const
{
  log.output ("NO3", NO3);
  log.output ("NH4", NH4);
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

static double IM_get_NO3 (const AttributeList& al)
{
  if (al.check ("weight"))
    {
      if (al.name ("syntax") == "organic")
	{
	  // Organic fertilizer.
	  const double weight = al.number ("weight") 
	    * al.number ("dry_matter_fraction") 
	    * 0.01;			// T w.w. / ha --> g / cm²
	  const double N = weight * al.number ("total_N_fraction");
	  return N * al.number ("NO3_fraction");
	}
      // Mineral fertilizer.
      assert (al.name ("syntax") == "mineral");
      return al.number ("weight")
	* (1.0 - al.number ("NH4_fraction"))
	* (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0))); // kg/ha -> g/cm^2
    }
  // Other.
  return al.number ("NO3");
}

static double IM_get_NH4 (const AttributeList& al)
{
  if (al.check ("weight"))
    {
      if (al.name ("syntax") == "organic")
	{
	  // Organic fertilizer.
	  const double weight = al.number ("weight") 
	    * al.number ("dry_matter_fraction") 
	    * 0.01;			// T w.w. / ha --> g / cm²
	  const double N = weight * al.number ("total_N_fraction");
	  return N * al.number ("NH4_fraction");
	}
      // Mineral fertilizer.
      assert (al.name ("syntax") == "mineral");
      const double volatilization 
	= al.check ("NH4_evaporation") 
	? al.number ("NH4_evaporation")
	: al.number ("volatilization");
      
      return al.number ("weight")
	* al.number ("NH4_fraction") * (1.0 - volatilization)
	* (1000.0 / ((100.0 * 100.0) * (100.0 * 100.0))); // kg/ha -> g/cm^2
    }
  // Other.
  assert (!al.check ("NH4_evaporation") && !al.check ("volatilization"));
  return al.number ("NH4");
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
  : NH4 (IM_get_NH4 (al)),
    NO3 (IM_get_NO3 (al))
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
The dimensions depend on which model the `IM' fixed component is used in.");
  syntax.add ("NH4", Syntax::Unknown (), Syntax::State,
	      "Ammonium content.");
  alist.add ("NH4", 0.0);
  syntax.add ("NO3", Syntax::Unknown (), Syntax::State,
	      "Nitrate content.");
  alist.add ("NO3", 0.0);
}

static Submodel::Register im_submodel ("IM", IM::load_syntax);
