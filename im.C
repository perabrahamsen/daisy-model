// im.C

#include "im.h"
#include "log.h"
#include "alist.h"
#include "syntax.h"

void
IM::output (Log& log, Filter& filter) const
{
  log.output ("NO3", filter, NO3);
  log.output ("NH4", filter, NH4);
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

double 
IM::N_left (const AttributeList& al)
{
  const double N = al.number ("total_N_fraction");
  const AttributeList& im  = al.alist ("im");
  return N * (1.0 - im.number ("NO3") - im.number ("NH4"));
}

static double IM_get_NO3 (const AttributeList& al)
{
  if (al.check ("weight"))
    {
      assert (al.name ("syntax") == "organic");

      const double weight = al.number ("weight") 
	* al.number ("dry_matter_fraction") 
	* 0.1;			// kg / m² --> g / cm²
      const double N = weight * al.number ("total_N_fraction");

      IM im (al.alist ("im"));
      
      return N * im.NO3;
    }
  else 
    return al.number ("NO3");
}

static double IM_get_NH4 (const AttributeList& al)
{
  if (al.check ("weight"))
    {
      assert (al.name ("syntax") == "organic");

      const double weight = al.number ("weight") 
	* al.number ("dry_matter_fraction") 
	* 0.1;			// kg / m² --> g / cm²
      const double N = weight * al.number ("total_N_fraction");

      IM im (al.alist ("im"));
      
      return N * im.NH4;
    }
  else 
    return al.number ("NH4") * (1.0 - al.number ("NH4_evaporation"));
}

IM::IM ()
  : NO3 (0.0),
    NH4 (0.0)
{ }

IM::IM (const IM& im)
  : NO3 (im.NO3),
    NH4 (im.NH4)
{ }

IM::IM (const AttributeList& al)
  : NO3 (IM_get_NO3 (al)),
    NH4 (IM_get_NH4 (al))
{ }

IM::IM (const IM& n, double flux)
  : NO3 (n.NO3 * flux),
    NH4 (n.NH4 * flux)
{ }

IM::~IM ()
{ }

void 
IM::load_syntax (Syntax& syntax, AttributeList& alist)
{
  syntax.add ("NO3", Syntax::Number, Syntax::State);
  alist.add ("NO3", 0.0);
  syntax.add ("NH4", Syntax::Number, Syntax::State);
  alist.add ("NH4", 0.0);
  syntax.add ("NH4_evaporation", Syntax::Number, Syntax::Const);
  alist.add ("NH4_evaporation", 0.0);
}
