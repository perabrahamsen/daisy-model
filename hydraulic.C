// hydraulic.C

#include "hydraulic.h"
#include "plf.h"
#include "log.h"

void 
Hydraulic::set_porosity (double Theta)
{ 
  assert (Theta > Theta_res);
  Theta_sat = Theta; 
}

void 
Hydraulic::output (Log& log) const
{
  log.output ("Theta_sat", Theta_sat); 
}

void
Hydraulic::K_to_M (PLF& plf, const int intervals) const
{
  static const double h0 = -20000.0;
  const double Ksat = K (0.0);
  const double max_change = pow (Ksat / K (h0), 1.0 / intervals);
  double step = (0 - h0) / 4.0;

  double h = h0;
  double sum = 0.0;
  while (h < 0)
    {
      plf.add (h, sum);
      step *= 2;
      while (K (h + step) / K (h) > max_change)
	step /= 2;
      sum += step * (K (h) + K (h + step)) / 2;
      h += step;
    }
  plf.add (h, sum);
}

static bool
check_alist (const AttributeList& al)
{
  bool ok = true;

  const double Theta_res = al.number ("Theta_res");
  const double Theta_sat = al.number ("Theta_sat");

  non_negative (Theta_res, "Theta_res", ok);

  if (Theta_sat >= 0.9)
    {
      CERR << "Theta_sat should be below 0.9 (is " << Theta_sat << ")\n";
      ok = false;
    }

  if (Theta_res >= Theta_sat)
    {
      CERR << "Theta_sat should be above Theta_res\n";
      ok = false;
    }
  return ok;
}  


void
Hydraulic::load_syntax (Syntax& syntax, AttributeList& alist)
{ 
  syntax.add_check (check_alist);
  syntax.add ("Theta_sat", "cm^3 H2O/cm^3", Syntax::State,
	      "Saturation point.");
  syntax.add ("Theta_res", "cm^3 H2O/cm^3", Syntax::Const,
	      "Soil residual water.");
  alist.add ("Theta_res", 0.0);
}

Hydraulic::Hydraulic (const AttributeList& al)
  : name (al.name ("type")),
    Theta_sat (al.number ("Theta_sat")),
    Theta_res (al.number ("Theta_res"))
{ }

Hydraulic::~Hydraulic ()
{ }

Librarian<Hydraulic>::Content* Librarian<Hydraulic>::content = NULL;

const char *const Hydraulic::description = "\
This component is responsible for specifying the soils hydraulic\n\
properties.";
