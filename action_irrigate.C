// action_irrigate.C

#include "action.h"
#include "daisy.h"
#include "field.h"
#include "im.h"

#ifdef BORLAND_TEMPLATES
template class add_submodule<IM>;
#endif

struct ActionIrrigate : public Action
{
  static const double at_air_temperature;

  const double flux;
  const double temp;
  const IM& sm;
  
  virtual void irrigate (Field&,
			 double flux, double temp, const IM&) const = 0;

  void doIt (Daisy& daisy)
  {
    COUT << "[Irrigating]\n";      
    double t = temp;

    irrigate (daisy.field, flux, t, sm);
  }

  static bool check_alist (const AttributeList& alist)
  {
    bool ok = true;
    non_negative (alist.number ("flux"), "flux", ok);
    if (alist.check ("temperature"))
      non_negative (alist.number ("temperature"), "temperature", ok);
    return ok;
  }

  static void load_syntax (Syntax& syntax, AttributeList& alist)
  {
    syntax.add_check (&check_alist);
    syntax.add ("flux", "mm/h", Syntax::Const, 
		"Amount of irrigation applied.");
    syntax.order ("flux");
    syntax.add ("temperature", "dg C", Syntax::OptionalConst,
		"Temperature of irrigation (default: air temperature).");
    add_submodule<IM> ("solute", syntax, alist, Syntax::Const,
		       "\
Nitrogen content of irrigation water [g/mm] (default: none).");
  }

  ActionIrrigate (const AttributeList& al)
    : Action (al),
      flux (al.number ("flux")),
      temp (al.check ("temperature") 
	    ? al.number ("temperature")
	    : at_air_temperature),
      sm (*new IM (al.alist ("solute")))
  { }
  ~ActionIrrigate ()
  { }
};

struct ActionIrrigateOverhead : public ActionIrrigate
{
  void irrigate (Field& f, double flux, double temp, const IM& im) const
  { 
    if (temp == at_air_temperature)
      f.irrigate_overhead (flux, im); 
    else
      f.irrigate_overhead (flux, temp, im); 
  }
  ActionIrrigateOverhead (const AttributeList& al)
    : ActionIrrigate (al)
  { }
};

struct ActionIrrigateSurface : public ActionIrrigate
{
  void irrigate (Field& f, double flux, double temp, const IM& im) const
  {
    if (temp == at_air_temperature)
      f.irrigate_surface (flux, im);
    else
      f.irrigate_surface (flux, temp, im); 
  }
  ActionIrrigateSurface (const AttributeList& al)
    : ActionIrrigate (al)
  { }
};

const double ActionIrrigate::at_air_temperature = -500;

static struct ActionIrrigateOverheadSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateOverhead (al); }
  ActionIrrigateOverheadSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    alist.add ("description", "\
Irrigate the field from above.");
    Librarian<Action>::add_type ("irrigate_overhead", alist, syntax, &make);
  }
} ActionIrrigateOverhead_syntax;

static struct ActionIrrigateSurfaceSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateSurface (al); }
  ActionIrrigateSurfaceSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    alist.add ("description", "\
Irrigate the field directly on the soil surface, bypassing the canopy.");
    Librarian<Action>::add_type ("irrigate_surface", alist, syntax, &make);
  }
} ActionIrrigateSurface_syntax;

static struct ActionIrrigateTopSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateOverhead (al); }
  static bool check_alist (const AttributeList&)
  {
    static bool warned = false;
    if (warned)
      return true;
    warned = true;
    CERR << "OBSOLETE: Use `irrigate_overhead' instead of `irrigate_top'.\n";
    return true;
  }
  ActionIrrigateTopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    ActionIrrigate::load_syntax (syntax, alist);
    syntax.add_check (&check_alist);
    alist.add ("description", "\
OBSOLETE.  Use `irrigate_overhead' instead.");
    Librarian<Action>::add_type ("irrigate_top", alist, syntax, &make);
  }
} ActionIrrigateTop_syntax;
