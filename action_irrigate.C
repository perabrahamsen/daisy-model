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
    COUT << " [Irrigating]\n";      
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
Nitrogen content of irrigation water [mg N/l] (default: none).");
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

const double ActionIrrigate::at_air_temperature = -500;

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

struct ActionIrrigateSubsoil : public Action
{
  const double flux;
  const double from;
  const double to;
  const IM& sm;

  void doIt (Daisy& daisy)
  {
    daisy.field.set_subsoil_irrigation (flux, sm, from, to);
    if (flux != 0.0)
      COUT << " [Subsoil irrigating with " << flux << " mm/h]\n";
    else
      COUT << " [Subsoil irrigating turned off]\n";
  }

  ActionIrrigateSubsoil (const AttributeList& al)
    : Action (al),
      flux (al.number ("flux")),
      from (al.number ("from")),
      to (al.number ("to")),
      sm (*new IM (al.alist ("solute")))
  { }
  ~ActionIrrigateSubsoil ()
  { }
};

struct ActionIrrigateStop : public Action
{
  void doIt (Daisy& daisy)
  {
    IM sm;
    daisy.field.set_subsoil_irrigation (0.0, sm, 0.0, -0.1);
    COUT << " [Subsoil irrigating turned off]\n";
  }
  ActionIrrigateStop (const AttributeList& al)
    : Action (al)
  { }
  ~ActionIrrigateStop ()
  { }
};

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

static struct ActionIrrigateSubsoilSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateSubsoil (al); }

  static bool check_alist (const AttributeList& al)
  { 
    bool ok = true;
    const double from = al.number ("from");
    const double to = al.number ("to");
    non_negative (al.number ("flux"), "flux", ok);
    non_positive (from, "from", ok);
    non_positive (to, "to", ok);
    if (from <= to)
      {
	CERR << "`from' must be higher than `to' in"
	     << " the subsoilirrigation zone.\n";
	ok = false;
      }
    return ok;
  }

  ActionIrrigateSubsoilSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList (alist);
    syntax.add_check (&check_alist);
    alist.add ("description", "\
Incorporate irrigation water directly in the soil.\n\
This command specifies the flux, set it to zero to turn off irrigation.");
    syntax.add ("flux", "mm/h", Syntax::Const, 
		"Amount of irrigation applied.");
    syntax.order ("flux");
    syntax.add ("from", "cm", Syntax::Const, "\
Height where you want to start the incorporation (a negative number).");
    alist.add ("from", 0.0);
    syntax.add ("to", "cm", Syntax::Const, "\
Height where you want to end the incorporation (a negative number).");

    add_submodule<IM> ("solute", syntax, alist, Syntax::Const,
		       "\
Nitrogen content of irrigation water [mg N/l] (default: none).");

    Librarian<Action>::add_type ("set_subsoil_irrigation",
				 alist, syntax, &make);
  }
} ActionIrrigateSubsoil_syntax;

static struct ActionIrrigateStopSyntax
{
  static Action& make (const AttributeList& al)
  { return *new ActionIrrigateStop (al); }

  ActionIrrigateStopSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList (alist);
    alist.add ("description", "Stop subsoil irrigation.");
    Librarian<Action>::add_type ("stop_subsoil_irrigation",
				 alist, syntax, &make);
  }
} ActionIrrigateStop_syntax;
