// action_irrigate.C

#include "action.h"
#include "daisy.h"
#include "weather.h"
#include "field.h"
#include "am.h"
#include "im.h"

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

    if (temp == at_air_temperature) 
      t = daisy.weather.hourly_air_temperature ();

    irrigate (daisy.field, flux, t, sm);
  }

  ActionIrrigate (const AttributeList& al)
    : Action (al),
      flux (al.number ("flux")),
      temp (al.number ("temperature")),
      sm (*new IM (al.alist ("solute")))
  { }
public:
  ~ActionIrrigate ()
  { }
};

struct ActionIrrigateTop : public ActionIrrigate
{
  void irrigate (Field& f, double flux, double temp, const IM& im) const
    { f.irrigate_top (flux, temp, im); }
  ActionIrrigateTop (const AttributeList& al)
    : ActionIrrigate (al)
    { }
};

struct ActionIrrigateSurface : public ActionIrrigate
{
  void irrigate (Field& f, double flux, double temp, const IM& im) const
    { f.irrigate_surface (flux, temp, im); }
  ActionIrrigateSurface (const AttributeList& al)
    : ActionIrrigate (al)
    { }
};


const double ActionIrrigate::at_air_temperature = -500;

#ifdef BORLAND_TEMPLATES
template class add_submodule<IM>;
#endif

static struct ActionIrrigateSyntax
{
  static Action& make_top (const AttributeList& al)
    { return *new ActionIrrigateTop (al); }
  static Action& make_surface (const AttributeList& al)
    { return *new ActionIrrigateSurface (al); }
  ActionIrrigateSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "\
Irrigate the field.  Irrigation can either be applied from the top, in\n\
which case it will typically hit the canopy, or directly on the surface,\n\
bypassing the canopy.");
      syntax.add ("flux", "mm/h", Syntax::Const, 
		  "Amount of irrigation applied");
      syntax.order ("flux");
      syntax.add ("temperature", "dg C", Syntax::Const,
		  "Temperature of irrigation (default: air temperature)");
      alist.add ("temperature", ActionIrrigate::at_air_temperature);
      add_submodule<IM> ("solute", syntax, alist, Syntax::Const,
			 "\
Nitrogen content of irrigation water [g/mm] (default: none).");
      Librarian<Action>::add_type ("irrigate_top", alist, syntax, &make_top);
      Librarian<Action>::add_type ("irrigate_surface", alist, syntax,
				   &make_surface);
    }
} ActionIrrigate_syntax;
