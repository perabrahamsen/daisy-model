// action_irrigate.C

#include "action.h"
#include "daisy.h"
#include "frame.h"
#include "weather.h"
#include "column.h"
#include "am.h"
#include "im.h"

struct ActionIrrigate : public Action
{
  static const double at_air_temperature;

  const double flux;
  const double temp;
  const IM& sm;

  void doIt (const Frame& frame, Daisy& daisy)
  {
    cout << " [Irrigating]";
    double t = temp;

    if (temp == at_air_temperature) 
      t = daisy.weather.hourly_air_temperature ();

    ColumnList& cl = daisy.columns;
    for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
      {
	if (frame.match_column (**i))
	  (*i)->irrigate (flux, t, sm, Column::top_irrigation);
      }
  }

  ActionIrrigate (const AttributeList& al)
    : Action (al.name ("type")),
      flux (al.number ("flux")),
      temp (al.number ("temperature")),
      sm (*new IM (al.alist ("solute")))
  { }
public:
  ~ActionIrrigate ()
  { }
};

const double ActionIrrigate::at_air_temperature = -500;

#ifdef BORLAND_TEMPLATES
template class add_submodule<IM>;
#endif

static struct ActionIrrigateSyntax
{
  static Action& make (const AttributeList& al)
    { return *new ActionIrrigate (al); }
  ActionIrrigateSyntax ()
    { 
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      syntax.add ("flux", Syntax::Number, Syntax::Const);
      syntax.order ("flux", "solute");
      syntax.add ("temperature", Syntax::Number, Syntax::Const);
      alist.add ("temperature", ActionIrrigate::at_air_temperature);
      add_submodule<IM> ("solute", syntax, alist);
      Librarian<Action>::add_type ("irrigate", alist, syntax, &make);
    }
} ActionIrrigate_syntax;

