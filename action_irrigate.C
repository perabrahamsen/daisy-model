// action_irrigate.C

#include "action.h"
#include "daisy.h"
#include "weather.h"
#include "column.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include "am.h"
#include "im.h"
#include <iostream.h>

class ActionIrrigate : public Action
{
public:
  static const double at_air_temperature = -500;

private:
  const double flux;
  const double temp;
  const IM& sm;

public:
  void doIt (Daisy& daisy)
  {
    cout << " [Irrigating]";
    double t = temp;

    if (temp == at_air_temperature) 
      t = daisy.weather.AirTemperature ();

    ColumnList& cl = daisy.columns;
    for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
      {
	if (match (**i))
	  (*i)->irrigate (flux, t, sm, Column::top_irrigation);
      }
  }

  // Create and Destroy.
private:
  friend class ActionIrrigateSyntax;
  static Action& make (const AttributeList& al, const Action *const p)
  {
    return *new ActionIrrigate (al, p);
  }
  ActionIrrigate (const AttributeList& al, const Action *const p)
    : Action (p),
      flux (al.number ("flux")),
      temp (al.number ("temperature")),
      sm (*new IM (al.list ("solute")))
  { }
public:
  ~ActionIrrigate ()
  { }
};

const double ActionIrrigate::at_air_temperature;

// Add the ActionIrrigate syntax to the syntax table.

static struct ActionIrrigateSyntax
{
  ActionIrrigateSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    syntax.add ("flux", Syntax::Number, Syntax::Const);
    syntax.order ("flux", "solute");
    syntax.add ("temperature", Syntax::Number, Syntax::Const);
    alist.add ("temperature", ActionIrrigate::at_air_temperature);
    add_submodule<IM> ("solute", syntax, alist);
    Action::add_type ("irrigate", alist, syntax, &ActionIrrigate::make);
  }
} ActionIrrigate_syntax;

