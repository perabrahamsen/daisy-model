// action_irrigate.C

#include "action.h"
#include "daisy.h"
#include "weather.h"
#include "column.h"
#include "syntax.h"
#include "alist.h"
#include "common.h"
#include <iostream.h>
#include "aom.h"
#include "inorganic_matter.h"

class ActionIrrigate : public Action
{
public:
  static const double at_air_temperature = -500;

private:
  const double flux;
  const double temp;
  const SoluteMatter& sm;

public:
  void doIt (Daisy&) const;

  // Create and Destroy.
private:
  friend class ActionIrrigateSyntax;
  static Action& make (const AttributeList&);
  ActionIrrigate (const AttributeList&);
public:
  ~ActionIrrigate ();
};

const double ActionIrrigate::at_air_temperature;

void 
ActionIrrigate::doIt (Daisy& daisy) const
{
  double t = temp;

  if (temp == at_air_temperature) 
    t = daisy.weather.AirTemperature ();
  
  ColumnList& cl = daisy.columns;
  for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++)
    {
      (*i)->irrigate (flux, t, sm, Column::top_irrigation);
    }
}

ActionIrrigate::ActionIrrigate (const AttributeList& al)
  : flux (al.number ("flux")),
    temp (al.number ("temperature")),
    sm (*new SoluteMatter (al.list ("solute")))
{ }

ActionIrrigate::~ActionIrrigate ()
{ }

// Add the ActionIrrigate syntax to the syntax table.
Action&
ActionIrrigate::make (const AttributeList& al)
{
  return *new ActionIrrigate (al);
}

static struct ActionIrrigateSyntax
{
  ActionIrrigateSyntax ();
} ActionIrrigate_syntax;

ActionIrrigateSyntax::ActionIrrigateSyntax ()
{ 
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();
  syntax.add ("flux", Syntax::Number, Syntax::Const);
  syntax.order ("flux", "solute");
  syntax.add ("temperature", Syntax::Number, Syntax::Const);
  alist.add ("temperature", ActionIrrigate::at_air_temperature);
  add_submodule<SoluteMatter> ("solute", syntax, alist);
  Action::add_type ("irrigate", alist, syntax, &ActionIrrigate::make);
}
