#include <iostream.h>
#include <string>
#include "syntax.h"
#include "minimanager.h"

bool MiniManager::check (Daisy& daisy) const
{
    // Convention: MiniManager::MiniManager will not
    // create an EQ if there is something wrong with
    // the alist given to ActionManager::ActionManager
    // so just check that it is created
    return (EQ) ? true : false;
}
void MiniManager::doIt (Daisy& d){
   EQ->DoTick(d);
}

Event::EventType EventType(const string& s) {
	static string start("Start"),
                 end("End"),
                 sow("Sowing"),
                 harvest("Harvest"),
                 Funcharvest("FuncHarvest"),
                 tillage("Tillage"),
                 fertilize("Fertilize"),
                 irrigate("Irrigate");
   if (s==start)
      return Event::start;
   if (s==end)
      return Event::end;
   if (s==sow)
      return Event::sow;
   if (s==harvest)
      return Event::harvest;
   if (s==Funcharvest)
      return Event::Funcharvest;
   if (s==tillage)
      return Event::tillage;
   if (s==fertilize)
      return Event::fertilize;
   if (s==irrigate)
      return Event::irrigate;
   return Event::undefined;
}

MiniManager::MiniManager (const AttributeList& alist, const Action *const p) : Action (p){
  vector<AttributeList*> actions =  alist.alist_sequence ("Action");
  vector<Event *> events(actions.size());
  Event* e;
  bool StartDefined=false, EndDefined=false, error = false;
  Time Start(1,1,1,0), Slut(1,1,1,0);

  for (unsigned int i = 0; i < actions.size () ; i++) events[i] = 0;
    // Define the events
  for (unsigned int i = 0; (i < actions.size ()) && !error ; i++)
  {
     if (actions[i]->check("Id") &&
         actions[i]->check("Type")&&
         actions[i]->check("Params")&&
         actions[i]->check("Time")) {

        string type = actions[i]->name("Type");
        int id = actions[i]->integer("Id");

        // events must come in squence
        if (i+1 != id)
           error = true;
        else {
           switch(EventType(type)) {
              case Event::start:
                 if (StartDefined)
                    error=true;
                 else {
                    StartDefined=true;
                    if (0 != (e=(Event*)StartEvent::create(actions[i]->alist("Params"))))
                       error=true;
                    else {
                       events[i] = e;
                       if (actions[i]->alist("Time").name("Type") == string("Abs"))
                          Start  = actions[i]->alist("Time").time("Dato");
                       else
                          error = true;
                    }
                 }
                 break;
              case Event::end:
                 if (EndDefined)
                    error=true;
                 else {
                    EndDefined=true;
                    if (0 != (e=(Event*)EndEvent::create(actions[i]->alist("Params"))))
                       error=true;
                    else
                       events[i] = e;
                       if (actions[i]->alist("Time").name("Type") == string("Abs"))
                          Slut  = actions[i]->alist("Time").time("Dato");
                       else
                          error = true;
                 }
                 break;
              case Event::sow:
                 // TODO: if sowevent has a "Model" add a ModelIrrigate event
                 if (0 != (e=(Event*)SowEvent::create(actions[i]->alist("Params"))))
                    error=true;
                 else
                    events[i] = e;
                 break;
              case Event::harvest:
                 if (0 != (e=(Event*)HarvestEvent::create(actions[i]->alist("Params"))))
                    error=true;
                 else
                    events[i] = e;
                 break;
              case Event::Funcharvest:
                 if (0 != (e=(Event*)FuncHarvestEvent::create(actions[i]->alist("Params"))))
                    error=true;
                 else
                    events[i] = e;
                 break;
              case Event::tillage:
                 if (0 != (e=(Event*)TillageEvent::create(actions[i]->alist("Params"))))
                    error=true;
                 else
                    events[i] = e;
                 break;
              case Event::fertilize:
                 if (0 != (e=(Event*)FertilizeEvent::create(actions[i]->alist("Params"))))
                    error=true;
                 else
                    events[i] = e;
                 break;
              case Event::irrigate:
                 if (0 != (e=(Event*)IrrigateEvent::create(actions[i]->alist("Params"))))
                    error=true;
                 else
                    events[i] = e;
                 break;
              case Event::undefined:
                 error=true;
                 break;
           } // switch
        }
     } else
        error=true;
  } // for
  if (error || !StartDefined || !EndDefined) {
     for (unsigned int i = 0; (i < events.size ()) ; i++)
        if (events[i]) delete events[i];
     return;
  }
  // So far so good !
  EQ = new EventQueue(Start,Slut);
  // Build time dependencies
  for (unsigned int i = 0; (i < actions.size ()) && !error ; i++) {
     AttributeList &a = actions[i]->alist("Time");
     if (a.name("Type") == string("Abs")) {
        if (a.check("Dato")){
           Time when = a.time("Dato");
           EQ->TilfoejEvent(when,EventQueue::TFastDato,events[i]);
        } else
           error = true;
     } else if (a.name("Type") == string("Ca")) {
        if (a.check("Dato")){
           Time when = a.time("Dato");
           EQ->TilfoejEvent(when,EventQueue::TCirkaDato,events[i]);
        } else
           error = true;
     } else if (a.name("Type") == string("Rel")) {
        if (a.check("Days") && a.check("Id")) {
           unsigned dis = a.integer("Days");
           unsigned id  = a.integer("Id");
           assert(id <= actions.size ());
           events[id-1]->AddRelEvent(new RelEvent(dis,events[i]));
        } else
           error = true;

     } else if (a.name("Type") == string("Sow")) {
        if (a.check("Id")) {
           unsigned id = a.integer("Id");
           // unsigned y  = a.integer("Year");
           // unsigned no = a.integer("NoInYear");
           assert((id <= actions.size ())&& actions[i]->name("Type")== string("Sowing"));
           ((FuncHarvestEvent*)events[i])->SetStage(a.number("DevelopmentStage"));
           ((FuncHarvestEvent*)events[i])->SetSowEvent((SowEvent*)events[id-1]);
           ((SowEvent*)events[id-1])->AddFuncHarvest(events[i]);
        } else
           error = true;

     } else
        error = true;
  }
  if (error) {
     delete EQ;
     EQ = 0;
     return;
  }
  // Build Sow - (Func)Harvest dependencies
  for (unsigned int i = 0; (i < actions.size ()) && !error ; i++) {
     string type = actions[i]->name("Type");
     if (EventType(type)==Event::harvest) {
         ((HarvestEvent*)events[i])->SetSowEvent((SowEvent*)events[actions[i]->alist("Params").integer("Id")-1]);
			((SowEvent*)events[actions[i]->alist("Params").integer("Id")-1])->SetHarvest((HarvestEvent*)events[i]);
         // Lookup id of SowEvent
     } else if (EventType(type)==Event::Funcharvest) {
			((SowEvent*)events[actions[i]->alist("Params").integer("Id")-1])->AddFuncHarvest((HarvestEvent*)events[i]);
         // Lookup id of SowEvent
     }

  }
  // TopSort and check dates
}

static struct MiniManagerSyntax
{
  MiniManagerSyntax ();
} MiniManager_syntax;

MiniManagerSyntax::MiniManagerSyntax ()
{
  // Time of action
  Syntax& time = *new Syntax ();
  time.add("Type", Syntax::String, Syntax::Const, Syntax::Singleton);
  time.order("Type");
  time.add("Dato", Syntax::Date, Syntax::Optional, Syntax::Singleton);
  time.add("Id", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  time.add("Days", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  time.add("NoInYear", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  time.add("Year", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  time.add("DevelopmentStage", Syntax::Number, Syntax::Optional, Syntax::Singleton);

  // Action Parameters
  Syntax& params = *new Syntax ();

  // Irrigate parameters
  params.add("Temp", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  params.add("HowMuch", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  params.add("UseAir", Syntax::Boolean, Syntax::Optional, Syntax::Singleton);
  params.add("OverheadIrr", Syntax::Boolean, Syntax::Optional, Syntax::Singleton);

  // Tillage parameters
  params.add("How", Syntax::String, Syntax::Optional, Syntax::Singleton);

  // Sowing parameters
  params.add("What", Syntax::String, Syntax::Optional, Syntax::Singleton);
  params.add("Model", Syntax::String, Syntax::Optional, Syntax::Singleton);
  params.add("No", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  params.add("Years", Syntax::Integer, Syntax::Optional, Syntax::Singleton);

  // Fertilize parameters
  params.add("WithWhat", Syntax::String, Syntax::Optional, Syntax::Singleton);
  params.add("Ammount", Syntax::Integer, Syntax::Optional, Syntax::Singleton);

  // FuncHarvest action
  // Id of sowing action
  params.add("Id", Syntax::Integer, Syntax::Optional, Syntax::Singleton);

  // Harvest action (Also params for FuncHarvest)
  params.add("SO", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  params.add("Stem", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  params.add("DM", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  params.add("Leaf", Syntax::Integer, Syntax::Optional, Syntax::Singleton);
  params.add("Stub", Syntax::Integer, Syntax::Optional, Syntax::Singleton);

  // Action
  Syntax& action = *new Syntax ();
  action.add("Id", Syntax::Integer, Syntax::Const, Syntax::Singleton);
  action.add("Type", Syntax::String, Syntax::Const, Syntax::Singleton);
  action.order("Id", "Type");
  action.add("Params", params, Syntax::Const, Syntax::Singleton);
  action.add("Time", time, Syntax::Optional, Syntax::Singleton);

  // Manage a sequence of Action(s)
  Syntax& actions = *new Syntax ();
  actions.add("Action", action, Syntax::Const, Syntax::Sequence);
  actions.order("Action");
  AttributeList& alist = *new AttributeList ();
  Action::add_type ("mini", alist, actions, &MiniManager::make);
}

