#include <assert.h>
#include "alist.h"
#include "event.h"
#include "eventqueue.h"
#include "minimanager.h"
#include "column.h"
#include "am.h"
#include "im.h"
#include "daisy.h"
#include "weather.h"
#include "crop.h"

void Event::Udfoer(Daisy& daisy,const Time& dato, EventQueue& EQ) {
   // Wrapper to make sure Visit is done
	Visit(dato,RelativTilDenne,EQ);
   // Now do the Event
   Do_It(daisy,dato, EQ);
}

void Event::Visit(const Time& dato,RelEventList& curr,EventQueue& EQ) {
   RelEventList::iterator i;
	while(!curr.empty()) {
      i = curr.begin();
      Time NyDato(dato);
      NyDato.tick_day((*i)->dis);
      EQ.TilfoejEvent(NyDato,EventQueue::TCirkaDato,(*i)->event);
      Visit(NyDato,(*i)->event->RelativTilDenne,EQ);
      delete (*i);
      curr.erase(i);
   }
   assert(curr.empty());
}

FertilizeEvent* FertilizeEvent::create(AttributeList &al){
   if(al.check("WithWhat") &&  al.check("Ammount")&& AM::library().check(al.name("WithWhat"))) {
      string w(al.name("WithWhat"));
      return new FertilizeEvent(w,al.integer("Ammount"));
   }
   return 0;
}

void FertilizeEvent::Do_It(Daisy& ,const Time& , EventQueue& ){
	// Fertilize the WORLD

/* TODO
  AM am;
  ColumnList& cl = daisy.columns;
  for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++){
     if (!match (**i))
	     continue;
     // Add inorganic matter.
     if (to < from)
	     (*i)->fertilize (IM (am), from, to);
     else
	     (*i)->fertilize (IM (am));

     // Add organic matter, if any.
     if (am.name ("syntax") != "mineral"){
	     if (to < from)
	        (*i)->fertilize (am, daisy.time, from, to);
	     else
	        (*i)->fertilize (am, daisy.time);
	  }
  }
*/
}

bool FertilizeEvent::KanUdfoeres(Daisy& ,const Time& ){
   // Can I Fertilize the WORLD ?
   return true;
}

IrrigateEvent* IrrigateEvent::create(AttributeList &al){
   if (al.check("Temp") && al.check("HowMuch") &&
       al.check("UseAir") && al.check("OverheadIrr"))
      return new IrrigateEvent(al.integer("Temp"), al.integer("HowMuch"),
                                     al.flag("UseAir"), al.flag("OverheadIrr"));
   return 0;
}
void IrrigateEvent::Do_It(Daisy& daisy,const Time& , EventQueue& ){

   IM sm;

   double t = (useairtemperature)?
               daisy.weather.hourly_air_temperature () :
               temperature;

   Column::irrigation_from from = (overheadirrigation) ?
                                   Column::top_irrigation :
                                   Column::surface_irrigation;

   ColumnList& cl = daisy.columns;
   for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++){
      if (match (**i))
	      (*i)->irrigate (howmuch, t, sm, from);
   }
}

bool IrrigateEvent::KanUdfoeres(Daisy& ,const Time& ){
   // Can I Irrigate the WORLD ?
   return true;
}

TillageEvent* TillageEvent::create(AttributeList &al){
   if (al.check("How")) {
      string h(al.name("How"));
      return new TillageEvent(h);
   }
   return 0;
}
void TillageEvent::Do_It(Daisy& daisy,const Time& , EventQueue& ){
   bool mix_it=true;
   double middle, depth, penetration = 1.0;

   if (how == string("Seed Bed Preparation")){
      depth  = -8.0;
   } else if (how == string("Stubbel Cultivation")) {
      penetration = 0.6;
   } else if (how == string("Plowing")) {
      mix_it = false;
      middle = -9.0;
      depth  = -18.0;
   } else if (how == string("Rotavation")) {
      depth  = -15.0;
   } else if (how == string("Disk Harrowing")) {
      depth  = -15.0;
      penetration = 0.8;
   } else
     assert(false);

   ColumnList& cl = daisy.columns;
   for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++) {
	   if (match (**i)) {
         if (mix_it)
            (*i)->mix (daisy.time, 0.0, depth, penetration);
	      else
            (*i)->swap (daisy.time, 0.0, middle, depth);
      }
   }
}

bool TillageEvent::KanUdfoeres(Daisy& ,const Time& ){
   return true; // Hvilke betingelser ?   Bære evne
}


HarvestEvent* HarvestEvent::create(AttributeList &al){
   if (al.check("SO") && al.check("Stem") && al.check("DM") &&
      al.check("Leaf") && al.check("Stub"))
      return new HarvestEvent(al.integer("SO"), al.integer("Leaf"),
                              al.integer("Stem") , al.integer("Stub"),
                              al.integer("DM"));
   return 0;
}

void HarvestEvent::Do_It(Daisy& daisy,const Time&, EventQueue& ){
   ColumnList& cl = daisy.columns;
   for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++){
      if (!match (**i))
	      continue;
      vector<const Harvest*> entry = (*i)->harvest (daisy.time, CropName, Stub, Stem, Leaf, StorageOrgans);
      daisy.harvest.insert (daisy.harvest.end (), entry.begin (), entry.end ());
   }
}
bool HarvestEvent::KanUdfoeres(Daisy& ,const Time& ){
   return true;
}


FuncHarvestEvent* FuncHarvestEvent::create(AttributeList &al) {
   if (al.check("SO") && al.check("Stem") && al.check("DM") &&
      al.check("Leaf") && al.check("Stub"))
      return new FuncHarvestEvent(al.integer("SO"), al.integer("Leaf"),
                                  al.integer("Stem") , al.integer("Stub"),
                                  al.integer("DM"));
   return 0;
}
void FuncHarvestEvent::Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ){
   HarvestEvent::Do_It(daisy,dato,EQ);
}
bool FuncHarvestEvent::KanUdfoeres(Daisy& ,const Time& ){
   // Er developmentstage opnået ?
   return true;
}


SowEvent* SowEvent::create(AttributeList &al){
   const Library& library = Librarian<struct Crop>::library ();
   if (al.check("What") /* && al.check("Model")*/&& library.check (al.name("What"))){
      string m(""/*al.name("Model")*/), w(al.name("What"));
      return new SowEvent(m, w,library.lookup(al.name("What")));
   }
   return 0;
}
void SowEvent::Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ){
   ColumnList& cl = daisy.columns;
   for (ColumnList::iterator i = cl.begin (); i != cl.end (); i++){
      if (match (**i)) {
	    (*i)->sow (CropAttributes);
       if (harvest)
          harvest->SetCropName(Crop);
       else {
          Time temp(dato);
          temp.tick_day();
          EQ.TilfoejEvent(temp,EventQueue::TRelativ,RelativTilUdviklingsTrin);
       }
      }
  }
}

bool SowEvent::KanUdfoeres(Daisy&,const Time&){
   return true;
}

StartEvent* StartEvent::create(AttributeList &){
   return new StartEvent();
}

void StartEvent::Do_It(Daisy&,const Time&, EventQueue&){
}
bool StartEvent::KanUdfoeres(Daisy&,const Time&){
   return true;
}

EndEvent* EndEvent::create(AttributeList &){
   return new EndEvent();
}
void EndEvent::Do_It(Daisy& daisy,const Time&, EventQueue&){
   daisy.running = false;
}
bool EndEvent::KanUdfoeres(Daisy& ,const Time& ){
   return true;
}


#ifdef BORLAND_TEMPLATES
template class list<RelEvent*>;
#endif

