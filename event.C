#include <assert.h>
#include "alist.h"
#include "event.h"
#include "eventqueue.h"
#include "minimanager.h"
#include "field.h"
#include "am.h"
#include "im.h"
#include "daisy.h"
#include "weather.h"
#include "crop.h"
#include "library.h"

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
   if(al.check("WithWhat") &&  al.check("Ammount") /* && AM::library().check(al.name("WithWhat"))*/ ) {
      string w(al.name("WithWhat"));
      return new FertilizeEvent(w,al.integer("Ammount"));
   }
   return 0;
}

void FertilizeEvent::Do_It(Daisy& daisy,const Time&, EventQueue&)
{
  // Fertilize the WORLD
  // const AttributeList& am = AM::library ().lookup ("mineral");
  AttributeList am = AttributeList(Librarian<AM>::library ().lookup ("mineral"));
  am.add("NO3", 50.0e-5);
  am.add("NH4", 50.0e-5);
    // Add inorganic matter.
#if 0
    if (to < from)
      daisy.field.fertilize (IM (am), from, to);
    else
#endif
      daisy.field.fertilize (IM (am));

    // Add organic matter, if any.
    if (am.name ("syntax") != "mineral"){
      AttributeList am_creation (am);
      am_creation.add ("creation", daisy.time);
#if 0
      if (to < from)
	daisy.field.fertilize (am_creation, from, to);
      else
#endif
	daisy.field.fertilize (am_creation);
    }
  COUT << "[ (Not really) Fertilizing with " <<fertilizer<<"]\n";
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

	COUT << "[Irrigating]\n";
   if (howmuch != 0) {
   	if (overheadirrigation)
	      daisy.field.irrigate_top (howmuch, t, sm);
	   else
	      daisy.field.irrigate_surface (howmuch, t, sm);
   }

}

bool IrrigateEvent::KanUdfoeres(Daisy&,const Time&){
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
void TillageEvent::Do_It(Daisy& daisy,const Time&, EventQueue& ){
   bool mix_it=true;
   double middle, depth, penetration = 1.0;

   if (how == string("Seed Bed Preparation")){
      depth  = -8.0;
   } else if (how == string("Stubble Cultivation")) {
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

         if (mix_it)
            daisy.field.mix (daisy.time, 0.0, depth, penetration);
	      else
            daisy.field.swap (daisy.time, 0.0, middle, depth);
         COUT << "[Tilling]\n";

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

void HarvestEvent::Do_It(Daisy& daisy,const Time& , EventQueue& ){
      const double DS = daisy.field.crop_ds(CropName);
      COUT << "[" << CropName << ": developmentstage = "<< DS << "\n";
      vector<const Harvest*> entry = daisy.field.harvest (daisy.time, CropName, Stub, Stem, Leaf, StorageOrgans);
      daisy.harvest.insert (daisy.harvest.end (), entry.begin (), entry.end ());
      COUT << "[Harvesting "<<CropName<<"]\n";

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
bool FuncHarvestEvent::KanUdfoeres(Daisy& d,const Time& ){
   // Er developmentstage opnået ?
   const double DS = d.field.crop_ds(CropName);
   if (Stage > DS)
      return true;
   return false;
}


SowEvent* SowEvent::create(AttributeList &al){
   const Library& library = Librarian<struct Crop>::library ();
   if (al.check("What") /* && al.check("Model")*/&& library.check (al.name("What"))){

      string m(""/*al.name("Model")*/), w(al.name("What"));
#if 0
      // JRI code:
      return new SowEvent(m, w,library.lookup(al.name("What")));
#else
      // PA code:
      AttributeList& cal = *new AttributeList (library.lookup (w));
      cal.add ("type", w);
      return new SowEvent (m, w, cal);
#endif      
   }
   return 0;
}
void SowEvent::Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ){
	daisy.field.sow (CropAttributes);
	COUT << "[Sowing]\n";

       if (harvest)
          harvest->SetCropName(Crop);
       else {
          Time temp(dato);
          temp.tick_day();
          EQ.TilfoejEvent(temp,EventQueue::TRelativ,RelativTilUdviklingsTrin);
       }
}

bool SowEvent::KanUdfoeres(Daisy& ,const Time& ){
   return true;
}

StartEvent* StartEvent::create(AttributeList &){
   return new StartEvent();
}

void StartEvent::Do_It(Daisy& ,const Time& , EventQueue& ){
         COUT << "[Start]\n";

}
bool StartEvent::KanUdfoeres(Daisy& ,const Time& ){
   return true;
}

EndEvent* EndEvent::create(AttributeList &){
   return new EndEvent();
}
void EndEvent::Do_It(Daisy& daisy,const Time& , EventQueue& ){
         COUT << "[Stopping]\n";
   daisy.running = false;
}
bool EndEvent::KanUdfoeres(Daisy& ,const Time& ){
   return true;
}


#ifdef BORLAND_TEMPLATES
template class list<RelEvent*>;
#endif

