#ifndef _EVENT_H
#define _EVENT_H
#include "common.h"
#include <list>
#include <string>
#include <assert.h>
#include "time.h"
#include "alist.h"
#include "am.h"
#include "library.h"

class Daisy;
class Column;
class EventQueue;
class Event;
class SowEvent;
class RelEvent;
typedef list<Event*> EventList;
typedef list<EventList*> EventListList;
typedef list<RelEvent*> RelEventList;

class RelEvent {
public:
   unsigned int dis;
   Event* event;
   RelEvent(unsigned int d, Event* e) {dis=d,event=e;};
   ~RelEvent(){};
};
// "Base" Event
class Event {
   unsigned int indegree;                          // TopSort
public:
   Event():indegree(0){};                          // TopSort
   void Up(void){indegree++;};                     // TopSort
   void Down(void){indegree--;};                   // TopSort
   unsigned int Indegree(void) {return indegree;}; // TopSort
public:
   enum EventType {undefined,
                   start, end,
                   sow, harvest,
                   Funcharvest,
                   tillage,
                   fertilize,
                   modelirrigate,
                   irrigate};
   RelEventList RelativTilDenne;
protected:
   virtual void Do_It(Daisy& daisy, const Time& dato, EventQueue& EQ) = 0;
public:
   void AddRelEvent(RelEvent* r) {RelativTilDenne.push_back(r);r->event->Up();};
   virtual bool KanUdfoeres(Daisy& daisy,const Time& dato) = 0;
   virtual EventType Type(void) = 0;
   void Visit(const Time&,RelEventList&,EventQueue&);
   void Udfoer(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool match(Column& ) const {return true;};
  virtual ~Event(){};
};

class FertilizeEvent : Event {
   string fertilizer;
   int ammount;
   // AttributeList& am;

   FertilizeEvent(string& f,int amm):fertilizer(f),ammount(amm)/*,
                                     am(AM::library().lookup(f))*/{};
public:
   static FertilizeEvent* create(AttributeList &al);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return fertilize;};
};

class ModelIrrigateEvent : Event {
   string model;
public:
   ModelIrrigateEvent(string m);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return modelirrigate;};
};

class IrrigateEvent : Event {
   int temperature, howmuch;
   bool overheadirrigation, useairtemperature;
   IrrigateEvent(int t,int h,bool o,bool u):
       temperature(t), howmuch(h),
       overheadirrigation(o),
       useairtemperature(u){};
public:
   static IrrigateEvent* create(AttributeList &al);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return irrigate;};
};

class TillageEvent : Event {
   string how;
   TillageEvent(string& h):how(h){};
public:
   static TillageEvent* create(AttributeList &al);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return tillage;};
};



class HarvestEvent : public Event{
protected:
   int StorageOrgans, Leaf, Stem, Stub, DeadMaterial;
	SowEvent* sowEvent;
   string CropName;
   HarvestEvent(int SO, int Le, int St, int Sb, int DM):
                  StorageOrgans(SO),
                  Leaf(Le),
                  Stem(St),
                  Stub(Sb),
                  DeadMaterial(DM),
                  CropName("") {};
public:
   void SetSowEvent(SowEvent *s){sowEvent=s;};
   void SetCropName(string& cropname) {CropName = cropname;};
   static HarvestEvent* create(AttributeList &al);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return harvest;};
};

class FuncHarvestEvent : public HarvestEvent {
   double Stage;
   FuncHarvestEvent(int SO, int Le, int St, int Sb, int DM):
                 HarvestEvent(SO,Le,St,Sb,DM) {};
public:
   void SetStage(double s){Stage=s;};
   static FuncHarvestEvent* create(AttributeList &al);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return Funcharvest;};
};


class SowEvent : Event {
   string Crop;
   AttributeList& CropAttributes;
   EventList* RelativTilUdviklingsTrin;
   HarvestEvent* harvest;
   Event* ModelIrr;
   SowEvent(string&, string& crop, AttributeList &ca): Crop(crop),CropAttributes(ca),harvest(0){RelativTilUdviklingsTrin = new EventList;};
public:
   static SowEvent* create(AttributeList &al);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return sow;};
  void SetHarvest(HarvestEvent *h) {
        assert((harvest == 0)&&(RelativTilUdviklingsTrin->size() == 0));
        harvest = h;
        };
   void AddFuncHarvest(Event* ev) {
        assert(harvest == 0);
        RelativTilUdviklingsTrin->push_back(ev);};
};

class StartEvent : Event {
   StartEvent(){};
public:
   static StartEvent* create(AttributeList &al);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return start;};
};

class EndEvent : Event {
   EndEvent(){};
public:
   static EndEvent* create(AttributeList &al);
   void Do_It(Daisy& daisy,const Time& dato, EventQueue& EQ);
   bool KanUdfoeres(Daisy& daisy,const Time& dato);
   EventType Type(void) {return end;};
};

#endif
