#ifndef __EVENTQUEUE_H
#define __EVENTQUEUE_H
#include "event.h"
#include <queue>

class Daisy;
class EventsOfDate {
   Time dato;
   static unsigned int count;
public:
	static EventList FlyttetCirkaDato;
   EventList FastDato;
   EventList CirkaDato;
   EventListList Relativ;
   EventList VenterPaaFlyt;
   const Time& GetDato(){return dato;};
   void Udfoer(Daisy&,EventQueue&);
   EventsOfDate(const Time& t): dato(t){count++;};
   ~EventsOfDate();
};

struct eventComparison {
	bool operator () (EventsOfDate * left, EventsOfDate * right) const
		{ return left->GetDato() > right->GetDato(); }
};


class EventQueue {
   Time StartDato, SlutDato;
   Time Nu;
   EventsOfDate* Find(const Time&);
   typedef priority_queue<EventsOfDate*, deque<EventsOfDate*>, eventComparison> aPQ;
   aPQ TheQueue;
public:
   EventQueue(const Time& StDato, const Time& SlDato): StartDato(StDato), SlutDato(SlDato), Nu(StDato){};
	enum TidsType {TFastDato,TCirkaDato,TFlyttetCirkaDato,TRelativ};
   void TilfoejEvent(const Time&,TidsType,Event*);
   void TilfoejEvent(const Time&,TidsType,EventList*);
   bool DoTick(Daisy&);
};

#endif
