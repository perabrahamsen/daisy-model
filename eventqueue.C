#include "common.h"
#include "event.h"
#include "eventqueue.h"
#include "daisy.h"
#include <assert.h>

// Allocate static member
EventList EventsOfDate::FlyttetCirkaDato;
unsigned int EventsOfDate::count=0;

EventsOfDate::~EventsOfDate() {
   assert(count > 0);
   count--;
   for(EventList::iterator i = FastDato.begin();i != FastDato.end();i++)
      if (*i != 0)
         delete (*i);
   for(EventList::iterator i = CirkaDato.begin();i != CirkaDato.end();i++)
      if (*i != 0)
         delete (*i);
   for(EventList::iterator i = VenterPaaFlyt.begin();i != VenterPaaFlyt.end();i++)
      if (*i != 0)
         delete (*i);

   for(EventListList::iterator il = Relativ.begin(); il != Relativ.end(); il++){
      for(EventList::iterator i = (*il)->begin();i != (*il)->end();i++)
         if (*i != 0)
            delete (*i);
      delete (*il);
   }
   if (count == 0) {
      for(EventList::iterator i = FlyttetCirkaDato.begin();i != FlyttetCirkaDato.end();i++)
         if (*i != 0)
            delete (*i);
   }

}
void EventsOfDate::Udfoer(Daisy& d,EventQueue& EQ) {
   bool UdfoerFlyttetEvent = false;
	bool Stop = false;
   EventList::iterator el_i;

   UdfoerFlyttetEvent = !FastDato.empty();
   while(!FastDato.empty()) {
   	el_i = FastDato.begin();
      (*el_i)->Udfoer(d,dato,EQ);
      delete (*el_i);
      FastDato.erase(el_i);
   }
   if (UdfoerFlyttetEvent) {
   	while(!FlyttetCirkaDato.empty()) {
      	el_i = FlyttetCirkaDato.begin();
     		(*el_i)->Udfoer(d,dato,EQ);
        	delete (*el_i);
      	FlyttetCirkaDato.erase(el_i);
   	}
   }
  	EventListList::iterator ell_i;
   while(!Relativ.empty()) {
      ell_i = Relativ.begin();
      if (!(*ell_i)->empty()) {
      	el_i = (*ell_i)->begin();
         if ((*el_i)->KanUdfoeres(d,dato)) { // er udviklingstrin opnaet ?
            (*el_i)->Udfoer(d,dato,EQ);
            (*ell_i)->erase(el_i);
         }
         // Flyt liste til dato+1
         Time TempDato(dato);
         TempDato.tick_day();
         EQ.TilfoejEvent(TempDato,EventQueue::TRelativ,*ell_i);
      } else
      	delete (*ell_i);
      Relativ.erase(ell_i);
   }
   while(!CirkaDato.empty()) {
      el_i = CirkaDato.begin();
      if ((*el_i)->KanUdfoeres(d,dato)&& (!Stop)) {
       	(*el_i)->Udfoer(d,dato,EQ);
         delete (*el_i);
      } else {
         Stop = true;
         EQ.TilfoejEvent(dato,EventQueue::TFlyttetCirkaDato,(*el_i));
      }
      CirkaDato.erase(el_i);
   }
}

EventsOfDate* EventQueue::Find(const Time& dato){
   list<EventsOfDate*> tempList;
   list<EventsOfDate*>::iterator l_i;
   EventsOfDate* temp;
   bool found = false;
   while(!TheQueue.empty() && !found) {
   	tempList.push_back(temp = TheQueue.top());
      TheQueue.pop();
      if (temp->GetDato() > dato)
         break;
      found = (temp->GetDato() == dato);
   }
   while(!tempList.empty()) {
   	l_i = tempList.begin();
      TheQueue.push(*l_i);
      tempList.erase(l_i);
   }
   if (found) return temp;
   return 0;
}

void EventQueue::TilfoejEvent(const Time& dato,TidsType tid,Event* e){
/*
	if ( tid == TCirkaDato) {

      return;
   }
*/
   EventsOfDate* temp = Find(dato);
   if (!temp) {
 	   temp = new EventsOfDate(dato);
      TheQueue.push(temp);
   }
   switch(tid) {
     	case TFastDato:
         temp->FastDato.push_back(e);
         break;

      case TFlyttetCirkaDato:
         EventsOfDate::FlyttetCirkaDato.push_back(e);
         break;
      case TCirkaDato:
         temp->CirkaDato.push_back(e);
         break;
      case TRelativ:
   	   assert(0);
         break;
      default:
         assert(0==1);
   }
}
void EventQueue::TilfoejEvent(const Time& dato ,TidsType tid ,EventList* el){
	EventsOfDate* temp = Find(dato);
   if (!temp) {
   	temp = new EventsOfDate(dato);
      TheQueue.push(temp);
   }
   switch(tid) {
   	case TFastDato:
      case TCirkaDato:
      case TFlyttetCirkaDato:
      	assert(0);
      case TRelativ:
			temp->Relativ.push_back(el);
         break;
      default:
         assert(0==1);
   }
}

bool EventQueue::DoTick(Daisy& d) {
   if (TheQueue.empty())
      return false;
   EventsOfDate *E = TheQueue.top();
   Nu = d.time;

   if (Nu == E->GetDato()) { // har første elemente i køen samme dato så udfør
      E->Udfoer(d,*this);
      TheQueue.pop(); // Fjern første element
      delete E;       // Og frigiv hukommelse
   }
   return true;
}


#ifdef BORLAND_TEMPLATES
template class deque<EventsOfDate*>;
#endif

