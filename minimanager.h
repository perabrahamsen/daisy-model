#ifndef __MINIMANAGER_H
#define __MINIMANAGER_H
#include "common.h"      // Common "settings"
#include "action.h"      // The MiniManager is in the Action Library
#include "event.h"     // but it manages "Events"
#include "eventqueue.h"  // in a EventQueue
//#include "time.h"      // Queued in time order

class Daisy;
class MiniManager : public Action
{
  EventQueue *EQ;
public:
  void doIt (Daisy& daisy);

  // Create and Destroy.
public:
  bool check (Daisy& daisy) const;

  // Create and Destroy.
// private:
  friend class MiniManagerSyntax;
  static Action& make (const AttributeList& al)
  { return *new MiniManager (al); }
  MiniManager (const AttributeList&);
public:
  ~MiniManager ()
  {if(EQ) delete EQ; }
};

#endif
