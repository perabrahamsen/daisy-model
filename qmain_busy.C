// qmain_busy.C -- Handle busy indicator.

#include "qmain_busy.h"

#include <qapplication.h>
#include <qmainwindow.h>
#include <qstatusbar.h>

#include <assert.h>

QApplication* Busy::global_app = NULL;

void
Busy::set_global_app (QApplication* app)
{ 
  assert (!global_app);
  global_app = app;
};

Busy::Busy (QMainWindow* w, const QString& m)
  : widget (w),
    message (m)
{ 
  QApplication::setOverrideCursor (Qt::waitCursor);
  widget->statusBar ()->message (message);
  assert (global_app);
  global_app->processEvents ();
}
  
Busy::~Busy ()
{
  widget->statusBar ()->message (message + "done", 2000);
  QApplication::restoreOverrideCursor();
}

NotBusy::NotBusy ()
{ QApplication::setOverrideCursor (Qt::arrowCursor); }

NotBusy::~NotBusy ()
{ QApplication::restoreOverrideCursor(); }
