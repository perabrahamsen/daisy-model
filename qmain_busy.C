// qmain_busy.C -- Handle busy indicator.
//
// Copyright 1996-2001 Per Abrahamsen.
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include "qmain_busy.h"
#include "assertion.h"
#include <qapplication.h>
#include <qmainwindow.h>
#include <qstatusbar.h>

QApplication* Busy::global_app = NULL;

void
Busy::set_global_app (QApplication* app)
{ 
  daisy_assert (!global_app);
  global_app = app;
};

Busy::Busy (QMainWindow* w, const QString& m)
  : widget (w),
    message (m)
{ 
  QApplication::setOverrideCursor (Qt::waitCursor);
  widget->statusBar ()->message (message);
  daisy_assert (global_app);
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
