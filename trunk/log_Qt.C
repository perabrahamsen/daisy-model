// log_Qt.C --- Logging to Qt window.
// 
// Copyright 2007 Per Abrahamsen and KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#include "log_Qt.h"
#include "librarian.h"
#include "syntax.h"
#include "alist.h"

void
LogQt::done (const std::vector<Time::component_t>& time_columns,
	     const Time& time, double dt)
{ 
  {
    QMutexLocker lock (&mutex);
    LogExtern::done (time_columns, time, dt);
  }
  emit ready ();
}

LogQt::LogQt (Block& block)
  : LogExtern (block)
{ }

LogQt::~LogQt ()
{ }

static struct LogQtSyntax
{
  static Model& make (Block& al)
  { return dynamic_cast<Log&> (*new LogQt (al)); }

  LogQtSyntax ()
  { 
    Syntax& syntax = *new Syntax ();
    AttributeList& alist = *new AttributeList ();
    LogExtern::load_syntax (syntax, alist);
    alist.add ("description", "\
Log simulation state for use by the Qt user interface.");
    Librarian::add_type (Log::component, "Qt", alist, syntax, &make);
  }
} LogQt_syntax;

// log_Qt.C ends here.

