// log_Qt.h --- Logging to Qt window.
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


#ifndef LOG_QT_H
#define LOG_QT_H

#include "log_extern.h"

#include <QtCore/QMutex>
#include <QtCore/QObject>

class LogQt : public QObject, public LogExtern
{
  Q_OBJECT

public:
  QMutex mutex;

signals:
  void ready ();
  
  // Use.
private:
  void done (const Time&, double dt);

  // Create and destroy.
public:
  LogQt (Block& block);
  ~LogQt ();
};

#endif // LOG_QT_H
