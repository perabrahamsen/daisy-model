// ui_Qt.h -- Qt based top level user interface.
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


#ifndef UI_QT_H
#define UI_QT_H

#include "ui.h"

#include <QtCore/QString>

class QApplication;

class UIQt : public UI
{
  // Class variables.
#ifdef SELECTABLE_UI
private:
  class Content;
  static Content* content;
#else // !SELECTABLE_UI
public:
  static QApplication* app;
  static void set_application (QApplication& a);
#endif // !SELECTABLE_UI
protected:
  void run_user_interface ();
  QString application_name () const;

  // Create.
private:
  UIQt& operator= (const UIQt&); // Disable.
  UIQt (const UIQt&); // Disable.
protected:
  explicit UIQt (Block&);
  ~UIQt ();
};

#endif // UI_QT_H
