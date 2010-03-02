// run_Qt.C -- Run a program in a seperate Qt thread.
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

#include "run_Qt.h"
#include "vis_Qt.h"
#include "toplevel.h"
#include "program.h"

#include <QtCore/QMetaType>

void 
RunQtMain::run()
{
  emit is_now_running (true);
  emit progress_state (toplevel.state ());

  try
    { 
      if (toplevel.state () != Toplevel::is_uninitialized)
        throw EXIT_FAILURE;

      toplevel.initialize ();
      emit progress_state (toplevel.state ());

      if (toplevel.state () != Toplevel::is_ready)
        throw EXIT_FAILURE;
      
      toplevel.program ().attach_ui (this, logs);
      emit progress_state (Toplevel::is_running);

      toplevel.run ();

#if 0
      if (toplevel.state () != Toplevel::is_done)
	throw EXIT_FAILURE;

      throw EXIT_SUCCESS;
#endif
    }
  catch (const char* error)
    { toplevel.error (std::string ("Exception: ") + error); }
  catch (const std::string& error)
    { toplevel.error (std::string ("Exception raised: ") + error); }
  catch (const std::exception& e)
    {
      toplevel.error (std::string ("Standard exception: ") 
                      + typeid (e).name () + ": " + e.what ());
    }
  catch (const int exit_code)
    {
      // The program already reported the error.
    }
  catch (...)
    {
      toplevel.error ("Unknown exception");
    }
#if 0
  emit progress_state (toplevel.state ());
  emit is_now_running (false);
#endif
}

bool
RunQtMain::running () const
{
  QMutexLocker lock (&mutex);
  return is_running;
}
 
void 
RunQtMain::set_progress (const double value)
{ emit progress_changed (value); }

void 
RunQtMain::stop ()
{
  QMutexLocker lock (&mutex);
  is_running = false;
}


RunQtMain::RunQtMain (Toplevel& top, const std::vector<Log*>& l,
                      QObject* parent)
  : QThread (parent),
    toplevel (top),
    logs (l),
    is_running (true)
{ }
  
RunQtMain::~RunQtMain()
{ }

void 
RunQtText::send_text (const std::string& text) const
{ emit text_ready (text); }

void 
RunQtText::signal_error () const
{ emit error_occured (); }

RunQtText::RunQtText(QObject* parent)
  : QObject (parent)
{ }

RunQtText::~RunQtText()
{ }

void 
RunQtNumbers::send_numbers (const std::vector<double>& numbers) const
{ emit numbers_ready (numbers); }

RunQtNumbers::RunQtNumbers(QObject* parent)
  : QObject (parent)
{ }

RunQtNumbers::~RunQtNumbers()
{ }

const QObject*
TreelogQtText::tracker () const
{ return &qt_text; }

void 
TreelogQtText::write (const std::string& text)
{ qt_text.send_text (text); }

void 
TreelogQtText::touch ()
{ header (); }

void 
TreelogQtText::debug (const std::string&)
{ }

void 
TreelogQtText::flush ()
{ }

void
TreelogQtText::error (const std::string& text)
{
  TreelogText::error (text);
  qt_text.signal_error ();
}

TreelogQtText::TreelogQtText ()
{ }
  
TreelogQtText::~TreelogQtText ()
{ }
