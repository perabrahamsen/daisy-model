// qmain.C --- Qt interface to Daisy.

#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"

#include <qapplication.h>
#include <qlistview.h>

int main( int argc, char **argv )
{
  // Create the Daisy item and subtree.
  Syntax& syntax = *new Syntax ();
  AttributeList& alist = *new AttributeList ();

  Daisy::load_syntax (syntax, alist);


  // Application.
  QApplication app (argc, argv);

  // QPushButton hello ("Hello world!", 0);
  // hello.resize (100, 30);
  QListView libs (0, 0);
  app.setMainWidget (&libs);
  libs.show ();
  return app.exec ();
}
