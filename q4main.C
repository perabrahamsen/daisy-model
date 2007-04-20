// q4main.C -- Main program for Q4 based interface.

#include "program.h"
#include "ui_Qt.h"
#include "toplevel.h"
#include <typeinfo>

int main(int argc, char *argv[])
{
    UIQt::setup (argc, argv);

    Toplevel toplevel;
    try
      {
        toplevel.command_line (argc, argv);
        toplevel.user_interface ();
        toplevel.initialize ();
        toplevel.run ();

        // All is well.
        throw EXIT_SUCCESS;
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
        // The program already reported the error, just exit.
        return exit_code;
      }
    catch (...)
      {
        toplevel.error ("Unknown exception");
      }
}

// q4main.C ends here
