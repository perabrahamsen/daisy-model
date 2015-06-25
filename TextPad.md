# Introduction #

Although you can run Daisy from the command line or through the primitive GUI interface, by far the most convenient environment is a good text editor.  One such editor is TextPad.  By following the instructions on this page, you will gain

  * Parenthesis pair matching.
  * Highlighting of models, fixed components and parentheses in input files.
  * Run Daisy within TextPad.

# Instructions #

The configuration process can be divided into three tasks, first downloading and installing the files, then adding support for Daisy setup files, and finally for running the daisy command line executable (daisy.exe) from within TextPad.

## Download and install ##

  1. [Fetch TextPad](http://www.textpad.com/) by following the link.
  1. Install TextPad on you computer, and remember the installation directory.
  1. Fetch and unpack the [TextPad Daisy support files](http://daisy-model.googlecode.com/files/textpad-daisy-v2.0.zip).
  1. Copy the unpacked files (Dai.syn, Daisy.tcl, Daisy2.tcl, and Daisy3.tcl) to the "Samples" directory, below the TextPad install directory.

## Create a Daisy document class ##

  1. Start TextPad.
  1. Open the "Configure" menu.
  1. Select "New Document Class..."
  1. Specify `Daisy` as the class name, and press "Next".
  1. Specify `*.dai` as the class members, and press "Next".
  1. Check the "Enable syntax highlighting" check mark box.
  1. Select "Dai.syn" from the "Syntax definition file:" drop down menu.
  1. Press "Next" and "Apply".
  1. Exit TextPad to make the changes take effect.

If you open a Daisy setup file with a file name suffix of ".dai" with TextPad, you should now see pretty colors.

## Add Daisy to the "Tools" menu ##

  1. Start TextPad.
  1. Open the "Configure" menu.
  1. Select "Preferences..."
  1. Click on "Tools" (the name, not the small box with a beside it).
  1. Click on "Add" and choose "Program..." from the drop  down menu.
  1. Find and select the "daisy.exe" file.  It is normally located in the "bin" subfolder of the folder where you installed Daisy.
  1. Press "Apply".
  1. Now open "Configure" and "Preferences" again.
  1. This time, click the box next to "Tools" to view the available tools.
  1. Select "Daisy".
  1. Copy the exact string `([^:\\]+):(\d+):(\d+)` to text field named "Regular expression to match output", replacing the old content.  Use cut and paste to get it right.
  1. Choose "3" from the "Column" drop down menu.
  1. Press Apply.
  1. Exit TextPad to make the changes take effect.

If you open a Daisy setup file, you should now be able to run the simulation by opening the "Tools" menu, the "External tools" submenu, and then selecting "Daisy".  The output from the simulation will be placed in another window.  If an error message contain a file name and line number, you will (sometimes) be able to go directly to the specified location by double clicking on the error message.