# Building Daisy #

Daisy is build with GNU Make, and requires a Unix-like environment, such as GNU/Linux or Cygwin.  The current (as of 2012-05-14) development environment is Ubuntu 10.04, while an old version Cygwin is used for building an executable for MS Windows. The code in CVS (post 5.11, pre 5.12) has been updated to compile with [Ubuntu](http://www.ubuntu.com/) 12.04 and the current (as of 2012-05-14) [Cygwin](http://www.cygwin.com/).

## Tools ##

You need to install make and svn under both Cygwin and Ubuntu, they both come with the package manager.

### Daisy source ###

Fetch the Daisy source by clicking in the "Source" tab above, and follow the instructions.  Renamed the "daisy-model" directory to "daisy".

### Ubuntu ###

Install boost and cxsparse from the package manager.

Change the Makefile so CXSPARSELIB points the right place.

> CXSPARSELIB = /usr/lib/libcxsparse.so.2.2.3

You should now be able to build the daisy executable with

> make linux

Set the DAISYHOME environment variable to point to the Daisy directory, and you are ready to go.

### Cygwin ###

Install i686-w64-mingw32-g++.exe from the package manager.

Install [NSIS](http://nsis.sourceforge.net/Main_Page). Edit MAKENSIS in the Makefile to point to the right place.

Fetch and unpack [boost](http://www.boost.org/), [UFconfig](http://www.cise.ufl.edu/research/sparse/UFconfig/), and [CXSparse](http://www.cise.ufl.edu/research/sparse/CXSparse/) from their sources. They should be unpacked in your Cygwin home directory.

Compile UFconfig and CXSparse with the command "make CC=i686-w64-mingw32-g++.exe".

Create a "libdeps" directory under daisy, and move CXSparse/Lib/libcxsparse.a there.

Boost doesn't need compilation, but edit the "win64" target in the Makefile so BOOSTINC points in the right direction.

You should now be able to create an installer by typing

> make setup-native

in the daisy diretcory.

## Misc ##

The C# interface has only been compiled with Microsoft `csc.exe` under Cygwin.

The GUI libraries used is Qt.

# Accessing Daisy #

Here will eventually be information about accessing Daisy from other programs through C++, C, C#, .net, and OpenMI.

# Changing the Daisy source code #

Here will be information about changing the Daisy source code.

Currently, we have information about the code that implements the [tertiary](CodeTertiary.md) [soil domain](SoilDomains.md).

In general, we mostly follow the [GNU Coding Standards](http://www.gnu.org/prep/standards/standards.html).

# See also #

&lt;wiki:gadget url="http://www.ohloh.net/projects/daisy-model/widgets/project\_basic\_stats.xml" height="240" border="1" /&gt;