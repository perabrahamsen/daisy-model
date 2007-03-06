// options.h -- Parsing command line options.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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


#ifndef OPTIONS_H
#define OPTIONS_H

#include <string>

class Syntax;
class AttributeList;
class Treelog;

#ifdef __GNUC__
#define NORETURN __attribute__ ((noreturn))
#elif defined (_MSC_VER)
#define NORETURN __declspec(noreturn)
#else
#define NORETURN
#endif

class Options
{
  bool has_printed_copyright;
  const std::string program_name;
  static std::string get_arg (int& argc, char**& argv);
  NORETURN void usage (Treelog&) const;
public: 
  static void initialize_path ();
  void copyright (Treelog&);
Options (int& argc, char**& argv, 
           Syntax& syntax, AttributeList& alist, Treelog&);
private:                        // Disable.
  Options (const Options&);
  Options& operator= (const Options&);
};

#endif // OPTIONS_H
