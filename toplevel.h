// toplevel.h -- The top level syntax for .dai files.
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


#ifndef TOPLEVEL_H
#define TOPLEVEL_H

#include "metalib.h"
#include <string>
#include <memory>
#include <ctime>

class Program;
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

class Toplevel
{
  //Content.
private:
  std::string program_name;
  std::auto_ptr<Program> program_;
public:
  const std::auto_ptr<Treelog> msg;
private:
  Metalib metalib;
  std::time_t start_time;
  bool has_printed_copyright;
public:
  enum state_t { is_uninitialized, is_ready, is_running, is_done, is_error };
private:
  state_t state_;

  // Accessors.
private:
  Syntax& syntax ();
  AttributeList& alist ();
public:
  const Syntax& program_syntax () const;
  const AttributeList& program_alist () const;
  Program& program () const;

  // Messages.
private:
  NORETURN void usage ();
  void copyright ();
  void start_message () const;
  void end_message () const;

  // Use.
public:
  void run ();
  void error (const std::string&);
  state_t state () const;

  // Create and Destroy.
private:  
  static void initialize_once ();
public:
  void initialize ();
private:
  static std::string get_arg (int& argc, char**& argv);
public:
  void command_line (int& argc, char**& argv);
  void parse_file (const std::string&);
  static void load_run (Syntax&, AttributeList&);
private:
  static void load_syntax (Syntax&, AttributeList&);
public:
  Toplevel (const std::string& logname);
private:                        // Disable defaults.
  Toplevel(const Toplevel&);
};

#endif // TOPLEVEL_H
