// program_spawn.C -- Spawn a number of Daisy programs.
// 
// Copyright 2023 Per Abrahamsen and KU.
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

#define BUILD_DLL

#include "program.h"
#include "block_model.h"
#include "treelog.h"
#include "librarian.h"
#include "symbol.h"
#include "assertion.h"
#include "metalib.h"

#include <boost/asio/io_service.hpp>
#include <boost/dll/runtime_symbol_info.hpp>
#include <boost/process.hpp>
#include <sstream>
#include <vector>
#include <memory>
#include <thread>
#include <fstream>

namespace bp = boost::process;

struct ProgramSpawn : public Program
{
  struct Cleaner
  {
    Treelog& msg;
    int& running;
    const symbol name;
    void operator ()(int exit, const std::error_code& ec_in)
    {
      std::ostringstream tmp;
      tmp << "'" + name + "' ";
      if (exit == EXIT_SUCCESS)
	{
	  tmp << "finished sucessfully";
#ifdef CPP23			// noreplace is a c++23 feature.
	  std::ofstream success (name + "/SUCCESS", std::ios::noreplace);
	  if (!success)
	    {
	      tmp << "... twice? Possible race condition, check results";
	      std::ofstream toomuchwin (name + "/SUCCESS_FAILED");
	    }
#else
	  std::ofstream file (name + "/SUCCESS");
#endif
	}
      else
	{
	  tmp << "failed with exit code " << exit
	      << " (error " << ec_in.message ()  << ")";
	  std::ofstream file (name + "/FAILED");
	  file << "Exit code " << exit
	       << " (error " << ec_in.message ()  << ")";
	}
      msg.message (tmp.str ());
      running--;
    }
    explicit Cleaner (Treelog& m, int& r, const symbol p)
      : msg (m),
	running (r),
	name (p)
    { }
  };


  // Content.
  const symbol input_directory;
  const symbol exe;
  const int parallel;
  const std::vector<symbol> program;
  const std::vector<symbol> directory;
  const std::vector<symbol> file;
  const int length;
  
  // State.
  int index;
  int running;
  std::vector<std::shared_ptr<bp::child>> children;
  std::vector<std::shared_ptr<Cleaner>> cleaners;
  boost::asio::io_service ios;

  bool done () const
  { return index >= length; }

  void spawn_all (Treelog& msg)
  {
    while (!done () && (parallel == 0 || running < parallel))
      spawn_one (msg);
  }
  void spawn_one (Treelog& msg)
  {
    // Find program to run (if any).
    // 0: Don't run a named program.
    // 1: Always run this program.
    // n: Use these programs.
    daisy_assert (program.size () < 2 || program.size () > index);
    const symbol program_one =
      (program.size () == 0) ? Attribute::None ()
      : ((program.size () == 1) ? program[0]
	 : program[index]);

    // Find setup file.
    // 1: Always use this file.
    // n: Run these files.
    daisy_assert (file.size () > 0);
    daisy_assert (file.size () == 1 || file.size () > index);
    const symbol file_one = (file.size () == 1) ? file[0] : file[index];
    
    // Directory.
    // 0: Use program names.
    // n: Use these directories.
    daisy_assert (directory.size () == 0 || directory.size () > index);
    const symbol directory_one
      = (directory.size () == 0) ? program_one : directory[index];
    Treelog::Open nest (msg, directory_one);

    index++;
    if (!boost::filesystem::create_directory (directory_one.name ()))
      {
	std::ostringstream tmp;
	tmp << "Skipping '" << directory_one << "'";
	msg.message (tmp.str ());
	return;
      }
    std::ostringstream tmp;
    tmp << "Spawning '" << directory_one << "'";
    msg.message (tmp.str ());
    msg.flush ();
    std::shared_ptr<Cleaner> cleaner (new Cleaner (msg, running,
						   directory_one));
    if (program_one == Attribute::None ())
      {
	std::shared_ptr<bp::child> c
	  (new bp::child (exe.name (),
			  ios,
			  bp::start_dir = directory_one.name (),
			  bp::on_exit = *cleaner,
			  "-D", input_directory.name (),
			  "-q", file_one.name ()));
	children.push_back (c);
      }
    else
      {
	std::shared_ptr<bp::child> c
	  (new bp::child (exe.name (),
			  ios,
			  bp::start_dir = directory_one.name (),
			  bp::on_exit = *cleaner,
			  "-D", input_directory.name (),
			  "-q", file_one.name (),
			  "-p", program_one.name()));
	children.push_back (c);
      }
    cleaners.push_back (cleaner);
    running++;
  }

  // Use.
  bool run (Treelog& msg)
  {
    TREELOG_MODEL (msg);
    if (file.size () == 0)
      {
	msg.warning ("No setup file");
	return true;
      }
    {
      std::ostringstream tmp;
      tmp << "Executable '" << exe << "'";
      msg.message (tmp.str ());
    }
    if (parallel == 0)
      msg.message ("Unlimited parallelism!");
    else
      {
	std::ostringstream tmp;
	tmp << "Spawning at most " << parallel << " programs in parallel";
	msg.message (tmp.str ());
      }
    msg.message ("Initial spawn");
    spawn_all (msg);
    msg.message ("Running...");
    while (running > 0 && !done ())
      {
	ios.run_one ();
	spawn_all (msg);
      }
    ios.reset ();
    ios.run ();
    msg.message ("Done");
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }
  bool check (Treelog&)
  { return true; }

  static std::vector<symbol> find_file (const BlockModel& al)
  {
    const std::vector<symbol>& al_file = al.name_sequence ("file");
    if (al_file.size () > 0)
      return al_file;

    const std::vector<symbol>& parsed = al.metalib().parser_files ();

    std::vector<symbol> result;
    if (parsed.size () > 0)
      result.push_back (parsed.back ());

    return result;
  }
    
    
  ProgramSpawn (const BlockModel& al)
    : Program (al),
      input_directory (al.name ("input_directory")),
      exe (al.name ("exe",  boost::dll::program_location().string ())),
      parallel (al.integer ("parallel", std::thread::hardware_concurrency ())),
      program (al.name_sequence ("program")),
      directory (al.name_sequence ("directory")),
      file (find_file (al)),
      length (std::max ({ program.size (),
			  directory.size (),
			  file.size ()})),
      index (0),
      running (0)
  { }
  ~ProgramSpawn ()
  {  }
};

static struct ProgramSpawnSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramSpawn (al); }
  ProgramSpawnSyntax ()
    : DeclareModel (Program::component, "spawn", "\
Spawn a number of programs in parallel.")
  { }

  static bool check_alist (const Metalib& metalib, const Frame& al,
			   Treelog& msg)
  {
    bool ok = true;

    const int program_len = al.name_sequence ("program").size ();
    const int directory_len = al.name_sequence ("directory").size ();
    const int file_len = al.name_sequence ("file").size ();

    if (program_len > 1 && directory_len > 0 && program_len != directory_len)
      {
	ok = false;
	std::ostringstream tmp;
	tmp << "You have " << program_len << " programs but "
	    << directory_len << " directories";
	msg.error (tmp.str ());
      }
    
    if (file_len > 1 && directory_len > 0 && file_len != directory_len)
      {
	ok = false;
	std::ostringstream tmp;
	tmp << "You have " << file_len << " files but "
	    << directory_len << " directories";
	msg.error (tmp.str ());
      }
    
    if (program_len > 1 && file_len > 1 && program_len != file_len)
      {
	ok = false;
	std::ostringstream tmp;
	tmp << "You have " << program_len << " programs but "
	    << file_len << " files";
	msg.error (tmp.str ());
      }
    const int sim_len = std::max (program_len, file_len);
    if (directory_len > sim_len)
      {
	ok = false;
	std::ostringstream tmp;
	tmp << "You have " << sim_len << " simulations but "
	    << directory_len << " directories";
	msg.error (tmp.str ());
      }
    const int named_len = std::max (program_len, directory_len);
    if (file_len > named_len)
      {
	ok = false;
	std::ostringstream tmp;
	tmp << "You have " << file_len << " files but "
	    << named_len << " names";
	msg.error (tmp.str ());
      }
    
    return ok;
  }
  
  void load_frame (Frame& frame) const
  {
    frame.add_check (check_alist);
    frame.declare_string ("input_directory", Attribute::Const, "\
When trying to open files from the current directory look here instead.");
    frame.set ("input_directory", "..");
    frame.declare_string ("exe", Attribute::OptionalConst,  "\
Name of executable to spawn. By default, the currently running executable.");
    frame.declare_integer ("parallel", Attribute::OptionalConst, "\
Maximum number of programs to run in parallel.\n\
By default this is determined by the hardware.\n\
Select 0 to spawn all in parallel.");
    frame.declare_string ("program", Attribute::Const, Attribute::Variable, "\
Names of programs to run.");
    frame.set_empty ("program");
    frame.declare_string ("directory", Attribute::Const, Attribute::Variable, "\
Directories to run the programs in.\n\
By default, this will be the names of the programs.");
    frame.set_empty ("directory");
    frame.declare_string ("file", Attribute::Const, Attribute::Variable, "\
Setup files containing programs to run.\n\
By default, use the present setup file.");
    frame.set_empty ("file");
  }
} ProgramSpawn_syntax;

// program_spawn.C ends here.
