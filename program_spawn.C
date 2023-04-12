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
#include "block_top.h"
#include "block_model.h"
#include "treelog.h"
#include "librarian.h"
#include "symbol.h"
#include "assertion.h"

#include <boost/asio/io_service.hpp>
#include <boost/dll/runtime_symbol_info.hpp>
#include <boost/process.hpp>
#include <sstream>
#include <vector>
#include <memory>
#include <thread>

namespace bp = boost::process;

struct ProgramSpawn : public Program
{
  struct Cleaner
  {
    Treelog& msg;
    int& running;
    const symbol program;
    
    void operator ()(int exit, const std::error_code& ec_in)
    {
      msg.message ("'" + program + "' finished");
      running--;
    }
    explicit Cleaner (Treelog& m, int& r, const symbol p)
      : msg (m),
	running (r),
	program (p)
    { }
  };


  // Content.
  const std::vector<symbol> programs;
  const symbol exe;
  const symbol setup;
  const int parallel;
  
  // State.
  int index;
  int running;
  std::vector<std::shared_ptr<bp::child>> children;
  std::vector<std::shared_ptr<Cleaner>> cleaners;
  boost::asio::io_service ios;

  bool done () const
  { return index >= programs.size (); }

  void spawn_all (Treelog& msg)
  {
    while (!done () && (parallel == 0 || running < parallel))
      spawn_one (msg);
  }
  void spawn_one (Treelog& msg)
  {
    daisy_assert (index < programs.size ());
    const symbol program = programs[index];
    index++;
    if (!boost::filesystem::create_directory (program.name ()))
      {
	std::ostringstream tmp;
	tmp << "Skipping '" << program << "'";
	msg.message (tmp.str ());
	return;
      }
    std::ostringstream tmp;
    tmp << "Spawning '" << program << "'";
    msg.message (tmp.str ());
    msg.flush ();
    std::shared_ptr<Cleaner> cleaner
      (new Cleaner (msg, running, program));
    std::shared_ptr<bp::child> c
      (new bp::child (exe.name (),
		      ios,
		      bp::start_dir = program.name (),
		      bp::on_exit = *cleaner,
		      "-q",
		      setup.name (),
		      "-p", program.name()));
    children.push_back (c);
    cleaners.push_back (cleaner);
    running++;
  }

  // Use.
  bool run (Treelog& msg)
  {
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

  ProgramSpawn (const BlockModel& al)
    : Program (al),
      programs (al.name_sequence ("run")),
      exe (al.name ("exe",  boost::dll::program_location().string ())),
      setup (al.name ("setup")),
      parallel (al.integer ("parallel", std::thread::hardware_concurrency ())),
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
  void load_frame (Frame& frame) const
  {
    frame.declare_string ("run", Attribute::Const, Attribute::Variable, "\
Names of programs to run.");
    frame.declare_string ("exe", Attribute::OptionalConst,  "\
Name of executable to spawn.  By default, the same is currently ruinning.");
    frame.declare_string ("setup", Attribute::Const, "\
Setup file containing programs to run.");
    frame.declare_integer ("parallel", Attribute::OptionalConst, "\
Maximum number of programs to run in parallel.\n\
By default this is determined by the hardware.\n\
Select 0 to spawn all in parallel.");
  }
} ProgramSpawn_syntax;

// program_spawn.C ends here.
