// metalib.C -- A library of libraries.
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

#define BUILD_DLL

#include "metalib.h"
#include "intrinsics.h"
#include "librarian.h"
#include "library.h"
#include "block_model.h"
#include "assertion.h"
#include "memutils.h"
#include "librarian.h"
#include "path.h"
#include "units.h"
#include "unit_model.h"
#include "frame_model.h"
#include <map>
#include <sstream>

struct Metalib::Implementation
{
  // Content.
  const Frame::load_frame_t load_frame;
  Path path;
  std::unique_ptr<Units> units;

  typedef std::map<symbol, Library*> library_map;
  library_map all;
  
  int sequence;
  std::vector<symbol> parser_files;
  auto_vector<const Frame*> parser_inputs;

  // Create and destroy.
  void initialize (Metalib& metalib)
  {
    daisy_assert (!units.get ());
    units.reset (new Units (metalib));
    daisy_assert (units.get ());
  }
  Implementation (Frame::load_frame_t lf)
    : load_frame (lf),
      all (Librarian::intrinsics ().clone ()),
      sequence (0)
  { }
  ~Implementation ()
  { map_delete (all.begin (), all.end ()); }
};

const Units& 
Metalib::units () const
{ return *impl->units; }

const Unit& 
Metalib::get_unit (const symbol name) const
{ return units ().get_unit (name); }

Path& 
Metalib::path () const
{ return impl->path; }

bool
Metalib::exist (const symbol name) const
{ return impl->all.find (name) != impl->all.end (); }

Library& 
Metalib::library (const symbol name) const
{ return *impl->all[name]; }

Library& 
Metalib::library (const char *const name) const
{ return library (symbol (name)); }

void
Metalib::all (std::vector<symbol>& libraries) const
{ 
  for (Implementation::library_map::const_iterator i = impl->all.begin (); 
       i != impl->all.end ();
       i++)
    libraries.push_back (symbol ((*i).first)); 
}

void 
Metalib::clear_all_parsed ()
{
  for (Implementation::library_map::iterator i = impl->all.begin (); 
       i != impl->all.end (); 
       i++)
    (*i).second->clear_parsed ();
}

#if 0
void 
Metalib::refile_parsed (const std::string& from, const std::string& to)
{
  for (Implementation::library_map::iterator i = impl->all.begin (); 
       i != impl->all.end (); 
       i++)
    (*i).second->refile_parsed (from, to);
}
#endif

void 
Metalib::added_object (const symbol library, const symbol object)
{
  // Make sure we can use units right after we defined them.
  if (library == symbol (MUnit::component))
    impl->units->add_unit (*this, object);
}

int 
Metalib::get_sequence ()
{ 
  impl->sequence++;
  // Nobody will ever need more than two billion objects --- Per 1998.
  daisy_assert (impl->sequence > 0);
  return impl->sequence;
}

const std::vector<symbol>& 
Metalib::parser_files () const
{ return impl->parser_files; }

void 
Metalib::add_parser_file (const symbol file)
{ impl->parser_files.push_back (file); }

const std::vector<const Frame*>& 
Metalib::parser_inputs () const
{ return impl->parser_inputs; }

void 
Metalib::set_parser_inputs (const std::vector<boost::shared_ptr<const FrameModel>/**/>& inputs)
{
  sequence_delete (impl->parser_inputs.begin (), impl->parser_inputs.end ());
  impl->parser_inputs.erase (impl->parser_inputs.begin (), 
                             impl->parser_inputs.end ());
  daisy_assert (impl->parser_inputs.size () == 0);
  for (size_t i = 0; i < inputs.size (); i++)
    impl->parser_inputs.push_back (&inputs[i]->clone ());
}

void
Metalib::reset ()
{ 
  Frame::reset ();
  load_frame_t load_frame = impl->load_frame;
  impl.reset (new Implementation (load_frame)); 
  load_frame (*this);
  impl->initialize (*this);
}

Metalib::Metalib (load_frame_t load_frame)
  : Frame (),
    impl (new Implementation (load_frame))
{
  load_frame (*this);
  impl->initialize (*this);
}

Frame& 
Metalib::clone () const         // Used by checkpoint.
{ 
  struct FrameClone : public Frame
  {
    Frame& clone () const 
    { return *new FrameClone (*this); }
    FrameClone (const Frame& frame)
      : Frame (frame)
    { }
  };
  return *new FrameClone (*this); 
}

Metalib::~Metalib ()
{ }

// metalib.C ends here

