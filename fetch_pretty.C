// fetch_pretty.C --- Fetch data from a log model to a summary model.
// 
// Copyright 2003-2004 Per Abrahamsen and KVL
// Copyright 2009 University of Copenhagen
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

#include "fetch_pretty.h"
#include "librarian.h"

void
FetchPretty::clear (const std::vector<FetchPretty*>& fetch)
{ 
  for (size_t i = 0; i != fetch.size (); i++)
    fetch[i]->clear ();
}

void
FetchPretty::initialize (const std::vector<FetchPretty*>& fetch,
                         std::vector<Select*>& select, Treelog& msg)
{ 
  for (size_t i = 0; i != fetch.size (); i++)
    fetch[i]->initialize (select, msg);
}

FetchPretty::FetchPretty (const FrameSubmodel& al)
  : Fetch (al)
{ }

FetchPretty::FetchPretty (const symbol key)
  : Fetch (key)
{ }

void
FetchPretty::load_syntax (Frame& frame)
{ Fetch::load_syntax (frame); }

static DeclareSubmodel 
fetch_prettysubmodel (FetchPretty::load_syntax, "FetchPretty", "\
A summary file line.");

// fetch_pretty.C ends here.
