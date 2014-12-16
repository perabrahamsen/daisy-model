// fetch.C --- Fetch data from a log model to a summary model.
// 
// Copyright 2003-2004 Per Abrahamsen and KVL
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

#include "fetch.h"
#include "select.h"
#include "treelog.h"
#include "frame_submodel.h"
#include "librarian.h"

void 
Fetch::missing ()
{ 
  switch (type)
    {
    case NewContent:
      type = Content;
      initial = last = 0.0;
      break;
    case Content:
      last = 0.0;
      break;
    case Flux:
      break;
    case Error:
      break;
    }
}

void 
Fetch::add (const std::vector<double>&)
{ type = Error; }

void 
Fetch::add (const double value)
{ 
  switch (type)
    {
    case NewContent:
      type = Content;
      initial = last = value;
      break;
    case Content:
      last = value;
      break;
    case Flux:
      sum += value;
      break;
    case Error:
      break;
    }
}

void 
Fetch::add (const symbol)
{ type = Error; }

symbol
Fetch::dimension () const
{ return select_dimension; }

void
Fetch::clear (const std::vector<Fetch*>& fetch)
{ 
  for (size_t i = 0; i != fetch.size (); i++)
    fetch[i]->clear ();
}

void 
Fetch::clear ()
{ 
  if (type == Content)
    type = NewContent;
  else if (type == Flux)
    sum = 0; 
}

void
Fetch::initialize (const std::vector<Fetch*>& fetch,
                   std::vector<Select*>& select, Treelog& msg)
{ 
  for (size_t i = 0; i != fetch.size (); i++)
    fetch[i]->initialize (select, msg);
}

void
Fetch::initialize (std::vector<Select*>& select, Treelog& msg)
{ 
  Treelog::Open nest (msg, this->tag);
  bool found = false;
  
  for (size_t j = 0; j != select.size (); j++)
    {
      Treelog::Open nest (msg, select[j]->tag ());

      if (this->tag == select[j]->tag ())
        {
          if (found)
            msg.warning ("Duplicate tag ignored");
          else
            {	
              select[j]->add_dest (this);
              this->select_dimension = select[j]->dimension ();
              this->type = ((select[j]->handle != Select::Handle::current)
                            && !select[j]->accumulate)
                ? Fetch::Flux 
                : Fetch::NewContent;
              found = true;
            }
        }
    }
  if (!found)
    msg.warning ("No tag found");
}

void
Fetch::load_syntax (Frame& frame)
{ 
  frame.declare_string ("tag", Attribute::Const, "\
The tag of a column in the log file to summarize in this line.");
  frame.order ("tag");
}

Fetch::Fetch (const FrameSubmodel& al)
  : tag (al.name ("tag")),
    select_dimension (Attribute::Unknown ()),
    type (Error),
    initial (-42.42e42),
    last (-42.42e42),
    sum (-42.42e42)
{ }

Fetch::Fetch (const symbol key)
  : tag (key),
    select_dimension (Attribute::Unknown ()),
    type (Error),
    initial (-42.42e42),
    last (-42.42e42),
    sum (-42.42e42)
{ }

static DeclareSubmodel fetch_submodel (Fetch::load_syntax, "Fetch", "\
A summary file line.");

// fetch.C ends here.
