// treelog_store.C -- Store messages and distribute to client treelogs.
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


#include "treelog_store.h"
#include "assertion.h"
#include "memutils.h"
#include <vector>

class TreelogStore::Implementation
{
  // Type.
private:
  static const int is_unknown = -1;
  static const int is_debug = -2;
  static const int is_plain = -3;
  static const int is_warning = -4;
  static const int is_error = -5;
  static const int is_close = -7;
  static const int is_touch = -8;
  static const int is_flush = -9;

  // Clients.
private:
  auto_vector<Treelog*> client;
  void propagate (int nest, const std::string& text)
  {
    for (size_t i = 0; i < client.size (); i++)
      propagate (*client[i], nest, text);
  }
  static void propagate (Treelog& msg, int nest, const std::string& text)
  {
    switch (nest)
      {
      case is_unknown:
        msg.entry (text);
        break;
      case is_debug:
        msg.debug (text);
        break;
      case is_plain:
        msg.message (text);
        break;
      case is_warning:
        msg.warning (text);
        break;
      case is_error:
        msg.error (text);
        break;
      case is_close:
        msg.close ();
        break;
      case is_touch:
        msg.touch ();
        break;
      case is_flush:
        msg.flush ();
        break;
      default:
        msg.open (text);
      }
  }
public:
  void add_client (Treelog *const msg)
  {
    daisy_assert (msg);
    client.push_back (msg);
    for (size_t i = 0; i < entries.size (); i++)
      propagate (*msg, entries[i].nest, entries[i].text);
  }

  // Entries.
private:
  int level;
  struct Entry
  {
    int nest;
    std::string text;
    Entry& operator= (const Entry& other)
    { 
      nest = other.nest; 
      text = other.text; 
      return *this;
    }
    Entry (int n, const std::string& t)
      : nest (n),
        text (t)
    { }
  };
  std::vector<Entry> entries;
  void add (int nest, const std::string& text)
  { 
    entries.push_back (Entry (nest, text)); 
    propagate (nest, text);
  }
public:
  void open (const std::string& name)
  { 
    add (level, name); 
    level++;
  }
  void close ()
  {
    add (is_close, ""); 
    level--;
    daisy_assert (level >= 0);
  }
  void debug (const std::string& text)
  { add (is_debug, text); }
  void entry (const std::string& text)
  { add (is_unknown, text); }
  void message (const std::string& text)
  { add (is_plain, text); }
  void warning (const std::string& text)
  { add (is_warning, text); }
  void error (const std::string& text)
  { add (is_error, text); }
  void touch ()
  { add (is_touch, ""); }
  void flush ()
  { add (is_flush, ""); }

  // Create and Destroy.
public:
  Implementation ()
    : level (0)
  { }
  ~Implementation ()
  { daisy_assert (level == 0); }
};

void 
TreelogStore::open (const std::string& name)
{ impl->open (name); }

void 
TreelogStore::close ()
{ impl->close (); }

void 
TreelogStore::debug (const std::string& text)
{ impl->debug (text); }

void 
TreelogStore::entry (const std::string& text)
{ impl->entry (text); }

void 
TreelogStore::message (const std::string& text)
{ impl->message (text); }

void 
TreelogStore::warning (const std::string& text)
{ impl->warning (text); }

void 
TreelogStore::error (const std::string& text)
{ impl->error (text); }

void 
TreelogStore::touch ()
{ impl->touch (); }

void 
TreelogStore::flush ()
{ impl->flush (); }

void 
TreelogStore::add_client (Treelog *const msg)
{ impl->add_client (msg); }

TreelogStore::TreelogStore ()
  : impl (new Implementation ())
{ }

TreelogStore::~TreelogStore ()
{ }

// treelog_store.C ends here.
