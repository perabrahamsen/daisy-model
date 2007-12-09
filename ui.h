// ui.h -- Top level user interface.
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

#ifndef UI_H
#define UI_H

#include "model.h"
#include "symbol.h"

class Block;
class Toplevel;

class EXPORT UI : public Model
{
  // Content.
public:
  const symbol name;
  static const char *const component;

  // Utilities.
protected:
  void set_low_priority () const;

  // Use.
public:
  virtual void attach (Toplevel&) = 0;
  virtual void run (Toplevel&) = 0;
  virtual void failure (Toplevel&) = 0;

  // Create and Destroy.
private:
  UI ();                        // Disable.
  explicit UI (const UI&);      // Disable.
  UI& operator= (const UI&);    // Disable.
protected:
  explicit UI (Block&);
public:
  explicit UI (const char*);
  ~UI ();
};

class UIProgress : public UI
{
  // Use.
public:
  void attach (Toplevel&);
  void run (Toplevel&);
  void failure (Toplevel&);

  // Control.
private:
  bool running () const;

  // Create.
private:
  UIProgress& operator= (const UIProgress&); // Disable.
  UIProgress (const UIProgress&); // Disable.
public:
  explicit UIProgress ();
  explicit UIProgress (Block&);
  ~UIProgress ();
};

class UINone : public UI
{
  // Use.
public:
  void attach (Toplevel&);
  void run (Toplevel&);
  void failure (Toplevel&);

  // Control.
private:
  bool running () const;

  // Create.
private:
  UINone& operator= (const UINone&); // Disable.
  UINone (const UINone&); // Disable.
public:
  explicit UINone ();
  explicit UINone (Block&);
  ~UINone ();
};

#endif // UI_H
