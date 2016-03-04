// timestep.h -- Time difference.
// 
// Copyright 2006 Per Abrahamsen and KVL.
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

#ifndef TIMESTEP_H
#define TIMESTEP_H

#include "time.h"
#include "vcheck.h"
#include <string>

class Frame;
class FrameSubmodel;
class Block;

class Timestep
{
  // Content.
private:
  struct Implementation;
  const std::unique_ptr<Implementation> impl;

  // Prebuild values.
public:
  static const Timestep& day ();
  static const Timestep& hour ();
  static const Timestep& minute ();
  static const Timestep& second ();
  static const Timestep& microsecond ();
  static const Timestep& zero ();

  // Extract elements.
public:
  int days () const;
  int hours () const;
  int minutes () const;
  int seconds () const;
  int microseconds () const;

  // Extract totals.
public:
  double total_hours () const;
  std::string print () const;

  // Create.
public:
  struct GenCheck : public VCheck
  {
  private:
    virtual bool check_dt (double dt, Treelog&) const = 0;
    bool verify (const Metalib&, const Frame&, const symbol, Treelog&) const;
  };
  static const VCheck& positive ();
  static const VCheck& non_zero ();
  static void load_syntax (Frame&);
  static void load_frame (Frame&);
  static const Timestep& null ();
  static Timestep build_hours (const double dt /* h */);
  explicit Timestep (const Block&);
  Timestep (int days, int hours, int minutes, int seconds,
            int microseconds = 0);
  ~Timestep ();
  Timestep (const Timestep&);
  const Timestep& operator= (const Timestep&);
private:                    
  explicit Timestep ();
  explicit Timestep (const FrameSubmodel&);
};

// Time operations.
void operator+= (Time&, const Timestep&);

Time operator+ (const Time&, const Timestep&);
Time operator- (const Time&, const Timestep&);

Timestep operator- (const Timestep& step);
Timestep operator- (const Time&, const Time&);
Timestep operator+ (const Timestep&, const Timestep&);
Timestep operator/ (const Timestep&, int divisor);
bool operator== (const Timestep&, const Timestep&);

#endif // TIMESTEP_H
