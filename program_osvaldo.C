// program_osvaldo.C -- Tools for Osvaldo
// 
// Copyright 2010 KU.
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
#include "librarian.h"
#include "block_model.h"
#include "mathlib.h"
#include "path.h"
#include "treelog.h"
#include <cstdio>
#include <istream>
#include <sstream>
#include <iomanip>
#include <memory>
#include <numeric>

struct ProgramOsvaldo : public Program
{
  Path& path;
  const int pars;
  const symbol par_file_prefix;
  const symbol par_file_suffix;
  const int par_width;
  const symbol measured_file;
  std::vector<double> measured;
  double measured_diff2;

  bool read_measure (Treelog& msg)
  {
    Treelog::Open next (msg, measured_file);
    std::unique_ptr<std::istream> measure = path.open_file (measured_file);
    if (!measure.get ())
      {
        msg.error ("Could not open file");
        return false;
      }
    size_t count = 0;
    double measured_avg = 0.0;
    while (true)
      {
        double number;
        *measure >> number;
        if (!*measure)
          break;
        measured.push_back (number);
        measured_avg += number;
        count++;
      }
    if (!measure->eof ())
      {
        msg.error ("Problems reading file");
        return false;
      }
    if (count < 1)
      {
        msg.error ("No data");
        return false;
      }
    measured_avg /= (count + 0.0);
    daisy_assert (count == measured.size ());
    measured_diff2 = 0.0;
    for (size_t i = 0; i < count; i++)
      measured_diff2 += sqr (measured[i] - measured_avg);
    if (!std::isnormal (measured_diff2))
      {
        msg.error ("I don't like your data");
        return false;
      }
    return true;
  }
                     
  // Use.
  bool run (Treelog& msg)
  {
    if (!read_measure (msg))
      return false;
    const size_t size = measured.size ();

    std::ostringstream result;
    for (int par = 1; par <= pars; par++)
      {
        // File name.
        std::ostringstream par_file_name_stream;
        par_file_name_stream << par_file_prefix
                             << std::setw (5) << std::setfill ('0')
                             << par
                             << std::setw (0) << par_file_suffix;
        const symbol par_file_name = par_file_name_stream.str ();
        Treelog::Open next (msg, "Reading " + par_file_name);

        // Read data.
        std::unique_ptr<std::istream> par_file = path.open_file (par_file_name);
        if (!par_file.get ())
          {
            msg.error ("Could not open file");
            return false;
          }
        std::vector<double> simulated;
        while (true)
          {
            double number;
            *par_file >> number;
            if (!*par_file)
              break;
            simulated.push_back (number);
          }
        if (!par_file->eof ())
          {
            msg.error ("Problems reading file");
            return false;
          }

        // Check data.
        if (simulated.size () != size)
          {
            std::ostringstream tmp;
            tmp << "Has " << simulated.size () << " simulated numbers, but "
                << size << " measured numbers";
            msg.error (tmp.str ());
            return false;
          }
        
        // Find squared difference:
        double diff2 = 0;
        for (int i = 0; i < size; ++i)
          diff2 += sqr (simulated[i] - measured[i]);
        
        // Output result.
        const double simsum 
          = std::accumulate (simulated.begin (), simulated.end (), 0.0);
        daisy_assert (measured_diff2 > 0.0);
        const double E = 1.0 - diff2 / measured_diff2;
        result << par << "\t" << simsum << "\t" << E << "\n";
      }
    msg.message (result.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }

  bool check (Treelog& msg)
  {
    bool ok = true;
    int required_width = 0;
    for (int digits = pars; digits; digits /= 10)
      required_width++;
    if (required_width > par_width && par_width > 0)
      msg.warning ("The 'par_width' parameter is to small");
    return ok; 
  }

  ProgramOsvaldo (const BlockModel& al)
    : Program (al),
      path (al.path ()),
      pars (al.integer ("pars")),
      par_file_prefix (al.name ("par_file_prefix")),
      par_file_suffix (al.name ("par_file_suffix")),
      par_width (al.integer ("par_width")),
      measured_file (al.name ("measured_file"))
  { }
  ~ProgramOsvaldo ()
  { }
};

static struct ProgramOsvaldoSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramOsvaldo (al); }
  ProgramOsvaldoSyntax ()
    : DeclareModel (Program::component, "Osvaldo", "\
Find the modelling error.\n\
This is done between one set of measured data and multiple sets of\n\
simulated data.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("pars", Attribute::Const, "\
Number of simulated dataset to compare.");
    frame.declare_string ("par_file_prefix", Attribute::Const, "\
Beginning of file name before simulation number.");
    frame.declare_string ("par_file_suffix", Attribute::Const, "\
End of file name after simulation number.");
    frame.declare_integer ("par_width", Attribute::Const, "\
Number of digits in simulated file name.");
    frame.declare_string ("measured_file", Attribute::Const, "\
Name of file containing measurments.");
  }
} ProgramOsvaldo_syntax;

// program_osvaldo.C ends here.
