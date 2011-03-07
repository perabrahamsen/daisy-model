// rootdens_rootmatch.C -- Match root data table with GP2D model.
// 
// Copyright 2011 KU.
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
#include "GP2D.h"
#include "iterative.h"
#include "lexer_table.h"
#include "librarian.h"
#include "check.h"
#include "units.h"
#include "assertion.h"
#include "mathlib.h"

#include <sstream>

struct ProgramRootmatch : public Program
{
  // Read data.
  const Units& units;
  LexerTable lex;
  const symbol pos_dim;
  const symbol dens_dim;
  int c_y_pos;
  int c_z_min;
  int c_z_max;
  int c_density;

  // GP2D function.
  struct GP2Dfun : public Iterative::PointFunction
  {
    GP2D root;

    double value (const Iterative::Point& p) const
    {
      daisy_assert (p.size () == 2);
      const double x = p[0];
      const double z = p[1];
      return root.density (x, z);
    }
    
    GP2Dfun (const double row_position /* [cm] */,
             const double row_distance /* [cm] */,
             const double DensRtTip /* [cm/cm^3] */,
             const double SpRtLength /* [m/g] */)
      : root (row_position, row_distance, DensRtTip, SpRtLength)
    { }
  };
  GP2Dfun gp2d;

  // Utils
  double get_value (const symbol from, const symbol to, std::string& entry)
  { return units.convert (from, to, lex.convert_to_double (entry)); }

  // Use.
  bool run (Treelog& msg)
  {
    std::vector<Iterative::PointValue> obs;    

    static const symbol dens_dim_to ("cm/cm^3");
    while (lex.good ())
      {
        // Read entries.
        std::vector<std::string> entries;
        if (!lex.get_entries (entries))
          {
            if (!lex.good ())
              break;
            continue;
          }
        const double y_pos = get_value (pos_dim,  Units::cm (), 
                                        entries[c_y_pos]);
        const double z_min = get_value (pos_dim,  Units::cm (), 
                                        entries[c_z_min]);
        const double z_max = get_value (pos_dim,  Units::cm (), 
                                        entries[c_z_max]);
        const double z_pos = -0.5 * (z_min + z_max);
        const double density = get_value (dens_dim, dens_dim_to, 
                                          entries[c_density]);
        Iterative::Point p;
        p.push_back (y_pos);
        p.push_back (-z_pos);
        Iterative::PointValue pv;
        pv.point = p;
        pv.value = density;
        obs.push_back (pv);
      }

    // Find best fit.
    struct ToMinimize : Iterative::PointFunction
    {
      const std::vector<Iterative::PointValue>& obs;
      GP2Dfun& fun;
      const int debug;
      Treelog& msg;

      double value (const Iterative::Point& p) const
      {
        Treelog::Open nest (msg, "minimize");
        
        daisy_assert (p.size () == 4);
        const double SoilDepth = p[0];
        const double CropDepth = p[1];
        const double CropWidth = p[2];
        const double WRoot = p[3];

        if (debug > 0)
          {
            std::ostringstream tmp;
            tmp << "SoilDepth = " << SoilDepth << " cm\n"
                << "CropDepth = " << CropDepth << " cm\n"
                << "CropWidth = " << CropWidth << " cm\n"
                << "WRoot = " << (0.01 * WRoot)  << " Mg DM/ha";
            msg.message (tmp.str ());
          }
       
        // Restrictions.
        const double LARGE_NUMBER = 42.42e42;
        if (SoilDepth <= 0)
          return LARGE_NUMBER;
        if (CropDepth <= 0)
          return LARGE_NUMBER;
        if (CropWidth <= 0)
          return LARGE_NUMBER;
        if (WRoot <= 0)
          return LARGE_NUMBER;

        bool ok = fun.root.set_dynamic (SoilDepth, CropDepth, CropWidth, WRoot,
                                        debug, msg);
        if (!ok)
          return LARGE_NUMBER;

        const double Rsqr = Iterative::RSquared (obs, fun);
        if (debug > 0)
          {
            std::ostringstream tmp;
            tmp << "R^2 = " << Rsqr;
            msg.message (tmp.str ());
          }

        return -Rsqr;
      }

      ToMinimize (const std::vector<Iterative::PointValue>& o, GP2Dfun& f,
                  const int d, 
                  Treelog& m)
        : obs (o),
          fun (f),
          debug (d),
          msg (m)
      { }
    };
    ToMinimize to_minimize (obs, gp2d, 0, msg);

    // Initialial guess.
    const double SoilDepth = 150; // [cm]
    const double CropDepth = 70; // [cm]
    const double CropWidth = 100; // [cm]
    const double WRoot = 50;     // 150 [g DM/m^2] = 1.5 [Mg DM/ha]
    Iterative::Point start;
    start.push_back (SoilDepth);
    start.push_back (CropDepth);
    start.push_back (CropWidth);
    start.push_back (WRoot);
    daisy_assert (start.size () == 4);
    const double epsilon = 0.01;
    const size_t min_iter = 10000;
    const size_t max_iter = 300000;
    Iterative::Point result;
    const bool solved = Iterative::NelderMead (min_iter, max_iter, epsilon,
                                               to_minimize, start, result);
    const double Rsqr = -to_minimize.value (result);
    std::ostringstream out;
    
    out << "R^2 = " << Rsqr << " ";
    if (solved)
      out << " (solved)\n";
    else
      out << " (no solution)\n";
    out << "SoilDepth = " << result[0] << " cm\n"
        << "CropDepth = " << result[1] << " cm\n"
        << "CropWidth = " << result[2] << " cm\n"
        << "WRoot = " << (0.01 * result[3])  << " Mg DM/ha\n";

    out << "L00 = " << gp2d.root.L00 << " cm/cm^3\n"
        << "a_x = " << gp2d.root.a_x << " cm^-1\n"
        << "a_z = " << gp2d.root.a_z << " cm^-1\n"
        << "d_a = " << gp2d.root.d_a << " cm\n"
        << "k* = " << gp2d.root.kstar << "\n";

    out << "Y\tZ\tobs\tsim\n"
        << Units::cm () << "\t" << Units::cm () << "\t" 
        << dens_dim_to << "\t" << dens_dim_to;
    for (size_t i = 0; i < obs.size (); i++)
      {
        const double x = obs[i].point[0];
        const double z = obs[i].point[1];
        out << "\n" << x << "\t" << z 
            << "\t" << obs[i].value << "\t" << gp2d.root.density (x, z);
      }
    msg.message (out.str ());
    return true;
  }

  // Create and Destroy.
  void initialize (Metalib&, Block& al)
  { 
    if (!lex.read_header (al.msg ()))
      return;

    c_y_pos = lex.find_tag ("Position Y");
    c_z_min = lex.find_tag ("Position Z minimum");
    c_z_max = lex.find_tag ("Position Z maximum");
    c_density = lex.find_tag ("Root lenght density");
  }

  bool check (Treelog& msg)
  {
    if (!lex.good ())
      return false;
    
    bool ok = true;
    if (c_y_pos < 0)
      {
        msg.error ("Position Y: tag missing");
        ok = false;
      }
    if (c_z_min < 0)
      {
        msg.error ("Position Z minimum: tag missing");
        ok = false;
      }
    if (c_z_max < 0)
      {
        msg.error ("Position Z maximum: tag missing");
        ok = false;
      }
    if (c_density < 0)
      {
        msg.error ("Root lenght density: tag missing");
        ok = false;
      }

    return ok; 
  }

  ProgramRootmatch (const BlockModel& al)
    : Program (al),
      units (al.units ()),
      lex (al),
      pos_dim (al.name ("pos_dim")),
      dens_dim (al.name ("dens_dim")),
      c_y_pos (-1),
      c_z_min (-1),
      c_z_max (-1),
      c_density (-1),
      gp2d (al.number ("row_position"),
            al.number ("row_distance"),
            al.number ("DensRtTip"),
            al.number ("SpRtLength"))
  { }
  ~ProgramRootmatch ()
  { }
};

static struct ProgramRootmatchSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramRootmatch (al); }
  ProgramRootmatchSyntax ()
    : DeclareModel (Program::component, "rootmatch", "\
Match root data with GP2D model.")
  { }
  bool check_alist (const Metalib&, const Frame& al, Treelog& msg)
  {
    bool ok = true;
    return ok;
  }
  void load_frame (Frame& frame) const
  { 
    Model::load_model (frame);
    LexerTable::load_syntax (frame);
    frame.declare_string ("pos_dim", Attribute::Const, "\
Position dimension");
    frame.declare_string ("dens_dim", Attribute::Const, "\
Root density dimension.");

    frame.declare ("row_position", "cm", Attribute::State, "\
Horizontal position of row crops.");
    frame.set ("row_position", 0.0);
    frame.declare ("row_distance", "cm", Attribute::State, 
                "Distance between rows of crops.");
    frame.declare ("DensRtTip", "cm/cm^3", Check::positive (), Attribute::Const,
                "Root density at (potential) penetration depth.");
    frame.set ("DensRtTip", 0.1);
    frame.declare ("SpRtLength", "m/g", Check::positive (), Attribute::Const,
                   "Specific root length");
    frame.set ("SpRtLength", 100.0);
  }
} ProgramRootmatch_syntax;

// program_rootmatch.C ends here.
