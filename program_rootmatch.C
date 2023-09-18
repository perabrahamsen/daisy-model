// program_rootmatch.C -- Match root data table with GP2D model.
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
#include "submodeler.h"
#include <boost/math/distributions/fisher_f.hpp>

#include <sstream>

namespace math = boost::math;

struct ProgramRootmatch : public Program
{
  // Read data.
  const Units& units;
  LexerTable lex;
  const symbol pos_dim;
  const symbol dens_dim;
  const symbol tag_x;
  const symbol tag_z_min;
  const symbol tag_z_max;
  const symbol tag_density;
  int c_x_pos;
  int c_z_min;
  int c_z_max;
  int c_density;
  const int debug;
  const bool show_data;
  const bool show_match;
  const double x_offset;
  const bool tabular;
  const double row_position;
  const double min_dist;
  static const symbol dens_dim_to;
  
  // Result.
  std::vector<symbol> res_name;
  std::vector<double> res_value;
  std::vector<symbol> res_dim;
  void store (const symbol name, const double value, const symbol dim)
  {
    res_name.push_back (name);
    res_value.push_back (value);
    res_dim.push_back (dim);
  }
                         
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

  // GP1D function.
  struct GP1Dfun : public Iterative::PointFunction
  {
    GP1D root;

    double value (const Iterative::Point& p) const
    {
      daisy_assert (p.size () == 1);
      const double z = p[0];
      return root.density (z);
    }
    
    GP1Dfun (const double DensRtTip /* [cm/cm^3] */,
             const double SpRtLength /* [m/g] */)
      : root (DensRtTip, SpRtLength)
    { }
  };
  GP1Dfun gp1d;

  const double SoilDepth;

  struct Gnuplot : boost::noncopyable
  {
    const double x_start;
    const double x_offset;
    const double x_end_pos;
    const double z_end_pos;
    const double dx;
    const double dz;
    void show (const GP2D& root, std::ostream& tmp)
    {
      if (!std::isfinite (z_end_pos))
        return;
      const double x_end = std::isfinite (x_end_pos)
        ? x_end_pos + dx * 0.1 
        :  x_start + root.row_distance + dx * 0.1;
      const double z_end = z_end_pos + dz * 0.1;
      tmp << "\n";
      for (double x = x_start; x < x_end; x++)
        {
          for (double z = 0.0; z < z_end; z++)
            tmp << (x - x_offset) << "\t" << -z << "\t" << root.density (x, z) << "\n";
          tmp << "\n";
        }
      tmp << "e";
    }
    static void load_syntax (Frame& frame)
    {
      frame.declare ("x_start", "cm", Check::none (), Attribute::Const,
                     "Start table here.");
      frame.set ("x_start", 0.0);
      frame.declare ("x_offset", "cm", Check::none (), Attribute::OptionalConst,
                     "Subtract this from printed x values.");
      frame.set ("x_offset", 0.0);
      frame.declare ("x_end", "cm", Check::none (), Attribute::OptionalConst,
                     "End table here.\n\
By default, this is `x_start' plus `row_distance'.");
      frame.declare ("z_end", "cm", Check::none (), Attribute::OptionalConst,
                     "End table here at this depth.");
      frame.declare ("dx", "cm", Check::none (), Attribute::Const,
                     "Horizontal interval size.");
      frame.set ("dx", 1.0);
      frame.declare ("dz", "cm", Check::none (), Attribute::OptionalConst,
                     "Vertical interval size. \n\
By default identical to `dx'.");
      
    }

    Gnuplot (const Block& al)
      : x_start (al.number ("x_start")),
        x_offset (al.number ("x_offset")),
        x_end_pos (al.number ("x_end", NAN)),
        z_end_pos (al.number ("z_end", NAN)),
        dx (al.number ("dx")),
        dz (al.number ("dz", dx))
    { }
  };
  const std::unique_ptr<Gnuplot> gnuplot;

  // Utils
  double get_value (const symbol from, const symbol to, std::string& entry)
  { return units.convert (from, to, lex.convert_to_double (entry)); }

  // Use.
  bool run (Treelog& msg)
  {
    // Read data.
    std::vector<Iterative::PointValue> obs2d;
    std::vector<Iterative::PointValue> obs1d;

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
        const double x_pos = get_value (pos_dim,  Units::cm (), 
                                        entries[c_x_pos]);
        const double z_min = get_value (pos_dim,  Units::cm (), 
                                        entries[c_z_min]);
        const double z_max = get_value (pos_dim,  Units::cm (), 
                                        entries[c_z_max]);
        const double z_pos = 0.5 * (z_min + z_max);
        const double density = get_value (dens_dim, dens_dim_to, 
                                          entries[c_density]);
        if (z_pos <= 0.0)
          continue;

        if (z_pos < min_dist && std::fabs (x_pos - row_position) < min_dist)
          {
            std::ostringstream tmp;
            tmp << z_pos << ", " << x_pos << " too close, ignoring";
            msg.message (tmp.str ());
            continue;
          }

        Iterative::Point p2d;
        p2d.push_back (x_pos);
        p2d.push_back (z_pos);
        Iterative::PointValue pv2d;
        pv2d.point = p2d;
        pv2d.value = density;
        obs2d.push_back (pv2d);

        Iterative::Point p1d;
        p1d.push_back (z_pos);
        Iterative::PointValue pv1d;
        pv1d.point = p1d;
        pv1d.value = density;
        obs1d.push_back (pv1d);
      }

    if (obs1d.size () < 1)
      {
        msg.error ("No observations found");
        return true;
      }

    // F-test
    const bool find_SoilDepth = !std::isfinite (SoilDepth);
    const double RSS2 = solve_2D (obs2d, msg);
    const double RSS1 = solve_1D (obs1d, msg);
    const double p2 = find_SoilDepth ? 4 : 3;
    const double p1 = find_SoilDepth ? 3 : 2;
    const double n = obs1d.size ();
    daisy_assert (obs1d.size () == obs2d.size ());
    daisy_assert (RSS2 > 0.0);
    const double F = ((RSS1 - RSS2)/(p2-p1))/(RSS2/(n-p2));
    store ("F", F, "");
    const double Flim 
      = math::quantile (math::complement (math::fisher_f (p2-p1, n-p2),
                                          0.05));
    store ("F (0.05)", Flim, "");
      
    // Show results.
    std::ostringstream tmp;
    if (tabular)
      {
        tmp << "Sim";
        for (size_t i = 0; i < res_name.size (); i++)
          tmp << "\t" << res_name[i];
        tmp << "\n";
        for (size_t i = 0; i < res_dim.size (); i++)
          tmp << "\t" << res_dim[i];
        tmp << "\n" << objid;
        for (size_t i = 0; i < res_value.size (); i++)
          tmp << "\t" << res_value[i];
      }
    else
      for (size_t i = 0; i < res_name.size (); i++)
        tmp << "\n" << res_name[i] 
            << " = " << res_value[i] << " " << res_dim[i];

    gnuplot->show (gp2d.root, tmp);
    
    msg.message (tmp.str ());

    return true;
  }
  
  double solve_2D (std::vector<Iterative::PointValue>& obs,
                 Treelog& msg)
  {
    // Find best fit.
    struct ToMinimize : Iterative::PointFunction
    {
      const std::vector<Iterative::PointValue>& obs;
      GP2Dfun& fun;
      const double fixed_SoilDepth; // [cm]
      const bool find_SoilDepth;
      const int debug;
      Treelog& msg;

      double value (const Iterative::Point& p) const
      {
        Treelog::Open nest (msg, "minimize");

        if (find_SoilDepth)
          daisy_assert (p.size () == 4);
        else
          daisy_assert (p.size () == 3);
        const double CropDepth = p[0];
        const double CropWidth = p[1];
        const double WRoot = p[2];
        const double SoilDepth = find_SoilDepth ? p[3] : fixed_SoilDepth;

        if (debug > 0)
          {
            std::ostringstream tmp;
            tmp << "CropDepth = " << CropDepth << " cm\n"
                << "CropWidth = " << CropWidth << " cm\n"
                << "WRoot = " << (0.01 * WRoot)  << " Mg DM/ha\n";
            if (find_SoilDepth)
              tmp << "SoilDepth = " << SoilDepth << " cm";
            msg.message (tmp.str ());
          }
       
        // Restrictions.
        const double LARGE_NUMBER = 42.42e42;
        if (CropDepth <= 0)
          return LARGE_NUMBER;
        if (CropWidth <= 0)
          return LARGE_NUMBER;
        if (WRoot <= 0)
          return LARGE_NUMBER;
        if (find_SoilDepth && SoilDepth <= 0)
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
                  const double sd,
                  const int d, 
                  Treelog& m)
        : obs (o),
          fun (f),
          fixed_SoilDepth (sd),
          find_SoilDepth (!std::isfinite (fixed_SoilDepth)),
          debug (d),
          msg (m)
      { }
    };
    ToMinimize to_minimize (obs, gp2d, SoilDepth, debug, msg);

    // Initialial guess.
    const double default_CropDepth = 70;  // [cm]
    const double default_CropWidth = 100; // [cm]
    const double default_WRoot = 50; // 150 [g DM/m^2] = 1.5 [Mg DM/ha]
    const double default_SoilDepth = 150; // [cm]
    Iterative::Point start;
    start.push_back (default_CropDepth);
    start.push_back (default_CropWidth);
    start.push_back (default_WRoot);
    if (!std::isfinite (SoilDepth))
      start.push_back (default_SoilDepth);  // [cm]
    daisy_assert (start.size () == 4 || start.size () == 3);
    const double epsilon = 0.01;
    const size_t min_iter = 10000;
    const size_t max_iter = 300000;
    Iterative::Point result;
    const bool solved = Iterative::NelderMead (min_iter, max_iter, epsilon,
                                               to_minimize, start, result,
                                               Treelog::null ());
    const double Rsqr = -to_minimize.value (result);
    
    store ("2D R^2", Rsqr, solved ? "(solved)" : "(no solution)");
    store ("2D CropDepth", result[0], "cm");
    store ("2D CropWidth", result[1], "cm");
    store ("2D WRoot", (0.01 * result[2]) , "Mg DM/ha");;
    if (result.size () == 4)
      store ("2D SoilDepth", result[3], "cm");;
        
    store ("2D L00", gp2d.root.L00, "cm/cm^3");
    store ("2D a_x", gp2d.root.a_x, "cm^-1");
    store ("2D a_z", gp2d.root.a_z, "cm^-1");;
    if (gp2d.root.d_a > 0.0)
      {
        store ("2D d_a", gp2d.root.d_a, "cm");
        store ("2D k*", gp2d.root.kstar , "");;
      }

    // Show data.
    if (show_data)
      {
        std::ostringstream out;
        out << "X\tZ\tobs\tsim\n"
            << Units::cm () << "\t" << Units::cm () << "\t" 
            << dens_dim_to << "\t" << dens_dim_to;
        for (size_t i = 0; i < obs.size (); i++)
          {
            const double x = obs[i].point[0];
            const double z = obs[i].point[1];
            const double o = obs[i].value;
            const double f = gp2d.root.density (x, z);
            out << "\n" << x << "\t" << z << "\t" << o << "\t" << f;
          }
        msg.message (out.str ());
      }
    
    // Show errorbars
    if (show_match)
      {
        std::ostringstream out;
        typedef std::map<double, std::map<double, double>/**/> ddmap ;
        ddmap sum;
        ddmap count;
        ddmap var;
        ddmap avg;

        // Clear all.
        for (size_t i = 0; i < obs.size (); i++)
          {
            const double x = std::fabs (obs[i].point[0] - x_offset);
            const double z = obs[i].point[1];
            
            sum[x][z] = 0.0;
            count[x][z] = 0.0;
            var[x][z] = 0.0;
          }

        // Count all.
        for (size_t i = 0; i < obs.size (); i++)
          {
            const double x = std::fabs (obs[i].point[0] - x_offset);
            const double z = obs[i].point[1];
            const double o = obs[i].value;
            
            sum[x][z] += o;
            count[x][z] += 1.0;
          }

        // Find mean.
        for (ddmap::iterator i = sum.begin (); i != sum.end (); i++)
          {
            const double x = (*i).first;
            std::map<double, double> zmap = (*i).second;
            for (std::map<double, double>::iterator j = zmap.begin ();
                 j != zmap.end ();
                 j++)
              {
                const double z = (*j).first;
                avg[x][z] = sum[x][z] / count[x][z];
              }
          }

        // Find variation
        for (size_t i = 0; i < obs.size (); i++)
          {
            const double x = std::fabs (obs[i].point[0] - x_offset);
            const double z = obs[i].point[1];
            const double o = obs[i].value;
            
            var[x][z] += sqr (avg[x][z] - o);
          }

        for (ddmap::iterator i = var.begin (); i != var.end (); i++)
          {
            const double x = (*i).first;
            out << "set output \"" << objid << "-" << x << ".tex\"\n\
plot '-' using 2:1:3 notitle with xerrorbars, '-' using 2:1 notitle with lines\n";
            std::map<double, double> zmap = (*i).second;
            for (std::map<double, double>::iterator j = zmap.begin ();
                 j != zmap.end ();
                 j++)
              {
                const double z = (*j).first;
                const double dev = sqrt (var[x][z] / count[x][z]);
                out << -z << "\t" << avg[x][z] << "\t" << dev << "\n";
              }
            out << "e\n\n";
            for (double z = 0.0; z < 100.1; z++)
              out << -z << "\t" << gp2d.root.density (x + x_offset, z) << "\n";
            out << "e\n\n";
          }
        msg.message (out.str ());
      }

    // Find RSS.
    double RSS = 0.0;
    for (size_t i = 0; i < obs.size (); i++)
      {
        const double x = obs[i].point[0];
        const double z = obs[i].point[1];
        const double o = obs[i].value;
        const double f = gp2d.root.density (x, z);
        RSS += sqr (o - f);
      }
    return RSS;
  }

  double solve_1D (std::vector<Iterative::PointValue>& obs,
                   Treelog& msg)
  {
    // Find best fit.
    struct ToMinimize : Iterative::PointFunction
    {
      const std::vector<Iterative::PointValue>& obs;
      GP1Dfun& fun;
      const double fixed_SoilDepth; // [cm]
      const bool find_SoilDepth;
      const int debug;
      Treelog& msg;

      double value (const Iterative::Point& p) const
      {
        Treelog::Open nest (msg, "minimize");

        if (find_SoilDepth)
          daisy_assert (p.size () == 3);
        else
          daisy_assert (p.size () == 2);
        const double CropDepth = p[0];
        const double WRoot = p[1];
        const double SoilDepth = find_SoilDepth ? p[2] : fixed_SoilDepth;

        if (debug > 0)
          {
            std::ostringstream tmp;
            tmp << "CropDepth = " << CropDepth << " cm\n"
                << "WRoot = " << (0.01 * WRoot)  << " Mg DM/ha\n";
            if (find_SoilDepth)
              tmp << "SoilDepth = " << SoilDepth << " cm";
            msg.message (tmp.str ());
          }
       
        // Restrictions.
        const double LARGE_NUMBER = 42.42e42;
        if (CropDepth <= 0)
          return LARGE_NUMBER;
        if (WRoot <= 0)
          return LARGE_NUMBER;
        if (find_SoilDepth && SoilDepth <= 0)
          return LARGE_NUMBER;

        bool ok = fun.root.set_dynamic (SoilDepth, CropDepth, WRoot,
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

      ToMinimize (const std::vector<Iterative::PointValue>& o, GP1Dfun& f,
                  const double sd,
                  const int d, 
                  Treelog& m)
        : obs (o),
          fun (f),
          fixed_SoilDepth (sd),
          find_SoilDepth (!std::isfinite (fixed_SoilDepth)),
          debug (d),
          msg (m)
      { }
    };
    ToMinimize to_minimize (obs, gp1d, SoilDepth, debug, msg);

    // Initialial guess.
    const double default_CropDepth = 70;  // [cm]
    const double default_WRoot = 50; // 150 [g DM/m^2] = 1.5 [Mg DM/ha]
    const double default_SoilDepth = 150; // [cm]
    Iterative::Point start;
    start.push_back (default_CropDepth);
    start.push_back (default_WRoot);
    if (!std::isfinite (SoilDepth))
      start.push_back (default_SoilDepth);  // [cm]
    daisy_assert (start.size () == 3 || start.size () == 2);
    const double epsilon = 0.01;
    const size_t min_iter = 10000;
    const size_t max_iter = 300000;
    Iterative::Point result;
    const bool solved = Iterative::NelderMead (min_iter, max_iter, epsilon,
                                               to_minimize, start, result,
                                               Treelog::null ());
    const double Rsqr = -to_minimize.value (result);
    
    store ("1D R^2", Rsqr, solved ? "(solved)" : "(no solution)");
    store ("1D CropDepth", result[0], "cm");
    store ("1D WRoot", (0.01 * result[1]) , "Mg DM/ha");;
    if (result.size () == 3)
      store ("1D SoilDepth", result[2], "cm");;
        
    store ("1D L0", gp1d.root.L0, "cm/cm^3");
    store ("1D a", gp1d.root.a, "cm^-1");;
    if (gp1d.root.d_a > 0.0)
      {
        store ("1D d_a", gp1d.root.d_a, "cm");
        store ("1D k*", gp1d.root.kstar, "");
      }

    std::ostringstream out;
    if (show_data)
      out << "Z\tobs\tsim\n"
          << Units::cm () << "\t" << Units::cm () << "\t" 
          << dens_dim_to << "\t" << dens_dim_to;
    double RSS = 0.0;
    for (size_t i = 0; i < obs.size (); i++)
      {
        const double z = obs[i].point[0];
        const double o = obs[i].value;
        const double f = gp1d.root.density (z);
        RSS += sqr (o - f);
        if (show_data)
          out << "\n" << z << "\t" << o << "\t" << f;
      }
    if (show_data)
      msg.message (out.str ());
    return RSS;
  }

  // Create and Destroy.
  void initialize (Block& al)
  { 
    if (!lex.read_header (al.msg ()))
      return;

    c_x_pos = lex.find_tag (tag_x);
    c_z_min = lex.find_tag (tag_z_min);
    c_z_max = lex.find_tag (tag_z_max);
    c_density = lex.find_tag (tag_density);
  }

  bool check (Treelog& msg)
  {
    if (!lex.good ())
      return false;
    
    bool ok = true;
    if (c_x_pos < 0)
      {
        msg.error ("Position X: tag missing");
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
      tag_x (al.name ("tag_x")),
      tag_z_min (al.name ("tag_z_min")),
      tag_z_max (al.name ("tag_z_max")),
      tag_density (al.name ("tag_density")),
      c_x_pos (-1),
      c_z_min (-1),
      c_z_max (-1),
      c_density (-1),
      debug (al.integer ("debug")),
      show_data (al.flag ("show_data")),
      show_match (al.flag ("show_match")),
      x_offset (al.number ("x_offset")),
      tabular (al.flag ("tabular")),
      row_position (al.number ("row_position")),
      min_dist (al.number ("min_dist")),
      gp2d (al.number ("row_position"),
            al.number ("row_distance"),
            al.number ("DensRtTip"),
            al.number ("SpRtLength")),
      gp1d (al.number ("DensRtTip"),
            al.number ("SpRtLength")),
      SoilDepth (al.number ("SoilDepth", NAN)),
      gnuplot (submodel<Gnuplot> (al, "gnuplot"))
  { }
  ~ProgramRootmatch ()
  { }
};

const symbol 
ProgramRootmatch::dens_dim_to ("cm/cm^3");

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
    frame.declare_string ("tag_x", Attribute::Const, "\
Name of column indicating horizontal distance from row.");
    frame.declare_string ("tag_z_min", Attribute::Const, "\
Name of column indicating minimal vertical distance from row.");
    frame.declare_string ("tag_z_max", Attribute::Const, "\
Name of column indicating maximal vertical distance from row.");
    frame.declare_string ("tag_density", Attribute::Const, "\
Name of column containing measured root density.");
    frame.declare_integer ("debug", Attribute::Const, "\
Show debug messages if larger than zero.");
    frame.set ("debug", 0);
    frame.declare_boolean ("show_data", Attribute::Const, "\
Show comparison of observed and modelled values.");
    frame.set ("show_data", true);
    frame.declare_boolean ("show_match", Attribute::Const, "\
Show matching observed and modelled values with errorbars.");
    frame.set ("show_match", false);
    frame.declare ("x_offset", "cm", Attribute::State, "\
Substract this from the x values shown with `show_match'.");
    frame.set ("x_offset", 0.0);
    
    frame.declare_boolean ("tabular", Attribute::Const, "\
Show parameters in tabular form for easy import to spreadsheet.");
    frame.set ("tabular", false);
    frame.declare ("min_dist", "cm", Attribute::State, "\
Ignore root data closer than this to the row in both dimensions.");
    frame.set ("min_dist", -1.0);
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
    frame.declare ("SoilDepth", "cm", Check::none (), Attribute::OptionalConst,
                   "Specifies how to handle soil imposed maximum root depth.\
If positive, always use this depth.\n\
If negative, disable maximum root depth (making the root zone infinite).\n\
If not specified, use the value that gives the best fit.\n\
\n\
Note that unless you specify a negative value, the root zone will be\n\
limited by DensRtTip, that is, depth where the corresponding 1D GP would\n\
give densities less than DensRtTip, will instead have a zero density.");
    frame.declare_submodule ("gnuplot", Attribute::Const, "\
Irrigation model for first season.  If missing, don't irrigate.", 
                             ProgramRootmatch::Gnuplot::load_syntax);
    
  }
} ProgramRootmatch_syntax;

// program_rootmatch.C ends here.
