// program_sbrdata.C -- Read Agrovand data.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
// Copyright 2007, 2008, 2009 Per Abrahamsen and KVL.
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
#include "lexer_data.h"
#include "librarian.h"
#include "assertion.h"
#include "time.h"
#include "treelog.h"
#include "block_model.h"
#include "path.h"
#include <fstream>
#include <sstream>

struct ProgramRS2WG : public Program
{
  const Path& path;
  const Time rshourly_origin;
  const Time wgcycle_begin;
  const Time wgcycle_end;
  const symbol rshourly_file;
  const symbol rsdaily_file;
  const symbol dwfhourly_file;
  const symbol hint_file;
  const std::vector<double> hlim;

  // Use.
  bool run (Treelog& msg)
  {
    // Open files.
    std::auto_ptr<std::istream> rsin = path.open_file (rshourly_file);
    daisy_assert (rsin.get ());
    LexerData rs_hourly (rshourly_file, *(rsin.get ()), msg);
    if (!rs_hourly.good ())
      {
        msg.error (std::string ("Problems opening '") + rshourly_file + "'");
        return false;
      }
  
    std::ofstream rs_daily (rsdaily_file.name ().c_str ());
    if (!rs_daily.good ())
      {
        msg.error (std::string ("Problems opening '") 
                   + rsdaily_file + "' for writing");
        return false;
      }

    std::ofstream dwf_hourly (dwfhourly_file.name ().c_str ());
    if (!dwf_hourly.good ())
      {
        msg.error (std::string ("Problems opening '")
                   + dwfhourly_file + "' for writing");
        return false;
      }
      
    dwf_hourly << "\
dwf-0.0 -- " << rshourly_file << "\n\
--------\n\
Date	Precip\n\
date	mm\n";

    std::vector<int> hcount (hlim.size (), 0);
    std::ofstream hint (hint_file.name ().c_str ());
    if (!hint.good ())
      {
        msg.error (std::string ("Problems opening '")
                   + hint_file + "' for writing");
        return false;
      }
    hint << "ddf-0.0 -- " << rshourly_file << "\n---------\n\
Year\tMonth\tR_max_H";
    for (size_t i = 0; i < hlim.size (); i++)
      hint << "\t" << hlim[i];
    hint << "\nyear\tmonth\tmm";
    for (size_t i = 0; i < hlim.size (); i++)
      hint << "\th";
    hint << "\n";

    Time time = rshourly_origin;
    Time cycle_time = wgcycle_begin;

    double R_max_H = 0.0;       // Max hourly rainfall [mm]
    double daily = 0.0;
    do
      {
        const double value = rs_hourly.get_number ();
        if (!rs_hourly.good ())
          break;
        dwf_hourly << time.print () << "\t" << value << "\n";
        daily += value;

        // Intervals.
        for (size_t i = 0; i < hlim.size (); i++)
          if (value <= hlim[i])
            {
              hcount[i]++;
              break;
            }

        // Max.
        if (value > R_max_H)
          R_max_H = value;

        Time next = time;
        next.tick_hour (1);
        if (next.mday () != time.mday ()) 
          // New day.
          {
            cycle_time.tick_day ();
            if (cycle_time == wgcycle_end)
              // New cycle.
              cycle_time = wgcycle_begin;

            if (cycle_time.mday () == next.mday ())
              // We agree on the time.
              rs_daily << daily << "\n";
            else if (cycle_time.mday () == 29)
              {
                // Generate extra leap day.
                daisy_assert (next.mday () == 1);
                rs_daily << daily << "\n";
                rs_daily << daily << "\n";
                cycle_time.tick_day ();
              }
            else if (next.mday () == 29)
              {
                // Skip leap day.
                daisy_assert (cycle_time.mday () == 1);
                cycle_time.tick_day (-1);

              }
            else
              // We should agree all other days.
              {
                msg.error ("RS = " + next.print () 
                           + ", WG = " + cycle_time.print ());
                daisy_notreached ();
              }

            daily = 0.0;
          }
        if (time.month () != next.month ())
          {
            hint << time.year () << "\t" << time.month () << "\t" << R_max_H;
            for (size_t i = 0; i < hcount.size (); i++)
              hint << "\t" << hcount[i];
            hint << "\n";
            std::fill (hcount.begin (), hcount.end (), 0);
            R_max_H = 0.0;
          }

        if (time.year () / 100 != next.year () / 100)
          msg.message (next.print ());
        time = next;
        rs_hourly.next_line ();
      }
    while (rs_hourly.good ());

    if (!rs_daily.good ())
      {
        msg.error (std::string ("Problems writing to '") 
                   + rsdaily_file + "'");
      }

    if (!dwf_hourly.good ())
      {
        msg.error (std::string ("Problems writing to '")
                   + dwfhourly_file + "'");
      }

    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }

  bool check (Treelog& msg)
  {
    bool ok = true;
    return ok; 
  }

  ProgramRS2WG (const BlockModel& al)
    : Program (al),
      path (al.path ()),
      rshourly_origin (al.integer ("rshourly_origin"), 1, 1, 0),
      wgcycle_begin (al.integer ("wgcycle_begin"), 1, 1, 0),
      wgcycle_end (al.integer ("wgcycle_begin") + al.integer ("wgcycle_length"),
                   1, 1, 0),
      rshourly_file (al.name ("rshourly_file")),
      rsdaily_file (al.name ("rsdaily_file")),
      dwfhourly_file (al.name ("dwfhourly_file")),
      hint_file (al.name ("hint_file")),
      hlim (al.number_sequence ("hlim"))
  { }
  ~ProgramRS2WG ()
  { }
};

static struct ProgramRS2WGSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramRS2WG (al); }
  ProgramRS2WGSyntax ()
    : DeclareModel (Program::component, "RS2WG", "\
Prepare data from RainSim to the weather generator.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("rshourly_origin", Attribute::Const, "\
Start of hourly data from RainSim.");
    frame.declare_integer ("wgcycle_begin", Attribute::Const, "\
Start of weather cycle for Weather Generator.");
    frame.declare_integer ("wgcycle_length", Attribute::Const, "\
Length (in years) of weather cycle for Weather Generator.");
    frame.declare_string ("rshourly_file", Attribute::Const, "\
Name of file with hourly data from RainSim.");
    frame.declare_string ("rsdaily_file", Attribute::Const, "\
Name of file with daily precipitation for the Weather Generator.");
    frame.declare_string ("dwfhourly_file", Attribute::Const, "\
Daisy weather file with hourly data from RainSim.");
    frame.declare_string ("hint_file", Attribute::Const, "\
Hourly precipitation interval file.");
    frame.declare ("hlim", "mm", Attribute::Const, Attribute::Variable, "\
Upper limits for each precipitation interval.");
  }
} ProgramRS2WG_syntax;

// Weather generator to Daisy

struct ProgramWG2DWF : public Program
{
  const Path& path;
  const Time rshourly_origin;
  const Time wgcycle_begin;
  const Time wgcycle_end;
  const symbol wg_file;
  const symbol dwfdaily_file;
  const symbol monthly_file;
  const symbol yearly_file;
  const symbol dint_file;
  const std::vector<double> dlim;
  const symbol header;

  // Daily data.
  int year;
  int month;
  int mday;
  double Precip;
  double Tmin;
  double Tmax;
  double Vp;
  double rh;
  double wind;
  double ss;
  double diffrad;
  double direct;
  double pet;
  
  // Monthly data.
  double R_max_D;               // Max daily rainfall [mm]
  double RD_MR;                 // Total rainfall in days with more than 1 mm
  int RD;                       // Number of days with more than 1 mm rain
  int WD;                       // Number of days with more than 10 mm rain
  double MR;                    // Total rainfall [mm]

  // Yearly data.
  int FD;                       // Days where T_min < 0 [dg C]
  int T25days;                  // Current heatwave length.
  int HWT25;                    // Heat wave days [3 days where T_avg > 25 dg C
  int T28days;                  // Current heatwave length.
  int HWT28;                    // Heat wave days [3 days where T_avg > 28 dg C
  double T_warm;                // Hottest day in the year (avg) [dg C]
  double T_cold;                // Coldest day in the year (avg) [dg C]
  int last_DFAF;                // Day of first frost [jday]
  int last_DLAF;                // Day of last frost [jday]
  int DFAF;                     // Day of first frost [jday]
  int DLAF;                     // Day of last frost [jday]
  double GROW;                  // Number of days where T_avg > 5 dg C.
  double R_max_DY;              // Greatest daily rainfall [mm]
  double RD_AD;                 // Rain on raindays (> 1 mm) [mm]
  double AR;                    // Annual rainfall. [mm]

  void reset_monthly ()
  {
    R_max_D = 0.0;
    RD_MR = 0.0;
    RD = 0;
    WD = 0;
    MR = 0.0;
  }
  void reset_yearly ()
  {
    FD = 0;
    T25days = 0;
    HWT25 = 0;
    T28days = 0;
    HWT28 = 0;
    T_warm = -42.42e42;
    T_cold = 42.42e42;
    GROW = 0;
    R_max_DY = 0.0;
    RD_AD = 0.0;
    AR = 0.0;
  }

  // Use.
  bool run (Treelog& msg)
  {
    // Open files.
    std::auto_ptr<std::istream> wgin = path.open_file (wg_file);
    daisy_assert (wgin.get ());
    LexerData wg (wg_file, *(wgin.get ()), msg);
    if (!wg.good ())
      {
        msg.error (std::string ("Problems opening '") + wg_file + "'");
        return false;
      }
  
    std::ofstream dwf_daily (dwfdaily_file.name ().c_str ());
    if (!dwf_daily.good ())
      {
        msg.error (std::string ("Problems opening '")
                   + dwfdaily_file + "' for writing");
        return false;
      }
      
    dwf_daily << "\
dwf-0.0 -- " << wg_file << "\n" << header << "\n----\n\
Year\tMonth\tDay\tPrecip\tT_min\tT_max\tVapPres\tWind\tDiffRad\tGlobRad\tRefEvap\n\
year\tmonth\tday\tmm/d\tdgC\tdgC\thPa\tm/s\tW/m^2\tW/m^2\tmm/d\n";


    reset_monthly ();
    std::ofstream monthly (monthly_file.name ().c_str ());
    if (!monthly.good ())
      {
        msg.error (std::string ("Problems opening '")
                   + monthly_file + "' for writing");
        return false;
      }
    monthly << "ddf-0.0 -- " << wg_file << "\n---------\n\
Year\tMonth\tR_max_D\tSDII\tRD\tWD\tMR\n\
year\tmonth\tmm\tmm/d\td\td\tMR\n";
      
    reset_yearly ();
    std::ofstream yearly (yearly_file.name ().c_str ());
    if (!yearly.good ())
      {
        msg.error (std::string ("Problems opening '")
                   + yearly_file + "' for writing");
        return false;
      }
    yearly << "ddf-0.0 -- " << wg_file << "\n---------\n\
Year\tFD\tHWT25\tHWT28\tT_warm\tT_cold\tDFAF\tDLAF\tGROW\tR_max_DY\tAR\n\
Year\td\td\td\tdG C\tdg C\tjday\tjday\td\tmm\tmm\n";

    std::vector<int> dcount (dlim.size (), 0);
    std::ofstream dint (dint_file.name ().c_str ());
    if (!dint.good ())
      {
        msg.error (std::string ("Problems opening '")
                   + dint_file + "' for writing");
        return false;
      }
    dint << "ddf-0.0 -- " << wg_file << "\n---------\n\
Year\tMonth";
    for (size_t i = 0; i < dlim.size (); i++)
      dint << "\t" << dlim[i];
    dint << "\nyear\tmonth";
    for (size_t i = 0; i < dlim.size (); i++)
      dint << "\td";
    dint << "\n";

    Time time = rshourly_origin;
    Time cycle_time = wgcycle_begin;

    while (wg.good ())
      {
        (void) wg.get_cardinal ();
        wg.skip_space ();
        year = wg.get_cardinal ();
        wg.skip_space ();
        month = wg.get_cardinal ();
        wg.skip_space ();
        mday = wg.get_cardinal ();
        wg.skip_space ();
        (void) wg.get_cardinal ();
        wg.skip_space ();
        (void) wg.get_cardinal ();
        wg.skip_space ();
        Precip = wg.get_number ();
        wg.skip_space ();
        Tmin = wg.get_number ();
        wg.skip_space ();
        Tmax = wg.get_number ();
        wg.skip_space ();
        Vp = wg.get_number ();
        wg.skip_space ();
        rh = wg.get_number ();
        wg.skip_space ();
        wind = wg.get_number ();
        wg.skip_space ();
        ss = wg.get_number ();
        wg.skip_space ();
        diffrad = wg.get_number ();
        wg.skip_space ();
        direct = wg.get_number ();
        wg.skip_space ();
        pet = wg.get_number ();
        wg.next_line ();

        daisy_assert (year == cycle_time.year ());
        daisy_assert (month == cycle_time.month ());
        daisy_assert (mday == cycle_time.mday ());

        std::ostringstream tmp;
        tmp << Precip << "\t" << Tmin << "\t" << Tmax << "\t" << Vp
            << "\t" << wind << "\t" << diffrad << "\t" << (diffrad + direct)
            << "\t" << pet << "\n"; 
        const std::string daily = tmp.str ();

        if (cycle_time.mday () == time.mday ())
          // We agree on the time.
          dwf_daily << time.year () << "\t" << time.month () 
                    << "\t" << time.mday () << "\t" << daily;
        else if (cycle_time.mday () == 29)
          {
            // Skip leap day.
            daisy_assert (time.mday () == 1);
            time.tick_day (-1);
          }
        else if (time.mday () == 29)
          {
            // Generate extra leap day.
            daisy_assert (cycle_time.mday () == 1);
            dwf_daily << time.year () << "\t" << time.month () 
                      << "\t" << time.mday () << "\t" << daily;
            time.tick_day ();
            dwf_daily << time.year () << "\t" << time.month () 
                    << "\t" << time.mday () << "\t" << daily;
          }
        else
          // We should agree all other days.
          {
            msg.error ("RS = " + time.print () 
                       + ", WG = " + cycle_time.print ());
            daisy_notreached ();
          }

        // Monthly 
        if (Precip > R_max_D)
          R_max_D = Precip;
        if (Precip > 1.0)
          {
            RD_MR += Precip;
            RD++;
          }
        if (Precip > 10.0)
          WD++;
        MR += Precip;

        // Yearly.
        if (Tmin < 0.0)
          FD++;
        const double Tavg = (Tmin + Tmax) / 2.0;
        if (Tavg > 25.0)
          {
            T25days++;
            if (T25days >= 3)
              HWT25++;
          }
        else
          T25days = 0;
        if (Tavg > 28.0)
          {
            T28days++;
            if (T28days >= 3)
              HWT28++;
          }
        else
          T28days = 0;
        if (Tavg > T_warm)
          T_warm = Tavg;
        if (Tavg < T_cold)
          T_cold = Tavg;
        if (DFAF < 0 && Tmin < 0)
          DFAF = time.yday ();
        if (Tmin < 0)
          DLAF = time.yday ();
        if (Tavg > 5.0)
          GROW++;
        if (Precip > R_max_DY)
          R_max_DY = Precip;
        if (Precip > 1.0)
          RD_AD += Precip;
        AR += Precip;

        // Intervals.
        for (size_t i = 0; i < dlim.size (); i++)
          if (Precip <= dlim[i])
            {
              dcount[i]++;
              break;
            }

        // Next day a new month or year?
        Time next = time;
        next.tick_day ();

        // Winter season.
        if (next.month () == 8 && time.month () == 7)
          {
            last_DFAF = DFAF;
            last_DLAF = DLAF;
            DFAF = -9999;
            DLAF = -9999;
          }

        // Monthly index.
        if (next.month () != time.month ()) 
          {
            monthly << time.year () << "\t" << time.month () 
                    << "\t" << R_max_D 
                    << "\t" << (RD_AD / (RD + 0.0)) // SDII
                    << "\t" << RD 
                    << "\t" << WD << "\t" << MR << "\n";
            reset_monthly ();

            dint << time.year () << "\t" << time.month ();
            for (size_t i = 0; i < dcount.size (); i++)
              dint << "\t" << dcount[i];
            dint << "\n";
            std::fill (dcount.begin (), dcount.end (), 0);
          }

        // Yearly index.
        if (next.year () != time.year ()) 
          {
            yearly << time.year () << "\t" << FD << "\t" << HWT25 
                   << "\t" << HWT28 << "\t" << T_warm << "\t" << T_cold 
                   << "\t" << last_DFAF << "\t" << last_DLAF << "\t" << GROW 
                   << "\t" << R_max_DY << "\t" << AR << "\n";
            reset_yearly ();
          }
        // Next day.
        time = next;
        cycle_time.tick_day ();
        if (cycle_time == wgcycle_end)
          cycle_time = wgcycle_begin;
      }

    if (!dwf_daily.good ())
      {
        msg.error (std::string ("Problems writing to '")
                   + dwfdaily_file + "'");
      }

    return true;
  }

  // Create and Destroy.
  void initialize (Block&)
  { }

  bool check (Treelog& msg)
  {
    bool ok = true;
    return ok; 
  }

  ProgramWG2DWF (const BlockModel& al)
    : Program (al),
      path (al.path ()),
      rshourly_origin (al.integer ("rshourly_origin"), 1, 1, 0),
      wgcycle_begin (al.integer ("wgcycle_begin"), 1, 1, 0),
      wgcycle_end (al.integer ("wgcycle_begin") + al.integer ("wgcycle_length"),
                   1, 1, 0),
      wg_file (al.name ("wg_file")),
      dwfdaily_file (al.name ("dwfdaily_file")),
      monthly_file (al.name ("monthly_file")),
      yearly_file (al.name ("yearly_file")),
      dint_file (al.name ("dint_file")),
      dlim (al.number_sequence ("dlim")),
      header (al.name ("header")),
      last_DFAF (-9999),
      last_DLAF (-9999),
      DFAF (-9999),
      DLAF (-9999)
  { }
  ~ProgramWG2DWF ()
  { }
};

static struct ProgramWG2DWFSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramWG2DWF (al); }
  ProgramWG2DWFSyntax ()
    : DeclareModel (Program::component, "WG2DWF", "\
Prepare data from the weather generator for Daisy.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_integer ("rshourly_origin", Attribute::Const, "\
Start of hourly data from RainSim.");
    frame.declare_integer ("wgcycle_begin", Attribute::Const, "\
Start of weather cycle for Weather Generator.");
    frame.declare_integer ("wgcycle_length", Attribute::Const, "\
Length (in years) of weather cycle for Weather Generator.");
    frame.declare_string ("wg_file", Attribute::Const, "\
Name of file generated by the Weather Generator.");
    frame.declare_string ("dwfdaily_file", Attribute::Const, "\
Daisy weather file with daily data from Weather Generator.");
    frame.declare_string ("monthly_file", Attribute::Const, "\
Montly index file.");
    frame.declare_string ("yearly_file", Attribute::Const, "\
Yearly index file..");
    frame.declare_string ("dint_file", Attribute::Const, "\
Daily precipitation interval file.");
    frame.declare ("dlim", "mm", Attribute::Const, Attribute::Variable, "\
Upper limits for each precipitation interval.");
    frame.declare_string ("header", Attribute::Const, "\
Stuff to put in the Daisy weather file header.");
  }
} ProgramWG2DWF_syntax;

// program_sbrdata.C ends here.
