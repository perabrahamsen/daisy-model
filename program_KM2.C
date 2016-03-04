// program_KM2.C -- Read KM2 data.
// 
// Copyright 2014 KU.
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
#include "lexer.h"
#include "librarian.h"
#include "assertion.h"
#include "time.h"
#include "treelog.h"
#include "path.h"
#include "block_model.h"
#include "mathlib.h"
#include <sstream>
#include <cctype>

struct ProgramKM2 : public Program
{
  const Path& path;
  const symbol filename;
  std::unique_ptr<std::istream> owned_stream;
  std::unique_ptr<Lexer> lex;

  std::string pretty (int c) const
  {
    std::ostringstream tmp;
    if (std::isprint (c))
      tmp << "'" << char (c) << "'";
    else
      tmp << c;
    return tmp.str ();
  }
      
  void skip (int skip)
  {
    const int c = lex->get ();
    if (c != skip)
      lex->warning ("Expected " + pretty (skip) + ", got " + pretty (c) + "");
  }

  void skip (const std::string& s)
  {
    for (int i = 0; i < s.size (); i++)
      skip (s[i]);
  }

  std::string get_string (int length)
  {
    std::string str;
    for (size_t i = 0; i < length; i++)
      {
        int c = lex->get ();
        str.push_back (c);
      }
    return str;
  }
  
  std::string get_nonspace (int length)
  {
    std::string str;
    for (size_t i = 0; i < length; i++)
      {
        int c = lex->get ();
        if (!std::isspace (c))
          str.push_back (c);
      }
    return str;
  }
  
  int get_integer (int length)
  {
    const std::string str = get_nonspace (length);
    return std::atoi (str.c_str ());
  }

  double get_double (int length)
  {
    const std::string str = get_nonspace (length);
    const char *c_str = str.c_str ();
    const char *endptr = c_str;
    const double value = strtod (c_str, const_cast<char**> (&endptr));
  
    if (*endptr != '\0')
      lex->error (std::string ("Junk at end of number '") + endptr + "'");

    return value;
  }

  Time get_time ()
  {
    const int year = get_integer (4);
    const int month = get_integer (2);
    const int mday = get_integer (2);
    skip (' ');
    const int hour = get_integer (2);
    const int minute = get_integer (2);

    if (!Time::valid (year, month, mday, hour, minute))
      {
        std::ostringstream tmp;
        tmp << year << "-" << month << "-" << mday 
            << "T" << hour << ":" << minute;
        lex->error ("Bad time: " + tmp.str ());
      }
    return Time (year, month, mday, hour, minute);
  }

  // Use.
  bool run (Treelog& msg)
  {
    owned_stream = path.open_file (filename.name ());
    lex.reset (new Lexer (filename.name (), *owned_stream, msg));

    if (!lex->good ())
        msg.error ("Problems opening '" + filename + "'");

    // Result.
    std::ostringstream out;
    double rain = 0;
    Time last = Time::null ();
    std::vector<double> monthly (13, 0.0);

    while (lex->good ())
      {
        // Status line
        // 1
        (void) get_integer (1); 
        // 2
        skip (' ');
        // 3-15
        Time time = get_time ();
        // 16-17
        skip (' ');
        skip (' ');
        // 18-22
        (void) get_integer (5);
        // 23-24
        skip (' ');
        skip (' ');
        // 25-28
        const int length = get_integer (4);
        // 29
        skip (' ');
        // 30-31
        const int resolution = get_integer (2);

        if (resolution != 1)
          lex->warning ("resolution != 1");

        // 32-38
        const double total = get_double (7);
        // 39
        skip (' ');
        // 40-45 status
        const std::string status = get_string (6);
        if (status[0] != '1' && status != "2    s")
          {
            lex->warning ("Status " + status + ", ignoring");
            // KM2 format not followed, skip data.
            while (lex->good () && (lex->get () != '\n'
                                    || lex->peek () == ' '))
              /* Nothing */;
            continue;
          }
        // Now get data.
        std::vector<double> data;
        int this_line = 10;     // Start a new line after 10 data.
        const int count = length / resolution;
        if ((length % resolution) != 0)
          {
            std::ostringstream tmp;
            tmp << "Can't have lenght " << length << " with resolution " 
                << resolution << ", there are " << (length % resolution) 
                << " left";
            lex->warning (tmp.str ());
          }

        const double um_per_s_to_mm = resolution * 60.0 / 1000.0;

        double found = 0.0;
        for (int i = 0; i < count; i++)
          {
            // Handle lines.
            if (this_line == 10)
              {
                skip ("\n ");
                this_line = 1;
              }
            else
              {
                daisy_assert (this_line < 10);
                this_line++;
              }
            const double value = get_double (7) * um_per_s_to_mm;

            for (int j = 0; j < resolution; j++)
              {
                found += value;
                const Time hour (time.year (), time.month (), time.mday (), 
                                 time.hour ());
                if (last == Time::null ())
                  // First entry.
                  {
                    last = hour;
                    last.tick_hour (-1);
                  }
                if (hour > last)
                  {
                    while (hour > last)
                      {
                        monthly[last.month ()] += rain;
                        out << last.year () << "\t" << last.month () 
                            << "\t"  << last.mday ()
                            << "\t"  << last.hour () << "\t" << rain << "\n";
                        rain = 0.0;
                        last.tick_hour (1);
                      }
                    daisy_assert (hour == last);
                  }
                rain += value;
                time.tick_minute ();
              }
          }
        daisy_assert (approximate (total, found, 0.01));

        // EOD
        skip ('\n');
      }
    for (size_t i = 1; i < 13; i++)
      out << i << "\t" << monthly[i] << "\n";
    msg.message (out.str ());
      
    return true;
  }

  void add_minute (const Time& time, const double value)
  {
  }
      
  // Create and Destroy.
  void initialize (Block&)
  { }

  bool check (Treelog& msg)
  {
    bool ok = true;
    return ok; 
  }
  ProgramKM2 (const BlockModel& al)
    : Program (al),
      path (al.path ()),
      filename (al.name ("file")),
      owned_stream (),
      lex ()
  { }
  ~ProgramKM2 ()
  { }
};

static struct ProgramKM2Syntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new ProgramKM2 (al); }
  ProgramKM2Syntax ()
    : DeclareModel (Program::component, "KM2", "\
Read KM2 precipitation data.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("file", Attribute::Const, "\
Name of KM2 file where data is found.");
  }
} ProgramKM2_syntax;

// program_KM2.C ends here.
