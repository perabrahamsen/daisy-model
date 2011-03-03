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
#include "lexer_table.h"
#include "librarian.h"
#include "units.h"

#define BOOST_UBLAS_NDEBUG
#define NDEBUG
#include <boost/numeric/ublas/matrix.hpp>
namespace ublas = boost::numeric::ublas;

#include <fstream>
#include <iomanip>

struct ProgramRootmatch : public Program
{
  const Units& units;
  LexerTable lex;
  const symbol pos_dim;
  const symbol dens_dim;
  int c_y_pos;
  int c_z_min;
  int c_z_max;
  int c_density;
  
  // Utils
  double get_value (const symbol from, const symbol to, std::string& entry)
  { return units.convert (from, to, lex.convert_to_double (entry)); }

  // Use.
  bool run (Treelog& msg)
  {
    static const symbol dens_dim_to ("cm/cm^3");
    std::ostringstream out;
    out << "Y\tZ\trho"
        << "\n" << Units::cm () << "\t" << Units::cm () << "\t" << dens_dim_to;
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
        out << "\n" << y_pos << "\t" << z_pos << "\t" << density;
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
      c_density (-1)
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
  }
} ProgramRootmatch_syntax;

// program_rootmatch.C ends here.
