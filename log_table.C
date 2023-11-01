// log_table.C -- Log selected data in tabular format.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "log_select.h"
#include "dlf.h"
#include "symbol.h"
#include "select.h"
#include "geometry.h"
#include "assertion.h"
#include "daisy.h"
#include "block_model.h"
#include "frame_model.h"
#include "treelog.h"
#include "filepos.h"
#include "librarian.h"
#include "library.h"
#include <sstream>
#include <fstream>
#include <vector>

class DestinationTable : public Destination
{
  // File Content.
  const symbol parsed_from_file; // Defined in...
  const symbol file;
  std::ofstream out;            // Output stream.
  const bool flush;             // Flush after each time step.
  // char* faster than symbol for output.
  const char *const record_separator; // String to print on records (time steps)
  const char *const field_separator; // String to print between fields.
  const char *const missing_value; // String to print for missing values.
  const char *const array_separator; // String to print between array entries.
  DLF print_header;             // How much header should be printed?
  bool print_tags;              // Set if tags should be printed.
  bool print_dimension;         // Set if dimensions should be printed.
  const bool std_time_columns;  // Add year, month, day and hour columns.

  // Data.
  bool first_entry;             // First entry in record.
  inline void seperate_field ();

  // Select::Destination
  void missing ();
  void add (const std::vector<double>& value);
  void add (const double value);
  void add (const symbol value);

public:
  // Use.
  void end_header (const Metalib& metalib, const FrameModel&);
private:
  void column_tag (const Select& select);
  void column_dimension (const Select& select);
public:
  void record_start (const std::vector<Time::component_t>&, const Time&, 
                     const std::vector<const Select*>&);
  void record_end ();

  // Create and destroy.
public:
  void initialize (const symbol log_dir, const symbol suffix,
		   const symbol objid,
                   const symbol description, const Volume&, 
                   const std::vector<std::pair<symbol, symbol>/**/>&
                   /**/ parameters);
  bool check (Treelog& msg) const;
private:
  static bool contain_time_columns (const std::vector<const Select*>& entries);
public:
  DestinationTable (const Block&, 
                    const std::vector<const Select*>& entries);
  ~DestinationTable ();
};

void 
DestinationTable::seperate_field ()
{
  if (first_entry)
    first_entry = false;
  else
    out << field_separator;
}

void 
DestinationTable::missing ()
{ 
  seperate_field ();
  out << missing_value; 
}

void 
DestinationTable::add (const std::vector<double>& value)
{ 
  seperate_field ();
  for (unsigned int i = 0; i < value.size (); i++)
    {
      if (i != 0)
        out << array_separator;
      out << value[i];
    }
}

void 
DestinationTable::add (const double value)
{ 
  seperate_field ();
  out << value; }

void 
DestinationTable::add (const symbol value)
{
  seperate_field ();
  out << value; 
}

void
DestinationTable::end_header (const Metalib& metalib, const FrameModel& frame)
{ print_header.finish (out, metalib, frame); }

void
DestinationTable::column_tag (const Select& select)
{
  const int type_size = select.type_size ();
  const Geometry *const geometry = select.geometry ();
  const symbol tag = select.tag ();
  const bool has_tag = (tag != symbol (""));
  switch (type_size)
    {
    case Attribute::Singleton:
      out << tag;
      return;
    case Attribute::Variable:
    case Attribute::CanopyCells:
    case Attribute::CanopyEdges:
    case Attribute::Unspecified:
      /* defer to value */
      break;
    case Attribute::SoilCells:
      {
	if (!geometry)
	  {
	    daisy_warning ("Soil cells with no known geometry");
	    /* defer to value */
	    break;
	  }
	const size_t cell_size = geometry->cell_size ();
	for (size_t c = 0; c < cell_size; ++c)
	  {
	    if (c > 0)
	      out << array_separator;
	    if (has_tag)
	      out << tag << " @ ";
	    out << geometry->cell_name (c);
	  }
      }
      return;
    case Attribute::SoilEdges:
      {
	if (!geometry)
	  {
	    daisy_warning ("Soil edges with no known geometry");
	    /* defer to value */
	    break;
	  }
	const size_t edge_size = geometry->edge_size ();
	for (size_t e = 0; e < edge_size; ++e)
	  {
	    if (e > 0)
	      out << array_separator;
	    if (has_tag)
	      out << tag << " @ ";
	    out << geometry->edge_name (e);
	  }
      }
      return;
    default:
      for (int i = 0; i < type_size; i++)
	{
	  if (i > 0)
	    out << array_separator;
	  if (has_tag)
	    out << tag;
	  out << "[" << i << "]";
	}
      return;
    }
  // Based on value instead of type.
  const int value_size = select.size ();
  if (value_size < 0)
    // Singleton.
    {
      out << select.tag ();
      return;
    }
  if (geometry)
    {
      const size_t cell_size = geometry->cell_size ();
      const size_t edge_size = geometry->edge_size ();
      if (value_size == cell_size)
	// SoilCells.
	{
	  for (size_t c = 0; c < cell_size; ++c)
	    {
	      if (c > 0)
		out << array_separator;
	      if (has_tag)
		out << tag << " @ ";
	      out << geometry->cell_name (c);
	    }
	  return;
	}
      if (value_size == edge_size)
	// SoilEdges.
	{
	  for (size_t e = 0; e < edge_size; ++e)
	    {
	      if (e > 0)
		out << array_separator;
	      if (has_tag)
		out << tag << " @ ";
	      out << geometry->edge_name (e);
	    }
	  return;
	}
    }
  // Generic array value.
  for (int i = 0; i < value_size; i++)
    {
      if (i > 0)
	out << array_separator;
      if (has_tag)
	out << tag;
      out << "[" << i << "]";
    }
}

void
DestinationTable::column_dimension (const Select& select)
{
  const int type_size = select.type_size ();
  const Geometry *const geometry = select.geometry ();
  symbol dimension = select.dimension ();
  if (dimension == Attribute::None () 
      || dimension == Attribute::Unknown ()
      || dimension == Attribute::Fraction ())
    dimension = "";
  switch (type_size)
    {
    case Attribute::Singleton:
      out << dimension;
      return;
    case Attribute::Variable:
    case Attribute::CanopyCells:
    case Attribute::CanopyEdges:
    case Attribute::Unspecified:
      /* defer to value */
      break;
    case Attribute::SoilCells:
      {
	if (!geometry)
	  {
	    daisy_warning ("Soil cells with no known geometry");
	    /* defer to value */
	    break;
	  }
	const size_t cell_size = geometry->cell_size ();
	for (size_t c = 0; c < cell_size; ++c)
	  {
	    if (c > 0)
	      out << array_separator;
	    out << dimension;
	  }
      }
      return;
    case Attribute::SoilEdges:
      {
	if (!geometry)
	  {
	    daisy_warning ("Soil edges with no known geometry");
	    /* defer to value */
	    break;
	  }
	const size_t edge_size = geometry->edge_size ();
	for (size_t e = 0; e < edge_size; ++e)
	  {
	    if (e > 0)
	      out << array_separator;
	    out << dimension;
	  }
      }
      return;
    default:
      for (int i = 0; i < type_size; i++)
	{
	  if (i > 0)
	    out << array_separator;
	  out << dimension;
	}
      return;
    }
  // Based on value instead of type.
  const int value_size = select.size ();
  if (value_size < 0)		// Singleton
    out << dimension;
  else for (size_t j = 0; j < value_size; j++)
	 {
	   if (j != 0)
	     out << array_separator;
	   out << dimension;
	 }
}

void 
DestinationTable::record_start (const std::vector<Time::component_t>& time_columns,
                                const Time& time,  
                                const std::vector<const Select*>& entries)
{ 
  if (print_tags)
    {
      if (std_time_columns)
        for (size_t i = 0; i < time_columns.size (); i++)
          out << Time::component_name (time_columns[i]) << field_separator;

      // Print the entry names in the first line of the log file..
      for (unsigned int i = 0; i < entries.size (); i++)
        {
          if (i != 0)
            out << field_separator;

	  column_tag (*entries[i]);
        }
      out << record_separator;
      print_tags = false;
    }
  if (print_dimension)
    {
      if (std_time_columns)
        for (size_t i = 0; i < time_columns.size (); i++)
          out << field_separator;

      // Print the entry names in the first line of the log file..
      for (unsigned int i = 0; i < entries.size (); i++)
        {
          if (i != 0)
            out << field_separator;

	  column_dimension (*entries[i]);
        }
      out << record_separator;
      print_dimension = false;
    }

  first_entry = true;
  if (std_time_columns)
    for (size_t i = 0; i < time_columns.size (); i++)
      {
        seperate_field ();
        out << time.component_value (time_columns[i]);
      }
}

void
DestinationTable::record_end ()
{
  out << record_separator;
  if (flush)
    out.flush ();
}

void
DestinationTable::initialize (const symbol log_dir, const symbol suffix,
			      const symbol objid,
                              const symbol description, const Volume& volume,
                              const std::vector<std::pair<symbol, symbol>/**/>&
                              /**/ parameters)
{
  if (file == "NUL")
#ifdef __unix
    out.open ("/dev/null");
#else
    out.open ("NUL");
#endif
  else
    {
      const std::string fn = log_dir.name () + file.name () + suffix.name ();
      out.open (fn.c_str ());
    }

  print_header.start (out, objid, file, parsed_from_file);

  for (size_t i = 0; i < parameters.size (); i++)
    print_header.parameter (out, parameters[i].first, parameters[i].second);

  print_header.interval (out, volume);
  print_header.log_description (out, description);

  out.flush ();
}

bool
DestinationTable::check (Treelog& msg) const
{
  bool ok = true;
  if (!out.good ())
    ok = false;
  return ok; 
}

bool 
DestinationTable::contain_time_columns (const std::vector<const Select*>& entries)
{
  static const symbol time ("time");
  static const symbol previous ("previous");
  for (unsigned int i = 0; i < entries.size (); i++)
    if (entries[i]->path[0] == time || (entries[i]->path[0] == previous))
      return true;
  return false;
}

DestinationTable::DestinationTable (const Block& al, 
                                    const std::vector<const Select*>& entries)
  : parsed_from_file (al.frame ().inherited_position ().filename ()),
    file (al.name ("where")),
    flush (al.flag ("flush")),
    record_separator (al.name ("record_separator").name ().c_str ()),
    field_separator (al.name ("field_separator").name ().c_str ()),
    missing_value (al.name ("missing_value").name ().c_str ()),
    array_separator (al.name ("array_separator").name ().c_str ()),
    print_header (al.name ("print_header")),
    print_tags (al.flag ("print_tags")),
    print_dimension (al.flag ("print_dimension")),
    std_time_columns (al.ok () && !contain_time_columns (entries)),
    first_entry (false)
{ }

DestinationTable::~DestinationTable ()
{
  if (!out.good ())
    Assertion::error ("Problems writing to '" + file + "'");
}

struct LogTable : public LogSelect
{
  const std::vector<const Select*> const_entries;

  // Data ends up here.
  DestinationTable destination;

  // Log.
  void done_print (const std::vector<Time::component_t>& time_columns,
                   const Time& time);

  // Initial line.
  bool initial_match (const Daisy&, const Time& previous, Treelog&);

  // Create and destroy.
  bool check (const Border&, Treelog& msg) const;
  void initialize (const symbol log_dir, const symbol suffix, Treelog&);
  explicit LogTable (const BlockModel& al);
  ~LogTable ();
};

void 
LogTable::done_print (const std::vector<Time::component_t>& time_columns,
                      const Time& time)
{ 
  destination.record_start (time_columns, time, const_entries); 
  for (size_t i = 0; i < entries.size (); i++)
    entries[i]->done_print ();
  destination.record_end ();
}

bool 
LogTable::initial_match (const Daisy& daisy, const Time& previous, Treelog& msg)
{
  destination.end_header (metalib (), daisy.frame ());
  
  return LogSelect::initial_match (daisy, previous, msg);
}

bool 
LogTable::check (const Border& border, Treelog& msg) const
{ 
  TREELOG_MODEL (msg);
  bool ok = LogSelect::check (border, msg);
  if (!destination.check (msg))
    {
      ok = false;
      std::ostringstream tmp;
      tmp << "Write error for '" << file << "'";
      msg.error (tmp.str ());
    }

  return ok; 
}

void
LogTable::initialize (const symbol log_dir, const symbol suffix, Treelog& msg)
{
  TREELOG_MODEL (msg);
  LogSelect::initialize (log_dir, suffix, msg);
  destination.initialize (log_dir, suffix, objid, description, *volume, parameters); 
}

LogTable::LogTable (const BlockModel& al)
  : LogSelect (al),
    const_entries (entries.begin (), entries.end ()),
    destination (al, const_entries)
{ 
  if (!al.ok ())
    return;

  for (unsigned int i = 0; i < entries.size (); i++)
    entries[i]->add_dest (&destination);
}

LogTable::~LogTable ()
{ }

static struct LogTableSyntax : public DeclareModel
{
  Model* make (const BlockModel& al) const
  { return new LogTable (al); }

  LogTableSyntax ()
    : DeclareModel (Log::component, "table", "select", "\
Write results in a tabular Daisy log file.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.declare_string ("where", Attribute::Const,
                          "Name of the log file to create.");
    DLF::add_syntax (frame, "print_header");
    frame.declare_boolean ("print_tags", Attribute::Const,
                           "Print a tag line in the file.");
    frame.set ("print_tags", true);
    frame.declare_boolean ("print_dimension", Attribute::Const,
                           "Print a line with units after the tag line.");
    frame.set ("print_dimension", true);
    frame.declare_boolean ("flush", Attribute::Const,
                           "Flush to disk after each entry (for debugging).");
    frame.set ("flush", false);
    frame.declare_string ("record_separator", Attribute::Const, "\
String to print between records (time steps).");
    frame.set ("record_separator", "\n");
    frame.declare_string ("field_separator", Attribute::Const, "\
String to print between fields.");
    frame.set ("field_separator", "\t");
    frame.declare_string ("missing_value", Attribute::Const, "\
String to print when the path doesn't match anything.\n\
This can be relevant for example if you are logging a crop, and there are\n\
no crops on the field.");
    frame.set ("missing_value", "00.00");
    frame.declare_string ("array_separator", Attribute::Const, "\
String to print between array entries.");
    frame.set ("array_separator", "\t");
    Librarian::add_doc_fun (Log::component, LogSelect::document_entries);
  }
} LogTable_syntax;

// log_table.C ends here.
