 // lexer_table.C --- Read tabular data from a file.
 // 
 // Copyright 2005 Per Abrahamsen and KVL.
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

 #include "lexer_table.h"
 #include "lexer_data.h"
 #include "frame.h"
 #include "assertion.h"
 #include "mathlib.h"
 #include "submodeler.h"
 #include "time.h"
 #include "vcheck.h"
 #include "units.h"
 #include "path.h"
 #include "memutils.h"
 #include "librarian.h"
 #include "treelog_text.h"
 #include <boost/algorithm/string/trim.hpp>
 #include <sstream>
 #include <cstring>
 #include <iomanip>
 #include <map>


 struct LexerTable::Implementation : private boost::noncopyable
 {
   // Content.
   const Metalib& metalib;
   const Units& units;
   const Path& path;
   const symbol filename;  
   std::auto_ptr<std::istream> owned_stream;
   boost::scoped_ptr<LexerData> lex;
   Filepos end_of_header;
   std::string field_sep;
   std::string type_;
   const std::vector<std::string> missing;
   std::vector<symbol> tag_names;
   std::map<symbol,int> tag_pos;
   std::vector<size_t> fil_col;
   struct Filter;
   auto_vector<const Filter*> filter;
   int year_c;
   int month_c;
   int mday_c;
   int hour_c;
   int minute_c;
   int second_c;
   int microsecond_c;
   int time_c;
   const std::vector<symbol> original;
   const bool dim_line;
   std::vector<symbol> dim_names;

   int find_tag (const symbol tag1, const symbol tag2) const;
   std::string get_entry () const;
   void get_entries_raw (std::vector<std::string>& entries) const;
   int get_date_component (const std::vector<std::string>& entries, 
                           int column, int default_value) const;
   bool good ();
   bool read_type (Treelog& msg);
   void read_keywords (Frame& keywords);
   void read_tags ();
   bool read_header (Treelog& msg);
   bool read_header_with_keywords (Frame& keywords, Treelog& msg);
   int find_tag (const symbol tag) const;
   bool get_entries (std::vector<std::string>& entries) const;
   static bool get_time (const std::string& entry, Time& time, 
                         int default_hour, bool& date_only); 
   bool get_time (const std::vector<std::string>& entries, Time& time, 
                  int default_hour, bool& date_only) const;
   bool is_missing (const std::string& value) const;
   double convert_to_double (const std::string& value) const;

   // Messages.
   void warning (const std::string& str) const;
   void error (const std::string& str) const;

   // Create and destroy.
   void rewind ();
   static std::vector<std::string> s2s_v (const std::vector<symbol>& syms);
   Implementation (const BlockModel& al);
 };

 struct LexerTable::Implementation::Filter
 {
   const symbol tag;
   const std::vector<symbol> allowed;

   bool match (const std::string& value) const
   {
     for (size_t i = 0; i < allowed.size (); i++)
       {
         if (allowed[i] == value)
           return true;
         // Try to pad out our allowed value with spaces...
         if (value.size () <= allowed[i].name ().size ())
           continue;
         const std::string allow
           = allowed[i] + std::string (value.size () 
                                       - allowed[i].name ().size (), ' ');
         daisy_assert (allow.size () == value.size ());
         if (allow == value)
           return true;
       }
     return false;
   }
   static void load_syntax (Frame&);
   explicit Filter (const Block&);
 };

 void 
 LexerTable::Implementation::Filter::load_syntax (Frame& frame)
 {
   frame.declare_string ("tag", Attribute::Const, "\
 Name of column in Daisy log file to filter for.");
   frame.declare_string ("allowed", Attribute::Const, Attribute::Variable, "\
 List of allowable values in filter.");
   frame.set_check ("allowed", VCheck::min_size_1 ());
   frame.order ("tag", "allowed");
 }

 LexerTable::Implementation::Filter::Filter (const Block& al)
   : tag (al.name ("tag")),
     allowed (al.name_sequence ("allowed"))
 { }

 int
 LexerTable::Implementation::find_tag (const symbol tag1,
                                       const symbol tag2) const
 {
   int tag1_c = find_tag (tag1);
   int tag2_c = find_tag (tag2);
   if (tag1_c < 0)
     return tag2_c;
   if (tag2_c >= 0)
     lex->warning ("'" + tag1 + "' overwrites '" + tag2 + "'");
   return tag1_c;
 }

 std::string
 LexerTable::Implementation::get_entry () const
 {
   std::string tmp_term;  // Data storage.
   const char* field_term;

   switch (field_sep.size ())
     { 
     case 0:
       // Whitespace
       field_term = " \t\n";
       break;
     case 1:
       // Single character field seperator.
       tmp_term = field_sep + "\n";
       field_term = tmp_term.c_str ();
       break;
     default:
       // Multi-character field seperator.
       daisy_notreached ();
     }

   // Find it.
   std::string entry = "";
   while (lex->good ())
     {
       int c = lex->peek ();
       if (std::strchr (field_term, c))
         break;
       entry += int2char (lex->get ());
     }
   return entry;
 }

 void
 LexerTable::Implementation::get_entries_raw (std::vector<std::string>& 
                                              /**/ entries) const
 {
   entries.clear ();
   lex->skip ("\n");
   while (lex->good () && lex->peek () == '#')
     {
       lex->skip_line ();
       lex->skip ("\n");
     }
   while (lex->good ())
     {
       entries.push_back (get_entry ());

       if (lex->peek () == '\n')
         break;

       if (field_sep == "")
         lex->skip_space ();
       else
         lex->skip(field_sep.c_str ());
     }
 }

 int
 LexerTable::Implementation::get_date_component (const std::vector<std::string>&
                                                 /**/ entries, 
                                                 const int column, 
                                                 const int default_value) const
 {
   if (column < 0)
     return default_value;
   daisy_assert (column < entries.size ());
   const char *const str = entries[column].c_str ();
   const char* end_ptr = str;
   const long lval = strtol (str, const_cast<char**> (&end_ptr), 10);
   if (*end_ptr != '\0')
     error (std::string ("Junk at end of number '") + end_ptr + "'");
   const int ival = lval;
   if (ival != lval)
     error ("Number out of range");
   return ival;
 }

 std::vector<std::string> 
 LexerTable::Implementation::s2s_v (const std::vector<symbol>& syms)
 {
   std::vector<std::string> result;
   for (size_t i = 0; i < syms.size (); i++)
     result.push_back (syms[i].name ());
   return result;
 }

 LexerTable::Implementation::Implementation (const BlockModel& al)
   : metalib (al.metalib ()),
     units (al.units ()),
     path (al.path ()),
     filename (al.name ("file")),
     owned_stream (NULL),
     lex (NULL),
     field_sep ("UNINITIALIZED"),
     type_ ("UNINITIALIZED"),
     missing (s2s_v (al.name_sequence ("missing"))),
     filter (map_submodel_const<Filter> (al, "filter")),
     year_c (-42),
     month_c (-42),
     mday_c (-42),
     hour_c (-42),
     minute_c (-42),
     second_c (-42),
     microsecond_c (-42),
     time_c (-42),
     original (al.check ("original")
               ? al.name_sequence ("original")
               : std::vector<symbol> ()),
     dim_line (al.flag ("dim_line", !al.check ("original")))
 { }

 bool 
 LexerTable::Implementation::good ()
 {
   if (!lex.get ())
     return false;
   if (lex->good ())
     return true;

   // Close file descriptor after first problem.
   lex.reset (NULL);
   return false;
 }

 bool 
 LexerTable::good ()
 { return impl->good (); }

 bool
 LexerTable::Implementation::read_type (Treelog& msg)
 {
   owned_stream = path.open_file (filename.name ());
   lex.reset (new LexerData (filename.name (), *owned_stream, msg));

   // Open errors?
   if (!lex->good ())
     return false;

   // Read first line.
   type_ = lex->get_word ();
   if (type_ == "dwf-0.0")
     field_sep = "";
   else if (type_ == "dlf-0.0")
     field_sep = "\t";
   else if (type_ == "ddf-0.0")
     field_sep = "\t";
   else
     {
       error ("Unknown file type '" + type_ + "'");
       field_sep = "\t";
     }
   lex->skip_line ();
   lex->next_line ();
   return true;
 }  

 void
 LexerTable::Implementation::read_keywords (Frame& keywords)
 {
   while (lex->good () && lex->peek () != '-')
     {
       lex->skip_space ();
       if (lex->peek () == '\n')
         {
           lex->skip ("\n");
           continue;
         }
       std::string name;
       while (lex->good () && lex->peek () != ':')
         name += lex->get ();
       lex->skip (":");
       lex->skip_space ();
       const symbol key (name);
       const Attribute::type type = keywords.lookup (key);
       if (type == Attribute::Error)
         {
           lex->skip_line ();
           error ("'" + key + "': Unknown keyword");
           lex->skip_line ();
           continue;
         }
       const int size = keywords.type_size (key);
       if (size == Attribute::Singleton)
         switch (type)
           {
           case Attribute::Number:
             {
               double val = lex->get_number ();
               lex->skip_space ();
               std::string dim;
               while (lex->good () && lex->peek () != '\n')
                 dim += lex->get ();
               boost::algorithm::trim (dim);
               const symbol has_dim (dim);
               const symbol want_dim = keywords.dimension (key);

               if (units.can_convert (has_dim, want_dim, val))
                 keywords.set (key, units.convert (has_dim, want_dim, val));
               else
                 {
                   error ("Cannot convert [" 
                          + has_dim + "] to [" + want_dim + "]");
                 }
             }
             break;
           case Attribute::String:
             {
               std::string text = "";
               if (keywords.is_text (key) && keywords.check (key))
                 text = keywords.name (key) + "\n";
               while (lex->good () && lex->peek () != '\n')
                 text += lex->get ();
               if (!keywords.is_text (key))
                 boost::algorithm::trim (text);
               keywords.set (key, text);
             }
             break;
           case Attribute::Submodel:
             {
               const symbol subname = keywords.submodel_name (key);
               if (subname != Librarian::submodel_name (Time::load_syntax))
                 {
                   lex->skip_line ();
                   error ("'" + key + "': Unsupported submodel '" + subname + "'");
                   break;
                 }
               Time time;
               lex->skip_space ();
               lex->read_date (time);
               time.set_time (keywords, key);
             }
             break;
           default:
             lex->skip_line ();
             error ("'" + key + "': Unsupported type '" 
                    + Attribute::type_name (type) + "'");
           }
       else
         switch (type)
           {
           case Attribute::Number:
             {
               std::vector<double> raw_numbers;
               do 
                 {
                   raw_numbers.push_back (lex->get_number ());
                   lex->skip_space ();
                 }
               while (lex->good () && strchr ("0123456789.-", lex->peek ()));

               std::string dim;
               while (lex->good () && lex->peek () != '\n')
                 dim += lex->get ();
               boost::algorithm::trim (dim);
               const symbol has_dim (dim);
               const symbol want_dim = keywords.dimension (key);

               std::vector<double> numbers;
               for (size_t i = 0; i < raw_numbers.size (); i++)
                 {
                   double val = raw_numbers[i];
                   if (units.can_convert (has_dim, want_dim, val))
                     val = units.convert (has_dim, want_dim, val);
                   else
                     error ("Cannot convert [" + has_dim
                            + "] to [" + want_dim + "]");
                   numbers.push_back (val);
                 }
               keywords.set (key, numbers);
             }
             break;
           default:
             lex->skip_line ();
             error ("'" + key + "': Unsupported type '" 
                    + Attribute::type_name (type) + "'");
           }
       if (keywords.check (key))
         {
           TreelogString msg;
           if (!keywords.check (metalib, key, msg))
            lex->error ("'" + key + "': bad value: " + msg.str ());
        }
      lex->next_line ();
    }
}

void
LexerTable::Implementation::read_tags ()
{
  // Read tags.
  std::vector<std::string> tag_names_raw;
  get_entries_raw (tag_names_raw);
  for (int count = 0; count < tag_names_raw.size (); count++)
    {
      const std::string name = tag_names_raw[count];
      const symbol candidate (name);
      tag_names.push_back (candidate);
      if (tag_pos.find (candidate) == tag_pos.end ())
        tag_pos[candidate] = count;
      else
	warning ("Duplicate tag: '" + name + "'");
    }

  // Time tags.
  year_c = find_tag ("year", "Year");
  month_c = find_tag ("month", "Month");
  mday_c = find_tag ("mday", "Day");
  hour_c = find_tag ("hour", "Hour");
  minute_c = find_tag ("minute", "Minute");
  second_c = find_tag ("second", "Second");
  microsecond_c = find_tag ("microsecond", "Microsecond");
  time_c = find_tag ("time", "Date");

  if (time_c >= 0)
    {
      if (year_c >= 0)
        warning ("Column year ignored because of time column");
      if (month_c >= 0)
        warning ("Column month ignored because of time column");
      if (mday_c >= 0)
        warning ("Column mday ignored because of time column");
      if (hour_c >= 0)
        warning ("Column hour ignored because of time column");
      if (minute_c >= 0)
        warning ("Column minute ignored because of time column");
      if (second_c >= 0)
        warning ("Column second ignored because of time column");
      if (microsecond_c >= 0)
        warning ("Column microsecond ignored because of time column");
    }

  // Filter tags.
  for (size_t i = 0; i < filter.size (); i++)
    {
      int c = find_tag (filter[i]->tag);
      if (c < 0)
	{
	  error ("Filter tag '" + filter[i]->tag + "' not found");
	  return;
	}
      fil_col.push_back (c);
    }
  
  // Read dimensions.
  if (dim_line)
    {
      std::vector<std::string> dim_names_raw;
      get_entries_raw (dim_names_raw);
      dim_names.clear ();
      for (size_t i = 0; i < dim_names_raw.size (); i++)
        dim_names.push_back (symbol (dim_names_raw[i]));
      daisy_assert (dim_names_raw.size () == dim_names.size ());
    }
  switch (original.size ())
    {
    case 0:
      if (!dim_line)
        dim_names.insert (dim_names.end (), tag_names.size (), 
                          Attribute::Unknown ());
      break;
    case 1:
      dim_names.clear ();
      dim_names.insert (dim_names.end (), tag_names.size (),
                        original[0]);
      break;
    default:
      dim_names = original;
    }

  if (dim_names.size () != tag_names.size ())
    {
      std::ostringstream tmp;
      tmp << "Got " << tag_names.size () << " tags and " << dim_names.size ()
          << " dimensions";
      error (tmp.str ());
      return;
    }
}  

bool
LexerTable::Implementation::read_header_with_keywords (Frame& keywords, 
                                                       Treelog& msg)
{
  // First line.
  if (!read_type (msg))
    return false;

  // Keywords.
  read_keywords (keywords);
  
  // Skip hyphens.
  while (lex->good () && lex->peek () == '-')
    lex->get ();
  lex->skip_space ();           // 'read_tags' expects a newline.

  // Tags.
  read_tags ();
  end_of_header = lex->position ();

  // Done
  return lex->get_error_count () < 1;
}

bool
LexerTable::Implementation::read_header (Treelog& msg)
{
  // First line.
  if (!read_type (msg))
    return false;

  // Keywords.
  while (lex->good () && lex->peek () != '-')
    {
      lex->skip_line ();
      lex->next_line ();
    }
  
  // Skip hyphens.
  while (lex->good () && lex->peek () == '-')
    lex->get ();
  lex->skip_space ();           // 'read_tags' expects a newline.

  // Tags.
  read_tags ();
  end_of_header = lex->position ();

  // Done
  return lex->good ();
}

bool
LexerTable::read_header (Treelog& msg)
{ return impl->read_header (msg); }

bool
LexerTable::read_header_with_keywords (Frame& keywords, Treelog& msg)
{ return impl->read_header_with_keywords (keywords, msg); }

const std::string&
LexerTable::type () const
{ return impl->type_; }

const std::vector<symbol>& 
LexerTable::tag_names () const
{ return impl->tag_names; }

int
LexerTable::Implementation::find_tag (const symbol tag) const
{
  if (tag_pos.find (tag) == tag_pos.end ())
    return -1;
  return tag_pos.find (tag)->second;
}

int
LexerTable::find_tag (const symbol tag) const
{ return impl->find_tag (tag); }

symbol
LexerTable::dimension (size_t tag_c) const
{ 
  daisy_assert (tag_c < impl->dim_names.size ());
  return impl->dim_names[tag_c];
}

bool
LexerTable::Implementation::get_entries (std::vector<std::string>& 
                                         /**/ entries) const
{
  get_entries_raw (entries);

  // Got the right number of entries?
  if (entries.size () != tag_names.size ())
    {
      if (entries.size () == 0 || !lex->good ())
        return false;

      std::ostringstream tmp;
      tmp << "Got " << entries.size () << " entries, expected " 
          << tag_names.size ();
      warning (tmp.str ());
      while (entries.size () < tag_names.size ())
        entries.push_back ("");
    }

  // Filter.
  for (size_t i = 0; i < filter.size (); i++)
    if (!filter[i]->match (entries[fil_col[i]]))
      return false;
  
  return true;
}

bool
LexerTable::get_entries (std::vector<std::string>& entries) const
{ return impl->get_entries (entries); }

bool
LexerTable::is_time (symbol tag)
{
  static struct times_t : public std::set<symbol>
  {
    times_t ()
    {
      insert ("year");
      insert ("Year");
      insert ("month");
      insert ("Month");
      insert ("mday");
      insert ("Day");
      insert ("hour");
      insert ("Hour");
      insert ("minute");
      insert ("Minute");
      insert ("second");
      insert ("Second");
      insert ("microsecond");
      insert ("Microsecond");
      insert ("time");
      insert ("Date");
    }
  } times;
  return times.find (tag) != times.end ();
}
      
bool
LexerTable::Implementation::get_time (const std::string& entry, Time& time,
                                      int default_hour, bool& date_only)
{
  std::istringstream in (entry);

  int val1;
  char sep1;
  int val2;
  char sep2;
  int val3;
  in >> val1 >> sep1 >> val2 >> sep2 >> val3;

  if (sep1 != sep2)
    return false;

  int hour = default_hour;
  if (in.good () && !in.eof ())
    {
      date_only = false;
      char sep3;
      in >> sep3 >> hour;
      if (sep3 != 'T')
        return false;
    }
  else
    date_only = true;

  int minute = 0;
  if (in.good () && !in.eof ())
    {
      char sep4;
      in >> sep4 >> minute;
      if (sep4 != ':')
        return false;
    }        

  int second = 0;
  if (in.good () && !in.eof ())
    {
      char sep5;
      in >> sep5 >> second;
      if (sep5 != ':')
        return false;
    }        

  int microsecond = 0;
  if (in.good () && !in.eof ())
    {
      double val6;
      in >> val6;
      microsecond = double2int (val6 * 1000000.0);
    }        

  if (!in.eof ())
    return false;

  int mday;
  int month;
  int year;
  if (sep1 == '-')
    {
      year = val1;
      month = val2;
      mday = val3;
    }
  else if (sep1 == '/')
    {
      mday = val1;
      month = val2;
      year = val3;
    }
  else
    return false;

  if (!Time::valid (year, month, mday, hour))
    return false;

  time = Time (year, month, mday, hour);
  return true;
}

bool
LexerTable::get_time (const std::string& entry, Time& time, int default_hour) 
{ 
  bool date_only;
  return Implementation::get_time (entry, time, default_hour, date_only); 
}

bool
LexerTable::get_time (const std::string& entry, Time& time,
                      bool& date_only)
{ return Implementation::get_time (entry, time, 0, date_only); }

bool
LexerTable::Implementation::get_time (const std::vector<std::string>& entries,
                                      Time& time, const int default_hour,
                                      bool& date_only) const
{
  // Extract date.
  if (time_c < 0)
    {
      int year = get_date_component (entries, year_c, 1000);
      int month = get_date_component (entries, month_c, 1);
      int mday = get_date_component (entries, mday_c, 1);
      int hour = get_date_component (entries, hour_c, default_hour);
      int minute = get_date_component (entries, minute_c, 0);
      int second = get_date_component (entries, second_c, 0);
      int microsecond = get_date_component (entries, microsecond_c, 0);

      if (!Time::valid (year, month, mday, hour, minute, second, microsecond))
        {
          std::ostringstream tmp;
          tmp << year << "-" << month << "-" << mday << "T" << hour 
              << ":" << minute << ":" << second 
              << ":" << std::setw (6) << std::setfill ('0') << microsecond
              << ": invalid date";
          warning (tmp.str ());
          return false;
        }
      else
        time = Time (year, month, mday, hour, minute, second, microsecond);

      if (time.year () == 9999)
        {
          std::ostringstream tmp;
          tmp << time.print () << ": invalid date";
          warning (tmp.str ());
          return false;
        }
    }
  else 
    {
      if (!get_time (entries[time_c], time, default_hour, date_only))
        {
          warning (entries[time_c] + ": invalid time");
          return false;
        }
    }

  // If we survived here, everything is fine.
  return true;
}

bool
LexerTable::get_time (const std::vector<std::string>& entries,
                      Time& time, const int default_hour) const
{ 
  bool date_only;
  return impl->get_time (entries, time, default_hour, date_only);
}

bool
LexerTable::get_time (const std::vector<std::string>& entries,
                      Time& time, bool& date_only) const
{ return impl->get_time (entries, time, 0, date_only); }

bool
LexerTable::Implementation::is_missing (const std::string& value) const
{ return find (missing.begin (), missing.end (), value) != missing.end (); }

bool
LexerTable::is_missing (const std::string& value) const
{ return impl->is_missing (value); }

double
LexerTable::convert_to_double (const std::string& value) const
{
  const char *const str = value.c_str ();
  const char* end_ptr = str;
  const double val = strtod (str, const_cast<char**> (&end_ptr));
  if (*end_ptr != '\0')
    impl->error (std::string ("Junk at end of number '") + end_ptr + "'");
  return val;
}

void
LexerTable::debug (const std::string& str) const
{ impl->lex->warning (str); }

void
LexerTable::Implementation::warning (const std::string& str) const
{ lex->warning (str); }

void
LexerTable::warning (const std::string& str) const
{ impl->warning (str); }

void 
LexerTable::Implementation::error (const std::string& str) const
{ lex->error (str); }

void 
LexerTable::error (const std::string& str) const
{ impl->error (str); }

void 
LexerTable::Implementation::rewind ()
{ lex->seek (end_of_header); }

void 
LexerTable::rewind ()
{ impl->rewind (); }

void 
LexerTable::load_syntax (Frame& frame)
{
  frame.declare_string ("file", Attribute::Const, "\
Name of Daisy log file where data is found.");
  frame.declare_string ("missing", Attribute::Const, Attribute::Variable, "\
List of strings indicating missing values.");
  std::vector<symbol> misses;
  misses.push_back (symbol (""));
  misses.push_back (symbol ("00.00"));
  frame.set ("missing", misses);
  frame.declare_submodule_sequence ("filter", Attribute::Const, "\
Only include data from rows that passes all these filters.",
                                    LexerTable::Implementation
                                    ::Filter::load_syntax);
  frame.set_empty ("filter");
  frame.declare_string ("original", Attribute::OptionalConst, 
              Attribute::Variable, "\
List of dimensions of the data in the data file.\n\
\n\
If the list has only one element, that element is used as the\n\
dimension for all columns in the file.  Otherwise, the list must have\n\
one element for each column.\n\
\n\
By default Daisy will use the names specified in data file.");
  frame.declare_boolean ("dim_line", Attribute::OptionalConst, "\
If true, assume the line after the tags contain dimensions.\n\
By default this will be true iff 'original' is not specified.");
}

LexerTable::LexerTable (const BlockModel& al)
  : impl (new Implementation (al))
{ }

LexerTable::~LexerTable ()
{ }

// lexer_table.C ends here.
