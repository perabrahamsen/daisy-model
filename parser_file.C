// parser_file.C
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
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

#include "parser_file.h"
#include "metalib.h"
#include "library.h"
#include "block.h"
#include "lexer.h"
#include "scope.h"
#include "number.h"
#include "integer.h"
#include "plf.h"
#include "time.h"
#include "treelog_text.h"
#include "path.h"
#include "units.h"
#include "mathlib.h"
#include "memutils.h"
#include "librarian.h"
#include "frame_model.h"
#include "frame_submodel.h"
#include <set>
#include <memory>
#include <sstream>

static const symbol error_symbol ("__PARSER_FILE_ERROR_MAGIC__");

struct ParserFile::Implementation
{
  // Environment.
  Metalib& metalib;
  Treelog& msg;

  // Inputs.
  auto_vector<const Frame*> inputs;

  // Lexer.
  const symbol file;
  std::auto_ptr<std::istream> owned_stream;
  std::auto_ptr<Lexer> lexer;
  std::auto_ptr<Treelog::Open> nest;

  int get ()
  { return lexer->get (); }
  int peek ()
  { return lexer->peek (); }
  bool good ()
  { return lexer->good (); }
  void error (const std::string& str)
  { lexer->error (str); }
  void error (const std::string& str, const Filepos& pos)
  { lexer->error (str, pos); }
  void warning (const std::string& str)
  { lexer->warning (str); }
  void eof ()
  { lexer->eof (); }

  // Lisp lexer.
  std::string get_string ();
  symbol get_symbol ()
  { return symbol (get_string ()); }
  int get_integer ();
  double get_number ();
  symbol get_dimension ();
  double get_number (const symbol dim);
  bool check_dimension (const symbol syntax, const symbol read);
  double convert (double value, const symbol syntax, const symbol read, 
		  const Filepos&);
  void skip (const char*);
  void skip ();
  void skip_to_end ();
  void skip_token ();
  bool looking_at (char);

  class Parskip 
  {
    ParserFile::Implementation& parser;
    bool skipped;
  public:
    Parskip (ParserFile::Implementation& p, bool skip = true)
      : parser (p),
        skipped (skip)
    { 
      if (skip)
        parser.skip ("(");
    }
    ~Parskip ()
    {
      if (skipped)
        parser.skip (")");
    }
  };
  // Parser.
  void check_value (const Frame& syntax, const symbol name,  double value);
  void add_derived (Library&);
  std::auto_ptr<FrameModel> load_object (const Library& lib, bool in_sequence,
                                         const FrameModel* original);
  void load_list (Frame&);

  // Create and destroy.
  void initialize ();
  Implementation (Metalib&, symbol, Treelog&);
  ~Implementation ();
};

std::string
ParserFile::Implementation::get_string ()
{
  static const struct IdExtra : public std::set<int>
  {
    bool operator() (int c) const
    { return find (c) != end (); }
    IdExtra ()
    { 
      insert ('<');
      insert ('>');
      insert ('_');
      insert ('+');
      insert ('-');
      insert ('*');
      insert ('/');
    }
  } id_extra;

  skip ();
  int c = peek ();
  
  if (c == '"')
    {
      // Get a string.
      std::string str ("");
      skip ("\"");

      for (c = get (); good () && c != '"'; c = get ())
	{
	  if (c == '\\')
	    {
	      c = get ();
	      switch (c)
		{
		case 'n':
		  c = '\n';
		  break;
                case 't':
                  c = '\t';
                  break;
		case '\n':
                  continue;
		case '\\':
		case '"':
		  break;
		default:
		  error (std::string ("Unknown character escape '")
			 + char (c) + "'");
		}
	    }
	  str += int2char (c);
	}
      return str;
    }
  else if (c == '[')
    return get_dimension ().name ();
  else if (!id_extra (c) && !isalpha (c))
    {
      error ("Identifier or string expected");
      skip_to_end ();
      return error_symbol.name ();
    }
  else
    {
      // Get an identifier.
      std::string str ("");
      do
	{
	  str += char (c);
	  get ();
	  c = peek ();
	}
      while (good() && (id_extra (c) || isalnum (c)));
    
      return str;
    }
}

int
ParserFile::Implementation::get_integer ()
{
  skip ();
  int c = peek ();

  // Check for number literals first.
  if (isdigit (c) || c == '-')
    {
      std::string str;

      while (good () && (isdigit (c) || c == '-' || c == '+'))
        {
          str += int2char (c);
          get ();
          c = peek ();
        }
      // Empty number?
      if (str.size () < 1U)
        {
          error ("Integer expected");
          skip_to_end ();
          return -42;
        }
      return atoi (str.c_str ());
    }
  // Then try an integer object.
  const Library& lib = metalib.library (Integer::component);
  std::auto_ptr<FrameModel> frame = load_object (lib, true, NULL);
  if (!frame.get ())
    return -42;
  const symbol obj = frame->type_name ();
  // Check for completness.
  TreelogString treelog;
  Treelog::Open nest (treelog, obj);
  if (!frame->check (metalib, treelog))
    {
      error ("Bogus integer '" + obj + "'\n--- details:\n"
             + treelog.str () + "---");
      return -42;
    }
  std::auto_ptr<Integer> integer 
    (Librarian::build_frame<Integer> (metalib, treelog, *frame, "integer"));
  if (!integer.get ()
      || !integer->initialize (metalib.units (), Scope::null (), treelog)
      || !integer->check (Scope::null (), treelog))
    {
      error ("Bad integer '" + obj + "'\n--- details:\n"
             + treelog.str () + "---");
      return -42;
    }
  if (treelog.str ().length () > 0)
    warning ("Warning for integer '" + obj + "'\n--- details:\n"
             + treelog.str () + "---");
  if (integer->missing (Scope::null ()))
    {
      error ("Missing integer '" + obj + "'");
      return -42;
    }
  return integer->value (Scope::null ());
}

double
ParserFile::Implementation::get_number ()
{
  skip ();
  std::string str;
  int c = peek ();

  while (good () && (isdigit (c) 
		     || c == '.' || c == '-' || c == '+' 
		     || c == 'e' || c == 'E'))
    {
      str += int2char (c);
      get ();
      c = peek ();
    }
  // Empty number?
  if (str.size () < 1U)
    {
      error ("Number expected");
      skip_to_end ();
      return -42.42e42;
    }
  const char *c_str = str.c_str ();
  const char *endptr = c_str;
  const double value = strtod (c_str, const_cast<char**> (&endptr));
  
  if (*endptr != '\0')
    error (std::string ("Junk at end of number '") + endptr + "'");

  return value;
}

symbol
ParserFile::Implementation::get_dimension ()
{
  skip ("[");
  std::string str;
  int c = peek ();

  while (good () && c != ']' && c != '\n')
    {
      str += int2char (c);
      get ();
      c = peek ();
    }
  skip ("]");
  return str;
}

double
ParserFile::Implementation::get_number (const symbol syntax_dim)
{
  skip ();

  // Check for number literals first.
  if (peek () == '.' || isdigit (peek ()) || peek () == '-')
    {
      double value = get_number ();
      Filepos pos = lexer->position ();

      if (looking_at ('['))
        {
          const symbol read_dim = get_dimension ();
          if (check_dimension (syntax_dim, read_dim))
            value = convert (value, syntax_dim, read_dim, pos);
        }
      return value;
    }
  
  // Then try a number object.
  const Library& lib = metalib.library (Number::component);
  std::auto_ptr<FrameModel> frame = load_object (lib, true, NULL);
  if (!frame.get ())
    return -42.42e42;
  const symbol obj = frame->type_name ();
  // Check for completness.
  TreelogString treelog;
  Treelog::Open nest (treelog, obj);
  if (!frame->check (metalib, treelog))
    {
      error ("Bogus number '" + obj + "'\n--- details:\n"
             + treelog.str () + "---");
      return -42.42e42;
    }
  std::auto_ptr<Number> number 
    (Librarian::build_frame<Number> (metalib, msg, *frame, "number"));
  if (!number.get ()
      || !number->initialize (metalib.units (), Scope::null (), treelog)
      || !number->check (metalib.units (), Scope::null (), treelog))
    {
      error ("Bad number '" + obj + "'\n--- details:\n"
             + treelog.str () + "---");
      return -42.42e42;
    }
  number->tick (metalib.units (), Scope::null (), treelog);
  if (treelog.str ().length () > 0)
    warning ("Warning for number '" + obj + "'\n--- details:\n"
             + treelog.str () + "---");
  if (number->missing (Scope::null ()))
    {
      error ("Missing number '" + obj + "'");
      return -42.42e42;
    }
  double value = number->value (Scope::null ());
  const std::string read_dim = number->dimension (Scope::null ()).name ();
  if (check_dimension (syntax_dim, read_dim))
    value = convert (value, syntax_dim, read_dim, lexer->position ());
  return value;
}

bool 
ParserFile::Implementation::check_dimension (const symbol syntax, 
                                             const symbol read)
{
  if (syntax != read)
    {
      if (syntax == Value::Unknown ())
	{
	  if (read.name ().length () == 0 || read.name ()[0] != '?')
	    warning ("you must use [?<dim>] for entries with unknown syntax");
	}
      else if (!metalib.units ().can_convert (symbol (read), symbol (syntax),
                                              msg))
	{
	  error (std::string ("expected [") 
                 + ((syntax == Value::Fraction ()
                     || syntax == Value::None ())
                    ? "" : syntax) + "] got [" + read + "]");
	  return false; 
	}
    }
  return true;
}

double 
ParserFile::Implementation::convert (double value,
				     const symbol syntax, 
				     const symbol read, 
				     const Filepos& pos)
{ 
  if (syntax == Value::Unknown ())
    return value; 
  if (syntax == read)
    return value;
  if (syntax == Value::Fraction () && read == "%")
    return value * 0.01;
  
  try
    {
      if (syntax == Value::None () || syntax == Value::Fraction ())
	if (read == "")
	  return value;
	else
	  return metalib.units ().convert (symbol (read), symbol (""), value);
      return metalib.units ().convert (symbol (read), symbol (syntax), value);
    }
  catch (const std::string& message)
    { 
      error (message, pos); 
      return value;
    }
}

void
ParserFile::Implementation::skip (const char* str)
{ 
  skip ();
  for (const char* p = str; *p; p++)
    if (*p != peek ())
      {
	error (std::string("Expected '") + str + "'");
	skip_token ();
	break;
      }
    else
      get ();
}

void
ParserFile::Implementation::skip ()
{ 
  while (true)
    if (!good ())
      return;
    else if (isspace (peek ()))
      get ();
    else if (peek () == ';')
      while (good () && get () != '\n')
      ;
    else
      return;
}

void
ParserFile::Implementation::skip_token () {
  if (peek () == ';' || isspace (peek ()))
    skip ();
  if (peek () == '"')
    get_string ();
  else if (peek () == '.' || isdigit (peek ()) || peek () == '-')
    get_number ();
  else if (peek () == '(') 
    {
      get ();
      skip_to_end ();
      skip (")");
    }
  else if (isalnum (peek ()) || peek () == '_')
    get_string ();
  else if (peek () == '[')
    get_dimension ();
  else
    get ();
}

void
ParserFile::Implementation::skip_to_end ()
{
  skip ();
  while (peek () != ')' && good ())
    {
      skip_token ();
      skip ();
    }
}

bool
ParserFile::Implementation::looking_at (char c)
{ 
  skip ();
  return peek () == c;
}

void
ParserFile::Implementation::check_value (const Frame& frame, 
                                         const symbol name,
                                         const double value)
{
  try
    {
      frame.check (name, value);
    }
  catch (const std::string& message)
    {
      error (name + ": " + message);
    }
}

class FrameParsed : public FrameModel
{
  // Content.
private:
  const symbol name;
  symbol type_name () const
  { return name; }
  int seq_id;
  const symbol file;
  Filepos position_;
public:
  const Filepos& own_position () const
  { return position_; }
  void reposition (const Filepos& pos)
  { position_ = pos; }
  int sequence_id () const
  { return seq_id; }

  // Create and Destroy.
private:
  FrameParsed (const FrameParsed& parent, parent_clone_t)
    : FrameModel (parent, parent_clone),
      name (parent.name),
      seq_id (parent.seq_id),
      file (parent.file),
      position_ (parent.position_)
  { }
  FrameParsed& clone () const
  { return *new FrameParsed (*this, parent_clone); }

public:
  FrameParsed (const symbol name_, const FrameModel& model, const int seq_id_,
               const Filepos pos)
    : FrameModel (model, parent_link),
      name (name_),
      seq_id (seq_id_),
      position_ (pos)
  { }
};

void
ParserFile::Implementation::add_derived (Library& lib)
{
  // Get the name of the class and the existing superclass to derive from.
  const symbol name = get_symbol ();
  // Check for duplicates.
  if (lib.check (name))
    {
      const Frame& old = lib.model (name);
      const Filepos& pos = old.own_position ();
      if (pos != Filepos::none ())
	warning (name + " is already defined in " 
		 + pos.filename () + ", overwriting");
      else
	warning (name + " is already defined, overwriting");
      lib.remove (name);
    }
  static const symbol const_symbol ("const");
  skip ();
  int c = peek ();
  Parskip skip (*this, c == '(');
  const symbol super = (isdigit (c) || c == '-' || c == '.')
    ? const_symbol
    : get_symbol ();
  if (!lib.check (super))
    {
      error (std::string ("Unknown '") + lib.name ()
             + "' model '" + super + "'");
      skip_to_end ();
      return;
    }
  std::auto_ptr<FrameModel> frame (new FrameParsed (name, lib.model (super), 
                                                    metalib.get_sequence (),
                                                    lexer->position ()));
  if (!frame.get ())
    throw "Failed to derive";

  // Doc string.
  daisy_assert (!frame->ordered () 
                || frame->order ().begin () != frame->order ().end ());
  if ((!frame->ordered () 
       || (frame->lookup (*(frame->order ().begin ())) != Value::String
           && frame->lookup (*(frame->order ().begin ())) != Value::Object)) 
      && looking_at ('"')
      && frame->lookup ("description") == Value::String
      && frame->type_size ("description") == Value::Singleton)
    frame->add ("description", get_string ());

  // Add separate attributes for this object.
  Treelog::Open nest (msg, "Defining " + lib.name () + " '" + name + "'");
  load_list (*frame);
  // Add new object to library.
  lib.add_model (name, *frame.release ());

  // Inform metalib.
  metalib.added_object (lib.name (), name);
}

std::auto_ptr<FrameModel>
ParserFile::Implementation::load_object (const Library& lib, bool in_sequence,
                                         const FrameModel *const original)
{
  std::auto_ptr<FrameModel> result;
  bool skipped = false;

  static const symbol original_symbol ("original");

  symbol type;
  skip ();
  int c = peek ();

  // Handle numeric literals.
  if (isdigit (c) || c == '.' || c == '-')
    {                          
      if (lib.name () == symbol (Number::component))
        {
          result.reset (new FrameModel (lib.model ("const"),
                                        Frame::parent_link));
          const double value = get_number ();
          const symbol dim = get_dimension ();
          result->add ("value", value, dim);
        }
      else if (lib.name () == symbol (Integer::component))
        {
          result.reset (new FrameModel (lib.model ("const"), 
                                        Frame::parent_link));
          result->add ("value", get_integer ());
        }
      else
        {
          error ("Number not expected");
          skip_to_end ();
        }
      return result;
    }
 
  if (c == '(' && in_sequence)
    {
      skip ("(");
      skipped = true;
      c = peek ();
    }

  if (original && original->used_to_be_a_submodel () && c == '(')
    {
      // Special hack to allow skipping the "original" keyword for
      // models that used to be submodels.
      const symbol original_type = original->type_name ();
      daisy_assert (lib.check (original_type));

      type = original_symbol;
      warning ("Model specified missing, assuming 'original'");
    }
  else
    {
      if (c == '(' && !in_sequence)
        {
          skip ("(");
          skipped = true;
        }
      type = get_symbol ();
    }

  try
    {
      if (type == original_symbol)
	{
	  if (!original)
	    throw (std::string ("No original value"));
	  result.reset (&original->clone ());
	  type = result->type_name ();
	  daisy_assert (lib.check (type));
	}
      else if (!lib.check (type))
        {
          // Special hack to handle numbers in scopes.
          if (lib.name () == symbol (Number::component))
            {
              static const symbol fetch ("fetch");
              result.reset (new FrameModel (lib.model (fetch), 
                                            Frame::parent_link));
              result->add ("name", type);
              goto skip_it;
            }
          throw (std::string ("Unknown '") + lib.name () + "' model '"
                 + type + "'");
        }
      else
        {
          result.reset (new FrameModel (lib.model (type), Frame::parent_link));
	}
      if (skipped || !in_sequence)
	{
          Treelog::Open nest (msg, "Deriving from '" + type + "'");
	  load_list (*result);
	}
    skip_it:;
    }
  catch (const std::string& wrong)
    {
      error (wrong);
      skip_to_end ();
    }
  if (skipped)
    skip (")");
  return result;
}

void
ParserFile::Implementation::load_list (Frame& frame)
{ 
  std::vector<symbol>::const_iterator current = frame.order ().begin ();
  const std::vector<symbol>::const_iterator end = frame.order ().end ();
  std::set<symbol> found;

  while ( good () && !looking_at (')'))
    {
      bool skipped = false;
      bool in_order = false;
      symbol name = "";
      if (current == end)
	// Unordered association list, get name.
	{
	  skip ("(");
	  skipped = true;
	  name = get_symbol ();
	}
      else
	// Ordered tupple, name know.
	{
	  in_order = true;
	  name = *current;
	  current++;
	}

      // Make sure we skip the ')' afterwards.
      struct RAII_skip
      { 
        ParserFile::Implementation& outer;
        const bool skipped;
        
        RAII_skip (ParserFile::Implementation& o, const bool s)
          : outer (o),
            skipped (s)
        { }
        ~RAII_skip ()
        {
          if (skipped)
            outer.skip (")");
          outer.skip ();
        }
      } raii_skip (*this, skipped);

      // Declarations.
      if (name == "declare")
	{
	  bool ok = true;
	  // Special handling of block local parameters.
	  const std::string var = get_string ();
	  if (frame.lookup (var) != Value::Error)
	    {
	      error ("'" + var + "' already exists");
	      ok = false;
	    }
          int size = Value::Singleton;
          if (looking_at ('['))
            {
              skip ("[");
              if (looking_at (']'))
                size = Value::Sequence;
              else
                size = get_integer ();
              skip ("]");
            }
	  const std::string type_name = get_string ();
	  symbol doc = "User defined " + type_name + ".";
	  const Value::type type = Value::type_number (type_name);
	  switch (type)
	    {
	    case Value::String:
	    case Value::Integer:
	      {
		if (!looking_at ('('))
		  doc = get_symbol ();
		if (ok)
		  frame.add (var, type, Value::Const, size, doc);
		break;
	      }
	    case Value::Number:
	      {
		symbol dim = Value::Unknown ();
		if (looking_at ('['))
		  dim = get_symbol ();
		if (!looking_at ('('))
		  doc = get_symbol ();
		if (ok)
		  frame.add (var, dim, Value::Const, size, doc);
		break;
	      }
	    case Value::Error:
	      {
                if (type_name == "fixed")
                  {
                    const std::string submodel = get_string ();
                    if (Librarian::submodel_registered (submodel))
                      {
                        if (!looking_at ('('))
                          doc = get_string ();
                        if (ok)
                          {
                            const Frame::load_syntax_t load
                              = Librarian::submodel_load (submodel);
                            frame.add_submodule (var, Value::Const, doc, load);
                          }
                        break;
                      }
                    /* else fallthrough to error handling. */
                  }
                else
                  {
                    const symbol type_symbol (type_name);
                    if (metalib.exist (type_symbol))
                      {
                        if (!looking_at ('('))
                          doc = get_string ();
                        if (ok)
                          frame.add_object (var, type_symbol, 
                                             Value::Const, size, doc);
                        break;
                      }
                    /* else fallthrough to error handling. */
                  }
	      }
	      // Fallthrough
	    default:
	      error ("'" + type_name + "' unhandled type");
	      ok = false;
	    }
          // Next attribute.
          continue;
	}

      // Duplicate warning.
      if (found.find (name) != found.end ())
	warning (name + " specified twice, last takes precedence");
      else if (frame.lookup (name) != Value::Library // (deffoo ...)
	       && (frame.lookup (name) != Value::Object
		   || (frame.library (metalib, name).name () // (input file )
		       != symbol (Parser::component))))
	found.insert (name);

      // Log variable warning.
      if (frame.is_log (name))
        warning (name + " is a log only variable, value will be ignored");

      if (looking_at ('$'))
        {
          skip ("$");
          const std::string var = get_string ();
          if (name == var)
            error ("Reference $" + var + " refers to itself");
          else
            frame.add_reference (name, var);
        }
      else if (frame.type_size (name) == Value::Singleton)
	switch (frame.lookup (name))
	  {
	  case Value::Number:
	    {
              if (frame.dimension (name) == Value::User ())
                {
                  const double value = get_number ();
                  const symbol dim = 
		    looking_at ('[') ? get_dimension () : Value::Unknown ();
                  check_value (frame, name, value);
                  frame.add (name, value, dim);
                  break;
                }
	      const double value = get_number (frame.dimension (name));
              check_value (frame, name, value);
	      frame.add (name, value);
	      break;
	    }
	  case Value::AList: 
	    {
              Treelog::Open nest (msg, "In submodel '" + name + "'");
	      bool alist_skipped = false;
	      if (in_order)
		{
		  // Last element of a total order does not need '('.
		  if (looking_at ('(') 
		      || current != end 
		      || !frame.total_order ())
		    {
		      alist_skipped = true;
		      skip ("(");
		    }
		}
              std::auto_ptr<FrameSubmodel> 
                child (&frame.submodel (name).clone ());
	      load_list (*child);
	      frame.add (name, *child);
	      if (alist_skipped)
		skip (")");
	      break;
	    }
	  case Value::PLF:
	    {
	      Parskip skip (*this, in_order);
	      PLF plf;
	      double last_x = -42;
	      int count = 0;
	      const symbol domain = frame.domain (name);
	      const symbol range = frame.range (name);
	      bool ok = true;
	      while (!looking_at (')') && good ())
		{
                  Parskip skip (*this);
		  double x = get_number (domain);
		  {
		    if (count > 0 && x <= last_x)
		      {
			error ("Non increasing x value");
			ok = false;
		      }
		    last_x = x;
		    count++;
		  }
		  const double y = get_number (range);
		  try
		    {
		      frame.check (name, y);
		    }
		  catch (const std::string& message)
		    {
		      error (name + ": " + message);
		      ok = false;
		    }
		  if (ok)
		    plf.add (x, y);
		}
	      if (count < 2)
		{
		  error ("Need at least 2 points");
		  ok = false;
		}
	      if (ok)
		frame.add (name, plf);
	      break;
	    }
	  case Value::String:
	    frame.add (name, get_string ());
	    // Handle "directory" immediately.
	    if (&frame == &metalib && name == "directory")
	      if (!metalib.path ().set_directory (frame.name (name).name ()))
		error ("Could not set directory '" + frame.name (name) + "'");
	    break;
	  case Value::Boolean:
	    {
	      const std::string flag = get_string ();

	      if (flag == "true")
		frame.add (name, true);
	      else
		{
		  frame.add (name, false);
		  if (flag != "false")
		    error ("Expected 'true' or 'false'");
		}
	      break;
	    }
	  case Value::Integer:
	    frame.add (name, get_integer ());
	    break;
	  case Value::Library:
	    // Handled specially: Put directly in global library.
	    add_derived (frame.library (metalib, name));
	    break;
	  case Value::Object:
	    {
              std::auto_ptr<FrameModel> child;
	      const Library& lib = frame.library (metalib, name);
              if (frame.check (name))
                {
                  Treelog::Open nest (msg, "Refining model '" + name + "'");
                  child = load_object (lib, in_order, &frame.model (name));
                }
              else
                {
                  Treelog::Open nest (msg, "In model '" + name + "'");
                  child = load_object (lib, in_order, NULL);
                }
              if (!child.get ())
                /* Do nothing */;
	      else if (lib.name () == symbol (Parser::component))
		{
		  std::auto_ptr<Parser> parser 
                    (Librarian::build_frame<Parser> (metalib, msg, *child,
                                                     name));
                  if (!parser.get ())
                    error ("file error");
                  else
                    {
                      if (parser->check ())
                        parser->load_nested ();
                      lexer->error_count += parser->error_count ();
                    }
		  inputs.push_back (&child->clone ());
		}
	      else
                frame.add (name, *child);
	    }
	    break;
	  case Value::Error:
	    error (std::string("Unknown singleton '") + name + "'");
	    skip_to_end ();
	    break;
	  default:
	    daisy_notreached ();
	  }
      else
	{
	  // If this is part of an order, expect parentheses arund the
	  // list, EXCEPT when there cannot possible be any more
	  // elements after this one.  I.e. when the element is the
	  // last of a totally ordered sequence.
	  if (!in_order)
	    // Unordered, we already skipped this one
	    daisy_assert (skipped);
	  // Support for sequences not really finished yet.
	  switch (frame.lookup (name))
	    {
	    case Value::Object:
	      {
		const Library& lib = frame.library (metalib, name);
		static const std::vector<const Frame*> no_sequence;
		auto_vector<const Frame*> sequence;
		const std::vector<const Frame*>& old_sequence
		  = frame.check (name) 
		  ? frame.frame_sequence (name) 
		  : no_sequence;
		while (!looking_at (')') && good ())
		  {
                    if (frame.type_size (name) == sequence.size ())
                      {
                        std::ostringstream tmp;
                        tmp << "The '" << name 
                            << "' sequence should only contain "
                            << frame.type_size (name) << " elements";
                        error (tmp.str ());
                      }
                    if (looking_at ('&'))
                      {
                        skip ("&old");
                        if (!frame.check (name))
                          error ("No originals available");
                        for (size_t i = 0; i < old_sequence.size (); i++)
                          sequence.push_back (&old_sequence[i]->clone ());
                        continue;
                      }
		    const size_t element = sequence.size ();
                    std::ostringstream tmp;
                    tmp << "In '" << name << "' model #" << element + 1U;
                    Treelog::Open nest (msg, tmp.str ());

                    std::auto_ptr<Frame> child;
                    if (old_sequence.size () > element)
                      {
                        const Frame* old_frame = old_sequence[element];
                        const FrameModel* old_model
                          = dynamic_cast<const FrameModel*> (old_frame);
                        daisy_assert (old_model);
                        child = load_object (lib, true, old_model);
                      }
                    else
                      child = load_object (lib, true, NULL);

                    if (child.get ())
                      sequence.push_back (child.release ());
		  }
		frame.add (name, sequence);
		break;
	      }
	    case Value::AList:
	      {
		const size_t size = frame.type_size (name);
		static const std::vector<const Frame*> no_sequence;
		const FrameSubmodel& default_frame = frame.default_frame (name);
		const std::vector<const Frame*>& old_sequence
		  = frame.check (name) 
		  ? frame.frame_sequence (name) 
		  : no_sequence;
		auto_vector<const Frame*> sequence;
		bool skipped = false;
		// We do not force parentheses around the alist if it
		// is the last member of a fully ordered list.
		if (in_order && (current != end || !frame.total_order ()))
		  // in order and (not the last or unordered may follow)
		  {
		    daisy_assert (!skipped);
		    skip ("(");
		    skipped = true;
		  }
		while (!looking_at (')') && good ())
		  {
                    if (looking_at ('&'))
                      {
                        skip ("&old");
                        if (!frame.check (name))
                          error ("No originals available");
                        for (size_t i = 0; i < old_sequence.size (); i++)
                          sequence.push_back (&old_sequence[i]->clone ());
                        continue;
                      }
		    Parskip skip (*this);
		    const size_t element = sequence.size ();
                    std::ostringstream tmp;
                    tmp << "In '" << name << "' submodel #" << element + 1U;
                    Treelog::Open nest (msg, tmp.str ());
                    std::auto_ptr<Frame> child
		      = old_sequence.size () > element
                      ? std::auto_ptr<Frame> (&old_sequence[element]->clone ())
                      : std::auto_ptr<Frame> (new FrameSubmodelValue 
                                              (default_frame,
                                               Frame::parent_link));
		    load_list (*child);
		    sequence.push_back (child.release ());
		  }
		if (skipped)
		  skip (")");
		if (size != Value::Sequence && sequence.size () != size)
		  {
		    std::ostringstream str;
		    str << "Got " << sequence.size ()
                        << " array members, expected " << size;
		    error (str.str ());
		  }
		frame.add (name, sequence);
		break;
	      }
	    case Value::PLF:
	      {
		std::vector<const PLF*> plfs;
		int total = 0;
		const int size = frame.type_size (name);
		while (good () && !looking_at (')'))
		  {
		    Parskip dummy (*this);
		    PLF& plf = *new PLF ();
		    double last_x = -42;
		    int count = 0;
		    const symbol domain = frame.domain (name);
		    const symbol range = frame.range (name);
		    while (!looking_at (')') && good ())
		      {
                        if (looking_at ('&'))
                          {
                            skip ("&old");
                            if (!frame.check (name))
                              error ("No originals available");
                            const std::vector<const PLF*>& old_sequence
                              = frame.plf_sequence (name);
                            for (size_t i = 0; i < old_sequence.size (); i++)
                              plfs.push_back (new PLF (*old_sequence[i]));
                            continue;
                          }
			Parskip dummy2 (*this);
			double x = get_number (domain);
			{
			  if (count > 0 && x <= last_x)
			    error ("Non increasing x value");
			  last_x = x;
			  count++;
			}
			double y = get_number (range);
			try
			  {
			    frame.check (name, y);
			  }
			catch (const std::string& message)
			  {
			    error (name + ": " + message);
			  }
			plf.add (x, y);
		      }
		    if (count < 2)
		      error ("Need at least 2 points");
		    total++;
		    plfs.push_back (&plf);
		  }
		if (size != Value::Sequence && total != size)
		  {
		    std::ostringstream str;
		    str << "Got " << total
                        << " array members, expected " << size;
		    error (str.str ());

		    for (;total < size; total++)
		      plfs.push_back (new PLF ());
		  }
		frame.add (name, plfs);
		sequence_delete (plfs.begin (), plfs.end ());
		break;
	      }
	    case Value::Number:
	      {
		std::vector<double> array;
		std::vector<Filepos> positions;
		int count = 0;
		const int size = frame.type_size (name);
		const symbol syntax_dim = frame.dimension (name);
		unsigned int first_unchecked = 0;
		while (good () && !looking_at (')'))
		  {
                    if (looking_at ('&'))
                      {
                        skip ("&old");
                        if (!frame.check (name))
                          error ("No originals available");
                        const std::vector<double>& old_sequence
                          = frame.number_sequence (name);
                        for (size_t i = 0; i < old_sequence.size (); i++)
                          array.push_back (old_sequence[i]);
                        continue;
                      }
		    if (looking_at ('*'))
		      {
			skip ("*");
			const int same = get_integer ();
			if (array.size () == 0)
			  error ("must specify number before '*'");
			else
			  {
			    // Append same - 1 copies of last value.
			    for (int i = 1; i < same; i++)
			      {
				array.push_back (array.back ());
				positions.push_back (Filepos::none ());
				count++;
			      }
			  }
		      }
                    else if (looking_at ('['))
                      {
                        const symbol read_dim = get_dimension ();
                        if (check_dimension (syntax_dim, read_dim))
                          {
                            daisy_assert (positions.size () == array.size ());
                            for (unsigned int i = first_unchecked;
                                 i < array.size ();
                                 i++)
                              {
                                array[i] = convert (array[i],
                                                    syntax_dim, read_dim, 
                                                    positions[i]);
                              }
                            first_unchecked = array.size ();
                          }
                      }
		    else 
		      {
			array.push_back (get_number ());
			positions.push_back (lexer->position ());
			count++;
		      }
		  }
		daisy_assert (positions.size () == array.size ());
		for (unsigned int i = 0; i < array.size (); i++)
		  {
		    if (positions[i] != Filepos::none ())
		      try
			{
			  frame.check (name, array[i]);
			}
		      catch (const std::string& message)
			{
			  std::ostringstream str;
			  str << name << "[" << i << "]: " << message;
			  error (str.str (), positions[i]);
			}
		  }
		if (size != Value::Sequence && count != size)
		  {
		    std::ostringstream str;
		    str << "Got " << count 
                        << " array members, expected " << size;
		    error (str.str ());

		    for (;count < size; count++)
		      array.push_back (-1.0);
		  }
		frame.add (name, array);
		break;
	      }
	    case Value::String:
	      {
		std::vector<symbol> array;
		int count = 0;
		const int size = frame.type_size (name);

		while (!looking_at (')') && good ())
		  {
                    if (looking_at ('&'))
                      {
                        skip ("&old");
                        if (!frame.check (name))
                          error ("No originals available");
                        const std::vector<symbol>& old_sequence
                          = frame.name_sequence (name);
                        for (size_t i = 0; i < old_sequence.size (); i++)
                          array.push_back (old_sequence[i]);
                        continue;
                      }
		    array.push_back (get_symbol ());
		    count++;
		  }
		if (size != Value::Sequence && count != size)
		  {
		    std::ostringstream str;
		    str << "Got " << count 
                        << " array members, expected " << size;
		    error (str.str ());

		    for (;count < size; count++)
		      array.push_back (symbol ("<error>"));
		  }
		frame.add (name, array);
		// Handle "path" immediately.
		if (&frame == &metalib && name == "path")
		  {
		    const std::vector<symbol>& symbols 
		      = frame.name_sequence (name);
		    metalib.path ().set_path (symbols);
		  }
		break;
	      }
	    case Value::Integer:
	      {
		std::vector<int> array;
		int count = 0;
		const int size = frame.type_size (name);

		while (!looking_at (')') && good ())
		  {
                    if (looking_at ('&'))
                      {
                        skip ("&old");
                        if (!frame.check (name))
                          error ("No originals available");
                        const std::vector<int>& old_sequence
                          = frame.integer_sequence (name);
                        for (size_t i = 0; i < old_sequence.size (); i++)
                          array.push_back (old_sequence[i]);
                        continue;
                      }
		    array.push_back (get_integer ());
		    count++;
		  }
		if (size != Value::Sequence && count != size)
		  {
		    std::ostringstream str;
		    str << "Got " << count 
                        << " array members, expected " << size;
		    error (str.str ());

		    for (;count < size; count++)
		      array.push_back (-42);
		  }
		frame.add (name, array);
		// Handle "path" immediately.
		break;
	      }
	    case Value::Error:
	      if (name != error_symbol)
                error (std::string("Unknown attribute '") + name + "'");
	      skip_to_end ();
	      break;
	    default:
              if (name != error_symbol)
                error (std::string("Unsupported sequence '") + name + "'");
	      skip_to_end ();
	    }
	}

      // Value check.
      if (frame.check (name))
	{
	  Treelog::Open nest (msg, "Checking value of '" + name + "'");
	  if (!frame.check (metalib, name, msg))
	    error ("Invalid value");
	}
    }
}

void
ParserFile::Implementation::initialize ()
{
  if (!nest.get ())
    nest.reset (new Treelog::Open (msg, "Parsing file: '" + file + "'"));
}

ParserFile::Implementation::Implementation (Metalib& mlib,
                                            const symbol filename,
                                            Treelog& treelog)
  : metalib (mlib),
    msg (treelog),
    inputs (std::vector<const Frame*> ()),
    file (filename),
    owned_stream (mlib.path ().open_file (filename.name ())),
    lexer (new Lexer (filename.name (), *owned_stream, msg))
{ }

ParserFile::Implementation::~Implementation ()

{ }

void
ParserFile::load_nested ()
{
  impl->initialize ();
  impl->skip ();
  impl->load_list (impl->metalib);
  impl->skip ();
  impl->eof ();
}

void
ParserFile::load_top ()
{
  load_nested ();
  
  // Add inputs.
  impl->metalib.set_parser_inputs (impl->inputs);
  sequence_delete (impl->inputs.begin (), impl->inputs.end ());
  impl->inputs.erase (impl->inputs.begin (), impl->inputs.end ());
  
  // Remember filename.
  impl->metalib.add_parser_file (impl->file);
}

int
ParserFile::error_count () const
{
 return impl->lexer->error_count; 
}

bool
ParserFile::check () const
{ return impl->lexer->good (); }

ParserFile::ParserFile (Metalib& metalib, 
                        const std::string& filename, Treelog& msg)
  : Parser (symbol ("file")),
    impl (new Implementation (metalib, filename, msg))
{ }

ParserFile::ParserFile (Block& al)
  : Parser (al),
    impl (new Implementation (al.metalib (), al.name ("where"), al.msg ()))
{  }

ParserFile::~ParserFile ()
{ }

static struct ParserFileSyntax : public DeclareModel
{
  Model* make (Block& al) const
  { return new ParserFile (al); }

  ParserFileSyntax ()
    : DeclareModel (Parser::component, "file", 
	       "Read a setup file containing lots of parentheses.")
  { }
  void load_frame (Frame& frame) const
  { 
    frame.add ("where", Value::String, Value::Const,
		"File to read from.");
    frame.order ("where");
  }
} ParserFile_syntax;

// parser_file.C ends here.
