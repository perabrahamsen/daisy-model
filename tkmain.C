// tkmain.C --- Interface between daisy and tcl/tk.

#include "daisy.h"
#include "syntax.h"
#include "alist.h"
#include "library.h"

extern "C" {
#define Time DAISY_X_Time	// Hack around X definition of `Time'.
#include <tix.h>
#undef Time

  // Hack required according to Sun.
  extern int matherr ();
  int* tclDummyMathPtr = (int*) matherr;
};

int
main (int argc, char** argv)
{
  Tk_Main (argc, argv, Tcl_AppInit);  // We wish.
  return 0;			// Not reached.
}

class TkDaisyState
{
private:
  friend class TkDaisyEntry;
  Syntax& syntax;
  AttributeList& alist;
public:
  TkDaisyState ()
    : syntax (*new Syntax),
      alist (*new AttributeList)
    { 
      Daisy::load_syntax (syntax, alist);
    }
  ~TkDaisyState ()
    {
      delete &syntax;
      delete &alist;
    }
};

class TkDaisyEntry
{
private:
  // Content.
  Syntax& top_syntax;
  AttributeList& top_alist;

  // State.
private:
  const Syntax* syntax;
  AttributeList* alist;
  const vector<AttributeList*>* alist_sequence;
  const Library* library;
  string name;
  
  // Examine.
public:
  Syntax::type type () const;
  int size () const;
  void list (vector<string>& list) const;
  string value () const;

  // Use.
public:
  void traverse (const string& path);
private:
  void step (const string& element);
  void step_alist ();

  // Create and Destroy
public:
  TkDaisyEntry (TkDaisyState& s);
  ~TkDaisyEntry ();
};

Syntax::type 
TkDaisyEntry::type () const
{
  if (syntax)
    {
      assert (!library);
      return syntax->lookup (name);
    }
  else if (library && library->check (name))
    return Syntax::AList;
  else
    return Syntax::Error;
}

int
TkDaisyEntry::size () const
{
  if (syntax)
    return syntax->size (name);
  else
    return Syntax::Unspecified;
}

void
TkDaisyEntry::list (vector<string>& list) const
{
  switch (type ())
    {
    case Syntax::AList:
      if (library)
	{
	  assert (!syntax);
	  library->syntax (name).entries (list);
	}
      else
	{
	  assert (syntax);
	  syntax->syntax (name).entries (list);
	}
      break;
    case Syntax::Library:
    case Syntax::Object:
      assert (!library);
      assert (syntax);
      syntax->library (name).entries (list);
      break;
    default:
      // Do nothing.
      break;
    }
}

string
TkDaisyEntry::value () const
{
  return "unknown";
}

void
TkDaisyEntry::traverse (const string& path)
{
  string rest = path;
  while (true)
    {
      // Find the first entry in the path.
      const string::size_type sep = rest.find ('.');
      const string first = rest.substr (0, sep);

      // Traverse it.
      step (first);
      
      // Done?
      if (sep == string::npos)
	break;
      
      // Else continue.
      rest = rest.substr (sep + 1);
    }
}

void
TkDaisyEntry::step (const string& element)
{
  switch (type ())
    {
    case Syntax::AList:
      // Directly nested objects.  Just do it.
      if (library)
	{
	  assert (!syntax);
	  syntax = &library->syntax (name);
	  library = NULL;
	}
      else
	{
	  assert (syntax);
	  step_alist ();
	  syntax = &syntax->syntax (name);
	}
      break;
    case Syntax::Object:
      // Reference to an object in a library.
      if (library)
	{
	  assert (!syntax);
	  // Second step.  Lookup the object in the library.
	  if (library->check (name))
	    {
	      syntax = &library->syntax (name);
	      // We keep the alist from the first step.
	    }
	  else
	    {
	      syntax = NULL;
	      alist = NULL;
	      alist_sequence = NULL;
	    }
	  library = NULL;
	}
      else
	{
	  assert (!library);
	  // First step.  Get the library.
	  step_alist ();
	  library = &syntax->library (name);
	  syntax = NULL;
	}
      break;
    case Syntax::Library:
      // A definition of an object in a library.
      library = &syntax->library (name);
      syntax = NULL;
      break;
    default:
      // We cannot traverse other types.
      syntax = NULL;
      alist = NULL;
      alist_sequence = NULL;
      library = NULL;
    }
  name = element;
  // Only syntax of library should be defined.  Never both.
  assert (!library || !syntax);
}

void
TkDaisyEntry::step_alist ()
{
  alist = NULL;
  alist_sequence = NULL;
  return;
#if 0
  if (alist_sequence && alist_sequence->size () > 0)
    {
      switch (type ())
	{
	case Syntax::AList:
	  alist = *alist_sequence->begin ();
	  break;
	case Syntax::Object:
	  for (unsigned int i = 0; i < alist_sequence->size (); i++)
	    {
	      AttributeList* step = (*alist_sequence)[i];
	      if (step->check ("type") && step->name ("type") == name)
		alist = step;
	    }
	  break;
	default:
	  // Do nothing.
	  break;
	}
    }

  if (alist)
    if (alist->check (name))
      if (size () == Syntax::Sequence || size () > 0)
	{
	  alist = NULL;
	  alist_sequence = &alist->alist_sequence (name);
	}
      else if (size () == Syntax::Singleton)
	{
	  alist = &alist->alist (name);
	  alist_sequence = NULL;
	}
      else
	{
	  alist = NULL;
	  alist_sequence = NULL;
	}
    else
      {
	alist = NULL;
	alist_sequence = NULL;
      }
#endif
}    

TkDaisyEntry::TkDaisyEntry (TkDaisyState& s)
  : top_syntax (*new Syntax),
    top_alist (*new AttributeList),
    syntax (&top_syntax),
    alist (&top_alist),
    alist_sequence (NULL),
    library (NULL),
    name ("daisy")
{ 
  top_syntax.add ("daisy", s.syntax);
  top_alist.add ("daisy", s.alist);
}

TkDaisyEntry::~TkDaisyEntry ()
{
  delete &top_syntax;
  delete &top_alist;
}

extern "C" int 
tkdaisy_cmd (ClientData cd, Tcl_Interp* interp, int argc, char** argv)
{
  // We need a subcommand and an argument.
  if (argc < 2 || argc > 3)
    {
      interp->result = "wrong # args";
      return TCL_ERROR;
    }

  // Get thet state.
  TkDaisyState* state = (TkDaisyState*) cd;

  // Find the right entry.
  TkDaisyEntry entry (*state);

  if (argc == 3)
    entry.traverse (argv[2]);

  // Handle the `type' subcommand.
  if (strcmp (argv[1], "type") == 0)
    {
      Tcl_AppendResult (interp, 
			Syntax::type_name (entry.type ()),
			(char*) NULL);
      return TCL_OK;
    }

  // Give error if path doesn't point anywhere.
  if (entry.type ()== Syntax::Error)
    {
      interp->result = "bad path";
      return TCL_ERROR;
    }

  // Handle the `size' subcommand.
  if (strcmp (argv[1], "size") == 0)
    {
      const int size = entry.size ();
      if (size == Syntax::Singleton)
	interp->result = "singleton";
      else if (size == Syntax::Sequence)
	interp->result = "sequence";
      else if (size == Syntax::Unspecified)
	interp->result = "n/a";
      else
	sprintf (interp->result, "%d", size);
      return TCL_OK;
    }

  // Handle the `value' subcommand.
  if (strcmp (argv[1], "value") == 0)
    {
      Tcl_SetResult (interp, (char*) entry.value ().c_str (), TCL_VOLATILE);
      return TCL_OK;
    }
    
  // Handle the `list' subcommand.
  if (strcmp (argv[1], "list") == 0)
    {
      vector<string> list;
      entry.list (list);

      for (unsigned int i = 0; i < list.size (); i++)
	{
	  Tcl_AppendElement (interp, (char*) list[i].c_str ());
	}
      return TCL_OK;
    }

  // No command found.
  Tcl_AppendResult (interp, "unkown arg `", argv[1], "'", (char*) NULL);
  return TCL_ERROR;
}

extern "C" int
Tcl_AppInit (Tcl_Interp *interp)
{
  // Initialize TCL and TK.
  if (Tcl_Init (interp) == TCL_ERROR 
      || Tk_Init (interp) == TCL_ERROR
      || Tix_Init (interp) == TCL_ERROR
      )
    return TCL_ERROR;

  Tcl_StaticPackage (interp, "Tk", Tk_Init, Tk_SafeInit);
  
  // Create the global state.
  TkDaisyState* state = new TkDaisyState ();

  // The `daisy' command.
  Tcl_CreateCommand (interp, "daisy", &tkdaisy_cmd, 
		     (ClientData) state, (Tcl_CmdDeleteProc *) NULL);

  // The tkdaisy initialization file.
  Tcl_SetVar (interp, "tcl_rcFileName", "~/.tkdaisyrc", TCL_GLOBAL_ONLY);

  // All is well.
  return TCL_OK;
}
