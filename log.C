// log.C

#include "log.h"
#include "daisy.h"
#include "message.h"

EMPTY_TEMPLATE
Librarian<Log>::Content* Librarian<Log>::content = NULL;

const char *const Log::description = "\
Running a simulation is uninteresting, unless you can get access to\n\
the results in one way or another.  The purpose of the 'log' component\n\
is to provide this access.  Most 'log' models does this by writing a\n\
summary of the state to a log file.";

struct Log::Implementation
{
  list<const Geometry*> geometries;
};

bool
Log::check_entry (const string& name, const Library& library) const
{
  const string* type = &name;
  bool looking = true;

  while (looking && !check (*type))
    {
      if (library.check (*type))
	{
	  const AttributeList alist = library.lookup (*type);
	  if (alist.check ("type"))
	    type = &alist.name ("type");
	  else
	    looking = false;
	}
      else
	looking = false;
    }
  return looking;
}


void 
Log::open_maybe (const string&)
{ }

void 
Log::close_maybe ()
{ }

void 
Log::open_alist (const string& name, const AttributeList&)
{ open (name); }

void 
Log::close_alist ()
{ close (); }

void 
Log::open_geometry (const Geometry& g)
{ 
  impl.geometries.push_back (&g);
}

void 
Log::close_geometry ()
{
  impl.geometries.pop_back ();
}

const Geometry*
Log::geometry ()
{
  return impl.geometries.back ();
}

void 
Log::done ()
{ }

void
Log::print_dlf_header (ostream& out, const AttributeList& al)
{
  if (al.check ("parser_files"))
  {
    const vector<string>& files = al.name_sequence ("parser_files");
    for (unsigned int i = 0; i < files.size (); i++)
      out << "SIMFILE: " << files[i] << "\n";
  }

  const string sim_description = al.name ("description");
  if (sim_description != Daisy::default_description)
    {
      out << "SIM: ";
      for (unsigned int i = 0; i < sim_description.size (); i++)
	if (sim_description[i] != '\n')
	  out << sim_description[i];
	else
	  out << "\nSIM: ";
      out << "\n";
    }
  out << "\n--------------------\n";
}

Log::Log (const AttributeList& al)
  : impl (*new Implementation ()),
    name (al.name ("type"))
{ }

Log::~Log ()
{
  delete &impl;
}
