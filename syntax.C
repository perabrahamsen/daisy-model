// syntax.C

#include "syntax.h"
#include "alist.h"

struct Syntax::Implementation
{
  // BUG: This should be replaced with STL maps.
  const int UGLY_MAX_SIZE = 1024;
  string UGLY_key[UGLY_MAX_SIZE];
  type UGLY_type[UGLY_MAX_SIZE];
  required UGLY_status[UGLY_MAX_SIZE];
  const Syntax* UGLY_syntax[UGLY_MAX_SIZE];
  const FTable* UGLY_function[UGLY_MAX_SIZE];
  int UGLY_size[UGLY_MAX_SIZE];
  int size;
};    

bool 
Syntax::check (string name, const AttributeList& vl, const Log& log) const
{
  bool error = false;

  for (int i = 0; i < impl.size; i++)
    {
      string key = impl.UGLY_key[i];
      if (impl.UGLY_status[i] == Mandatory && !vl.check (key))
	{
	  cerr << "Attributte " << key << "\n";
	  error = true;
	}
      else if (impl.UGLY_type[i] == List && vl.check (key))
	{
	  error |= !impl.UGLY_syntax[i]->check (key, 
						vl.list (key),
						log);
	}
    }
  if (error)
    cerr << "missing from " << name << "\n";

  return !error;
}

Syntax::type 
Syntax::lookup (string key) const
{
  for (int i = 0; i < impl.size; i++)
    if (impl.UGLY_key[i] == key)
      return impl.UGLY_type[i];
  return Error;
}

Syntax::required
Syntax::status (string key) const
{
  for (int i = 0; i < impl.size; i++)
    if (impl.UGLY_key[i] == key)
      return impl.UGLY_status[i];
  assert (false);
}

const Syntax&
Syntax::syntax (string key) const
{
  for (int i = 0; i < impl.size; i++)
    if (impl.UGLY_key[i] == key)
      return *impl.UGLY_syntax[i];
  assert (false);
}

const FTable*
Syntax::function (string key) const
{
  for (int i = 0; i < impl.size; i++)
    if (impl.UGLY_key[i] == key)
      return impl.UGLY_function[i];
  assert (false);
}

int
Syntax::size (string key) const
{
  for (int i = 0; i < impl.size; i++)
    if (impl.UGLY_key[i] == key)
      return impl.UGLY_size[i];
  assert (false);
}

void
Syntax::add (string key, type t, required req)
{
  assert (impl.size < impl.UGLY_MAX_SIZE);
  impl.UGLY_key[impl.size] = key;
  impl.UGLY_syntax[impl.size] = NULL;
  impl.UGLY_type[impl.size] = t;
  impl.UGLY_status[impl.size] = req;
  impl.UGLY_function[impl.size] = NULL;
  impl.UGLY_size[impl.size] = -1;
  impl.size++;
}

void
Syntax::add (string key, const Syntax* s, required req)
{
  add (key, List, req);
  impl.UGLY_syntax[impl.size - 1] = s;
}

void
Syntax::add (string key, const FTable* f, required req)
{
  add (key, Function, req);
  impl.UGLY_function[impl.size - 1] = f;
}

void
Syntax::add (string key, int s, required req)
{
  add (key, Array, req);
  impl.UGLY_size[impl.size - 1] = s;
}

Syntax::Syntax () : impl (*new Implementation ())
{ }

Syntax::~Syntax ()
{
  delete &impl;
}
