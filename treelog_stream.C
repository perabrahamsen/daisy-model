// treelog_stream.C -- Log hierarchical information in an ostream.

#include "treelog_stream.h"
// BCC5.01 lack ostream.
#include <iostream>
#include <deque>
// GCC 2.95.2 need a ".h".
#include <assert.h>

struct TreelogStream::Implementation
{
  ostream& out;
  deque<string> path;
  deque<bool> touched;

  Implementation (ostream& o)
    : out (o)
  { }
  ~Implementation ()
  {
    assert (path.size () == 0);
    assert (touched.size () == 0);
  }
};

void
TreelogStream::open (const string& name)
{ 
  impl.path.push_back (name); 
  impl.touched.push_back (false); 

}

void
TreelogStream::close ()
{
  impl.path.pop_back (); 
  impl.touched.pop_back (); 
}

void
TreelogStream::entry (const string& text)
{
  Treelog::entry (text);
  assert (impl.path.size () > 0);

  for (unsigned int i = 0; i < impl.touched.size (); i++)
    {
      if (!impl.touched[i])
	{
	  impl.touched[i] = true;
	  for (unsigned int j = 0; j <= i; j++)
	    impl.out << "*";
	  impl.out << " " << impl.path[i] << "\n";
	}
    }
  for (unsigned int i = 0; i <= impl.touched.size (); i++)
    impl.out << " ";
  impl.out << "  "<< text << "\n";
}

TreelogStream::TreelogStream (ostream& out)
  : impl (*new Implementation (out))
{ }

TreelogStream::~TreelogStream ()
{ delete &impl; }

