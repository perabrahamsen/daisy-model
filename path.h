// path.h -- Handle file lookup.

#ifndef PATH_H
#define PATH_H

#include <vector>
#include <string>
using namespace std;

#if defined (__BORLANDC__) && __BORLANDC__ < 0x0550
struct istream;
struct ostream;
#else
#include <iosfwd>
#endif

namespace Path
{
  class Output
  {
    ostream& out;
    bool owner;
  public:
    ostream& stream () const;
    bool good () const;
    Output (const string& file);
    ~Output ();
  };
  class Input
  {
    istream& in;
    bool owner;
  public:
    istream& stream () const;
    bool good () const;
    Input (const string& file);
    ~Input ();
  };

  istream& open_file (const string& name);
  bool set_directory (const string& directory);
  void set_path (const vector<string>& path);
  void get_path (vector<string>& path);
}

#endif // PATH_H
