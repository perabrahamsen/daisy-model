// path.h -- Handle file lookup.

#ifndef PATH_H
#define PATH_H

#include <vector>
#include <string>
using namespace std;

#if defined (__BORLANDC__) && __BORLANDC__ < 0x0550
struct istream;
#else
#include <iosfwd>
#endif

namespace Path
{
  istream& open_file (const string& name);
  bool set_directory (const string& directory);
  void set_path (const vector<string>& path);
  void get_path (vector<string>& path);
}

#endif // PATH_H
