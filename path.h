// path.h -- Handle file lookup.

#ifndef PATH_H
#define PATH_H

#include <vector>
#include <string>

#if defined (__BORLANDC__) && __BORLANDC__ < 0x0550
namespace std 
{ 
  struct istream;
  struct ostream;
}
#else
#include <iosfwd>
#endif

namespace Path
{
  class Output
  {
    std::ostream& out;
    bool owner;
  public:
    std::ostream& stream () const;
    bool good () const;
    Output (const std::string& file);
    ~Output ();
  };
  class Input
  {
    std::istream& in;
    bool owner;
  public:
    std::istream& stream () const;
    bool good () const;
    Input (const std::string& file);
    ~Input ();
  };

  istream& open_file (const std::string& name);
  bool set_directory (const std::string& directory);
  void set_path (const std::vector<std::string>& path);
  void get_path (std::vector<std::string>& path);
}

#endif // PATH_H
