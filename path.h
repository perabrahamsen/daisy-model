// path.h -- Handle file lookup.

#ifndef PATH_H
#define PATH_H

#include <vector>
#include <string>
#include <iosfwd>

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

  std::istream& open_file (const std::string& name);
  bool set_directory (const std::string& directory);
  const std::string get_directory ();
  void set_path (const std::vector<std::string>& path);
  void get_path (std::vector<std::string>& path);

  class InDirectory
  {
    const std::string from;
    const bool ok;
  public:
    InDirectory (const std::string& to);
    bool check () const;
    ~InDirectory ();
  };
}

#endif // PATH_H
