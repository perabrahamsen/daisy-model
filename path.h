// path.h -- Handle file lookup.

#ifndef PATH_H
#define PATH_H

#include <vector>
#include <string>
#include <iosfwd>
#include <memory>

#ifdef BUILD_DLL
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class Path
{
  // Content.
private:
  std::vector<std::string> path;

  // Use.
public:
  std::auto_ptr<std::istream> open_file (const std::string& name) const;
  bool set_directory (const std::string& directory);
  const std::string get_directory () const;
  void set_path (const std::vector<std::string>& path);
private:
  void get_path (std::vector<std::string>& path) const;

  // Utilities.
public:
  class InDirectory
  {
    Path& path;
    const std::string from;
    const bool ok;
  public:
    InDirectory (Path& path, const std::string& to);
    bool check () const;
    ~InDirectory ();
  };

  static EXPORT const std::string nodir (const std::string& name);
  static std::string get_daisy_home ();

  // Create and Destroy.
public:
  Path ();
};

#endif // PATH_H
