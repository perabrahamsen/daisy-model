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
  // Shared content.
private:
  static std::string daisy_home;
  static std::vector<std::string> daisy_path;

public:
  static const std::vector<std::string>& get_daisy_path ();
  static const std::string& get_daisy_home ();
  static EXPORT const std::string nodir (const std::string& name);

  // Content.
private:
  std::vector<std::string> path;
  std::string current_directory;

  // Use.
public:
  std::auto_ptr<std::istream> open_file (const std::string& name) const;
  bool set_directory (const std::string& directory);
  const std::string& get_directory () const;
  void set_path (const std::vector<std::string>& path);

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

  // Create and Destroy.
private:
  Path (const Path&);
  Path& operator= (const Path&);
public:
  void reset ();
  Path ();
  ~Path ();
};

#endif // PATH_H
