// path.h -- Handle file lookup.

#ifndef PATH_H
#define PATH_H

#include "symbol.h"
#include <vector>
#include <iosfwd>
#include <memory>
#include <boost/noncopyable.hpp>

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class Path : private boost::noncopyable
{
  // Shared content.
private:
  static std::vector<symbol> daisy_path;

  static void parse_path (const std::string& colonpath,
                          std::vector<symbol>& result);
public:
  static const std::vector<symbol>& get_daisy_path ();
  static symbol get_daisy_home ();
  static EXPORT symbol nodir (symbol name);

  // Content.
private:
  std::vector<symbol> path;
  symbol input_directory;
  symbol output_directory;

  // Use.
public:
  std::unique_ptr<std::istream> open_file (symbol name) const;
  bool set_directory (symbol directory);
  void set_input_directory (symbol directory);
  symbol get_input_directory () const
  { return input_directory; }
  symbol get_output_directory () const
  { return output_directory; }
  void set_path (const std::vector<symbol>& path);
  void set_path (const std::string& colon_path);

  // Utilities.
public:
  class InDirectory
  {
    Path& path;
    const symbol from;
    const bool ok;
  public:
    InDirectory (Path& path, symbol to);
    bool check () const;
    ~InDirectory ();
  };

  // Create and Destroy.
public:
  void reset ();
  Path ();
  ~Path ();
};

#endif // PATH_H
