// log_extern.h --- Logging to external model.

#ifndef LOG_EXTERN_H
#define LOG_EXTERN_H

#include <string>
#include <vector>

struct AttributeList;

class LogExternSource
{
  // Interface.
public:
  typedef enum { Error, Missing, Number, Name, Array } type;
  virtual type lookup (const string& tag) const = 0;
  virtual double number (const string& tag) const = 0;
  virtual const string& name (const string& tag) const = 0;
  virtual const vector<double>& array (const string& tag) const = 0;

  // Library.
  static const LogExternSource& find (const string& name);

  // Create and Destroy.
public:
  LogExternSource (const AttributeList&);
  virtual ~LogExternSource ();
};

#endif // LOG_EXTERN_H

