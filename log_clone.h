// log_clone.C
//
// Clone an object by using its log function.

#ifndef LOG_CLONE_H
#define LOG_CLONE_H

#include "log_alist.h"

class LogClone : public LogAList
{

  // Don't use this as a real log.
private:
  bool check(const string&) const;
  bool match (const Daisy& daisy);
  void done ();

  // Get result.
public:
  const AttributeList& result ();

  // Create and Destroy.
public:
  LogClone (const string& name, const Syntax&, const AttributeList&);
  ~LogClone ();
};

#endif // LOG_CLONE_H
