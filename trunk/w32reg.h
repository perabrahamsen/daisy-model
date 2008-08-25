// w32reg.h -- Access W32 registry.

#ifndef W32REG_H
#define W32REG_H

#if defined (_WIN32) || defined (__CYGWIN32__)

extern "C" char*
read_w32_registry_string (const char* root, const char* dir, const char* name);

extern "C" int
write_w32_registry_string(const char* root, const char* dir,
                          const char* name, const char* value);

#endif // _WIN32 || __CYGWIN32__
#endif // W32REG_H

// w32reg.h ends here.
