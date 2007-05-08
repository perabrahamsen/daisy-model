// w32reg.h -- Access W32 registry.

extern "C" char*
read_w32_registry_string (const char* root, const char* dir, const char* name);

extern "C" int
write_w32_registry_string(const char* root, const char* dir,
                          const char* name, const char* value);

// w32reg.h ends here.
