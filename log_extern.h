// log_extern.h --- Logging to external model.
// 
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL.
//
// This file is part of Daisy.
// 
// Daisy is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
// 
// Daisy is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser Public License for more details.
// 
// You should have received a copy of the GNU Lesser Public License
// along with Daisy; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


#ifndef LOG_EXTERN_H
#define LOG_EXTERN_H

#include "symbol.h"

struct Scope;
struct Treelog;

extern const Scope* find_extern_scope (symbol name);
extern const int daisy_scope_has_number (const Scope* scope, symbol name);
extern const double daisy_scope_number (const Scope* scope, symbol name);
extern const char* daisy_scope_dimension (const Scope* scope, symbol name);

#endif // LOG_EXTERN_H
