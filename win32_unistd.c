/* /////////////////////////////////////////////////////////////////////////////
 * File:        unistd.c
 *
 * Purpose:     Definition of the chdir() and other API functions for the Win32 platform.
 *
 * Created      1st November 2003
 * Updated:     19th December 2003
 *
 * Author:      Matthew Wilson, Synesis Software Pty Ltd.
 *
 * License:     (Licensed under the Synesis Software Standard Source License)
 *
 *              Copyright (C) 2002, Synesis Software Pty Ltd.
 *
 *              All rights reserved.
 *
 *              www:        http://www.synesis.com.au/
 *                          http://www.synesis.com.au/software/
 *
 *              email:      software@synesis.com.au
 *                          software@synesis-group.com
 *
 *              Redistribution and use in source and binary forms, with or
 *              without modification, are permitted provided that the following
 *              conditions are met:
 *
 *              (i) Redistributions of source code must retain the above
 *              copyright notice and contact information, this list of
 *              conditions and the following disclaimer.
 *
 *              (ii) Any derived versions of this software (howsoever modified)
 *              remain the sole property of Synesis Software.
 *
 *              (iii) Any derived versions of this software (howsoever modified)
 *              remain subject to all these conditions.
 *
 *              (iv) Neither the name of Synesis Software nor the names of any
 *              subdivisions, employees or agents of Synesis Software, nor the
 *              names of any other contributors to this software may be used to
 *              endorse or promote products derived from this software without
 *              specific prior written permission.
 *
 *              This source code is provided by Synesis Software "as is" and any
 *              warranties, whether expressed or implied, including, but not
 *              limited to, the implied warranties of merchantability and
 *              fitness for a particular purpose are disclaimed. In no event
 *              shall the Synesis Software be liable for any direct, indirect,
 *              incidental, special, exemplary, or consequential damages
 *              (including, but not limited to, procurement of substitute goods
 *              or services; loss of use, data, or profits; or business
 *              interruption) however caused and on any theory of liability,
 *              whether in contract, strict liability, or tort (including
 *              negligence or otherwise) arising in any way out of the use of
 *              this software, even if advised of the possibility of such
 *              damage.
 *
 * ////////////////////////////////////////////////////////////////////////// */


#define _SYNSOFT_VER_C_UNISTD_MAJOR       1
#define _SYNSOFT_VER_C_UNISTD_MINOR       2
#define _SYNSOFT_VER_C_UNISTD_REVISION    1
#define _SYNSOFT_VER_C_UNISTD_EDIT        5

#include <windows.h>
#include <errno.h>

#include "win32_unistd.h"

/* /////////////////////////////////////////////////////////////////////////////
 * Feature support
 */

#if defined(__BORLANDC__)
# define UNIXEM_chdir_PROVIDED_BY_COMPILER
# define UNIXEM_getcwd_PROVIDED_BY_COMPILER
#elif defined(__DMC__)
# define UNIXEM_chdir_PROVIDED_BY_COMPILER
# define UNIXEM_getcwd_PROVIDED_BY_COMPILER
#elif defined(__INTEL_COMPILER)
#elif defined(_MSC_VER)
#elif defined(__MWERKS__)
#else
# error Compiler not discriminated
#endif /* compiler */

/* /////////////////////////////////////////////////////////////////////////////
 * API functions
 */

#ifndef UNIXEM_chdir_PROVIDED_BY_COMPILER
int chdir(const char *dirName)
{
    if(SetCurrentDirectoryA(dirName))
    {
        return 0;
    }
    else
    {
        errno = ENOENT;

        return -1;
    }
}
#endif /* !UNIXEM_chdir_PROVIDED_BY_COMPILER */

#ifndef UNIXEM_getcwd_PROVIDED_BY_COMPILER
char *getcwd(char *buffer, size_t max_len)
{
    if(GetCurrentDirectoryA(max_len, buffer))
    {
        return buffer;
    }
    else
    {
        errno = ERANGE;

        return NULL;
    }
}
#endif /* !UNIXEM_getcwd_PROVIDED_BY_COMPILER */

int getpagesize(void)
{
	SYSTEM_INFO	sysinfo;

	GetSystemInfo(&sysinfo);

	return sysinfo.dwPageSize;
}

/* ////////////////////////////////////////////////////////////////////////// */
