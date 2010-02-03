/* /////////////////////////////////////////////////////////////////////////////
 * File:        unistd.h
 *
 * Purpose:     Declaration of the .
 *
 * Created      1st November 2003
 * Updated:     3rd January 2004
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


#ifndef _SYNSOFT_INCL_H_UNISTD
#define _SYNSOFT_INCL_H_UNISTD

#ifndef _SYNSOFT_DOCUMENTATION_SKIP_SECTION
# define _SYNSOFT_VER_H_UNISTD_MAJOR      1
# define _SYNSOFT_VER_H_UNISTD_MINOR      1
# define _SYNSOFT_VER_H_UNISTD_REVISION   2
# define _SYNSOFT_VER_H_UNISTD_EDIT       6
#endif /* !_SYNSOFT_DOCUMENTATION_SKIP_SECTION */

/* ////////////////////////////////////////////////////////////////////////// */

#ifndef WIN32
# error This file is only currently defined for Win32 compilation units
#endif /* WIN32 */

/* /////////////////////////////////////////////////////////////////////////////
 * Constants and definitions
 */

#ifndef PATH_MAX
# define PATH_MAX   (260)   /*!< \brief The maximum number of characters (including null terminator) in a directory entry name */
#endif /* !PATH_MAX */

/* /////////////////////////////////////////////////////////////////////////////
 * API functions
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** \brief Change the current working directory.
 *
 * This function changes the current working directory to the directory 
 * specified by dirName. dirName must refer to an existing directory.
 *
 * \param dirName Path of new working directory
 * \return O on success, or -1 if there is an error
 */
int chdir(const char *dirName);

/** \brief Get the current working directory
 *
 * This function gets the full path of the current working directory
 * and stores it in buffer.
 *
 * \param buffer Storage location for the current working directory
 * \param max_len Maximum length of path (in characters)
 * \return buffer on success, or NULL to indicate error.
 */
char *getcwd(char *buffer, size_t max_len);

/** Returns the size, in bytes, of the page size */
int getpagesize(void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

/* ////////////////////////////////////////////////////////////////////////// */

#endif /* _SYNSOFT_INCL_H_UNISTD */

/* ////////////////////////////////////////////////////////////////////////// */
