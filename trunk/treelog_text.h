// treelog_text.h -- Log hierarchical information as text.
// 
// Copyright 2007 Per Abrahamsen and KVL.
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


#ifndef TREELOG_TEXT_H
#define TREELOG_TEXT_H

#include "treelog.h"
#include <memory>

#ifdef __unix
#define EXPORT /* Nothing */
#elif defined (BUILD_DLL)
/* DLL export */
#define EXPORT __declspec(dllexport)
#else
/* EXE import */
#define EXPORT __declspec(dllimport)
#endif

class EXPORT TreelogText : public Treelog
{
  // Content.
private:
  struct Implementation;
  std::auto_ptr<Implementation> impl;

  // Output.
protected:
  virtual void write (const std::string&) = 0;
  void header ();

  // Nesting.
public:
  void open (const std::string& name);
  void close ();

  // Use.
public:
  void entry (const std::string&);

  // Create and Destroy.
private:
  TreelogText& operator= (const TreelogText&); // Disable.
  explicit TreelogText (const TreelogText&); // Disable.
protected:
  TreelogText ();
  ~TreelogText ();
};

class TreelogProgress : public TreelogText
{
  // Implement.
private:
  void write (const std::string&);
  void debug (const std::string&);
  void touch ();
  void flush ();

  // Create and Destroy.
private:
  TreelogProgress& operator= (const TreelogProgress&); // Disable.
  explicit TreelogProgress (const TreelogProgress&); // Disable.
public:
  TreelogProgress ();
  ~TreelogProgress ();
};

class TreelogString : public TreelogText
{
  // Implement.
private:
  struct Implementation;
  std::auto_ptr<Implementation> impl;
private:
  void write (const std::string&);
  void debug (const std::string&);
  void touch ();
  void flush ();

  // Use.
public:
  const std::string str () const;

  // Create and Destroy.
private:
  TreelogString& operator= (const TreelogString&); // Disable.
  explicit TreelogString (const TreelogString&); // Disable.
public:
  TreelogString ();
  ~TreelogString ();
};

class TreelogFile : public TreelogText
{
  // Content.
private:
  struct Implementation;
  std::auto_ptr<Implementation> impl;
  void write (const std::string&);
  void debug (const std::string&);
  void touch ();
  void flush ();

  // Create and Destroy.
private:
  TreelogFile& operator= (const TreelogFile&); // Disable.
  explicit TreelogFile (const TreelogFile&); // Disable.
public:
  TreelogFile (const std::string& filename );
  ~TreelogFile ();
};


#endif // TREELOG_TEXT_H
