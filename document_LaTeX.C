// document_LaTeX.C --- Support for documenting Daisy in LaTeX format.
// 
// Copyright 2002 Per Abrahamsen and KVL
// Copyright 1996-2001 Per Abrahamsen and Søren Hansen
// Copyright 2000-2001 KVL
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


#include "document.h"
#include "time.h"
#include "plf.h"
#include "tmpstream.h"
#include "version.h"
#include <ctype.h>
#include <time.h>

struct DocumentLaTeX : public Document
{
  // remember this for models.
  string current_component;
  bool ordered;
  bool submodel;

  // LaTeX functions.
  void print_quoted (ostream&, const string&);

  // Private functions.
  void print_entry_name (ostream& out, const string& name);
  void print_entry_type (ostream& out,
			 const string& name,
			 const Syntax::type type,
			 int size, 
			 const Syntax& syntax,
			 const AttributeList& alist);
  void print_entry_size (ostream& out, const string&, int size);
  void print_entry_description (ostream& out,
				const string& name, 
				const string& description);
  void print_entry_submodel (ostream& out,
			     const string& name, 
			     const Syntax::type type, 
			     int size,
			     int level,
			     const Syntax& syntax,
			     const AttributeList& alist);
  void print_entry_category (ostream& out,
			     const string& name, 
			     const Syntax::type type, 
			     const Syntax& syntax,
			     const AttributeList& alist);
  void print_entry_value (ostream& out,
			  const string& name, 
			  const Syntax::type type, 
			  int size,
			  const Syntax& syntax,
			  const AttributeList& alist);

  // Document functions.
  void print_submodel_entry (ostream&, const string&, int level,
			     const Syntax& syntax,
			     const AttributeList& alist);
  void print_submodel_empty (ostream&, const string&, int level);
  void print_submodel_header (ostream& out, const string&, int level);
  void print_submodel_trailer (ostream& out, const string&, int level);
  void print_sample_ordered (ostream& out, const string& name, bool seq);
  void print_log_header (ostream& out, const string&, int level);
  void print_log_trailer (ostream& out, const string&, int level);
  void print_sample_entry (ostream& out, const string& name, bool seq);
  void print_sample_header (ostream& out, const string& name);
  void print_sample_trailer (ostream& out, const string&);
  void print_model_header (ostream& out, const string& name);
  void print_model_description (ostream& out, const string& description);
  void print_model_trailer (ostream& out, const string& name);
  void print_fixed_header (ostream&, const string& name);
  void print_fixed_trailer (ostream&, const string& name);
  void print_component_header (ostream& out, const string& name);
  void print_component_description (ostream& out, const string& description);
  void print_component_trailer (ostream& out, const string& name);
  void print_fixed_all_header (ostream&) ;
  void print_fixed_all_trailer (ostream&);
  void print_document_header (ostream& out);
  void print_document_trailer (ostream& out);
  
  // Create & Destroy.
  DocumentLaTeX (const AttributeList& al)
    : Document (al),
      current_component ("Daisy"),
      ordered (false),
      submodel (false)
    { }
  ~DocumentLaTeX ()
    { }
};

void
DocumentLaTeX::print_quoted (ostream& out, const string& name)
{
  for (unsigned int i = 0; i < name.length (); i++)
    switch (name[i])
      {
      case '^':
	if (i+1 < name.length () && (isalnum (name[i+1]) || name[i+1] == '-'))
	  {
	    out << "$" << name[i] << "{";
	    do
	      {
		out << name[i+1];
		i++;
	      }
	    while (i+1 < name.length () && isalnum (name[i+1]));
	    out << "}$";
	  }
	else
	  out << "\\" << name[i] << "{ }";
	break;
      case '~':
	out << "\\" << name[i] << "{ }";
	break;
      case '_':
      case '#':
      case '$':
      case '%':
      case '&':
      case '{':
      case '}':
	out << "\\" << name[i];
	break;
      case '\\':
	out << "\\mbox{$\\backslash$}";
	break;
      case '[':
      case ']':
      case '+':
      case '=':
      case '<':
      case '>':
	out << "\\mbox{$" << name[i] << "$}";
	break;
      default:
	out << name[i];
      }
}

void 
DocumentLaTeX::print_entry_name (ostream& out, const string& name)
{
  out << "\n\\item \\textit{";
  print_quoted (out, name);
  out << "}: ";
}

void 
DocumentLaTeX::print_entry_type (ostream& out,
				 const string& name,
				 const Syntax::type type, 
				 int size, 
				 const Syntax& syntax,
				 const AttributeList& alist)
{
  switch (type)
    {
    case Syntax::Number:
      {
	out << "number ";
	const string& dimension = syntax.dimension (name);
	if (dimension == Syntax::None ())
	  out << "(dimensionless)";
	else if (dimension == Syntax::Unknown ())
	  out << "(dimension not specified)";
	else
	  {
	    out << "\\textbf{$[$";
	    print_quoted (out, dimension);
	    out << "$]$}";
	  }
      }
      break;
    case Syntax::AList:
      {
	string submodel = "";
	if (size != Syntax::Singleton || !alist.check (name))
	  {
	    const AttributeList& nested = syntax.default_alist (name);
	    if (nested.check ("submodel"))
	      submodel = nested.name ("submodel");
	  }
	else
	  {
	    const AttributeList& nested = alist.alist (name);
	    if (nested.check ("submodel"))
	      submodel = nested.name ("submodel");
	  }
	if (submodel.length () > 0)
	  {
	    out << "\\textbf{";
	    print_quoted (out, submodel);
	    out << "} fixed component (see section~\\ref{fixed:"
		<< submodel << "})";
	  }
	else
	  out << "submodel (see section~\\ref{type:alist})";
      }
      break;
    case Syntax::PLF:
      {
	out << "plf ";
	const string& domain = syntax.domain (name);
	const string& range = syntax.range (name);
	out << "\\textbf{$[$";
	print_quoted (out, domain);
	out << "} $\\rightarrow $ \\textbf{";
	print_quoted (out, range);
	out << "$]$}";
      }
      break;
    case Syntax::Boolean:
      out << "boolean (see section~\\ref{type:boolean})";
      break;
    case Syntax::String:
      out << "string (see section~\\ref{type:string})";
      break;
    case Syntax::Date:
      out << "time value (see section~\\ref{type:date})";
      break;
    case Syntax::Integer:
      out << "integer";
      break;
    case Syntax::Object:
      {
	const string& component = syntax.library (name).name ();
	out << "\\textbf{";
	print_quoted (out, component);
	out << "} component (see chapter~\\ref{component:"
	    << component << "})";
      }
      break;
    case Syntax::Library:
    case Syntax::Error:
    default:
      assert (false);
    };
}

void 
DocumentLaTeX::print_entry_size (ostream& out, const string&, int size)
{
  if (size == Syntax::Singleton)
    /* do nothing */;
  else if (size == Syntax::Sequence)
    out << " sequence";
  else
    out << " array of length " << size;
}

void 
DocumentLaTeX::print_entry_description (ostream& out,
					const string&, 
					const string& description)
{
  if (description != Syntax::Unknown ())
    {
      out << "\\\\\n";
      print_quoted (out, description);
      out << "\n";
    }
}

void 
DocumentLaTeX::print_entry_submodel (ostream& out,
				     const string& name, 
				     const Syntax::type type, 
				     int size,
				     int level,
				     const Syntax& syntax,
				     const AttributeList& alist)
{
  if (type == Syntax::AList)
    {
      submodel = true;		// Affects how the sample header looks.
      const Syntax& child = syntax.syntax (name);
      const AttributeList& nested 
	= (size != Syntax::Singleton || !alist.check (name))
	? syntax.default_alist (name)
	: alist.alist (name);
      if (!nested.check ("submodel"))
	{
	  print_sample (out, name, child, nested);
	  print_submodel (out, name, level, child, nested);
	}
      if (level == 1)
	submodel = false;
    }
}

void 
DocumentLaTeX::print_entry_category (ostream& out,
				     const string& name, 
				     const Syntax::type type, 
				     const Syntax& syntax,
				     const AttributeList& alist)
{
  if (type == Syntax::Number 
      && (syntax.is_state (name) || syntax.is_log (name)))
    {
      out << "\\index{";
      print_quoted (out, name);
      out << "}";
    }

  if (type == Syntax::Object)	// Objects and ALists don't have categories.
    {
      if (syntax.is_optional (name))
	out << "\\\\\nOptional component";
      else if (alist.check (name))
	out << "\\\\\nComponent";
    }
  else if (type == Syntax::AList)
    {
      if (syntax.is_optional (name))
	out << "\\\\\nOptional submodel";
      else if (alist.check (name))
	out << "\\\\\nSubmodel";
    }
  else if (syntax.is_optional (name))
    {
      if (syntax.is_const (name))
	out << "\\\\\nOptional parameter";
      else if (syntax.is_state (name))
	out << "\\\\\nOptional state variable";
      else if (syntax.is_log (name))
	out << "\\\\\nOptional log variable";
      else 
	assert (false);
    }
  else
    {
      if (syntax.is_const (name))
	out << "\\\\\nParameter";
      else if (syntax.is_state (name))
	out << "\\\\\nState variable";
      else if (syntax.is_log (name))
	out << "\\\\\nLog variable";
      else 
	assert (false);
    }
}

void 
DocumentLaTeX::print_entry_value (ostream& out,
				  const string& name, 
				  const Syntax::type type, 
				  int size,
				  const Syntax& syntax,
				  const AttributeList& alist)
{
  if (alist.check (name))
    {
      if (size == Syntax::Singleton)
	switch (type)
	  {
	  case Syntax::Number:
	    out << " (default " << alist.number (name) << ")";
	    break;
	  case Syntax::AList:
	    {
	      const bool has_errors
		= !syntax.syntax (name).check (alist.alist (name), 
					       Treelog::null ());
	      if (has_errors)
		out << " (has partially specified default value)";
	      else 
		out << " (has fully specified default value)";
	    }
	    break;
	  case Syntax::PLF:
	    out << " (has default value with" << alist.plf (name).size ()
		<< " points)";
	    break;
	  case Syntax::Boolean:
	    out << " (default " 
		<< (alist.flag (name) ? "true" : "false") << ")";
	    break;
	  case Syntax::String:
	    {
	      const string& value = alist.name (name);
	      if (value.length () < 30)
		{
		  out << " (default `";
		  print_quoted (out, value);
		  out << "')";
		}
	      else
		out << " (has default value with length "
		    << value.length () << ")";
	    }
	    break;
	  case Syntax::Date:
	    {
	      const Time& time = alist.time (name);
	      out << " (default " << time.year () << " "
		  << time.month () << " " << time.mday () << " "
		  << time.hour () << ")";
	    }
	    break;
	  case Syntax::Integer:
	    out << " (default " << alist.integer (name) << ")";
	    break;
	  case Syntax::Object:
	    {
	      const AttributeList& object = alist.alist (name);
	      if (object.check ("type"))
		{
		  out << " (default `";
		  print_quoted (out, object.name ("type"));
		  out << "')";
		}
	      else
		out << " (default anonymous model)";
	    }
	    break;
	  case Syntax::Library:
	  case Syntax::Error:
	    assert (false);
	  }
      else
	switch (type)
	  {
	  case Syntax::Number:
	  case Syntax::AList:
	  case Syntax::PLF:
	  case Syntax::Boolean:
	  case Syntax::String:
	  case Syntax::Date:
	  case Syntax::Integer:
	  case Syntax::Object:
	    if (alist.size (name) == 0)
	      out << " (default: an empty sequence)";
	    else
	      out << " (has default value with length " 
		  << alist.size (name) << ")";
	    break;
	  case Syntax::Library:
	  case Syntax::Error:
	    assert (false);
	  }
    }
}
void 
DocumentLaTeX::print_submodel_entry (ostream& out,
				     const string& name, int level,
				     const Syntax& syntax,
				     const AttributeList& alist)
{
  const Syntax::type type = syntax.lookup (name);

  // We ignore libraries.
  if (type == Syntax::Library)
    return;

  const int size = syntax.size (name);

  // Print name.
  print_entry_name (out, name);

  // Print type.
  print_entry_type (out, name, type, size, syntax, alist);

  // Print size.
  print_entry_size (out, name, size);

  if (!syntax.is_log (name))
    {
      // Print category.
      print_entry_category (out, name, type, syntax, alist);

      // Print value.
      print_entry_value (out, name, type, size, syntax, alist);
    }

  // Print description line.
  const string& description = syntax.description (name);
  print_entry_description (out, name, description);

  // print submodel entries, if applicable
  print_entry_submodel (out, name, type, size, level + 1, syntax, alist);
}


void
DocumentLaTeX::print_submodel_empty (ostream& out,
				     const string& name, int)
{ 
  out << "\n\n";
  print_quoted (out, name);
  out << " has no members\n";
}

void
DocumentLaTeX::print_sample_ordered (ostream& out, 
				     const string& name, bool sequence)
{ 
  ordered = true;
  out << "\\textit{";
  print_quoted (out, name);
  out << "}";
  if (sequence)
    out << "\\ldots{}";
  out << "~";
}

void
DocumentLaTeX::print_sample_entry (ostream& out,
				   const string& name, bool sequence)
{ 
  if (ordered)
    out << "\\\\\n\\>";
  else
    ordered = true;
      
  out << "(";
  print_quoted (out, name);
  out << "~\\textit{";
  print_quoted (out, name);
  out << "}";
  if (sequence)
    out << "\\ldots{}";
  out << ")~";
}

void
DocumentLaTeX::print_sample_header (ostream& out, const string& name)
{ 
  assert (ordered == false);
  out << "\n\\noindent\n\\begin{tt}\n\\begin{tabbing}\n$<$~";
  if (!submodel)
    {
      print_quoted (out, name);
      out << "~";
    }
  out << "\\=";
}

void
DocumentLaTeX::print_sample_trailer (ostream& out, const string&)
{ 
  out << "$>$\n\\end{tabbing}\n\\end{tt}";
  ordered = false;
}

void
DocumentLaTeX::print_submodel_header (ostream& out,
				      const string&, int level)
{ 
  if (level > 3)
    out << "\n\\begin{enumerate}";
  else
    out << "\n\\begin{itemize}";
}

void
DocumentLaTeX::print_submodel_trailer (ostream& out,
				       const string&, int level)
{ 
  if (level > 3)
    out << "\\end{enumerate}\n";
  else
    out << "\\end{itemize}\n";
}

void
DocumentLaTeX::print_log_header (ostream& out, const string& name, int level)
{ 
  if (level == 0)
    out << "\n\\subsection*{Log Variables}\n";
  else
    out << "\n\\textbf{Log Variables}\n";
    
  print_submodel_header (out, name, level);
}

void
DocumentLaTeX::print_log_trailer (ostream& out, const string& name, int level)
{  print_submodel_trailer (out, name, level); }

void
DocumentLaTeX::print_model_header (ostream& out, const string& name)
{ 
  out << "\n\\section{";
  print_quoted (out, name);
  out << "}\n\\label{model:" << current_component << "-" << name << "}\n";
}

void
DocumentLaTeX::print_model_description (ostream& out, 
					const string& description)
{ 
  out << "\n";
  print_quoted (out, description);
  out << "\n";
}

void
DocumentLaTeX::print_model_trailer (ostream&, const string&)
{ }

void
DocumentLaTeX::print_fixed_header (ostream& out, const string& name)
{ 
  out << "\n\\section{";
  print_quoted (out, name);
  out << "}\n\\label{fixed:" << name << "}\n";
}

void
DocumentLaTeX::print_fixed_trailer (ostream&, const string&)
{ }

void
DocumentLaTeX::print_component_header (ostream& out, const string& name)
{ 
  current_component = name;
  out << "\n\\chapter{";
  print_quoted (out, name);
  out << "}\n\\label{component:" << name << "}\n";
}

void
DocumentLaTeX::print_component_description (ostream& out, 
					    const string& description)
{ 
  out << "\n";
  print_quoted (out, description);
  out << "\n";
}

void
DocumentLaTeX::print_component_trailer (ostream&, const string&)
{ 
  current_component = "Daisy";
}

void
DocumentLaTeX::print_fixed_all_header (ostream& out)
{ 
  out << "\
\\chapter{Fixed Components}\n\
\\label{cha:fixed}\n\
\n\
Fixed components are similar to ordinary component, with the exceptions\n\
that there can only be one model, that is, only a single implementation\n\
of the component, and that it is not possible to define libraries of\n\
standard parameterizations for the model.\n\
"; }

void
DocumentLaTeX::print_fixed_all_trailer (ostream&)
{ }

void
DocumentLaTeX::print_document_header (ostream& out)
{ 
  out << "\
%%% components.tex --- Description of Daisy components.\n\
%%%\n\
%%% This file is automatically generated, do not edit.\n"; }

void
DocumentLaTeX::print_document_trailer (ostream& out)
{ 
  time_t now = time (NULL);
  out << "\
\n\
\\chapter*{Version}\n\
\\label{version}\n\
\\addcontentsline{toc}{chapter}{\\numberline{}Version}\n\
\n\
Daisy version " << version << ".\\\\\n\
LaTeX manual generated: " << ctime (&now) << "\n\
\n\
%%% Local Variables:\n\
%%% mode: latex\n\
%%% TeX-master: \"reference\"\n\
%%% End:\n\
\n\
%%% components.tex ends here\n";
}

static struct DocumentLaTeXSyntax
{
  static Document&
  make (const AttributeList& al)
    { return *new DocumentLaTeX (al); }
  DocumentLaTeXSyntax ()
    {
      Syntax& syntax = *new Syntax ();
      AttributeList& alist = *new AttributeList ();
      alist.add ("description", "Output Daisy components as LaTeX chapters.");
      Librarian<Document>::add_type ("LaTeX", alist, syntax, &make);
    }
} DocumentLaTeX_syntax;

