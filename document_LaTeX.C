// document_LaTeX.C --- Support for documenting Daisy in LaTeX format.

#include "document.h"
#include "time.h"

struct DocumentLaTeX : public Document
{
  // remember this for models.
  string current_component;

  // LaTeX functions.
  void print_quoted (ostream&, const string&);

  // Document functions.
  void print_submodel_entry (ostream&, const string&, int level,
			     const Syntax& syntax,
			     const AttributeList& alist);
  void print_submodel_empty (ostream&, const string&, int level);
  void print_submodel_header (ostream& out, const string&, int level);
  void print_submodel_trailer (ostream& out, const string&, int level);
  void print_sample_ordered (ostream& out, const string& name);
  void print_sample_entry (ostream& out, const string& name);
  void print_sample_header (ostream& out, const string& name);
  void print_sample_trailer (ostream& out, const string&);
  void print_model_header (ostream& out, const string& name);
  void print_model_description (ostream& out, const string& description);
  void print_model_trailer (ostream& out, const string& name);
  void print_component_header (ostream& out, const string& name);
  void print_component_description (ostream& out, const string& description);
  void print_component_trailer (ostream& out, const string& name);
  void print_document_header (ostream& out);
  void print_document_trailer (ostream& out);
  
  // Create & Destroy.
  DocumentLaTeX (const AttributeList& al)
    : Document (al),
      current_component ("Daisy")
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
DocumentLaTeX::print_submodel_entry (ostream& out,
				     const string& name, int level,
				     const Syntax& syntax,
				     const AttributeList& alist)
{
  const Syntax::type type = syntax.lookup (name);

  // We ignore libraries.
  if (type == Syntax::Library)
    return;

  // Print name.
  out << "\\item \\textit{";
  print_quoted (out, name);
  out << "}: ";

  // Print type.
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
      out << "submodel (see section~\\ref{type:alist})";
      break;
    case Syntax::CSMP:
      out << "csmp (see section~\\ref{type:csmp})";
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
	out << "} component (see chapter~\\ref{component:" << component
	    << "})";
      }
      break;
    case Syntax::Library:
    case Syntax::Error:
    default:
      assert (false);
    };

  // Print size.
  const int size = syntax.size (name);
  if (size == Syntax::Singleton)
    /* do nothing */;
  else if (size == Syntax::Sequence)
    out << " sequence";
  else
    out << " array of length " << size;

  // Print category.
  out << "\\\\\n";
  if (type == Syntax::Object)	// Objects and ALists don't have categories.
    {
      if (syntax.is_optional (name))
	out << "Optional component";
      else
	out << "Component";
    }
  else if (type == Syntax::AList)
    {
      if (syntax.is_optional (name))
	out << "Optional submodel";
      else
	out << "Submodel";
    }
  else if (syntax.is_optional (name))
    {
      if (syntax.is_const (name))
	out << "Optional parameter";
      else if (syntax.is_state (name))
	out << "Optional state variable";
      else if (syntax.is_log (name))
	out << "Optional log variable";
      else 
	assert (false);
    }
  else
    {
      if (syntax.is_const (name))
	out << "Parameter";
      else if (syntax.is_state (name))
	out << "State variable";
      else if (syntax.is_log (name))
	out << "Log variable";
      else 
	assert (false);
    }
  
  if (alist.check (name))
    {
      if (size == Syntax::Singleton)
	switch (type)
	  {
	  case Syntax::Number:
	    out << " (default " << alist.number (name) << ")";
	    break;
	  case Syntax::AList:
	  case Syntax::CSMP:
	    out << " (has default value)";
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
		out << " (has default value)";
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
	  case Syntax::CSMP:
	  case Syntax::Boolean:
	  case Syntax::String:
	  case Syntax::Date:
	  case Syntax::Integer:
	  case Syntax::Object:
	    out << " (has default value)";
	    break;
	  case Syntax::Library:
	  case Syntax::Error:
	    assert (false);
	  }
    }

  // Print description line.
  const string& description = syntax.description (name);
  if (description != Syntax::Unknown ())
    {
      out << "\\\\\n";
      print_quoted (out, description);
      out << "\n";
    }

  // print submodel entries, if applicable
  if (type == Syntax::AList)
    {
      if (!alist.check (name))
	{
	  AttributeList submodel_alist;
	  print_submodel (out, name, level + 1, 
			  syntax.syntax (name), submodel_alist);
	}
      else if (size == Syntax::Singleton)
	print_submodel (out, name, level + 1, 
			syntax.syntax (name), alist.alist (name));
      else 
	print_submodel (out, name, level + 1, 
			syntax.syntax (name), syntax.default_alist (name));
    }
}


void
DocumentLaTeX::print_submodel_empty (ostream& out,
				     const string& name, int)
{ 
  out << "\\\\\n\n";
  print_quoted (out, name);
  out << " has no members\n";
}

void
DocumentLaTeX::print_sample_ordered (ostream& out, const string& name)
{ 
  out << "~\\textit{";
  print_quoted (out, name);
  out << "}";
}

void
DocumentLaTeX::print_sample_entry (ostream& out, const string& name)
{ 
  out << "\\\\\n\\hspace*{1em}(";
  print_quoted (out, name);
  out << "~\\textit{";
  print_quoted (out, name);
  out << "})";
}

void
DocumentLaTeX::print_sample_header (ostream& out, const string& name)
{ 
  out << "\n\\noindent\n\\begin{tt}\n\(def";
  print_quoted (out, current_component);
  out << "~\\textit{name}~";
  print_quoted (out, name);
}

void
DocumentLaTeX::print_sample_trailer (ostream& out, const string&)
{ 
  out << ")\n\\end{tt}\n";
}

void
DocumentLaTeX::print_submodel_header (ostream& out,
				      const string&, int level)
{ 
  if (level > 3)
    out << "\n\\begin{enumerate}\n";
  else
    out << "\n\\begin{itemize}\n";
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
  out << "\\\\\n";
}

void
DocumentLaTeX::print_model_trailer (ostream&, const string&)
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
DocumentLaTeX::print_document_header (ostream& out)
{ 
  out << "\
%%% components.tex --- Description of Daisy components.\n\
%%%\n\
%%% This file is automatically generated, do not edit.\n"; }

void
DocumentLaTeX::print_document_trailer (ostream& out)
{ 
  out << "\
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
      Librarian<Document>::add_type ("LaTeX", alist, syntax, &make);
    }
} DocumentLaTeX_syntax;

