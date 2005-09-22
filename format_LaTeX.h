#include "format.h"

struct FormatLaTeX : public Format
{
  // Nesting.
  int list_level;
  void list_open ();
  void list_close ();
  void item_open (const std::string& name);
  void item_close ();
  void table_open (const std::string& format);
  void table_close ();
  std::stack<bool> table_first_row;
  void table_row_open ();
  void table_row_close ();
  std::stack<bool> table_first_column;
  void table_cell_open ();
  void table_cell_close ();
  void table_multi_cell_open (const int cells, const std::string& format);
  void table_multi_cell_close ();
  void typewriter_open ();
  void typewriter_close ();
  void section_open (const std::string& type, const std::string& title,
		     const std::string& scope, 
		     const std::string& label);
  void section_close ();
  void document_open ();
  void document_close ();

  // Use.
  void text (const std::string& text);
  void bold (const std::string& text);
  void italic (const std::string& text);
  void verbatim (const std::string& text);
  void raw (const std::string& format, const std::string& text);
  bool formatp (const std::string& format);
  void special (const std::string& name);
  void soft_linebreak ();
  void hard_linebreak ();
  void new_paragraph ();
  void index (const std::string& name);
  void label (const std::string& scope, const std::string& id);
  void pageref (const std::string& scope, const std::string& id);
  void ref (const std::string& scope, const std::string& id);
  void see (const std::string& type,
	    const std::string& scope, const std::string& id);
  void see_page (const std::string& scope, const std::string& id);

  // Create and Destroy.
  explicit FormatLaTeX (const AttributeList& al);
};

