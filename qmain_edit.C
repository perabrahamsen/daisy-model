// qmain_edit.C -- Edit alist items in a dialog window.

#include "qmain_edit.h"

#include "alist.h"
#include "syntax.h"

#include <qdialog.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qhgroupbox.h>
#include <qvalidator.h>
#include <qhbox.h>
#include <qpushbutton.h>
#include <qmessagebox.h>

class EditEntry : public QHGroupBox
{ 
  // Content.
protected:
  const Syntax& syntax;
  AttributeList& alist;
  const AttributeList& default_alist;
  const string& parameter;

  // Use.
public:
  virtual void reset () = 0;
  virtual bool apply () = 0;
  virtual QString value () = 0;

  // Create and Destroy.
public:
  EditEntry (QWidget* parent, 
	     const Syntax& syn, AttributeList& al, 
	     const AttributeList& def_al, 
	     const string& par)
  : QHGroupBox (par.c_str (), parent),
    syntax (syn),
    alist (al),
    default_alist (def_al),
    parameter (par)
  { }
};

class EditNumber : public EditEntry
{ 
  // Children.
  QLineEdit* edit;

  // Use.
  void reset ()
  {
    double value = 0;
    if (alist.check (parameter))
      value = alist.number (parameter);
    else if (default_alist.check (parameter))
      value = default_alist.number (parameter);

    edit->setText (QString::number (value));
  }    
  bool apply ()
  { 
    alist.add (parameter, edit->text ().toDouble ()); 
    return true;
  }
  QString value ()
  { return edit->text (); }
  // Create and Destroy.
public:
  EditNumber (QWidget* parent, 
	      const Syntax& syn, AttributeList& al, 
	      const AttributeList& def_al, 
	      const string& par)
    : EditEntry (parent, syn, al, def_al, par),
      edit (new QLineEdit (this))
  {
    reset ();
    edit->setValidator (new QDoubleValidator (this));
  };
};

class EditOther : public EditEntry
{ 
  // Use.
  void reset ()
  { }
  bool apply ()
  { 
    QMessageBox::warning (this, "QDaisy: Don't push it", 
			  "You cannot set the value of this item.");
    return false; 
  }
  QString value ()
  { return "<unknown value>"; }

  // Create and Destroy.
public:
  EditOther (QWidget* parent, 
	     const Syntax& syn, AttributeList& al, 
	     const AttributeList& def_al, 
	     const string& par)
    : EditEntry (parent, syn, al, def_al, par)
  {
    const Syntax::type type = syntax.lookup (parameter);
    const int size = syntax.size (parameter);

    QString excuse = "I can't handle ";
    excuse += Syntax::type_name (type);
    excuse += " ";
    if (size == Syntax::Singleton)
      excuse += "singletons";
    else if (size == Syntax::Sequence)
      excuse += "sequences";
    else
      excuse += "arrays";
    excuse += " yet!";
    new QLabel (excuse, this);
  };
};

QString
ItemDialog::value ()
{ return entry->value (); }

ItemDialog::ItemDialog (QWidget* parent, 
			const Syntax& syntax, AttributeList& alist, 
			const AttributeList& default_alist, 
			const string& parameter)
  : QDialog (parent, 0, true)
{ 
  const Syntax::type type = syntax.lookup (parameter);
  const int size = syntax.size (parameter);

  setCaption (QString ("QDaisy: Edit ") + parameter.c_str ());
  QVBoxLayout* layout = new QVBoxLayout (this);
  layout->setAutoAdd (true);

  if (type == Syntax::Number && size == Syntax::Singleton)
    entry = new EditNumber (this, syntax, alist, default_alist, parameter);
  else
    entry = new EditOther (this, syntax, alist, default_alist, parameter);

  QHBox* buttons = new QHBox (this);
  QPushButton* apply = new QPushButton ("Apply", buttons);
  connect (apply, SIGNAL (clicked ()), this, SLOT (apply ()));
  QPushButton* reset = new QPushButton ("Reset", buttons);
  connect (reset, SIGNAL (clicked ()), this, SLOT (reset ()));
  QPushButton* cancel = new QPushButton ("Cancel", buttons);
  connect (cancel, SIGNAL (clicked ()), this, SLOT (cancel ()));
}

ItemDialog::~ItemDialog ()
{ };

void 
ItemDialog::apply ()
{ 
  if (entry->apply ())
    accept (); 
}

void 
ItemDialog::reset ()
{ entry->reset (); }

void 
ItemDialog::cancel ()
{ reject (); }
