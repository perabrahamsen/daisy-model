// qmain_edit.C -- Edit alist items in a dialog window.

#include "qmain_edit.h"

#include "alist.h"
#include "syntax.h"
#include "plf.h"

#include <qdialog.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qlineedit.h>
#include <qmultilineedit.h>
#include <qvalidator.h>
#include <qhbox.h>
#include <qvbox.h>
#include <qpushbutton.h>
#include <qmessagebox.h>
#include <qcheckbox.h>
#include <qtooltip.h>
#include <qlistview.h>

EditEntry::EditEntry (QWidget* parent, 
		      const Syntax& syn, AttributeList& al, 
		      const AttributeList& def_al, 
		      const string& par)
  : QHGroupBox (par.c_str (), parent),
    syntax (syn),
    alist (al),
    default_alist (def_al),
    parameter (par)
{ 
  QLabel* q = new QLabel ("?", this);
  QToolTip::add (q, syntax.description (parameter).c_str ());
  check = new QCheckBox (this);
  QToolTip::add (check, QString ("Check this to give ") + parameter.c_str ()
		 + " a non-default value.");
  check->setChecked (!alist.subset (default_alist, syntax, parameter));
  connect (check, SIGNAL (clicked ()), this, SLOT (change ()));
}

EditEntry::~EditEntry ()
{ }

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
  void apply ()
  { 
    if (check->isChecked ())
      alist.add (parameter, edit->text ().toDouble ()); 
    else if (default_alist.check (parameter))
      alist.add (parameter, default_alist.number (parameter));
    else
      alist.remove (parameter);
  }
  bool valid ()
  { 
    QString value = edit->text ();
    int pos = 0;
    return edit->validator ()->validate (value, pos) == QValidator::Acceptable;
  }
  void change ()
  { 
    edit->setEnabled (check->isChecked ());

    if (check->isChecked ())
      return;

    double value = 0;
    if (default_alist.check (parameter))
      value = default_alist.number (parameter);

    edit->setText (QString::number (value));
  }


  // Create and Destroy.
public:
  EditNumber (QWidget* parent, 
	      const Syntax& syn, AttributeList& al, 
	      const AttributeList& def_al, 
	      const string& par)
    : EditEntry (parent, syn, al, def_al, par),
      edit (new QLineEdit (this))
  {
    new QLabel (syntax.dimension (parameter).c_str (), this);
    reset ();
    edit->setValidator (new QDoubleValidator (this));
    change ();
  };
};

class EditInteger : public EditEntry
{ 
  // Children.
  QLineEdit* edit;

  // Use.
  void reset ()
  {
    double value = 0;
    if (alist.check (parameter))
      value = alist.integer (parameter);
    else if (default_alist.check (parameter))
      value = default_alist.integer (parameter);

    edit->setText (QString::number (value));
  }    
  void apply ()
  { 
    if (check->isChecked ())
      alist.add (parameter, edit->text ().toInt ()); 
    else if (default_alist.check (parameter))
      alist.add (parameter, default_alist.integer (parameter));
    else
      alist.remove (parameter);
  }
  bool valid ()
  { 
    QString value = edit->text ();
    int pos = 0;
    return edit->validator ()->validate (value, pos) == QValidator::Acceptable;
  }
  void change ()
  { 
    edit->setEnabled (check->isChecked ());

    if (check->isChecked ())
      return;

    double value = 0;
    if (default_alist.check (parameter))
      value = default_alist.integer (parameter);

    edit->setText (QString::number (value));
  }


  // Create and Destroy.
public:
  EditInteger (QWidget* parent, 
	       const Syntax& syn, AttributeList& al, 
	       const AttributeList& def_al, 
	       const string& par)
    : EditEntry (parent, syn, al, def_al, par),
      edit (new QLineEdit (this))
  {
    reset ();
    edit->setValidator (new QIntValidator (this));
    change ();
  };
};


class EditName : public EditEntry
{ 
  // Children.
  QMultiLineEdit* edit;

  // Use.
  void reset ()
  {
    string value;
    if (alist.check (parameter))
      value = alist.name (parameter);
    else if (default_alist.check (parameter))
      value = default_alist.name (parameter);

    edit->setText (value.c_str ());
  }    
  void apply ()
  { 
    if (check->isChecked ())
      alist.add (parameter, edit->text ().latin1 ()); 
    else if (default_alist.check (parameter))
      alist.add (parameter, default_alist.name (parameter));
    else
      alist.remove (parameter);
  }
  bool valid ()
  { return true; }
  void change ()
  { 
    edit->setEnabled (check->isChecked ());

    if (check->isChecked ())
      return;

    string value;
    if (default_alist.check (parameter))
      value = default_alist.name (parameter);

    edit->setText (value.c_str ());
  }

  // Create and Destroy.
public:
  EditName (QWidget* parent, 
	    const Syntax& syn, AttributeList& al, 
	    const AttributeList& def_al, 
	    const string& par)
    : EditEntry (parent, syn, al, def_al, par),
      edit (new QMultiLineEdit (this))
  {
    reset ();
    change ();
  };
};

struct PLFItem : public QListViewItem
{
  // Content.
  int order;
  double x;
  double y;

  // Sort.
  QString key (int, bool) const
  { 
    QString tmp;
    tmp.sprintf ("%8d", order);
    return tmp;
  }

  // Create.
  PLFItem (QListView* main, double vx, double vy, int o)
    : QListViewItem (main, QString::number (vx), QString::number (vy)),
      order (o),
      x (vx),
      y (vy)
  { }
};

void
EditPLF::refresh (const PLF& value)
{
  table->clear ();
  for (int i = 0; i < value.size (); i++)
    new PLFItem (table, value.x (i), value.y (i), i);
}

void
EditPLF::reset ()
{
  PLF value;
  if (alist.check (parameter))
    value = alist.plf (parameter);
  else if (default_alist.check (parameter))
    value = default_alist.plf (parameter);

  refresh (value);
}    

void 
EditPLF::apply ()
{ 
  if (check->isChecked ())
    {
      PLF value;
      for (QListViewItem* i = table->firstChild (); i; i = i->nextSibling ())
	{
	  PLFItem* ii = dynamic_cast<PLFItem*> (i);
	  assert (ii);
	  value.add (ii->x, ii->y);
	}
      alist.add (parameter, value); 
    }
  else if (default_alist.check (parameter))
    alist.add (parameter, default_alist.plf (parameter));
  else
    alist.remove (parameter);
}

bool
EditPLF::valid ()
{ return true; }

void 
EditPLF::change ()
{ 
  vbox->setEnabled (check->isChecked ());

  if (check->isChecked ())
    return;

  PLF value;
  if (default_alist.check (parameter))
    value = default_alist.plf (parameter);

  refresh (value);
}

EditPLF::EditPLF (QWidget* parent, 
		  const Syntax& syn, AttributeList& al, 
		  const AttributeList& def_al, 
		  const string& par)
  : EditEntry (parent, syn, al, def_al, par)
{
  vbox = new QVBox (this);
  QHBox* xy = new QHBox (vbox);
  new QLabel ("X:", xy);
  x = new QLineEdit (xy);
  x->setValidator (new QDoubleValidator (this));
  new QLabel ("Y:", xy);
  y = new QLineEdit (xy);
  y->setValidator (new QDoubleValidator (this));
  QHBox* buttons = new QHBox (vbox);
  QPushButton* add = new QPushButton ("Add", buttons);
  connect (add, SIGNAL (clicked ()), this, SLOT (add ()));
  QPushButton* del = new QPushButton ("Delete", buttons);
  connect (del, SIGNAL (clicked ()), this, SLOT (remove ()));
  QPushButton* view = new QPushButton ("View", buttons);
  connect (view, SIGNAL (clicked ()), this, SLOT (view ()));
  table = new QListView (vbox);
  table->addColumn (QString ("X [") + syntax.domain (parameter).c_str () 
		    + "]");
  table->addColumn (QString ("Y [") + syntax.range (parameter).c_str () 
		    + "]");
  reset ();
  change ();
};

void
EditPLF::add ()
{ 
  QString x_value = x->text ();
  QString y_value = y->text ();
  int x_pos = 0;
  int y_pos = 0;
  const bool valid = ((x->validator ()->validate (x_value, x_pos) 
		       == QValidator::Acceptable)
		      && (y->validator ()->validate (y_value, y_pos) 
			  == QValidator::Acceptable));
  if (!valid)
    { 
      QMessageBox::warning (this, "QDaisy: Add", "Invalid value");
      return;
    }
  const double xx = x->text ().toDouble ();
  const double yy = y->text ().toDouble ();
  
  PLF value;
  bool found = false;
  for (QListViewItem* i = table->firstChild (); i; i = i->nextSibling ())
    {
      PLFItem* ii = dynamic_cast<PLFItem*> (i);
      assert (ii);
      if (found)
	{
	  assert (ii->x > xx);
	  value.add (ii->x, ii->y);
	}
      else if (ii->x > xx)
	{
	  value.add (xx, yy);
	  value.add (ii->x, ii->y);
	  found = true;
	}
      else if (ii->x == xx)
	{
	  value.add (xx, yy);
	  found = true;
	}
      else
	{
	  assert (ii->x < xx);
	  value.add (ii->x, ii->y);
	}
    }
  if (!found)
    value.add (xx, yy);
  
  refresh (value);
}

void
EditPLF::remove ()
{
  QListViewItem* selected = table->selectedItem ();
  if (selected)
    delete selected;
  else
    QMessageBox::warning (this, "QDaisy: remove", "No selection.");
}

void
EditPLF::view ()
{ QMessageBox::warning (this, "QDaisy: view", "Not yet implemented."); }


class EditOther : public EditEntry
{ 
  QLabel* edit;

  // Use.
  void reset ()
  { }
  void apply ()
  { 
    if (check->isChecked ())
      { }
    else if (default_alist.check (parameter))
      alist.add (parameter, default_alist.plf (parameter));
    else
      alist.remove (parameter);
  }
  bool valid ()
  { return true; }
  void change ()
  { edit->setEnabled (check->isChecked ()); }

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
    edit = new QLabel (excuse, this);
    change ();
  };
};

ItemDialog::ItemDialog (QWidget* parent, 
			const Syntax& syntax, AttributeList& alist, 
			const AttributeList& default_alist, 
			const string& parameter)
  : QDialog (parent, 0, true)
{ 
  assert (&alist != &default_alist);
  const Syntax::type type = syntax.lookup (parameter);
  const int size = syntax.size (parameter);

  setCaption (QString ("QDaisy: Edit ") + parameter.c_str ());
  QVBoxLayout* layout = new QVBoxLayout (this);
  layout->setAutoAdd (true);

  if (size == Syntax::Singleton)
    switch (type)
      {
      case Syntax::Number:
	entry = new EditNumber (this, syntax, alist, default_alist, parameter);
	break;
      case Syntax::Integer:
	entry 
	  = new EditInteger (this, syntax, alist, default_alist, parameter);
	break;
      case Syntax::String:
	entry = new EditName (this, syntax, alist, default_alist, parameter);
	break;
      case Syntax::PLF:
	entry = new EditPLF (this, syntax, alist, default_alist, parameter);
	break;
      default:
	entry = new EditOther (this, syntax, alist, default_alist, parameter);
      }
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
  if (!entry->valid ())
    QMessageBox::warning (this, "QDaisy: Apply", 
			  "Invalid value.");
  else
    {
      entry->apply ();
      accept (); 
    }
}

void 
ItemDialog::reset ()
{ entry->reset (); }

void 
ItemDialog::cancel ()
{ reject (); }
