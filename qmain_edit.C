// qmain_edit.C -- Edit alist items in a dialog window.

#include "qmain_edit.h"

#include "alist.h"
#include "syntax.h"
#include "library.h"
#include "plf.h"
#include "time.h"

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
#include <qapplication.h>
#include <qcombobox.h>

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


struct ShortLine : public QLineEdit
{
  int size;

  ShortLine (QWidget* main, int sz)
    : QLineEdit (main),
      size (sz)
  { }

  /* Stolen from qlinedit.h */
  QSize sizeHint() const
  {
    constPolish();
    QFontMetrics fm( font() );
    int h = fm.height();
    int w = fm.width( 'x' ) * size; // "some"
    if ( frame() ) {
      h += 8;
      if ( style() == WindowsStyle && h < 26 )
	h = 22;
      return QSize( w + 8, h ).expandedTo( QApplication::globalStrut() );
    } else {
      return QSize( w + 4, h + 4 ).expandedTo( QApplication::globalStrut() );
    }
  }
};

class EditDate : public EditEntry
{ 
  // Children.
  QLineEdit* year;
  QLineEdit* month;
  QLineEdit* mday;
  QLineEdit* hour;

  // Use.
  void reset ()
  {
    Time value (1, 1, 1, 0);
    if (alist.check (parameter))
      value = alist.time (parameter);
    else if (default_alist.check (parameter))
      value = default_alist.time (parameter);

    year->setText (QString::number (value.year ()));
    month->setText (QString::number (value.month ()));
    mday->setText (QString::number (value.mday ()));
    hour->setText (QString::number (value.hour ()));
  }    
  void apply ()
  { 
    if (check->isChecked ())
      {
	const int yy = year->text ().toInt ();
	const int mm = month->text ().toInt ();
	const int dd = mday->text ().toInt ();
	const int hh = hour->text ().toInt ();
	Time value (yy, mm, dd, hh);
	alist.add (parameter, value); 
      }
    else if (default_alist.check (parameter))
      alist.add (parameter, default_alist.time (parameter));
    else
      alist.remove (parameter);
  }
  bool valid ()
  { 
    // Check content.
    QString value = year->text ();
    int pos = 0;
    if (year->validator ()->validate (value, pos) != QValidator::Acceptable)
      return false;
    value = month->text ();
    if (month->validator ()->validate (value, pos) != QValidator::Acceptable)
      return false;
    value = mday->text ();
    if (mday->validator ()->validate (value, pos) != QValidator::Acceptable)
      return false;
    value = hour->text ();
    if (hour->validator ()->validate (value, pos) != QValidator::Acceptable)
      return false;
    
    // Check date.
    const int yy = year->text ().toInt ();
    const int mm = month->text ().toInt ();
    const int dd = mday->text ().toInt ();
    const int hh = hour->text ().toInt ();
    if (!Time::valid (yy, mm, dd, hh))
      return false;

    // Ok.
    return true;
  }
  void change ()
  { 
    year->setEnabled (check->isChecked ());
    month->setEnabled (check->isChecked ());
    mday->setEnabled (check->isChecked ());
    hour->setEnabled (check->isChecked ());

    if (check->isChecked ())
      return;

    Time value (1, 1, 1, 0);
    if (default_alist.check (parameter))
      value = default_alist.time (parameter);

    year->setText (QString::number (value.year ()));
    month->setText (QString::number (value.month ()));
    mday->setText (QString::number (value.mday ()));
    hour->setText (QString::number (value.hour ()));
  }


  // Create and Destroy.
public:
  EditDate (QWidget* parent, 
	    const Syntax& syn, AttributeList& al, 
	    const AttributeList& def_al, 
	    const string& par)
    : EditEntry (parent, syn, al, def_al, par)
  {
    year = new ShortLine (this, 5);
    year->setMaxLength (4);
    new QLabel ("-", this);
    month = new ShortLine (this, 2);
    month->setMaxLength (2);
    new QLabel ("-", this);
    mday = new ShortLine (this, 2);
    mday->setMaxLength (2);
    new QLabel ("T", this);
    hour = new ShortLine (this, 2);
    hour->setMaxLength (2);

    reset ();
    year->setValidator (new QIntValidator (1, 9999, this));
    month->setValidator (new QIntValidator (1, 12, this));
    mday->setValidator (new QIntValidator (1, 31, this));
    hour->setValidator (new QIntValidator (0, 23, this));
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

void
EditBoolean::refresh ()
{
  if (value)
    state->setText ("true");
  else
    state->setText ("false");
}    

void
EditBoolean::reset ()
{
  if (alist.check (parameter))
    value = alist.flag (parameter);
  else if (default_alist.check (parameter))
    value = default_alist.flag (parameter);

  refresh ();
}    

void 
EditBoolean::apply ()
{ 
  if (check->isChecked ())
    alist.add (parameter, value); 
  else if (default_alist.check (parameter))
    alist.add (parameter, default_alist.flag (parameter));
  else
    alist.remove (parameter);
}

bool
EditBoolean::valid ()
{ return true; }

void 
EditBoolean::change ()
{ 
  toggle->setEnabled (check->isChecked ());
  state->setEnabled (check->isChecked ());

  if (check->isChecked ())
    return;

  if (default_alist.check (parameter))
    value = default_alist.flag (parameter);

  refresh ();
}

EditBoolean::EditBoolean (QWidget* parent, 
			  const Syntax& syn, AttributeList& al, 
			  const AttributeList& def_al, 
			  const string& par)
  : EditEntry (parent, syn, al, def_al, par),
    value (false)
{
  toggle = new QPushButton ("Toggle", this);
  state = new QLabel ("maybe", this);
  connect (toggle, SIGNAL (clicked ()), this, SLOT (invert ()));

  reset ();
  change ();
};

void
EditBoolean::invert ()
{ 
  value = !value;
  refresh ();
}

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
  table->addColumn (syntax.domain (parameter).c_str ());
  table->addColumn (syntax.range (parameter).c_str ());
  if (table->columnWidth (0) < 40)
    table->setColumnWidth (0, 40);
  if (table->columnWidth (1) < 40)
    table->setColumnWidth (1, 40);
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

void
EditObject::refresh ()
{
  if (value.check ("type"))
    {
      const QString type = value.name ("type").c_str ();
      
      bool found = false;
      for (unsigned int i = 0; i < choice->count (); i++)
	if (type == choice->text (i))
	  {
	    assert (!found);
	    choice->setCurrentItem (i);
	    found = true;
	  }
      assert (found);
    }
  else
    activate (0);
}    

void
EditObject::reset ()
{
  if (alist.check (parameter))
    value = alist.alist (parameter);
  else if (default_alist.check (parameter))
    value = default_alist.alist (parameter);

  refresh ();
}    

void 
EditObject::apply ()
{ 
  if (check->isChecked ())
    alist.add (parameter, value); 
  else if (default_alist.check (parameter))
    alist.add (parameter, default_alist.alist (parameter));
  else
    alist.remove (parameter);
}

bool
EditObject::valid ()
{ return value.check ("type"); }

void 
EditObject::change ()
{ 
  button->setEnabled (check->isChecked ());
  choice->setEnabled (check->isChecked ());

  if (check->isChecked ())
    return;

  if (default_alist.check (parameter))
    {
      value = default_alist.alist (parameter);
      refresh ();
    }
}

EditObject::EditObject (QWidget* parent, 
			const Syntax& syn, AttributeList& al, 
			const AttributeList& def_al, 
			const string& par)
  : EditEntry (parent, syn, al, def_al, par)
{
  button = new QPushButton ("Edit", this);
  choice = new QComboBox (false, this);
  connect (button, SIGNAL (clicked ()), this, SLOT (edit ()));
  connect (choice, SIGNAL (activated (int)), this, SLOT (activate (int)));

  const Library& library = syntax.library (parameter);
  vector<string> models;
  library.entries (models);
  assert (models.size () > 0);
  for (unsigned int i = 0; i < models.size (); i++)
    choice->insertItem (models[i].c_str ());

  reset ();
  change ();
};

void
EditObject::edit ()
{ }

void
EditObject::activate (int index)
{ 
  const string type = choice->text (index).latin1 ();
  const Library& library = syntax.library (parameter);
  value = library.lookup (type);
  value.add ("type", type);
}

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
      case Syntax::Date:
	entry = new EditDate (this, syntax, alist, default_alist, parameter);
	break;
      case Syntax::String:
	entry = new EditName (this, syntax, alist, default_alist, parameter);
	break;
      case Syntax::PLF:
	entry = new EditPLF (this, syntax, alist, default_alist, parameter);
	break;
      case Syntax::Boolean:
	entry 
	  = new EditBoolean (this, syntax, alist, default_alist, parameter);
	break;
      case Syntax::Object:
	entry = new EditObject (this, syntax, alist, default_alist, parameter);
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
