/****************************************************************************
** EditEntry meta object code from reading C++ file 'qmain_edit.h'
**
** Created: Fri Oct 6 18:17:36 2000
**      by: The Qt MOC ($Id$)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#define Q_MOC_EditEntry
#if !defined(Q_MOC_OUTPUT_REVISION)
#define Q_MOC_OUTPUT_REVISION 9
#elif Q_MOC_OUTPUT_REVISION != 9
#error "Moc format conflict - please regenerate all moc files"
#endif

#include "qmain_edit.h"
#include <qmetaobject.h>
#include <qapplication.h>

#if defined(Q_SPARCWORKS_FUNCP_BUG)
#define Q_AMPERSAND
#else
#define Q_AMPERSAND &
#endif


const char *EditEntry::className() const
{
    return "EditEntry";
}

QMetaObject *EditEntry::metaObj = 0;

void EditEntry::initMetaObject()
{
    if ( metaObj )
	return;
    if ( qstrcmp(QHGroupBox::className(), "QHGroupBox") != 0 )
	badSuperclassWarning("EditEntry","QHGroupBox");
    (void) staticMetaObject();
}

#ifndef QT_NO_TRANSLATION

QString EditEntry::tr(const char* s)
{
    return qApp->translate( "EditEntry", s, 0 );
}

QString EditEntry::tr(const char* s, const char * c)
{
    return qApp->translate( "EditEntry", s, c );
}

#endif // QT_NO_TRANSLATION

QMetaObject* EditEntry::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    (void) QHGroupBox::staticMetaObject();
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    typedef void(EditEntry::*m1_t0)();
    m1_t0 v1_0 = Q_AMPERSAND EditEntry::change;
    QMetaData *slot_tbl = QMetaObject::new_metadata(1);
    QMetaData::Access *slot_tbl_access = QMetaObject::new_metaaccess(1);
    slot_tbl[0].name = "change()";
    slot_tbl[0].ptr = (QMember)v1_0;
    slot_tbl_access[0] = QMetaData::Public;
    metaObj = QMetaObject::new_metaobject(
	"EditEntry", "QHGroupBox",
	slot_tbl, 1,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    metaObj->set_slot_access( slot_tbl_access );
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    return metaObj;
}


const char *EditPLF::className() const
{
    return "EditPLF";
}

QMetaObject *EditPLF::metaObj = 0;

void EditPLF::initMetaObject()
{
    if ( metaObj )
	return;
    if ( qstrcmp(EditEntry::className(), "EditEntry") != 0 )
	badSuperclassWarning("EditPLF","EditEntry");
    (void) staticMetaObject();
}

#ifndef QT_NO_TRANSLATION

QString EditPLF::tr(const char* s)
{
    return qApp->translate( "EditPLF", s, 0 );
}

QString EditPLF::tr(const char* s, const char * c)
{
    return qApp->translate( "EditPLF", s, c );
}

#endif // QT_NO_TRANSLATION

QMetaObject* EditPLF::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    (void) EditEntry::staticMetaObject();
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    typedef void(EditPLF::*m1_t0)();
    typedef void(EditPLF::*m1_t1)();
    typedef void(EditPLF::*m1_t2)();
    m1_t0 v1_0 = Q_AMPERSAND EditPLF::add;
    m1_t1 v1_1 = Q_AMPERSAND EditPLF::remove;
    m1_t2 v1_2 = Q_AMPERSAND EditPLF::view;
    QMetaData *slot_tbl = QMetaObject::new_metadata(3);
    QMetaData::Access *slot_tbl_access = QMetaObject::new_metaaccess(3);
    slot_tbl[0].name = "add()";
    slot_tbl[0].ptr = (QMember)v1_0;
    slot_tbl_access[0] = QMetaData::Public;
    slot_tbl[1].name = "remove()";
    slot_tbl[1].ptr = (QMember)v1_1;
    slot_tbl_access[1] = QMetaData::Public;
    slot_tbl[2].name = "view()";
    slot_tbl[2].ptr = (QMember)v1_2;
    slot_tbl_access[2] = QMetaData::Public;
    metaObj = QMetaObject::new_metaobject(
	"EditPLF", "EditEntry",
	slot_tbl, 3,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    metaObj->set_slot_access( slot_tbl_access );
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    return metaObj;
}


const char *ItemDialog::className() const
{
    return "ItemDialog";
}

QMetaObject *ItemDialog::metaObj = 0;

void ItemDialog::initMetaObject()
{
    if ( metaObj )
	return;
    if ( qstrcmp(QDialog::className(), "QDialog") != 0 )
	badSuperclassWarning("ItemDialog","QDialog");
    (void) staticMetaObject();
}

#ifndef QT_NO_TRANSLATION

QString ItemDialog::tr(const char* s)
{
    return qApp->translate( "ItemDialog", s, 0 );
}

QString ItemDialog::tr(const char* s, const char * c)
{
    return qApp->translate( "ItemDialog", s, c );
}

#endif // QT_NO_TRANSLATION

QMetaObject* ItemDialog::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    (void) QDialog::staticMetaObject();
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    typedef void(ItemDialog::*m1_t0)();
    typedef void(ItemDialog::*m1_t1)();
    typedef void(ItemDialog::*m1_t2)();
    m1_t0 v1_0 = Q_AMPERSAND ItemDialog::apply;
    m1_t1 v1_1 = Q_AMPERSAND ItemDialog::reset;
    m1_t2 v1_2 = Q_AMPERSAND ItemDialog::cancel;
    QMetaData *slot_tbl = QMetaObject::new_metadata(3);
    QMetaData::Access *slot_tbl_access = QMetaObject::new_metaaccess(3);
    slot_tbl[0].name = "apply()";
    slot_tbl[0].ptr = (QMember)v1_0;
    slot_tbl_access[0] = QMetaData::Public;
    slot_tbl[1].name = "reset()";
    slot_tbl[1].ptr = (QMember)v1_1;
    slot_tbl_access[1] = QMetaData::Public;
    slot_tbl[2].name = "cancel()";
    slot_tbl[2].ptr = (QMember)v1_2;
    slot_tbl_access[2] = QMetaData::Public;
    metaObj = QMetaObject::new_metaobject(
	"ItemDialog", "QDialog",
	slot_tbl, 3,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    metaObj->set_slot_access( slot_tbl_access );
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    return metaObj;
}
