/****************************************************************************
** EditEntry meta object code from reading C++ file 'qmain_edit.h'
**
** Created: Fri May 3 12:09:47 2002
**      by: The Qt MOC ($Id$)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#undef QT_NO_COMPAT
#include "qmain_edit.h"
#include <qmetaobject.h>
#include <qapplication.h>

#include <private/qucomextra_p.h>
#if !defined(Q_MOC_OUTPUT_REVISION) || (Q_MOC_OUTPUT_REVISION != 19)
#error "This file was generated using the moc from 3.0.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

const char *EditEntry::className() const
{
    return "EditEntry";
}

QMetaObject *EditEntry::metaObj = 0;
static QMetaObjectCleanUp cleanUp_EditEntry;

#ifndef QT_NO_TRANSLATION
QString EditEntry::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditEntry", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString EditEntry::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditEntry", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* EditEntry::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QHGroupBox::staticMetaObject();
    static const QUMethod slot_0 = {"change", 0, 0 };
    static const QMetaData slot_tbl[] = {
	{ "change()", &slot_0, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"EditEntry", parentObject,
	slot_tbl, 1,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_EditEntry.setMetaObject( metaObj );
    return metaObj;
}

void* EditEntry::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "EditEntry" ) ) return (EditEntry*)this;
    return QHGroupBox::qt_cast( clname );
}

bool EditEntry::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: change(); break;
    default:
	return QHGroupBox::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool EditEntry::qt_emit( int _id, QUObject* _o )
{
    return QHGroupBox::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool EditEntry::qt_property( int _id, int _f, QVariant* _v)
{
    return QHGroupBox::qt_property( _id, _f, _v);
}
#endif // QT_NO_PROPERTIES


const char *EditBoolean::className() const
{
    return "EditBoolean";
}

QMetaObject *EditBoolean::metaObj = 0;
static QMetaObjectCleanUp cleanUp_EditBoolean;

#ifndef QT_NO_TRANSLATION
QString EditBoolean::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditBoolean", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString EditBoolean::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditBoolean", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* EditBoolean::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = EditEntry::staticMetaObject();
    static const QUMethod slot_0 = {"invert", 0, 0 };
    static const QMetaData slot_tbl[] = {
	{ "invert()", &slot_0, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"EditBoolean", parentObject,
	slot_tbl, 1,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_EditBoolean.setMetaObject( metaObj );
    return metaObj;
}

void* EditBoolean::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "EditBoolean" ) ) return (EditBoolean*)this;
    return EditEntry::qt_cast( clname );
}

bool EditBoolean::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: invert(); break;
    default:
	return EditEntry::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool EditBoolean::qt_emit( int _id, QUObject* _o )
{
    return EditEntry::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool EditBoolean::qt_property( int _id, int _f, QVariant* _v)
{
    return EditEntry::qt_property( _id, _f, _v);
}
#endif // QT_NO_PROPERTIES


const char *EditPLF::className() const
{
    return "EditPLF";
}

QMetaObject *EditPLF::metaObj = 0;
static QMetaObjectCleanUp cleanUp_EditPLF;

#ifndef QT_NO_TRANSLATION
QString EditPLF::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditPLF", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString EditPLF::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditPLF", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* EditPLF::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = EditEntry::staticMetaObject();
    static const QUMethod slot_0 = {"add", 0, 0 };
    static const QUMethod slot_1 = {"remove", 0, 0 };
    static const QUMethod slot_2 = {"view", 0, 0 };
    static const QMetaData slot_tbl[] = {
	{ "add()", &slot_0, QMetaData::Public },
	{ "remove()", &slot_1, QMetaData::Public },
	{ "view()", &slot_2, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"EditPLF", parentObject,
	slot_tbl, 3,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_EditPLF.setMetaObject( metaObj );
    return metaObj;
}

void* EditPLF::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "EditPLF" ) ) return (EditPLF*)this;
    return EditEntry::qt_cast( clname );
}

bool EditPLF::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: add(); break;
    case 1: remove(); break;
    case 2: view(); break;
    default:
	return EditEntry::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool EditPLF::qt_emit( int _id, QUObject* _o )
{
    return EditEntry::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool EditPLF::qt_property( int _id, int _f, QVariant* _v)
{
    return EditEntry::qt_property( _id, _f, _v);
}
#endif // QT_NO_PROPERTIES


const char *EditObject::className() const
{
    return "EditObject";
}

QMetaObject *EditObject::metaObj = 0;
static QMetaObjectCleanUp cleanUp_EditObject;

#ifndef QT_NO_TRANSLATION
QString EditObject::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditObject", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString EditObject::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditObject", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* EditObject::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = EditEntry::staticMetaObject();
    static const QUParameter param_slot_0[] = {
	{ 0, &static_QUType_int, 0, QUParameter::In }
    };
    static const QUMethod slot_0 = {"activate", 1, param_slot_0 };
    static const QMetaData slot_tbl[] = {
	{ "activate(int)", &slot_0, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"EditObject", parentObject,
	slot_tbl, 1,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_EditObject.setMetaObject( metaObj );
    return metaObj;
}

void* EditObject::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "EditObject" ) ) return (EditObject*)this;
    return EditEntry::qt_cast( clname );
}

bool EditObject::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: activate(static_QUType_int.get(_o+1)); break;
    default:
	return EditEntry::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool EditObject::qt_emit( int _id, QUObject* _o )
{
    return EditEntry::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool EditObject::qt_property( int _id, int _f, QVariant* _v)
{
    return EditEntry::qt_property( _id, _f, _v);
}
#endif // QT_NO_PROPERTIES


const char *EditList::className() const
{
    return "EditList";
}

QMetaObject *EditList::metaObj = 0;
static QMetaObjectCleanUp cleanUp_EditList;

#ifndef QT_NO_TRANSLATION
QString EditList::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditList", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString EditList::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "EditList", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* EditList::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = EditEntry::staticMetaObject();
    static const QUMethod slot_0 = {"before", 0, 0 };
    static const QUMethod slot_1 = {"at", 0, 0 };
    static const QUMethod slot_2 = {"after", 0, 0 };
    static const QUMethod slot_3 = {"remove", 0, 0 };
    static const QUParameter param_slot_4[] = {
	{ 0, &static_QUType_ptr, "QListViewItem", QUParameter::In }
    };
    static const QUMethod slot_4 = {"select", 1, param_slot_4 };
    static const QMetaData slot_tbl[] = {
	{ "before()", &slot_0, QMetaData::Public },
	{ "at()", &slot_1, QMetaData::Public },
	{ "after()", &slot_2, QMetaData::Public },
	{ "remove()", &slot_3, QMetaData::Public },
	{ "select(QListViewItem*)", &slot_4, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"EditList", parentObject,
	slot_tbl, 5,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_EditList.setMetaObject( metaObj );
    return metaObj;
}

void* EditList::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "EditList" ) ) return (EditList*)this;
    return EditEntry::qt_cast( clname );
}

bool EditList::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: before(); break;
    case 1: at(); break;
    case 2: after(); break;
    case 3: remove(); break;
    case 4: select((QListViewItem*)static_QUType_ptr.get(_o+1)); break;
    default:
	return EditEntry::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool EditList::qt_emit( int _id, QUObject* _o )
{
    return EditEntry::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool EditList::qt_property( int _id, int _f, QVariant* _v)
{
    return EditEntry::qt_property( _id, _f, _v);
}
#endif // QT_NO_PROPERTIES


const char *ItemDialog::className() const
{
    return "ItemDialog";
}

QMetaObject *ItemDialog::metaObj = 0;
static QMetaObjectCleanUp cleanUp_ItemDialog;

#ifndef QT_NO_TRANSLATION
QString ItemDialog::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "ItemDialog", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString ItemDialog::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "ItemDialog", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* ItemDialog::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QDialog::staticMetaObject();
    static const QUMethod slot_0 = {"apply", 0, 0 };
    static const QUMethod slot_1 = {"reset", 0, 0 };
    static const QUMethod slot_2 = {"cancel", 0, 0 };
    static const QMetaData slot_tbl[] = {
	{ "apply()", &slot_0, QMetaData::Public },
	{ "reset()", &slot_1, QMetaData::Public },
	{ "cancel()", &slot_2, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"ItemDialog", parentObject,
	slot_tbl, 3,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_ItemDialog.setMetaObject( metaObj );
    return metaObj;
}

void* ItemDialog::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "ItemDialog" ) ) return (ItemDialog*)this;
    return QDialog::qt_cast( clname );
}

bool ItemDialog::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: apply(); break;
    case 1: reset(); break;
    case 2: cancel(); break;
    default:
	return QDialog::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool ItemDialog::qt_emit( int _id, QUObject* _o )
{
    return QDialog::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool ItemDialog::qt_property( int _id, int _f, QVariant* _v)
{
    return QDialog::qt_property( _id, _f, _v);
}
#endif // QT_NO_PROPERTIES
