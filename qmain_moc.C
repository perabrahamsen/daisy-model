/****************************************************************************
** MainWindow meta object code from reading C++ file 'qmain.h'
**
** Created: Fri May 3 11:24:33 2002
**      by: The Qt MOC ($Id$)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#undef QT_NO_COMPAT
#include "qmain.h"
#include <qmetaobject.h>
#include <qapplication.h>

#include <private/qucomextra_p.h>
#if !defined(Q_MOC_OUTPUT_REVISION) || (Q_MOC_OUTPUT_REVISION != 19)
#error "This file was generated using the moc from 3.0.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

const char *MainWindow::className() const
{
    return "MainWindow";
}

QMetaObject *MainWindow::metaObj = 0;
static QMetaObjectCleanUp cleanUp_MainWindow;

#ifndef QT_NO_TRANSLATION
QString MainWindow::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "MainWindow", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString MainWindow::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "MainWindow", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* MainWindow::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QMainWindow::staticMetaObject();
    static const QUMethod slot_0 = {"menu_action", 0, 0 };
    static const QUMethod slot_1 = {"file_new", 0, 0 };
    static const QUMethod slot_2 = {"file_open", 0, 0 };
    static const QUMethod slot_3 = {"file_save", 0, 0 };
    static const QUMethod slot_4 = {"file_save_as", 0, 0 };
    static const QUMethod slot_5 = {"edit_edit", 0, 0 };
    static const QUMethod slot_6 = {"edit_raw", 0, 0 };
    static const QUMethod slot_7 = {"edit_after", 0, 0 };
    static const QUMethod slot_8 = {"edit_child", 0, 0 };
    static const QUMethod slot_9 = {"edit_copy", 0, 0 };
    static const QUMethod slot_10 = {"edit_inherit", 0, 0 };
    static const QUMethod slot_11 = {"edit_delete", 0, 0 };
    static const QUMethod slot_12 = {"view_selected", 0, 0 };
    static const QUMethod slot_13 = {"view_check", 0, 0 };
    static const QUMethod slot_14 = {"toggle_view_defaults", 0, 0 };
    static const QUMethod slot_15 = {"view_dependencies", 0, 0 };
    static const QUMethod slot_16 = {"toggle_view_logonly", 0, 0 };
    static const QUMethod slot_17 = {"toggle_view_parameters", 0, 0 };
    static const QUMethod slot_18 = {"toggle_view_empty", 0, 0 };
    static const QUMethod slot_19 = {"toggle_check_composite", 0, 0 };
    static const QUParameter param_slot_20[] = {
	{ "i", &static_QUType_ptr, "unsigned int", QUParameter::In }
    };
    static const QUMethod slot_20 = {"select_view_filter", 1, param_slot_20 };
    static const QUMethod slot_21 = {"help_about", 0, 0 };
    static const QUMethod slot_22 = {"help_aboutQt", 0, 0 };
    static const QMetaData slot_tbl[] = {
	{ "menu_action()", &slot_0, QMetaData::Public },
	{ "file_new()", &slot_1, QMetaData::Public },
	{ "file_open()", &slot_2, QMetaData::Public },
	{ "file_save()", &slot_3, QMetaData::Public },
	{ "file_save_as()", &slot_4, QMetaData::Public },
	{ "edit_edit()", &slot_5, QMetaData::Public },
	{ "edit_raw()", &slot_6, QMetaData::Public },
	{ "edit_after()", &slot_7, QMetaData::Public },
	{ "edit_child()", &slot_8, QMetaData::Public },
	{ "edit_copy()", &slot_9, QMetaData::Public },
	{ "edit_inherit()", &slot_10, QMetaData::Public },
	{ "edit_delete()", &slot_11, QMetaData::Public },
	{ "view_selected()", &slot_12, QMetaData::Public },
	{ "view_check()", &slot_13, QMetaData::Public },
	{ "toggle_view_defaults()", &slot_14, QMetaData::Public },
	{ "view_dependencies()", &slot_15, QMetaData::Public },
	{ "toggle_view_logonly()", &slot_16, QMetaData::Public },
	{ "toggle_view_parameters()", &slot_17, QMetaData::Public },
	{ "toggle_view_empty()", &slot_18, QMetaData::Public },
	{ "toggle_check_composite()", &slot_19, QMetaData::Public },
	{ "select_view_filter(unsigned int)", &slot_20, QMetaData::Public },
	{ "help_about()", &slot_21, QMetaData::Public },
	{ "help_aboutQt()", &slot_22, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"MainWindow", parentObject,
	slot_tbl, 23,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_MainWindow.setMetaObject( metaObj );
    return metaObj;
}

void* MainWindow::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "MainWindow" ) ) return (MainWindow*)this;
    return QMainWindow::qt_cast( clname );
}

bool MainWindow::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: menu_action(); break;
    case 1: file_new(); break;
    case 2: file_open(); break;
    case 3: file_save(); break;
    case 4: file_save_as(); break;
    case 5: edit_edit(); break;
    case 6: edit_raw(); break;
    case 7: edit_after(); break;
    case 8: edit_child(); break;
    case 9: edit_copy(); break;
    case 10: edit_inherit(); break;
    case 11: edit_delete(); break;
    case 12: view_selected(); break;
    case 13: view_check(); break;
    case 14: toggle_view_defaults(); break;
    case 15: view_dependencies(); break;
    case 16: toggle_view_logonly(); break;
    case 17: toggle_view_parameters(); break;
    case 18: toggle_view_empty(); break;
    case 19: toggle_check_composite(); break;
    case 20: select_view_filter(*((unsigned int*)static_QUType_ptr.get(_o+1))); break;
    case 21: help_about(); break;
    case 22: help_aboutQt(); break;
    default:
	return QMainWindow::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool MainWindow::qt_emit( int _id, QUObject* _o )
{
    return QMainWindow::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool MainWindow::qt_property( int _id, int _f, QVariant* _v)
{
    return QMainWindow::qt_property( _id, _f, _v);
}
#endif // QT_NO_PROPERTIES


const char *Filter::className() const
{
    return "Filter";
}

QMetaObject *Filter::metaObj = 0;
static QMetaObjectCleanUp cleanUp_Filter;

#ifndef QT_NO_TRANSLATION
QString Filter::tr( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "Filter", s, c, QApplication::DefaultCodec );
    else
	return QString::fromLatin1( s );
}
#ifndef QT_NO_TRANSLATION_UTF8
QString Filter::trUtf8( const char *s, const char *c )
{
    if ( qApp )
	return qApp->translate( "Filter", s, c, QApplication::UnicodeUTF8 );
    else
	return QString::fromUtf8( s );
}
#endif // QT_NO_TRANSLATION_UTF8

#endif // QT_NO_TRANSLATION

QMetaObject* Filter::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    QMetaObject* parentObject = QObject::staticMetaObject();
    static const QUMethod slot_0 = {"select_filter", 0, 0 };
    static const QMetaData slot_tbl[] = {
	{ "select_filter()", &slot_0, QMetaData::Public }
    };
    metaObj = QMetaObject::new_metaobject(
	"Filter", parentObject,
	slot_tbl, 1,
	0, 0,
#ifndef QT_NO_PROPERTIES
	0, 0,
	0, 0,
#endif // QT_NO_PROPERTIES
	0, 0 );
    cleanUp_Filter.setMetaObject( metaObj );
    return metaObj;
}

void* Filter::qt_cast( const char* clname )
{
    if ( !qstrcmp( clname, "Filter" ) ) return (Filter*)this;
    return QObject::qt_cast( clname );
}

bool Filter::qt_invoke( int _id, QUObject* _o )
{
    switch ( _id - staticMetaObject()->slotOffset() ) {
    case 0: select_filter(); break;
    default:
	return QObject::qt_invoke( _id, _o );
    }
    return TRUE;
}

bool Filter::qt_emit( int _id, QUObject* _o )
{
    return QObject::qt_emit(_id,_o);
}
#ifndef QT_NO_PROPERTIES

bool Filter::qt_property( int _id, int _f, QVariant* _v)
{
    return QObject::qt_property( _id, _f, _v);
}
#endif // QT_NO_PROPERTIES
