/****************************************************************************
** MainWindow meta object code from reading C++ file 'qmain.h'
**
** Created: Sat Sep 30 13:19:44 2000
**      by: The Qt MOC ($Id$)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#define Q_MOC_MainWindow
#if !defined(Q_MOC_OUTPUT_REVISION)
#define Q_MOC_OUTPUT_REVISION 9
#elif Q_MOC_OUTPUT_REVISION != 9
#error "Moc format conflict - please regenerate all moc files"
#endif

#include "qmain.h"
#include <qmetaobject.h>
#include <qapplication.h>

#if defined(Q_SPARCWORKS_FUNCP_BUG)
#define Q_AMPERSAND
#else
#define Q_AMPERSAND &
#endif


const char *MainWindow::className() const
{
    return "MainWindow";
}

QMetaObject *MainWindow::metaObj = 0;

void MainWindow::initMetaObject()
{
    if ( metaObj )
	return;
    if ( qstrcmp(QMainWindow::className(), "QMainWindow") != 0 )
	badSuperclassWarning("MainWindow","QMainWindow");
    (void) staticMetaObject();
}

#ifndef QT_NO_TRANSLATION

QString MainWindow::tr(const char* s)
{
    return qApp->translate( "MainWindow", s, 0 );
}

QString MainWindow::tr(const char* s, const char * c)
{
    return qApp->translate( "MainWindow", s, c );
}

#endif // QT_NO_TRANSLATION

QMetaObject* MainWindow::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    (void) QMainWindow::staticMetaObject();
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    typedef void(MainWindow::*m1_t0)();
    typedef void(MainWindow::*m1_t1)();
    typedef void(MainWindow::*m1_t2)();
    typedef void(MainWindow::*m1_t3)();
    typedef void(MainWindow::*m1_t4)();
    typedef void(MainWindow::*m1_t5)();
    typedef void(MainWindow::*m1_t6)();
    typedef void(MainWindow::*m1_t7)();
    typedef void(MainWindow::*m1_t8)();
    typedef void(MainWindow::*m1_t9)();
    typedef void(MainWindow::*m1_t10)();
    typedef void(MainWindow::*m1_t11)();
    typedef void(MainWindow::*m1_t12)(unsigned int);
    typedef void(MainWindow::*m1_t13)();
    typedef void(MainWindow::*m1_t14)();
    m1_t0 v1_0 = Q_AMPERSAND MainWindow::menu_action;
    m1_t1 v1_1 = Q_AMPERSAND MainWindow::file_new;
    m1_t2 v1_2 = Q_AMPERSAND MainWindow::file_open;
    m1_t3 v1_3 = Q_AMPERSAND MainWindow::file_save;
    m1_t4 v1_4 = Q_AMPERSAND MainWindow::file_save_as;
    m1_t5 v1_5 = Q_AMPERSAND MainWindow::edit_raw;
    m1_t6 v1_6 = Q_AMPERSAND MainWindow::edit_delete;
    m1_t7 v1_7 = Q_AMPERSAND MainWindow::view_check;
    m1_t8 v1_8 = Q_AMPERSAND MainWindow::view_dependencies;
    m1_t9 v1_9 = Q_AMPERSAND MainWindow::toggle_view_logonly;
    m1_t10 v1_10 = Q_AMPERSAND MainWindow::toggle_view_parameters;
    m1_t11 v1_11 = Q_AMPERSAND MainWindow::toggle_view_empty;
    m1_t12 v1_12 = Q_AMPERSAND MainWindow::select_view_filter;
    m1_t13 v1_13 = Q_AMPERSAND MainWindow::help_about;
    m1_t14 v1_14 = Q_AMPERSAND MainWindow::help_aboutQt;
    QMetaData *slot_tbl = QMetaObject::new_metadata(15);
    QMetaData::Access *slot_tbl_access = QMetaObject::new_metaaccess(15);
    slot_tbl[0].name = "menu_action()";
    slot_tbl[0].ptr = (QMember)v1_0;
    slot_tbl_access[0] = QMetaData::Public;
    slot_tbl[1].name = "file_new()";
    slot_tbl[1].ptr = (QMember)v1_1;
    slot_tbl_access[1] = QMetaData::Public;
    slot_tbl[2].name = "file_open()";
    slot_tbl[2].ptr = (QMember)v1_2;
    slot_tbl_access[2] = QMetaData::Public;
    slot_tbl[3].name = "file_save()";
    slot_tbl[3].ptr = (QMember)v1_3;
    slot_tbl_access[3] = QMetaData::Public;
    slot_tbl[4].name = "file_save_as()";
    slot_tbl[4].ptr = (QMember)v1_4;
    slot_tbl_access[4] = QMetaData::Public;
    slot_tbl[5].name = "edit_raw()";
    slot_tbl[5].ptr = (QMember)v1_5;
    slot_tbl_access[5] = QMetaData::Public;
    slot_tbl[6].name = "edit_delete()";
    slot_tbl[6].ptr = (QMember)v1_6;
    slot_tbl_access[6] = QMetaData::Public;
    slot_tbl[7].name = "view_check()";
    slot_tbl[7].ptr = (QMember)v1_7;
    slot_tbl_access[7] = QMetaData::Public;
    slot_tbl[8].name = "view_dependencies()";
    slot_tbl[8].ptr = (QMember)v1_8;
    slot_tbl_access[8] = QMetaData::Public;
    slot_tbl[9].name = "toggle_view_logonly()";
    slot_tbl[9].ptr = (QMember)v1_9;
    slot_tbl_access[9] = QMetaData::Public;
    slot_tbl[10].name = "toggle_view_parameters()";
    slot_tbl[10].ptr = (QMember)v1_10;
    slot_tbl_access[10] = QMetaData::Public;
    slot_tbl[11].name = "toggle_view_empty()";
    slot_tbl[11].ptr = (QMember)v1_11;
    slot_tbl_access[11] = QMetaData::Public;
    slot_tbl[12].name = "select_view_filter(unsigned int)";
    slot_tbl[12].ptr = (QMember)v1_12;
    slot_tbl_access[12] = QMetaData::Public;
    slot_tbl[13].name = "help_about()";
    slot_tbl[13].ptr = (QMember)v1_13;
    slot_tbl_access[13] = QMetaData::Public;
    slot_tbl[14].name = "help_aboutQt()";
    slot_tbl[14].ptr = (QMember)v1_14;
    slot_tbl_access[14] = QMetaData::Public;
    metaObj = QMetaObject::new_metaobject(
	"MainWindow", "QMainWindow",
	slot_tbl, 15,
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


const char *Filter::className() const
{
    return "Filter";
}

QMetaObject *Filter::metaObj = 0;

void Filter::initMetaObject()
{
    if ( metaObj )
	return;
    if ( qstrcmp(QObject::className(), "QObject") != 0 )
	badSuperclassWarning("Filter","QObject");
    (void) staticMetaObject();
}

#ifndef QT_NO_TRANSLATION

QString Filter::tr(const char* s)
{
    return qApp->translate( "Filter", s, 0 );
}

QString Filter::tr(const char* s, const char * c)
{
    return qApp->translate( "Filter", s, c );
}

#endif // QT_NO_TRANSLATION

QMetaObject* Filter::staticMetaObject()
{
    if ( metaObj )
	return metaObj;
    (void) QObject::staticMetaObject();
#ifndef QT_NO_PROPERTIES
#endif // QT_NO_PROPERTIES
    typedef void(Filter::*m1_t0)();
    m1_t0 v1_0 = Q_AMPERSAND Filter::select_filter;
    QMetaData *slot_tbl = QMetaObject::new_metadata(1);
    QMetaData::Access *slot_tbl_access = QMetaObject::new_metaaccess(1);
    slot_tbl[0].name = "select_filter()";
    slot_tbl[0].ptr = (QMember)v1_0;
    slot_tbl_access[0] = QMetaData::Public;
    metaObj = QMetaObject::new_metaobject(
	"Filter", "QObject",
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
