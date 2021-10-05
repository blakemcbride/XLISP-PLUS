/*
 * File:   application.cpp
 * Author: almy
 *
 * Created on February 15, 2011, 1:29 PM
 */

#include <QtWidgets/QApplication> // For QT5
#include <QtWidgets/QScrollArea>  // for QT5
//#include <QApplication>  // for QT4
//#include <QScrollArea>   // for QT4
#include "application.h"
#include "Grapher.h"
#include "CThread.h"

static Grapher *grapher;
QScrollArea *scrollArea;
CThread *ct;
int main(int argc, char *argv[]) {
    // initialize resources, if needed
    // Q_INIT_RESOURCE(resfile);

    QApplication app(argc, argv);

    // create and show your widgets here

    ct = new CThread(argc, argv);

#ifdef STSZ
    ct->setStackSize(STSZ);
#endif
 

      grapher = new Grapher;
      grapher->setSizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
//      grapher->setWindowTitle(QObject::tr("XLISP Graphics"));
      
      scrollArea = new QScrollArea;
      scrollArea->setWidget(grapher);
      scrollArea->setWindowTitle(QObject::tr("XLISP Graphics"));
    ct->start();

    QObject::connect(ct, SIGNAL(finished()), &app, SLOT(quit()));

    app.setQuitOnLastWindowClosed(false);
    // We probably don't need the following, but just in case:
    return app.exec();
}


extern "C"    void move_to(int x, int y) {
    ct->move_to(x, y);
}

extern "C"    void draw_to(int x, int y) {
    ct->draw_to(x, y);
}

extern "C"    void set_size(int width, int height) {
    ct->set_size(width, height);
    ct->show();
}

extern "C"    void erase_screen() {
    ct->erase_screen();
    ct->show();
}

extern "C"    void set_foreground(int r, int g, int b) {
    ct->set_foreground(r,g,b);
}

extern "C"    void set_background(int r, int g, int b) {
    ct->set_background(r,g,b);
}

extern "C"    void write_text(char *string) {
    ct->write_text(string);
}

extern "C"    void update() {
    ct->show();
    ct->update();
};

extern "C"  void get_metrics(int* width, int* height) {
    *width = grapher->chwidth;
    *height = grapher->chspacing;
}

extern "C"  void draw_rectangle(int x1, int y1, int x2, int y2) {
    ct->draw_rectangle(x1, y1, x2, y2);
}

extern "C"  void draw_ellipse(int x, int y, int w, int h) {
    ct->draw_ellipse(x, y, w, h);
}

extern "C"  void set_font(int size, int type, int style) {
    ct->set_font(size, type, style);
}

extern "C"    void set_brush(int r, int g, int b, int style) {
    ct->set_brush(r, g, b, style);
}

