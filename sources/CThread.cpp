/* 
 * File:   CThread.cpp
 * Author: almy
 * 
 * Created on February 23, 2011, 8:55 AM
 */
#include <QtCore/QtCore>
#include <QtGui/QtGui>
#include "CThread.h"

CThread::CThread(int argc, char **argv) : QThread()  {
    cargv = argv;
    cargc = argc;
}

extern "C" void cmain(int argc, char**argv);
extern "C" void osfinish(void);

void CThread::run() {
    cmain(cargc, cargv);
}

    void CThread::erase_screen() { emit s_erase_screen(); }
    void CThread::move_to(int x, int y) { emit s_move_to(x, y); }
    void CThread::draw_to(int x, int y) { emit s_draw_to(x, y); }
    void CThread::show() { emit s_show(); }
    void CThread::update() { emit s_update(); }
    void CThread::set_size(int width, int height) {emit s_set_size(width, height); }
    void CThread::set_foreground(int r, int g, int b) {
        emit s_set_foreground(r, g, b);
    }
    void CThread::set_background(int r, int g, int b) {emit s_set_background(r, g, b); }
    void CThread::write_text(const char *text) {emit s_write_text(QString(text)); }
    void CThread::draw_rectangle(int x1, int y1, int x2, int y2) { emit s_draw_rectangle(x1, y1, x2, y2); }
    void CThread::draw_ellipse(int x, int y, int w, int h) { emit s_draw_ellipse(x, y, w, h); }
    void CThread::set_font(int size, int type, int style) { emit s_set_font(size, type, style); }
    void CThread::set_brush(int r, int g, int b, int style) { emit s_set_brush(r, g, b, style); }
