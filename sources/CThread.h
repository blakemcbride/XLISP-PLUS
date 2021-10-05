/* 
 * File:   CThread.h
 * Author: almy
 *
 * Created on February 23, 2011, 8:55 AM
 *
 * We need to run the C code from a separate thread.
 */

#ifndef CTHREAD_H
#define	CTHREAD_H

#include <QtGui/QtGui>

class CThread : public QThread {
    Q_OBJECT

public:
    CThread(int argc, char **argv);

    void erase_screen();
    void move_to(int x, int y);
    void draw_to(int x, int y);
    void show();
    void update();
void set_size(int width, int height);
void set_foreground(int r, int g, int b);
void set_background(int r, int g, int b);
void write_text(const char *text);
void draw_rectangle(int x1, int y1, int x2, int y2);
void draw_ellipse(int x, int y, int w, int h);
void set_font(int size, int type, int style);
    void set_brush(int r, int g, int b, int style);

    signals:
void s_update();
void s_erase_screen();
void s_move_to(int x, int y);
void s_draw_to(int x, int y);
void s_show();
void s_set_size(int width, int height);
void s_set_foreground(int r, int g, int b);
void s_set_background(int r, int g, int b);
void s_write_text(QString text);
    void s_draw_rectangle(int x1, int y1, int x2, int y2);
    void s_draw_ellipse(int x, int y, int w, int h);
    void s_set_font(int size, int type, int style);
    void s_set_brush(int r, int g, int b, int style);

protected:
    void run();

private:
    int cargc;
    char **cargv;
};

#endif	/* CTHREAD_H */

