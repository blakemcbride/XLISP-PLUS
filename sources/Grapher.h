/* 
 * File:   Grapher.h
 * Author: almy
 *
 * Created on February 15, 2011, 2:55 PM
 */

#ifndef GRAPHER_H
#define	GRAPHER_H
#include <QPixmap>
#include <QImage>
#include <QWidget>

class Grapher : public QWidget {
    Q_OBJECT
public:
    Grapher(QWidget *parent = 0);

    QSize minimumSizeHint() const;
    QSize sizeHint() const;
    int chspacing;
    int chwidth;

private slots:
    void move_to(int x, int y);
    void draw_to(int x, int y);
    void set_size(int width, int height);
    void erase_screen();
    void set_foreground(int r, int g, int b);
    void set_background(int r, int g, int b);
    void write_text(QString);
    void myshow();
    void draw_rectangle(int x1, int y1, int x2, int y2);
    void draw_ellipse(int x, int y, int w, int h);
    void set_font(int size, int type, int style);
    void set_brush(int r, int g, int b, int style);
protected:
    void paintEvent(QPaintEvent *event);
//    void resizeEvent(QResizeEvent *event);
private:
    void setOurPen();
    void clearPixmap();
    QPixmap pixmap;
//    QImage pixmap;
    QPainter *painter;
    int xSize, ySize;
    int curx, cury;
    int forRed, forGreen, forBlue, backRed, backGreen, backBlue;
    bool xorMode;
    Qt::BrushStyle bs;
    int brRed, brGreen, brBlue;
};

#endif	/* GRAPHER_H */

