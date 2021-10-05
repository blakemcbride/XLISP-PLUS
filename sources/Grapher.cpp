/* 
 * File:   Grapher.cpp
 * Author: almy
 * 
 * Created on February 15, 2011, 2:55 PM
 */

#include <QtWidgets> // For QT5
//#include <QtGui/QtGui> // For QT4
#include <string.h>
#include "Grapher.h"
#include "CThread.h"
#include "application.h"
#include <stdio.h>
Grapher::Grapher(QWidget *parent) :QWidget(parent) {
    xSize=800;
    ySize=640;
    forRed=255;
    forGreen=255;
    forBlue=255;
    backRed=0;
    backGreen=0;
    backBlue=0;
    brRed = 255;
    brGreen = 255;
    brBlue = 255;
    bs = Qt::NoBrush;

    painter = NULL;

    setBackgroundRole(QPalette::Dark);
    setAutoFillBackground(true);
    QFont newFont("Courier", 10);
    this->setFont(newFont);
    chspacing = this->fontMetrics().lineSpacing();
    chwidth = this->fontMetrics().width('m');

    clearPixmap();

    connect(ct, SIGNAL(s_update()), this, SLOT(update()));
    connect(ct, SIGNAL(s_erase_screen()), this, SLOT(erase_screen()));
    connect(ct, SIGNAL(s_move_to(int, int)), this, SLOT(move_to(int, int)));
    connect(ct, SIGNAL(s_draw_to(int, int)), this, SLOT(draw_to(int, int)));
    connect(ct, SIGNAL(s_show()), this, SLOT(myshow()));
    connect(ct, SIGNAL(s_set_size(int, int)), this, SLOT(set_size(int,int)));
    connect(ct, SIGNAL(s_set_foreground(int, int, int)), this, SLOT(set_foreground(int,int,int)));
    connect(ct, SIGNAL(s_set_background(int, int, int)), this, SLOT(set_background(int,int,int)));
    connect(ct, SIGNAL(s_write_text(QString)), this, SLOT(write_text(QString)));
    connect(ct, SIGNAL(s_draw_rectangle(int, int, int, int)), this, SLOT(draw_rectangle(int, int, int, int)));
    connect(ct, SIGNAL(s_draw_ellipse(int, int, int, int)), this, SLOT(draw_ellipse(int, int, int, int)));
    connect(ct, SIGNAL(s_set_font(int, int, int)), this, SLOT(set_font(int, int, int)));
    connect(ct, SIGNAL(s_set_brush(int, int, int, int)), this, SLOT(set_brush(int, int, int, int)));
}

    void Grapher::draw_rectangle(int x1, int y1, int x2, int y2) {
        if (x1 > x2) { int temp = x1; x1 = x2; x2 = temp; }
        if (y1 > y2) { int temp = y1; y1 = y2; y2 = temp; }
        painter->drawRect(x1, y1, x2-x1, y2-y1);
    }

    void Grapher::draw_ellipse(int x, int y, int w, int h) {
       painter->drawEllipse(x, y, w, h);
    }

    void Grapher::set_font(int size, int type, int style) {
        enum QFont::Weight weight;
        bool italic;
        switch (style) {
            case 0: default: weight = QFont::Normal; italic= false; break;
            case 1: weight = QFont::Bold; italic = false; break;
            case 2: weight = QFont::Normal; italic = true; break;
            case 3: weight = QFont::Bold; italic = true; break;
        }
    const char  *family;
        switch (type) {
            case 0: default: family = "Courier"; break;
            case 1: family = "Helvetica"; break;
            case 2: family = "Times"; break;
        }
    QFont newFont(family, size, weight, italic);
    this->setFont(newFont);
    painter->setFont(newFont);
    chspacing = this->fontMetrics().lineSpacing();
    chwidth = this->fontMetrics().width('m');
    }

    void Grapher::set_brush(int r, int g, int b, int style) {
        switch (style) {
            case 0: default: bs = Qt::NoBrush; break;
            case 1: bs = Qt::SolidPattern; break;
            case 2: bs = Qt::Dense1Pattern; break;
            case 3: bs = Qt::Dense2Pattern; break;
            case 4: bs = Qt::Dense3Pattern; break;
            case 5: bs = Qt::Dense4Pattern; break;
            case 6: bs = Qt::Dense5Pattern; break;
            case 7: bs = Qt::Dense6Pattern; break;
            case 8: bs = Qt::Dense7Pattern; break;
            case 9: bs = Qt::HorPattern; break;
            case 10: bs = Qt::VerPattern; break;
            case 11: bs = Qt::CrossPattern; break;
            case 12: bs = Qt::BDiagPattern; break;
            case 13: bs = Qt::FDiagPattern; break;
            case 14: bs = Qt::DiagCrossPattern; break;
        }
        brRed = r;
        brGreen = g;
        brBlue = b;
        painter->setBrush(QBrush(QColor(r,g,b), bs));
    }

void Grapher::myshow() {
//    this->show();
    scrollArea->show();
}

void Grapher::setOurPen() {
#ifdef MACOSG
    if (xorMode) {
        painter->setPen(QColor(forRed, forGreen, forBlue, 255));
        painter->setCompositionMode(QPainter::CompositionMode_Xor);
    } else {
        painter->setPen(QColor(forRed, forGreen, forBlue));
       painter->setCompositionMode(QPainter::CompositionMode_Source);
    }
#else
    painter->setPen(QColor(forRed, forGreen, forBlue));
#endif
}

void Grapher::clearPixmap() {
    if (painter != NULL) delete painter;
//    pixmap = /*QPixmap*/ QImage(xSize, ySize, QImage::Format_RGB32);
    pixmap = QPixmap(xSize, ySize);
    painter = new QPainter(&pixmap);
    painter->initFrom(this);
//    painter->translate(0.5, 0.5);
    painter->setRenderHint(QPainter::Antialiasing, false);
    setOurPen();
    painter->setBrush(QBrush(QColor(brRed, brGreen, brBlue), bs));
//    pixmap.fill(QColor(backRed, backGreen, backBlue).rgb());
    pixmap.fill(QColor(backRed, backGreen, backBlue));
    update();
}

void Grapher::paintEvent(QPaintEvent* event) {
    QStylePainter painter(this);
//    painter.drawImage(0,0,pixmap);
    painter.drawPixmap(0,0,pixmap);
}
QSize Grapher::minimumSizeHint() const {
    return QSize(xSize, ySize);
}

QSize Grapher::sizeHint() const {
    return QSize(xSize, ySize);
}

    void Grapher::move_to(int x, int y) {
        curx = x;
        cury = y;
    }
    void Grapher::draw_to(int x, int y) {
         painter->drawLine(curx, cury, x, y);
        curx = x;
        cury = y;
    }
    void Grapher::set_size(int width, int height) {
        if (width != xSize || height != ySize) {
            xSize = width;
            ySize = height;
            clearPixmap();
            this->resize(QSize(xSize, ySize));
        }
    }

    void Grapher::erase_screen() {
        clearPixmap();
    }
    void Grapher::set_foreground(int r, int g, int b) {
        xorMode = (r & 256) != 0;
        forRed = r & 255;
        forGreen = g;
        forBlue = b;
        if (painter != NULL) {
            setOurPen();
        }
    }
    void Grapher::set_background(int r, int g, int b) {
        backRed = r;
        backGreen = g;
        backBlue = b;
    }
    void Grapher::write_text(QString string) {
 //       QPainter painter(&pixmap);
 //       painter.initFrom(this);
//        painter->setPen(QColor(forRed, forGreen, forBlue));
        painter->drawText(curx, cury, string);
    }
