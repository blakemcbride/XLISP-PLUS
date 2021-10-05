/* 
 * File:   application.h
 * Author: almy
 *
 * Created on February 23, 2011, 8:06 AM
 */

#ifndef APPLICATION_H
#define	APPLICATION_H
#ifdef __cplusplus
class CThread;
extern CThread *ct;
extern QScrollArea *scrollArea;
extern "C"
{
#endif
    // These functions are called from the C thread
    void move_to(int x, int y);
    void draw_to(int x, int y);
    void set_size(int width, int height);
    void erase_screen(void);
    void set_foreground(int r, int g, int b);
    void set_background(int r, int g, int b);
    void set_brush(int r, int g, int b, int style);
    void draw_rectangle(int x1, int y1, int x2, int y2);
    void draw_ellipse(int x, int y, int w, int h);
    void set_font(int size, int type, int style);
    void write_text(char *string);
    void update(void);
    void get_metrics(int *width, int *height);

#ifdef __cplusplus
}
#endif

#endif	/* APPLICATION_H */

