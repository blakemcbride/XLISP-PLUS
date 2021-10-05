/* winstuff.c for MS Windows and Borland compiler */

/* Both ANSI and IBM ASCII character sets supported via the ANSI8 compilation
   option (see XLISP.H file). Use the IBM ASCII option when compatibility
   with the IBM character set is desired, otherwise specifying ANSI8 is
   probably a better choice.
*/


#include "xlisp.h"
#include "osdefs.h"

#include <dos.h>
#include <process.h>
#include <math.h>
#include <io.h>
#include <float.h>
#ifdef TIMES
 #include <time.h>
#endif

#undef FAR
#undef NEAR
#undef CDECL
#undef HIWORD
#define STRICT
#include <windows.h>
#include <commdlg.h>
#include <shellapi.h>
#include "xlispwin.h"

/* Menu Operations */
#define C_OPEN (512 + M_OPEN)
#define C_RESTORE (512 + M_RESTORE)
#define C_SAVE (512 + M_SAVE)
#define C_SAVEAS (512 + M_SAVEAS)
#define C_DRIBBLE (512 + M_DRIBBLE)
#define C_DROPFILE (512)
#define FNAMESIZE (80)
char lastRestore[FNAMESIZE];            /* full lastrestore file name */
char openFile[FNAMESIZE];               /* last file read via menus */
typedef struct dflist {
    struct dflist *next;
    char fname[FNAMESIZE];
} DFLIST;
DFLIST *dropfile = NULL;            /* files to load via dropfile feature */

#define KBSIZE (16)             /* size of keyboard typeahead buffer */
static int keybuf[KBSIZE];      /* keyboard typeahead buffer */
static int *keyin = keybuf;     /* buffer pointer (in) */
static int *keyout = keybuf;    /* buffer pointer (out) */

#define CHBSIZE 256                 /* size of output buffer */
static char outbuf[CHBSIZE];        /* output character buffer */
static char *outbufp = &outbuf[0];
static int direct = FALSE;      /* set to TRUE for unbuffered output */

#define SCRNX (80)              /* screen size */
static int SCRNY;               /* Number of lines is not a constant */
static POINT cursor = {0, 0};           /* where the cursor is located */
static int first = 0;                   /* top line in buffer */
static char FAR *screenbuf;     /* array of chars for the display */
static int charSpacing[SCRNX];      /* character spacing array */

#ifdef GRAPHICS
static HPEN ourPen;                     /* holds our pen */
static int maxX, maxY;                  /* maximum coordinate positions */
#endif
static HBRUSH ourBrush;                 /* holds our background brush */
static HFONT ourFont;                   /* holds our font */
static POINT charSize = {10, 20};       /* character dimensions */
static int charAscent = 16;             /* character ascent */
static COLORREF charColor;              /* character color */
static COLORREF bkgColor;
    
static BOOL inFocus;                /* window has focus */
static BOOL inReadLoop;             /* reading from window */
static BOOL inGC;                   /* doing a garbage collection */

/* for the Copy operation */
static POINT pnt1, pnt2;            /* Initial and current point */
static int inMarking;               /* are we marking/marked text? */
#define NOTMOVED (1)
#define MARKING (2)
#define MARKED (3)


/* for the Paste operation */
static char FAR *pastch;    /* start of buffer */
static char FAR *pastchp;   /* current location */
static BOOL inPaste;        /* We are pasting */


long FAR PASCAL _export WndProc( HWND, UINT, WPARAM, LPARAM);
int CDECL main(int, char **);
static HANDLE hInst;    /* current instance */
static HWND hWnd;       /* the window */
static HWND hStatusWnd; /* the status window */
static BOOL statusActive;   /* status window active? */
static HANDLE hAccel;   /* Accelerators */
static HDC DC;                      /* for when we need one */
static PAINTSTRUCT PS;
static HFONT savedFont;
static char szAppName[] = "XLISPWIN";
static char szTitleName[] = "XLISP-PLUS 3.04";
static char section[] = "Xlisp";
static char iniName[] = "xlispwin.ini";
#define HELPNAME "xlisp300.hlp"

static HCURSOR arrowCursor, waitCursor, gcCursor;

static int ourArgc;
static char **ourArgv;

/* Memory Management */
/* later versions of Borland C have farmalloc support! */

/* This function is limited to 64k */
void _far *wcalloc(unsigned long n1, unsigned long n2) {
    unsigned len = (unsigned int) (n1*n2);
    char _far *result = MALLOC(len);

    _fmemset(result, 0, len);

    return (void _far *)result;
}

unsigned _far *wfree(void _far *p) {
    HGLOBAL h =  (HGLOBAL) LOWORD(GlobalHandle(HIWORD(p)));

    GlobalUnlock(h);
    GlobalFree(h);
    return NULL;
}

/* Interrogate as to the size of characters so we can make the window
   the correct size */
static void setCharSizes(void) {
    TEXTMETRIC tm;
    int i;

#ifdef ANSI8
    char fontname[LF_FACESIZE];
    int fontsize;
#endif

    ourArgc = _argc;
    ourArgv = _argv;
    if (_argc == 1) {
        char argbuf[128];
        GetPrivateProfileString(section, "Arglist", "", argbuf, 128, iniName);
        if (strlen(argbuf) > 0) {
            /* use new arg list */
            int argcount=0;
            char argtemp[80];
            int i=0, j;
            /* get number of arguments */
            while (sscanf(&argbuf[i],"%80s%n", argtemp, &j)==1) {
                i += j;
                argcount++;
            }
            ourArgc = argcount+1;
            ourArgv = (char **) calloc(argcount+2, sizeof(char *));
            ourArgv[0] = _argv[0];
            /* now fill in arguments */
            i = 0;
            argcount = 1;
            while (sscanf(&argbuf[i],"%80s%n", argtemp, &j)==1) {
                i += j;
                ourArgv[argcount] = (char *)malloc(sizeof(argtemp)+1);
                strcpy(ourArgv[argcount++], argtemp);
            }
        }
    }

    DC = GetDC(hWnd);

#ifdef ANSI8    
    GetPrivateProfileString(section, "Font", "FIXEDSYS",
                            fontname, LF_FACESIZE, iniName);
    fontsize = GetPrivateProfileInt(section, "FontSize", 16, iniName);
    ourFont = CreateFont(-fontsize,0,0,0,0,0,0,0,0,OUT_DEFAULT_PRECIS,0,0,
                         FIXED_PITCH|FF_DONTCARE,fontname);
#else
    ourFont = GetStockObject(OEM_FIXED_FONT);
#endif
    savedFont = SelectObject(DC, ourFont);

    
    SCRNY = GetPrivateProfileInt(section, "Lines", 25, iniName);


    GetTextMetrics(DC, &tm);

    charSize.x = tm.tmAveCharWidth;
    charSize.y = (tm.tmHeight +
                  (int)GetPrivateProfileInt(section,"Leading",
                                       tm.tmExternalLeading,iniName));
    charAscent = tm.tmAscent;

    for (i=0; i<SCRNX; i++) charSpacing[i] = charSize.x;

    SelectObject(DC, savedFont);
    ReleaseDC(hWnd, DC);


}


int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpszCmdLine, int nCmdShow) {

    hInst = hInstance;

    if (!hPrevInstance) { /* must register our window class */
        WNDCLASS wc;
        wc.style = CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc = WndProc; 
        wc.cbClsExtra = 0;
        wc.cbWndExtra = 0;
        wc.hInstance = hInstance; 
        wc.hIcon = LoadIcon(hInstance, "ICON_1");
        wc.hCursor = NULL; 
        wc.hbrBackground = 0;
        wc.lpszMenuName =  "MENU_1";
        wc.lpszClassName = szAppName; /* Name used in call to CreateWindow. */

        RegisterClass(&wc);
    }

    /* initialize colors and background brush */
    charColor = GetSysColor(COLOR_WINDOWTEXT);
    bkgColor = GetSysColor(COLOR_WINDOW);
    ourBrush = CreateSolidBrush(bkgColor);

    hWnd = CreateWindow(
        szAppName,
        szTitleName,          /* Text for window title bar. */
        WS_OVERLAPPED|WS_CAPTION|WS_SYSMENU|WS_MINIMIZEBOX,
        CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
        NULL,                 /* Overlapped windows have no parent. */
        NULL,                 /* Use the window class menu.         */
        hInstance,            /* This instance owns this window.    */
        lpszCmdLine           /* Pointer not needed.                */
        );

    if (hWnd == NULL) return FALSE; /* could not create window */

    arrowCursor = LoadCursor(NULL, IDC_ARROW);
    waitCursor = LoadCursor(NULL, IDC_WAIT);
    gcCursor = LoadCursor(hInstance, "CURSOR_1");
    SetClassWord(hWnd, GCW_HBRBACKGROUND, (WORD)ourBrush);

    hAccel = LoadAccelerators(hInstance, "ACCELERATORS_1");

    setCharSizes();
#ifdef GRAPHICS
    maxX = charSize.x*SCRNX - 1 ;   /* display dimenensions for drawing */
    maxY = charSize.y*SCRNY - 1 ;
    ourPen = CreatePen(PS_SOLID, 0, 0L);
#endif
    screenbuf = (char FAR *)MALLOC(SCRNX*SCRNY);
    _fmemset(screenbuf, ' ', SCRNX*SCRNY);  /* clear virtual display */

    SetWindowPos(hWnd, NULL, NULL, NULL,
                 charSize.x*SCRNX + 2*GetSystemMetrics(SM_CXBORDER),
                 charSize.y*SCRNY + 2*GetSystemMetrics(SM_CYBORDER)
                 + GetSystemMetrics(SM_CYCAPTION)
                 + GetSystemMetrics(SM_CYMENU),
                 SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOZORDER|SWP_NOREDRAW);
    
    hStatusWnd = CreateWindow(
        szAppName,
        "Xlisp Status",
        WS_OVERLAPPED|WS_CAPTION,
        CW_USEDEFAULT, CW_USEDEFAULT,
        charSize.x*17 + 2*GetSystemMetrics(SM_CXBORDER),
#ifdef STSZ
        charSize.y*6 +
#else
        charSize.y*5 +
#endif
        2*GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYCAPTION),
        hWnd,     /* parent window is other window */
        NULL,
        hInstance,
        lpszCmdLine);

    if (hStatusWnd) {
        SetMenu(hStatusWnd, NULL);  /* No menu on the status window */
    }

    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);

    return main(ourArgc, ourArgv);  /* Run XLISP, message loop is in
                                   character input functions */
}



/* Flip cursors during garbage collection */

void startGC(void) {
    inGC = TRUE;
    if (inFocus) SetCursor(gcCursor);
}

void endGC(void) {
    inGC = FALSE;
    if (inFocus) SetCursor(waitCursor);
}


/* Position the caret (keyboard cursor) */
static void placeCaret(void) {
    CreateCaret(hWnd, 0, charSize.x, 2);
    SetCaretPos(cursor.x*charSize.x, cursor.y*charSize.y +charAscent);
    ShowCaret(hWnd);
}

/* For Copy operation, invert between pnt1 and pnt2 */
static void updateMarkedRegion(void) {
    int minx = min(pnt1.x, pnt2.x), maxx = max(pnt1.x, pnt2.x);
    int miny = min(pnt1.y, pnt2.y), maxy = max(pnt1.y, pnt2.y);
    RECT rect;

    if (inMarking == NOTMOVED) return;  /* don't draw yet */
    rect.top = miny * charSize.y;
    rect.bottom = (maxy + 1) * charSize.y;
    rect.left = minx * charSize.x;
    rect.right = (maxx + 1) * charSize.x;
    InvertRect(DC, &rect);
}

/* updateMarkedRegion when we don't have a Device Context, yet */
static void drawMarkedRegion(void) {
    if (inMarking == NOTMOVED) return;  /* don't draw yet */
    DC = GetDC(hWnd);
    updateMarkedRegion();
    ReleaseDC(hWnd, DC);
}


/* Update the display when redraw required */
static void drawDisplay(void) {
    int xmin, xlength, y, ymax, yr = first;

    DC = BeginPaint(hWnd, &PS);
    savedFont = SelectObject(DC, ourFont);
    SetTextColor(DC, charColor);
    SetBkColor(DC, bkgColor);

    xmin = PS.rcPaint.left/charSize.x;
    xlength = (PS.rcPaint.right + charSize.x - 1) / charSize.x - xmin;
    y = PS.rcPaint.top/charSize.y;
    ymax = (PS.rcPaint.bottom + charSize.y - 1) / charSize.y;

    yr += y;

    while (y < ymax) {
        if (yr >= SCRNY) yr -= SCRNY; 
        ExtTextOut(DC, xmin*charSize.x, y*charSize.y,
                   0, NULL,
                   screenbuf + yr*SCRNX + xmin, xlength,
                   charSpacing);
        y++;
        yr++;
    }

    if (inMarking)
        updateMarkedRegion();

    SelectObject(DC, savedFont);
    EndPaint(hWnd, &PS);
}


/* calculate index into display array */
int getbufxy(int x, int y) {
    int val = y + first;

    return x + ((val >= SCRNY) ? val - SCRNY : val)*SCRNX;
}


/* Redraw current line between boundaries, drop out of marking mode
   if we were in it */
static void updateCurrent(int left, int right) {
    if (inMarking) {    /* TAA FIX 2/94, was under if (right > left) */
        updateMarkedRegion();   /* undraw it */
        inMarking = FALSE;
    }
    if (right > left) {
        DC = GetDC(hWnd);
        savedFont = SelectObject(DC, ourFont);
        SetTextColor(DC, charColor);
        SetBkColor(DC, bkgColor);
        ExtTextOut(DC, left*charSize.x, cursor.y*charSize.y,
                   0, NULL,
                   screenbuf + getbufxy(left, cursor.y), right-left,
                   charSpacing);
        SelectObject(DC, savedFont);
        ReleaseDC(hWnd, DC);
    }
}

/* Process newline (drop out of marking mode if we were in it */
static void newLine(void) {
    cursor.x = 0;   /* cr */
    if (cursor.y == SCRNY-1)    { /* we gotta scroll */
        if (++first == SCRNY) first=0; /* wrap around */
        _fmemset(screenbuf + getbufxy(0, cursor.y), ' ', SCRNX); /* clear line */
        ScrollWindow(hWnd, 0, -charSize.y, NULL, NULL);
        UpdateWindow(hWnd);
    }
    else    /* just go to next line */
        cursor.y++;
    if (inMarking) {
        drawMarkedRegion(); /* undraw it */
        inMarking = FALSE;
    }
}


/* Flush the display output character buffer, writing everything to
   the display. We buffer to get maximum display speed */           
void flushbuf(void) {
    char *buf = &outbuf[0];
    int start = cursor.x, right = cursor.x;
                /* updated region on line (to save time) */

    while (buf < outbufp) {
        switch (*buf) {
            case 8: /* backspace */
                if (cursor.x > 0)
                    --cursor.x;
                break;
            default:
                *(screenbuf + getbufxy(cursor.x, cursor.y)) = *buf;
                if (++cursor.x > right) right = cursor.x;
                if (cursor.x < SCRNX) break;
            case '\n':
                updateCurrent(start, right);
                newLine();
                start = right = 0;
                break;
        }
        buf++;
    }
    outbufp = &outbuf[0];
    updateCurrent(start, right);
}
                
/* These menu selections must not be executed from the Windows procedure.
   Why? Because in the event of an error we will jump out of the winproc
   instead of returning -- very bad news! */
/* Menu selection to OPEN/LOAD */
static void loadAFile(void) {
    OPENFILENAME ofn;
    char szFile[FNAMESIZE];
    char szPath[FNAMESIZE];
    char *cp;

    memset(&ofn, 0, sizeof(OPENFILENAME));

    strcpy(szPath, openFile);
    if ((cp = strrchr(szPath, '\\')) != NULL)
        *cp = '\0';
    strcpy(szFile, openFile);
    if ((cp = strrchr(szFile, '\\')) != NULL)
        strcpy(szFile, cp+1);

    ofn.lStructSize = sizeof(OPENFILENAME);
    ofn.hwndOwner = hWnd;
    ofn.lpstrFilter = "Lisp files (*.LSP)\0*.lsp\0All files (*.*)\0*.*\0";
    ofn.nFilterIndex = 1;
    ofn.lpstrFile = szFile;
    ofn.nMaxFile = FNAMESIZE;
    ofn.lpstrInitialDir = szPath;
    ofn.lpstrDefExt = "lsp";
    ofn.lpstrTitle = "Load File";
    ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;

    if (GetOpenFileName(&ofn)) {
        LVAL oldenv, oldfenv;

        /* Save the current environment and set global environment */
        xlstkcheck(2);
        xlprotect(oldenv);
        xlprotect(oldfenv);
        oldenv = xlenv;
        oldfenv = xlfenv;
        xlenv = xlfenv = NIL;

        xlload(szFile, FALSE, FALSE);
        strcpy(openFile, szFile);

        /* restore everything */
        xlenv = oldenv;
        xlfenv = oldfenv;
        xlpopn(2);
    }
}
        
/* Loading file manager dropped files */
static void loadDroppedFiles(void) {
    LVAL oldenv, oldfenv;
    DFLIST *next;
    
    /* Save the current environment and set global environment */
    xlstkcheck(2);
    xlprotect(oldenv);
    xlprotect(oldfenv);
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;

    while (dropfile) {
        xlload(dropfile->fname, FALSE, FALSE);
        strcpy(openFile, dropfile->fname);
        next = dropfile->next;
        free(dropfile);
        dropfile = next;
    }

    /* restore everything */
    xlenv = oldenv;
    xlfenv = oldfenv;
    xlpopn(2);
}

    
/* Menu selection to RESTORE a workspace image*/
static void restoreImage(void) {
    OPENFILENAME ofn;
    char szPath[FNAMESIZE];
    char szFile[FNAMESIZE];
    char *cp;

    memset(&ofn, 0, sizeof(OPENFILENAME));

    strcpy(szPath, lastRestore);
    if ((cp = strrchr(szPath, '\\')) != NULL)
        *cp = '\0';
    strcpy(szFile, lastRestore);
    if ((cp = strrchr(szFile, '\\')) != NULL)
        strcpy(szFile, cp+1);

    ofn.lStructSize = sizeof(OPENFILENAME);
    ofn.hwndOwner = hWnd;
    ofn.lpstrFilter = "Workspace (*.WKS)\0*.wks\0All files (*.*)\0*.*\0";
    ofn.nFilterIndex = 1;
    ofn.lpstrFile = szFile;
    ofn.nMaxFile = FNAMESIZE;
    ofn.lpstrInitialDir = szPath;
    ofn.lpstrDefExt = "wks";
    ofn.lpstrTitle = "Restore Workspace";
    ofn.Flags = OFN_PATHMUSTEXIST | OFN_FILEMUSTEXIST | OFN_HIDEREADONLY;

    if (GetOpenFileName(&ofn)) {
        if (xlirestore(szFile)) {
            dbgputstr("[ returning to the top level ]\n");
            longjmp(top_level,1);
        }
    }
}


/* Menu selection to SAVE a workspace image*/
static void saveImage(int needName) {
    if (needName) {
        char szPath[FNAMESIZE];
        char szFile[FNAMESIZE];
        char *cp;
        OPENFILENAME ofn;

        memset(&ofn, 0, sizeof(OPENFILENAME));

        strcpy(szPath, lastRestore);
        if ((cp = strrchr(szPath, '\\')) != NULL)
            *cp = '\0';
        strcpy(szFile, lastRestore);
        if ((cp = strrchr(szFile, '\\')) != NULL)
            strcpy(szFile, cp+1);

        ofn.lStructSize = sizeof(OPENFILENAME);
        ofn.hwndOwner = hWnd;
        ofn.lpstrFilter = "Workspace files (*.WKS)\0*.wks\0All files (*.*)\0*.*\0";
        ofn.nFilterIndex = 1;
        ofn.lpstrFile = szFile;
        ofn.nMaxFile = FNAMESIZE;
        ofn.lpstrInitialDir = szPath;
        ofn.lpstrTitle = "Save Workspace";
        ofn.lpstrDefExt = "wks";
        ofn.Flags = OFN_CREATEPROMPT|OFN_HIDEREADONLY|
                    OFN_NOREADONLYRETURN;

        if (!GetSaveFileName(&ofn) && xlstack-2 >= xlstkbase) return;
        strcpy(lastRestore, szFile);
    }
    xlisave(lastRestore);
}


/* Menu selection to set the transcript file */
static void dribbleFile(void) {
    static char szDrib[FNAMESIZE] = {0};    /* the final file name */

    if (tfp != CLOSED) {    /* was on -- turn off */
        strcpy(szDrib, filetab[tfp].tname);
        OSCLOSE(tfp);
        tfp = CLOSED;
    }
    else { /* was off -- turn on */
        char szPath[FNAMESIZE];
        char szFile[FNAMESIZE];
        char *cp;
        OPENFILENAME ofn;

        memset(&ofn, 0, sizeof(OPENFILENAME));

        strcpy(szPath, szDrib);
        if ((cp = strrchr(szPath, '\\')) != NULL)
            *cp = '\0';
        strcpy(szFile, szDrib);
        if ((cp = strrchr(szFile, '\\')) != NULL)
            strcpy(szFile, cp+1);

        ofn.lStructSize = sizeof(OPENFILENAME);
        ofn.hwndOwner = hWnd;
        ofn.lpstrFilter = "Text files (*.TXT)\0*.txt\0All files (*.*)\0*.*\0";
        ofn.nFilterIndex = 1;
        ofn.lpstrFile = szFile;
        ofn.nMaxFile = FNAMESIZE;
        ofn.lpstrInitialDir = szPath;
        ofn.lpstrTitle = "Dribble Into";
        ofn.lpstrDefExt = "txt";
        ofn.Flags = OFN_CREATEPROMPT|OFN_HIDEREADONLY|
                    OFN_OVERWRITEPROMPT|OFN_NOREADONLYRETURN;

        if (!GetSaveFileName(&ofn)) return;
        strcpy(szDrib, szFile);
        tfp = OSAOPEN(szDrib, CREATE_WR);
    }
}


long FAR PASCAL _export WndProc(HWND dummy, UINT message,
                                WPARAM wParam, LPARAM lParam) {
    if (dummy == hStatusWnd) {
        if (message == WM_PAINT) {
#ifdef STSZ
            int i;
#endif
            static char statbuf[20];
            SetClassWord(hStatusWnd, GCW_HBRBACKGROUND, COLOR_WINDOW+1);
            DC = BeginPaint(hStatusWnd, &PS);
            savedFont = SelectObject(DC, ourFont);
            SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT));
            SetBkColor(DC, GetSysColor(COLOR_WINDOW));
            sprintf(statbuf, " Free:  %8ld ", nfree);
            ExtTextOut(DC, 0, 0, 0, NULL, statbuf, 17, charSpacing);
            sprintf(statbuf, " Total: %8ld ", total);
            ExtTextOut(DC, 0, charSize.y, 0, NULL, statbuf, 17, charSpacing);
            sprintf(statbuf, " GC calls: %5d ", gccalls);
            ExtTextOut(DC, 0, charSize.y*2, 0, NULL, statbuf, 17, charSpacing);
            sprintf(statbuf, " Edepth: %7d ", xlstack-xlstkbase);
            ExtTextOut(DC, 0, charSize.y*3, 0, NULL, statbuf, 17, charSpacing);
            sprintf(statbuf, " Adepth: %7d ", xlargstktop-xlsp);
            ExtTextOut(DC, 0, charSize.y*4, 0, NULL, statbuf, 17, charSpacing);
#ifdef STSZ
            sprintf(statbuf, " Sdepth: %7d ", STACKREPORT(i));
            ExtTextOut(DC, 0, charSize.y*5, 0, NULL, statbuf, 17, charSpacing);
#endif
            SelectObject(DC, savedFont);
            EndPaint(hWnd, &PS);
            SetClassWord(hStatusWnd, GCW_HBRBACKGROUND, (WORD)ourBrush);
            return 0;
        }
        return DefWindowProc(dummy, message, wParam, lParam);
    }
    switch (message)  {
        case WM_PAINT:
            drawDisplay();
            return 0;

        case WM_LBUTTONDOWN:
            if (inMarking) { /* must unmark current selection */
                drawMarkedRegion();
            }
            pnt1.x = pnt2.x = LOWORD(lParam)/charSize.x;
            pnt1.y = pnt2.y = HIWORD(lParam)/charSize.y;
            inMarking = NOTMOVED;
            return 0;

        case WM_LBUTTONUP:
            if (inMarking == NOTMOVED)
                inMarking = FALSE;
            else if (inMarking == MARKING)
                inMarking = MARKED;
            return 0;

        case WM_MOUSEMOVE:
            if (inMarking == MARKING || inMarking == NOTMOVED) {
                int x = LOWORD(lParam)/charSize.x;
                int y = HIWORD(lParam)/charSize.y;

                if (pnt2.x != x || pnt2.y != y) {
                    DC = GetDC(hWnd);
                    updateMarkedRegion();
                    inMarking = MARKING;
                    pnt2.x = x;
                    pnt2.y = y;
                    updateMarkedRegion();
                    ReleaseDC(hWnd, DC);
                }
            }
            if (inReadLoop) SetCursor(arrowCursor);
            else if (inGC) SetCursor(gcCursor);
            else SetCursor(waitCursor);
            return 0;
                
        case WM_INITMENUPOPUP:
            if (HIWORD(lParam) == 0) {
                int whenReading = direct ? MF_ENABLED : MF_GRAYED;
                int whenNotReading = direct ? MF_GRAYED : MF_ENABLED;
                
                EnableMenuItem((HMENU)wParam, M_PASTE,
                               direct &&
                               IsClipboardFormatAvailable(CF_TEXT)
                               ? MF_ENABLED : MF_GRAYED);
                EnableMenuItem((HMENU)wParam, M_COPY,
                               inMarking == MARKED
                               ? MF_ENABLED : MF_GRAYED);
                EnableMenuItem((HMENU)wParam, M_OPEN, whenReading);
                EnableMenuItem((HMENU)wParam, M_RESTORE, whenReading);
                EnableMenuItem((HMENU)wParam, M_SAVE, whenReading);
                EnableMenuItem((HMENU)wParam, M_SAVEAS, whenReading);
                EnableMenuItem((HMENU)wParam, M_DRIBBLE, whenReading);
                EnableMenuItem((HMENU)wParam, M_BREAK, whenNotReading);
                whenReading = (direct && xldebug) ? MF_ENABLED : MF_GRAYED;
                EnableMenuItem((HMENU)wParam, M_CLEANUP, whenReading);
                EnableMenuItem((HMENU)wParam, M_PROCEED, whenReading);
                CheckMenuItem((HMENU)wParam, M_DRIBBLE,
                              tfp == CLOSED ? MF_UNCHECKED
                                            : MF_CHECKED);
                CheckMenuItem((HMENU)wParam, M_STATUS,
                              statusActive ? MF_CHECKED : MF_UNCHECKED);
                return 0;
            }
            break;


        case WM_COMMAND:        /* Menu selections go here */
            switch (wParam) {
                case M_EXIT:
                    SendMessage(hWnd, WM_CLOSE, 0, 0L);
                    return 0;
                case M_ABOUT:
                    MessageBox(hWnd, "XLISP-PLUS 3.04 for Windows\nby Tom Almy\nFree for noncommercial use\n\nConsult documentation for help",
                               "About", MB_ICONINFORMATION|MB_OK);
                    return 0;
                case M_OPEN:
                case M_RESTORE:
                case M_SAVE:
                case M_SAVEAS:
                case M_DRIBBLE:
                    SendMessage(hWnd, WM_CHAR, 512+wParam, 0L);
                    return 0;
                case M_BREAK:
                    SendMessage(hWnd, WM_CHAR, C_BREAK, 0L);
                    return 0;
                case M_CLEANUP:
                    SendMessage(hWnd, WM_CHAR, C_CLEAN, 0L);
                    return 0;
                case M_TOPLEVEL:
                    SendMessage(hWnd, WM_CHAR, C_TOPLEV, 0L);
                    return 0;
                case M_PROCEED:
                    SendMessage(hWnd, WM_CHAR, C_CONT, 0L);
                    return 0;
                case M_STATUS:
                    if (hStatusWnd) {
                        statusActive = !statusActive;
                        if (statusActive) { /*enable the timer*/
                            if (!SetTimer(hWnd, 1, 1000, NULL))
                                statusActive = FALSE;   /* timer failed */
                        }
                        else {
                            KillTimer(hWnd, 1);
                        }
                        ShowWindow(hStatusWnd,
                                   statusActive ? SW_SHOWNOACTIVATE : SW_HIDE);
                    }
                    return 0;
                case M_PASTE:
                {
                    HANDLE hClipMemory;
                    OpenClipboard(hWnd);
                    if ((hClipMemory=GetClipboardData(CF_TEXT)) != NULL) {
                        pastch=pastchp=
                                       (char FAR *)MALLOC(GlobalSize(hClipMemory));
                        if (pastch != NULL) {
                            lstrcpy(pastch, GlobalLock(hClipMemory));
                            inPaste = TRUE;
                            GlobalUnlock(hClipMemory);
                        }
                    }
                    CloseClipboard();
                }
                return 0;

                case M_COPY:
                {
                    int x,y;
                    int minx = min(pnt1.x, pnt2.x);
                    int maxx = max(pnt1.x, pnt2.x);
                    int miny = min(pnt1.y, pnt2.y);
                    int maxy = max(pnt1.y, pnt2.y);
                    /* Each line needs crlf at end, except for last.
                    A null is needed at the end of the buffer. */
                    int totsize = (maxx-minx+3)*(maxy-miny+1) + 1 -2;
                    HANDLE hGlobalMemory = GlobalAlloc(GHND, (DWORD) totsize);
                    char FAR *cp;

                    if (hGlobalMemory != NULL)  {
                        cp = GlobalLock(hGlobalMemory);
                        for (y = miny; y <= maxy; y++) {
                            for (x = minx; x <= maxx; x++)
                                *cp++ = *(screenbuf + getbufxy(x, y));
                            if (y == maxy) break;   /* TAA added 2/94 */
                            *cp++ = '\r';
                            *cp++ = '\n';
                        }
                        GlobalUnlock(hGlobalMemory);
                        OpenClipboard(hWnd);
                        EmptyClipboard();
                        SetClipboardData(CF_TEXT, hGlobalMemory);
                        CloseClipboard();
                    }
                    drawMarkedRegion();
                    inMarking = FALSE;
                }
                return 0;

                case M_CONTENTS:
                    WinHelp(hWnd, HELPNAME, HELP_CONTENTS, 0L);
                    return 0;
                case M_INDEX:
                    WinHelp(hWnd, HELPNAME, HELP_KEY, (long int)(char FAR *)"INDEX");
                    return 0;
                case M_SEARCH:
                    WinHelp(hWnd, HELPNAME, HELP_PARTIALKEY,
                            (long int)(char FAR *)"");
                    return 0;
                    
            }
            break;

        case WM_DROPFILES: /* Handle a dropped file */
        {
            int nfiles = DragQueryFile((HANDLE)wParam, -1, NULL, 0);
            DFLIST *temp;

            while (nfiles--) {
                temp = (DFLIST *)malloc(sizeof(DFLIST));
                DragQueryFile((HANDLE)wParam, nfiles, temp->fname, FNAMESIZE);
                temp->next = dropfile;
                dropfile = temp;
            }
            DragFinish((HANDLE)wParam);
            wParam = C_DROPFILE;
            goto wmchar;
        }

        case WM_KEYDOWN:    /* Handle special keys */
            switch (wParam) {
                case VK_HOME:
                    wParam = C_HOME;
                    break;
                case VK_END:
                    wParam = C_END;
                    break;
                case VK_LEFT:
                    wParam = C_LA;
                    break;
                case VK_RIGHT:
                    wParam = C_RA;
                    break;
                case VK_UP:
                    wParam = C_UA;
                    break;
                case VK_DOWN:
                    wParam = C_DA;
                    break;
                case VK_DELETE:
                    wParam = C_DEL;
                    break;
                default:
                    return DefWindowProc(dummy, message, wParam, lParam);
            };
            /* pass through to WM_CHAR for the chars we want! */

wmchar:
        case WM_CHAR: {
            int *cp = keyin+1;
#ifndef ANSI8   
            /* convert ANSI keystrokes to IBM ASCII */
            if ((wParam > 127) && (wParam < 256)) {
                unsigned char dummy[2];
                dummy[0] = wParam;
                AnsiToOemBuff(dummy, dummy, 1);
                wParam = dummy[0];
            }
#endif
            if (cp == &keybuf[KBSIZE]) cp = keybuf;
            if (cp != keyout) {
                *keyin = wParam;
                keyin = cp;
            }
                      }
                      return 0;

        case WM_SETFOCUS:
            inFocus = TRUE;
            if (inReadLoop) placeCaret();
            return 0;

        case WM_KILLFOCUS:
            if (inReadLoop) DestroyCaret();
            inFocus = FALSE;
            return 0;

        case WM_TIMER:
        {
            static long ofree;
            if (statusActive && ofree != nfree) {
                /* try to avoid update if nothing is happening */
                InvalidateRect(hStatusWnd, NULL, TRUE);
                UpdateWindow(hStatusWnd);
                ofree = nfree;
            }
            return 0;
        }
            
        case WM_CLOSE:
        {
            int i = MessageBox(hWnd, "Save Workspace before exit?",
                               szTitleName,
                               MB_DEFBUTTON2|MB_YESNOCANCEL|MB_ICONQUESTION);
            if (i == IDYES) saveImage(lastRestore[0] == '\0');
            WinHelp(hWnd, HELPNAME, HELP_QUIT, 0L);
            if (i != IDCANCEL) DestroyWindow(hWnd);
            return 0;
        }


        case WM_DESTROY:
            if (statusActive) KillTimer(hWnd, 1);
            PostQuitMessage(0);
            hWnd = 0;
            return 0;

    }   
    return DefWindowProc(dummy, message, wParam, lParam);
}


int keyhit(void) {  /* like the conio.h function kbhit */
    MSG msg;

    /* process any pending messages */
    while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE)) {
        if (msg.message == WM_QUIT) wrapup();
        if (!TranslateAccelerator(hWnd, hAccel, &msg)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }
    /* return TRUE if key in buffer */
    return (keyin != keyout);
}

static int getch(void) {   /* like the conio.h function */
    int val;
    if (!keyhit()) {
        MSG msg;
        
        /* no keystrokes are waiting, so turn on cursor and read */
        /* now wait for a key */
        inReadLoop = TRUE;
        if (inFocus) {
            placeCaret();
        }
        while (keyin == keyout) { /* loop on messages until we get a key */
            if (GetMessage(&msg, NULL, 0, 0)) { /* wait for a message */
                if (!TranslateAccelerator(hWnd, hAccel, &msg)) {
                    TranslateMessage(&msg);
                    DispatchMessage(&msg);
                }
            }
            else {  /* QUIT MESSAGE RECEIVED */
                wrapup();
            }
            if (direct && inPaste) { /* first character in paste operation */
                val = *pastchp++;
                if (*pastchp == '\0') {
                    MFREE(pastch);
                    inPaste = FALSE;
                }
                if (inFocus) DestroyCaret();
                inReadLoop = FALSE;
                return val;
            }
        }
        if (inFocus) {
            DestroyCaret();
        }
        inReadLoop = FALSE;
    }
    val = *keyout++;
    if (keyout == &keybuf[KBSIZE]) keyout = keybuf; /* wrap arout buffer */
    return val;
}
    

/* WHAT FOLLOWS IS WHAT YOU WOULD FIND IN EVERY *STUFF FILE, MODIFIED
   FOR THE WINDOWS ENVIRONMENT. */


#define LBSIZE (SCRNX-3)

char *stackbase;

/* local variables */
/* TAA mod 8/92 made unsigned for high ASCII support */
static unsigned char lbuf[LBSIZE];
static int lcount, lindex;

#ifdef TIMES
unsigned long kbdtime;
#endif

/* Command history */
#define HISTSIZE (20)
static unsigned char *history[HISTSIZE] = {NULL, NULL, NULL, NULL, NULL};
int curhist = -1;

/* forward declarations */
static void NEAR xinfo(void);
static void NEAR xflush(void);
static int  NEAR xgetc(void);
static void NEAR xputc(int ch);

#ifdef BIGNUMS
/* We need this because the one that Borland supplies doesn't work for
   exp values out of range */
unsigned char infp[8] = {0,0,0,0,0,0,0xf0, 0x7f};
unsigned char infn[8] = {0,0,0,0,0,0,0xf0, 0xff};

double myldexp(double val, int exp) {
    if (exp > DBL_MAX_EXP)
        return (val > 0 ? *(double *)&infp : *(double *)&infn);
    if (exp < DBL_MIN_EXP) return 0.0;
    return ldexp(val, exp);
}
#endif

#if defined(__TURBOC__) && (__TURBOC__ >= 0x400)
int CDECL _matherr(struct exception *er)    /* Borland renamed this function */
#else
int CDECL matherr(struct exception *er)
#endif
{
    char *emsg;

    switch (er->type) {
        case DOMAIN: emsg="domain"; break;
#ifdef BIGNUMS
        case OVERFLOW: return 1; /* xlmath3 will regularly overflow */
#else
        case OVERFLOW: emsg="overflow"; break;
#endif
        case PLOSS: case TLOSS: emsg="inaccurate"; break;
        case UNDERFLOW: return 1;
        default: emsg="????"; break;
    }
    xlerror(emsg,cvflonum(er->arg1));
    return 0; /* never happens */
}

/* osinit - initialize */

VOID osinit(banner)
  char *banner;
{
#ifdef STSZ
    stackbase = (char *)&banner;    /* find base of stack */
#endif
#ifdef TIMES
    kbdtime = real_tick_count();    /* initialize run time to zero */
#endif

    while (*banner) xputc(*banner++);
    xputc('\n');

    lposition = 0;
    lindex = 0;
    lcount = 0;

    /* let fp overflow pass and domain errors */
    _control87(EM_OVERFLOW|EM_INVALID,EM_OVERFLOW|EM_INVALID);
}

/* osfinish - clean up before returning to the operating system */
VOID osfinish()
{
    if (inFocus && inReadLoop) DestroyCaret();
    if (hWnd) DestroyWindow(hWnd);
    if (ourBrush)
        DeleteObject(ourBrush);
#ifdef GRAPHICS
    if (ourPen)
        DeleteObject(ourPen);
#endif
#ifdef ANSI8
    if (ourFont)
        DeleteObject(ourFont);
#endif
}

/* xoserror - print an error message */
VOID xoserror(msg)
  char *msg;
{
    MessageBox(hWnd, msg, "FATAL ERROR", MB_OK | MB_ICONSTOP);
}

/* osrand - return next random number in sequence */
long osrand(rseed)
  long rseed;
{
    long k1;

    /* make sure we don't get stuck at zero */
    if (rseed == 0L) rseed = 1L;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
        rseed += 2147483647L;

    /* return a random number between 0 and MAXFIX */
    return rseed;
}

extern void gc(void);

int truename(char *name, char *rname)
{
    union REGS regs;
    int i;
    char *cp;
    int drive;          /* drive letter */
    char pathbuf[FNAMEMAX+1];   /* copy of path part of name */
    char curdir[FNAMEMAX+1];    /* current directory of drive */
    char *fname;        /* pointer to file name part of name */
    
    /* use backslashes consistantly */
    
    for (cp = name; (cp = strchr(cp, '/')) != NULL; *cp = '\\') ;
    
    /* parse any drive specifier */

    if ((cp = strrchr(name, ':')) != NULL) {
        if (cp != name+1 || !isalpha(*name)) return FALSE;
        drive = toupper(*name);
        name = cp+1;            /* name now excludes drivespec */
    }
    else {
        regs.h.ah = 0x19;   /* get current disk */
        intdos(&regs, &regs);
        drive = regs.h.al + 'A';
    }
    
    /* check for absolute path (good news!) */
    
    if (*name == '\\') {
        sprintf(rname,"%c:%s",drive,name);
    }
    else {
        strcpy(pathbuf, name);
        if ((cp = strrchr(pathbuf, '\\')) != NULL) {    /* path present */
            cp[1] = 0;
            fname = strrchr(name, '\\') + 1;
        }
        else {
            pathbuf[0] = 0;
            fname = name;
        }

        /* get the current directory of the selected drive */
        
        regs.h.ah = 0x47;
        regs.h.dl = drive + 1 - 'A';
        regs.x.si = (unsigned) curdir;
        intdos(&regs, &regs);
        if (regs.x.cflag != 0) return FALSE;    /* invalid drive */

        /* peel off "..\"s */
        while (strncmp(pathbuf, "..\\", 3) == 0) {
            if (*curdir == 0) return FALSE;     /* already at root */
            strcpy(pathbuf, pathbuf+3);
            if ((cp=strrchr(curdir, '\\')) != NULL)
                *cp = 0;    /* peel one depth of directories */
            else
                *curdir = 0;    /* peeled back to root */
        }
        
        /* allow for a ".\" */
        if (strncmp(pathbuf, ".\\", 2) == 0)
            strcpy(pathbuf, pathbuf+2);
        
        /* final name is drive:\curdir\pathbuf\fname */

        if (strlen(pathbuf)+strlen(curdir)+strlen(fname)+4 > FNAMEMAX) 
            return FALSE;
        
        if (*curdir)
            sprintf(rname, "%c:\\%s\\%s%s", drive, curdir, pathbuf, fname);
        else
            sprintf(rname, "%c:\\%s%s", drive, pathbuf, fname);
    }
    
    /* lowercase the whole string */

    for (cp = rname; (i = *cp) != 0; cp++) {
        if (isupper(i)) *cp = tolower(i);
    }
    
    return TRUE;
}

LOCAL int NEAR getslot(VOID)
{
    int i=0;

    for (; i < FTABSIZE; i++)   /* look for available slot */
        if (filetab[i].fp == NULL) return i;

    gc();   /* is this safe??????? */

    for (i=0; i < FTABSIZE; i++) /* try again -- maybe one has been freed */
        if (filetab[i].fp == NULL) return i;

    xlfail("too many open files");

    return 0;   /* never returns */
}


FILEP osaopen(const char *name, const char *mode)
{
    int i=getslot();
    char namebuf[FNAMEMAX+1];
    FILE *fp;

    if (!truename((char *)name, namebuf))
        strcpy(namebuf, name);  /* should not happen */

    if ((filetab[i].tname = malloc(strlen(namebuf)+1)) == NULL) {
        xlfail("insufficient memory");
    }
    
    
    if ((fp = fopen(name,mode)) == NULL) {
        free(filetab[i].tname);
        return CLOSED;
    }

    filetab[i].fp = fp;

    strcpy(filetab[i].tname, namebuf);

    /* calculate mode to re-open file */
    if (mode[0]=='w') {
        strcpy(filetab[i].reopenmode, "r+");
        if (mode[strlen(mode)-1]=='b') strcat(filetab[i].reopenmode, "b");
    }
    else strcpy(filetab[i].reopenmode, mode);

    return i;
}


FILEP osbopen(const char *name, const char *mode)
{
    char bmode[10];

    strcpy(bmode,mode); strcat(bmode,"b");  

    return osaopen(name, bmode);
}

VOID osclose(FILEP f)
{
    fclose(filetab[f].fp);
    free(filetab[f].tname);
    filetab[f].tname = NULL;
    filetab[f].fp = NULL;
}

#ifdef PATHNAMES
/* ospopen - open for reading using a search path */
FILEP ospopen(char *name, int ascii)
{
    FILEP fp;
    char pathlist[128];
    char *path = pathlist;
    char *newnamep;
    char ch;
    char newname[256];

    GetPrivateProfileString(section, "FilePath", getenv(PATHNAMES),
                            pathlist, sizeof(pathlist), iniName);

    /* don't do a thing if user specifies explicit path */
    if (strchr(name,'/') != NULL && strchr(name, '\\') != NULL)
        return (ascii? osaopen: osbopen)(name,"r");

    do {
        if (*path == '\0')  /* no more paths to check */
            /* check current directory just in case */
            return (ascii? osaopen: osbopen)(name,"r");

        newnamep = newname;
        while ((ch=*path++) != '\0' && ch != ';' && ch != ' ')
            *newnamep++ = ch;

        if (ch == '\0') path--;

        if (newnamep != newname &&
            *(newnamep-1) != '/' && *(newnamep-1) != '\\')
            *newnamep++ = '/';  /* final path separator needed */
        *newnamep = '\0';

        strcat(newname, name);
        fp = (ascii? osaopen: osbopen)(newname,"r");
    } while (fp == CLOSED); /* not yet found */

    return fp;
}
#endif

/* rename argument file as backup, return success name */
/* For new systems -- if cannot do it, just return TRUE! */

int renamebackup(char *filename) {
    char *bufp, ch=0;

    strcpy(buf, filename);  /* make copy with .bak extension */

    bufp = &buf[strlen(buf)];   /* point to terminator */
    while (bufp > buf && (ch = *--bufp) != '.' && ch != '/' && ch != '\\') ;


    if (ch == '.') strcpy(bufp, ".bak");
    else strcat(buf, ".bak");

    remove(buf);

    return !rename(filename, buf);
}

#define beep() MessageBeep(0)

/* listmatches -- list interned symbols that match to current symbol name */

static void searchobarray(LVAL array, unsigned char *st, int l, int append,
                          int lpos, int *matchcount, LVAL *firstmatch,
                          int *endpos)
{
    unsigned char target[STRMAX+1];
    LVAL sym;       /* obarray accessing */
    int index;      /* obarray index */
    int i;
    unsigned char FAR *stp;  /* string in obarray */
    unsigned char c1;
    int j;
#ifdef READTABLECASE
    LVAL rtcase = getvalue(s_rtcase);
    int low=0, up=0;
    unsigned char *targ;
#endif

    for (i=0; i <= l; i++) {
        c1 = st[i];
#ifdef READTABLECASE
        if (rtcase==k_invert) target[i]=ISLOWER(c1) ? (low++, TOUPPER(c1)):
                                        (ISUPPER(c1) ? (up++, TOLOWER(c1)):c1);
        else if (rtcase==k_downcase) target[i]= ISUPPER(c1) ? TOLOWER(c1) : c1;
        else if (rtcase==k_preserve) target[i] = c1;
        else target[i]= ISLOWER(c1) ? TOUPPER(c1) : c1;
#else
        target[i] = ISLOWER(c1) ? TOUPPER(c1) : c1;
#endif
    }
#ifdef READTABLECASE
    if (low>0 && up>0) { /* invert -- but not inverted! */
        rtcase=k_preserve;
        strcpy((char *)target, (char *)st);
    }
#endif

    for (index = HSIZE; index-- > 0; ) {
        for (sym = getelement(array, index); !null(sym); sym = cdr(sym)) {
            stp = (unsigned char FAR *)getstring(getpname(car(sym)));
#ifdef READTABLECASE
            targ = target;
            if (rtcase==k_invert && (stp[0]==target[0]||stp[0]==st[0])) {
                /* must check for mixed case table entry */
                up=0; low=0;
                for (i=0; (c1 = stp[i]) != '\0' ; i++) {
                    if (ISUPPER(c1)) {
                        up++;
                        if (low>0) {targ=st; break;}
                    }
                    if (ISLOWER(c1)) {
                        low++;
                        if (up>0) {targ=st; break;}
                    }
                }
            }
            for (i = 0; stp[i] == targ[i]; i++) {
#else
            for (i = 0; stp[i] == target[i]; i++) {
#endif
                if (i == l) {
                    if (append) {
                        j = 0;
                        while (TRUE) {
                            c1 = stp[++i];
#ifdef READTABLECASE
                            if (rtcase==k_invert) {
                                if(!(up>0 && low>0)) {
                                    if (ISUPPER(c1)) c1 = TOLOWER(c1);
                                    else if (ISLOWER(c1)) c1 = TOUPPER(c1);
                                }
                            }
                            else if (rtcase!=k_preserve) {
                                if (ISUPPER(c1)) c1 = TOLOWER(c1);
                            }
#else
                            if (ISUPPER(c1)) c1 = TOLOWER(c1);
#endif                          
                            if (*matchcount==0) {
                                lbuf[lpos+j] = c1;
                            }
                            else if (lbuf[lpos+j] != c1) break;
                            if (c1 == '\0' || lpos+j >= LBSIZE-1) break;
                            j++;
                        }
                        lbuf[lpos+j] = ' '; /* put space in buffer */
                        *endpos = lpos+j;
                    }
                    if (++(*matchcount)==1 && append)
                        *firstmatch = car(sym);
                    else {
                        /* first to print? Start new line */
                        if (*matchcount==1 || (*matchcount==2 && append)) {
                            xputc('\n');
                        }
                        if (*matchcount==2 && append) {
                            xlprint(getvalue(s_debugio), *firstmatch, TRUE);
                            xputc(' ');
                            lposition++;
                        }
                        STRCPY(buf, (char FAR *)stp);
                        if (lposition + strlen(buf) > 77) {
                            lposition = 0;
                            xputc('\n');
                        }
                        xlprint(getvalue(s_debugio), car(sym), TRUE);
                        xputc(' ');
                        lposition++;
                    }
                    break;
                }
            }
        }
    }
}   

static void listmatches(void)
{
    unsigned char *st = &lbuf[lposition-1];     /* string to match */
    int l=0;        /* string length */
    int lpossave = lposition;   /* save to restore later */
    int matchcount = 0;     /* number of matches */
    int matchend = lposition;   /* end of match */
    int append = lposition == lcount; /* append partial matches */
    LVAL firstmatch;
#ifdef PACKAGES
    int i;
    unsigned char *st2 = NULL;
    int external_only = FALSE;
    LVAL pack = getvalue(s_package);
#endif
    
    if (lposition == 0) return; /* no string */

    /* find string start */
    while (l < lposition && !strchr(" \"\'`()\\|", *st)) {
#ifdef PACKAGES
        if (*st == ':') {
            if (st2 == st+1)  /* two in a row */
                external_only = FALSE;
            else
                external_only = TRUE;
            st2 = st;
        }
#endif
        st--;
        l++;
    }

    if (l == 0) return; /* no string */

    st++;
    l--;

#ifdef PACKAGES
    /* check for package name */
    if (st2 != NULL) {
        if (st == st2)
            pack = xlkeypack;   /* zero length name */
        else {
            i=st2-st;
            buf[i] = 0;
            /* cheat and force uppercase */
            while (i-- > 0)
                buf[i] = (ISLOWER(st[i])? TOUPPER(st[i]) : st[i]);
            pack = xlfindpackage(buf);
            if (!packagep(pack))
                return; /* no package of that name */
        }
        if (*(++st2) == ':') st2++; /* start of target string */
        l -= (st2-st);
        st = st2;
        if (l < 0) return; /* no string */
    }
#endif

    lposition = 0;
#ifdef PACKAGES
    searchobarray(getextsyms(pack), st, l, append, lpossave, &matchcount, &firstmatch, &matchend);
    if (!external_only) {
        searchobarray(getintsyms(pack), st, l, append, lpossave, &matchcount, &firstmatch, &matchend);
        pack = getuses(pack);   /* check out imports */
        for (; consp(pack); pack = cdr(pack))
            searchobarray(getextsyms(car(pack)), st, l, append, lpossave, &matchcount, &firstmatch, &matchend);
    }
#else
    searchobarray(getvalue(obarray), st, l, append, lpossave, &matchcount, &firstmatch, &matchend);
#endif
    if (append) lposition = lcount = matchend;
    else lposition = lpossave;
    if (matchcount > 1) beep(); /* signal multiple matches */
    if (matchcount > 1 || !append) {
        xputc('\n');
    }
    else {
        for (l=lpossave; l-- >0; ) xputc(8);
        /* appending and not multiple matches -- overwrite */
    }
}


/* flash matching quote character */
static void showmatchquote(char ch) {
    char matchfor = (ch == ')' ? '(' : ch);
    char thisch;
    int origx = cursor.x;
    int origy = cursor.y;
    int x = origx - 2, y, nestcount = 0;
    
    for (y = origy; y >= 0; y--) {
        for (; x >= 0 ; x--) {
            thisch = *(screenbuf + getbufxy(x, y));
            if (thisch == matchfor) {
                if (nestcount-- == 0) {
                    /* move caret for 1 second or until key hit */
                    long time = real_tick_count() + 1000;
                    cursor.x = x;
                    cursor.y = y;
                    placeCaret();
                    while (!keyhit() && time > real_tick_count()) ;
                    cursor.x = origx;
                    cursor.y = origy;
                    DestroyCaret();
                    return;
                }
            }
            else if (thisch == ch) /* increase nesting level */
                nestcount++;
        }
        x = SCRNX-1;
    }
    /* no match */
}

/* common completion code */
static void finishOstgetc(void)
{
    int i;

    DragAcceptFiles(hWnd, FALSE);
    
    direct = FALSE;

    /* reopen closed files */
    for (i=3; i < FTABSIZE; i++)
        if (filetab[i].fp != NULL) {
            filetab[i].fp = fopen(filetab[i].tname, filetab[i].reopenmode);
            fseek(filetab[i].fp, filetab[i].filepos, SEEK_SET);
        };

    /* make things like we are doing something */
    SetCursor(waitCursor);
}   

/* ostgetc - get a character from the terminal */
int ostgetc()
{
    int lmargin;
    int ch;
    int i;

    /* check for a buffered character */
    if (lcount-- > 0)
        return (lbuf[lindex++]);

close_and_try:
    while (dropfile)    { /* must clean up this list */
        DFLIST *temp = dropfile->next;
        free(dropfile);
        dropfile = temp;
    }
        
    /* get an input line, after some Windows preparations */
    /* close all files */
    for (i=3; i < FTABSIZE; i++)
        if (filetab[i].fp != NULL) {
            filetab[i].filepos = ftell(filetab[i].fp);
            fclose(filetab[i].fp);
        };
    if (inFocus) SetCursor(arrowCursor);
    DragAcceptFiles(hWnd, TRUE);
try_again:
    flushbuf();
    direct = TRUE;  /* no buffering now */
    lmargin = cursor.x;
    for (lcount = 0, lposition=0, lbuf[0] = '\0'; ; ) {
        if (inPaste) {          /* handle pasting */
            ch = *pastchp++;
            if (*pastchp == '\0') {
                MFREE(pastch);
                inPaste = FALSE;
            }
        }
        else ch = xgetc();
        switch (ch) {
        case C_TAB:  /* list matches */
            listmatches();
            cursor.x = lmargin;
            for (lindex = 0; lindex < lcount;)
                xputc(lbuf[lindex++]);
            cursor.x = lposition+lmargin;
            break;
        case '\n':  /* ignore newline */
            break;
        case '\r':  /* end of line */
            lbuf[lcount] = 0;
            if (history[0] == NULL || 
                strcmp((char *)history[curhist < 0?0:curhist], 
                       (char *)lbuf) != 0) {
                if (lcount > 0) {
                    /* New line to put in history buffer */
                    if (history[HISTSIZE-1] != NULL) 
                        free(history[HISTSIZE-1]);
                    memmove(&history[1], &history[0], 
                            (HISTSIZE-1)*sizeof(char *));
                    history[0] = (unsigned char *)malloc(lcount+1);
                    strcpy((char *)history[0], (char *)lbuf);
                    curhist = -1;   /* reset lookup pointer */
                }
            }
            else if (curhist >= 0) curhist--; /* TAA MOD 4/95 */
            lbuf[lcount] = '\n';
            xputc('\n');
            lposition = 0;
            lindex = 0;
            finishOstgetc();
            if (tfp!=CLOSED) OSWRITE(lbuf,1,lcount+1,tfp);
            return (lbuf[lindex++]);
        case C_BS: /* backspace -- delete to right of cursor */
            if (lposition != 0) {
                lcount--;
                lposition--;
                strcpy((char *)&lbuf[lposition], (char *)&lbuf[lposition+1]);
                xputc(8);   /* back up */
                for (lindex = lposition; lindex < lcount; )
                    xputc(lbuf[lindex++]);
                xputc(' '); /* "erase" at end of line */
                cursor.x = lposition+lmargin;
            }
            else beep();
            break;
        case C_DEL: /* Delete -- delete at cursor */
            if (lcount > lposition) {
                lcount--;
                strcpy((char *)&lbuf[lposition], (char *)&lbuf[lposition+1]);
                for (lindex = lposition; lindex < lcount; )
                    xputc(lbuf[lindex++]);
                xputc(' '); /* "erase" at end of line */
                cursor.x = lposition+lmargin;
            }
            else beep();
            break;
        case C_LA: /* Left arrow */
            if (lposition != 0) {
                lposition--;
                cursor.x = lposition+lmargin;
            }
            else beep();
            break;
        case C_RA: /* Right arrow */
            if (lposition < lcount) {
                cursor.x = ++lposition + lmargin;
            }
            else
                beep();
            break;
        case C_HOME: /* Home -- goto beginning of line */
            lposition = 0;
            cursor.x = lmargin;
            break;
        case C_END: /* End -- goto end of line */
            cursor.x = (lposition = lcount) + lmargin;
            break;
        case C_UA: /* Up arrow -- goto previous line */
            if (curhist != HISTSIZE-1 && history[curhist+1] != NULL) {
                curhist++;
                goto join_downarrow;
            }
            beep();
            break;
        case C_DA: /* Down arrow -- goto next line */
            if (lcount > 0 || curhist < 0) { /* empty line in history --
            restore current line */
                if (curhist <= 0) { /* bumped limit */
                    beep();
                    break;
                }
                curhist--;  /* restore next line */
            }
        join_downarrow:
            strcpy((char *)lbuf, (char *)history[curhist]);
            if (strlen((char *)lbuf) < lcount) {
                /* old line longer -- must "erase" it */
                lindex = lposition - strlen((char *)lbuf);
                while (lindex-- > 0) {
                    xputc(8);
                    lposition--;
                }
                for (lindex = lcount-lposition ; lindex-- > 0; )
                    xputc(' ');
                lposition = lcount;
            }
            for (lindex = lposition; lindex-- > 0; )
                xputc(8);
            lcount = lposition = strlen((char *)lbuf);
            for (lindex = 0; lindex < lposition;)
                xputc(lbuf[lindex++]);
            break;
        case C_TOPLEV:      /* control-c */
            xflush();
            finishOstgetc();
            xltoplevel();
        case C_CLEAN:       /* control-g */
            xflush();
            finishOstgetc();
            xlcleanup();
        case C_CONT:        /* control-p */
            xflush();
            finishOstgetc();
            xlcontinue();
        case C_EOF:     /* control-z */
            if (xldebug) {
                xflush();
                finishOstgetc();
                return (EOF);
            }
            break;
        case C_ESC: /* ESCAPE */
            cursor.x = lmargin; 
            _fmemset(screenbuf+getbufxy(lmargin, cursor.y), ' ', SCRNX-lmargin);
            updateCurrent(lmargin, SCRNX);
            goto try_again;
        case C_STATUS:  /* control-t */
            xinfo();        
        case C_BREAK:  /* ignore break */
            break;
        case C_DROPFILE:
            cursor.x = lmargin;  /* erase current line */
            _fmemset(screenbuf+getbufxy(lmargin, cursor.y), ' ', SCRNX-lmargin);
            updateCurrent(lmargin, SCRNX);
            osflush();
            finishOstgetc();
            loadDroppedFiles();
            goto close_and_try;
        case C_OPEN:
            cursor.x = lmargin;  /* erase current line */
            _fmemset(screenbuf+getbufxy(lmargin, cursor.y), ' ', SCRNX-lmargin);
            updateCurrent(lmargin, SCRNX);
            osflush();
            finishOstgetc();
            loadAFile();
            goto close_and_try;
        case C_RESTORE:
            cursor.x = lmargin; /* erase current line */
            _fmemset(screenbuf+getbufxy(lmargin, cursor.y), ' ', SCRNX-lmargin);
            updateCurrent(lmargin, SCRNX);
            osflush();  /* toss characters in case of success */
            finishOstgetc();
            restoreImage(); /* normally doesn't return */
            goto close_and_try;
        case C_SAVE:
        case C_SAVEAS:
            saveImage(ch==C_SAVEAS || lastRestore[0] == '\0');
            break;
        case C_DRIBBLE:
            cursor.x = lmargin; /* erase current line */
            _fmemset(screenbuf+getbufxy(lmargin, cursor.y), ' ', SCRNX-lmargin);
            updateCurrent(lmargin, SCRNX);
            osflush();  /* toss characters in case of success */
            finishOstgetc();
            dribbleFile();
            goto close_and_try;

        default:
            if (ch >= 0x20 && ch < 0x100 && lcount < LBSIZE-1) {
                memmove(&lbuf[lposition+1], &lbuf[lposition], 
                        lcount-lposition + 1);
                lbuf[lposition] = ch;
                xputc(ch); 
                lposition++;
                lcount++;
                for (lindex = lposition; lindex < lcount;)
                    xputc(lbuf[lindex++]);
                cursor.x = lposition+lmargin;
                if ((ch == ')' || ch == '"') && !inPaste)
                    showmatchquote(ch);
            }
            else beep();
        } /* end of character case statement and for loop */
    }
}

/* ostputc - put a character to the terminal */
VOID ostputc(ch)
  int ch;
{
    /* check for control characters */
    oscheck();

    /* output the character */
    if (ch == '\t')
        do { xputc(' '); } while (++lposition & 7);
    else {  /*TAA fix 2/94, bad logic caused lposition=1 after newline */
        xputc(ch);
        if (ch == '\n')
            lposition = 0;
        else
            lposition++;
   }

   /* output the character to the transcript file */
   if (tfp!=CLOSED)
        OSPUTC(ch,tfp);
}

/* osflush - flush the terminal input buffer */
VOID osflush()
{
    lindex = lcount = 0;
}

/* oscheck - check for control characters during execution */
VOID oscheck()
{
    int ch;

    if (keyhit() && (ch = getch()) != 0)
        switch (ch) {
        case C_BREAK:   /* control-b */
            xflush();
            xlbreak("**BREAK**",s_unbound);
            break;
        case C_TOPLEV:  /* control-c */
            xflush();
            xltoplevel();
            break;
        case C_PAUSE:   /* control-s */
            xgetc();    /* paused -- get character and toss */
            break;
        case C_STATUS:  /* control-t */
            xinfo();
            break;
        }
}

/* xinfo - show information on control-t */
static VOID NEAR xinfo()
{
#ifdef STSZ
    int i;
    sprintf(buf,
            "\n[ Free: %ld, Total: %ld, GC calls: %ld,\n"
            "  Edepth: %d, Adepth %d, Sdepth: %d ]",
            nfree, total, gccalls, xlstack-xlstkbase,
            xlargstktop-xlsp, STACKREPORT(i));
#else
    sprintf(buf,
            "\n[ Free: %ld, Total: %ld, GC calls: %ld,\n"
            "  Edepth: %d, Adepth %d ]",
            nfree, total, gccalls, xlstack-xlstkbase,
            xlargstktop-xlsp);
#endif
    errputstr(buf);

    flushbuf();
}

/* xflush - flush the input line buffer and start a new line */
static VOID NEAR xflush()
{
    if (inPaste) {  /* end the pasting */
        MFREE(pastch);
        inPaste = FALSE;
    }
    osflush();
    ostputc('\n');
}

/* xgetc - get a character from the terminal without echo */
/* TAA MOD 12/92 to handle extended keys (return 256+code) */

static int NEAR xgetc()
{
    int ch;
#ifdef TIMES
    unsigned long kbtime = real_tick_count();
#endif
    flushbuf();

    ch = getch();

#ifdef TIMES
    kbdtime += real_tick_count() - kbtime;
#endif
    return ch;
}

/* xputc - put a character to the terminal */
static void NEAR xputc(ch)
  char ch;
{
    *outbufp++ = ch;
    if (direct || ch == '\n' || outbufp == &outbuf[CHBSIZE])
            flushbuf(); /* flush on each line or full buffer*/
}

/* xsystem - execute a system command */
LVAL xsystem()
{
    int ok;
    if (moreargs()) {
        MEMCPY(buf, getstring(xlgastring()), STRMAX);
        xllastarg();
    }
    else {
        strcpy(buf, "dosprmpt.pif");
    }
    ok = WinExec(buf, SW_SHOWNORMAL);
    return (ok > 32 ? s_true : cvfixnum((FIXTYPE)errno));
}

/* xgetkey - get a key from the keyboard */
LVAL xgetkey()
{
    xllastarg();
    return (cvfixnum((FIXTYPE)xgetc()));
}


/* ossymbols - enter os specific symbols */
VOID ossymbols()
{
}

#ifdef TIMES
/* We want to cheat here because ticks_per_second would have to be rounded */

#define OURTICKS 1000

unsigned long ticks_per_second() {
    return((unsigned long) OURTICKS);
}

unsigned long run_tick_count()
{                               /*Real time in MSDOS*/
  return(((unsigned long) ((OURTICKS/CLK_TCK)*clock())) - kbdtime);
}

unsigned long real_tick_count()
{                               /* Real time */
  return((unsigned long) ((OURTICKS/CLK_TCK)*clock()));
}


LVAL xtime()
{
    LVAL expr,result;
    unsigned long tm;

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    tm = run_tick_count();
    result = xleval(expr);
    tm = run_tick_count() - tm;
    sprintf(buf, "The evaluation took %.2f seconds.\n",
            ((double)tm) / ticks_per_second());
    trcputstr(buf);

    flushbuf();

    return(result);
}


LVAL xruntime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) run_tick_count()));
}

LVAL xrealtime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) real_tick_count()));
}

#endif

#ifdef GRAPHICS
int xormode = FALSE;    /* XOR mode in drawing */
int xpos = 0, ypos = 0;

/* function goto-xy which set/obtains cursor position */
LVAL xgotoxy()
{
    LVAL oldpos;
    int x,y;
    flushbuf();

    oldpos = cons(cvfixnum((FIXTYPE)cursor.x),
                  cons(cvfixnum((FIXTYPE)cursor.y),NIL));

    if (moreargs()) {
        x = (int)getfixnum(xlgafixnum());
        y = (int)getfixnum(xlgafixnum());
        xllastarg();
        cursor.x = max(0, min(x, SCRNX-1));
        cursor.y = max(0, min(y, SCRNY-1));       
        lposition = cursor.x;
    }

    return oldpos;
}

LVAL xcls() /* clear the screen */
{
    flushbuf();
    lposition = 0;

    cursor.x = cursor.y = 0;
    _fmemset(screenbuf, ' ', SCRNX*SCRNY);  /* clear virtual display */
    first = 0;
    InvalidateRect(hWnd, NULL, TRUE);
    UpdateWindow(hWnd);
    return NIL;
}

LVAL xcleol()   /* clear to end of line */
{
    flushbuf();

    _fmemset(screenbuf + getbufxy(cursor.x, cursor.y), ' ', SCRNX-cursor.x);
    updateCurrent(cursor.x, SCRNX);
    return NIL;
}

LVAL xmode()
{
    LVAL res;
    /* ignore all arguments */
    while (moreargs()) (void)xlgetarg();

    /* return character area size and graphic maximum coordinates */
    xlsave1(res);

    res = consa(cvfixnum((FIXTYPE)maxY));
    res = cons(cvfixnum((FIXTYPE)maxX), res);
    res = cons(cvfixnum((FIXTYPE)SCRNY), res);
    res = cons(cvfixnum((FIXTYPE)SCRNX), res);

    xlpop();

    return res;
}

static COLORREF colorMap[16] = {
    PALETTERGB(0,0,0),                  /* These choices work somehow */
    PALETTERGB(0,0,128),
    PALETTERGB(0,128,0),
    PALETTERGB(0,128,128),
    PALETTERGB(128,0,0),
    PALETTERGB(128,0,128),
    PALETTERGB(128,128,0),
    PALETTERGB(192,192,192),
    PALETTERGB(128,128,128),
    PALETTERGB(96,96,255),
    PALETTERGB(0,255,0),
    PALETTERGB(0,255,255),
    PALETTERGB(255,0,0),
    PALETTERGB(255,0,255),
    PALETTERGB(255,255,0),
    PALETTERGB(255,255,255)};
    

LVAL xcolor()
{
    LVAL arg;
    FIXTYPE r, g, b, r2, g2, b2;

    flushbuf();

    arg = xlgafixnum();
    if (moreargs()) {
        r = getfixnum(arg);
        g = getfixnum(xlgafixnum());
        b = getfixnum(xlgafixnum());
        if (moreargs()) {   /* set background */
            r2 = getfixnum(xlgafixnum());
            g2 = getfixnum(xlgafixnum());
            b2 = getfixnum(xlgafixnum());
            xllastarg();
            bkgColor = PALETTERGB((int)max(0, min(r2, 255)),
                                  (int)max(0, min(g2, 255)),
                                  (int)max(0, min(b2, 255)));
        }
        xormode = (int)(r & 256);
        arg = s_true;
        charColor = PALETTERGB((int)max(0, min(r & ~256, 255)),
                               (int)max(0, min(g, 255)),
                               (int)max(0, min(b, 255)));

    }
    else {
        int v = (int)getfixnum(arg);
        xormode = (v&128) != 0;
        charColor = colorMap[v & 15];   /* support like a 16 color mode */
        bkgColor = colorMap[((v >> 4) & 7) + ((v >> 5) & 8)];
                /* and 16 background colors. Bright background comes
                   from adding 256 to color */
    }

    DeleteObject(ourPen);
    ourPen = CreatePen(PS_SOLID, 0, charColor);
    DeleteObject(ourBrush);
    ourBrush = CreateSolidBrush(bkgColor);
    SetClassWord(hWnd, GCW_HBRBACKGROUND, (WORD)ourBrush);

    return arg;
}

void draw(int x, int y) {
    DC = GetDC(hWnd);
    SelectObject(DC, ourPen);
    if (xormode) SetROP2(DC, R2_XORPEN);
    MoveTo(DC, xpos, maxY - ypos);
    LineTo(DC, x, maxY - y);
    ReleaseDC(hWnd, DC);
}


LVAL xdraw()
{
    LVAL arg;
    int newx, newy;

    while (moreargs()) {
        arg = xlgafixnum();
        newx = (int) getfixnum(arg);

        arg = xlgafixnum();
        newy = (int) getfixnum(arg);

        draw(newx,newy);

        xpos = newx;
        ypos = newy;
    }
    return s_true;
}

/* xdrawrel -- absolute draw */

LVAL xdrawrel()
{
    LVAL arg;
    int newx, newy;

    while (moreargs()) {
        arg = xlgafixnum();
        newx = xpos + (int) getfixnum(arg);

        arg = xlgafixnum();
        newy = ypos + (int) getfixnum(arg);

        draw(newx,newy);

        xpos = newx;
        ypos = newy;
    }
    return s_true;
}

/* xmove -- absolute move, then draw */

LVAL xmove()
{
    LVAL arg;

    arg = xlgafixnum();
    xpos = (int) getfixnum(arg);

    arg = xlgafixnum();
    ypos = (int) getfixnum(arg);

    
    return (xdraw());
}

/* xmoverel -- relative move */

LVAL xmoverel()
{
    LVAL arg;

    arg = xlgafixnum();
    xpos += (int) getfixnum(arg);

    arg = xlgafixnum();
    ypos += (int) getfixnum(arg);

    return (xdrawrel());
}

#endif
