/* osdefs.h - system specific function declarations */

extern LVAL xsystem(V);

#if !(defined(UNIX)||defined(AMIGA)||defined(__SASC__))
extern LVAL xgetkey(V);
#endif

#ifdef GRAPHICS
extern LVAL xmode(V), xcolor(V), xmove(V), xdraw(V),
    xmoverel(V), xdrawrel(V);
extern LVAL xcls(V), xcleol(V), xgotoxy(V);
#ifdef USEQT
extern LVAL xrect(V), xellipse(V), xfont(V), xbrush(V);
#endif
#endif

#if defined(UNIX) || defined(MACOSX) || defined(LINUX)
extern LVAL Prim_POPEN(V), Prim_PCLOSE(V);
#endif
