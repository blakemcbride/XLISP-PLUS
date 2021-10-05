/* osptrs.h - system specific function pointers */


{   "SYSTEM",   S,  xsystem },

#if !(defined(UNIX)||defined(AMIGA)||defined(__SASC__))
{   "GET-KEY",  S,  xgetkey },
#endif

#ifdef GRAPHICS
{   "CLS",      S,  xcls    },
{   "GOTO-XY",  S,  xgotoxy },
{   "CLEOL",    S,  xcleol  },
{   "MODE",     S,  xmode   },
{   "COLOR",    S,  xcolor  },
{   "MOVE",     S,  xmove   },
{   "DRAW",     S,  xdraw   },
{   "MOVEREL",  S,  xmoverel},
{   "DRAWREL",  S,  xdrawrel},
#ifdef USEQT
{   "DRAWRECT", S,  xrect   },
{   "DRAWELLIPSE", S, xellipse},
{   "FONT",     S,  xfont   },
{   "BRUSH",    S,  xbrush  },
#endif
#endif

#if defined(UNIX)||defined(MACOSX)
{   "POPEN",    S,  Prim_POPEN},
{   "PCLOSE",   S,  Prim_PCLOSE},
#endif
