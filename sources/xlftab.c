/* xlftab.c - xlisp function table */
/*  Copyright (c) 1985, by David Michael Betz
    All Rights Reserved
    Permission is granted for unrestricted non-commercial use   */

#include "xlisp.h"

/* include system dependant definitions */
#include "osdefs.h"

#ifdef COMPILER
#include "cominc.h"
#endif

/* SUBR/FSUBR indicator */
#define S   SUBR
#define F   FSUBR
#ifdef MULVALS
/* extra codes for subrs that return multiple values */
#define SM  (SUBR+ TYPEFIELD+1)
#define FM  (FSUBR+ TYPEFIELD+1)
#else
#define SM  SUBR
#define FM  FSUBR
#endif

/* xnotimp - function table entries that are currently not implemented */
#ifdef ANSI
LOCAL LVAL xnotimp(void)
#else
LOCAL LVAL xnotimp()
#endif
{
    xlfail("function not implemented");
    return NIL;
}


/* the function table */
FUNDEF funtab[] = {
/* DO NOT ALTER ENTRIES UNTIL AFTER OBPRIN1 */
    /* read macro functions */
{   NULL,                       S, rmhash       },
{   NULL,                       S, rmquote      },
{   NULL,                       S, rmdquote     },
{   NULL,                       S, rmbquote     },
{   NULL,                       S, rmcomma      },
{   NULL,                       S, rmlpar       },
{   NULL,                       S, rmrpar       },
{   NULL,                       S, rmsemi       },
{   NULL,                       S, xnotimp      },
{   NULL,                       S, xnotimp      },

    /* methods */
{   NULL,                       S, clnew        },
{   NULL,                       S, clisnew      },
{   NULL,                       S, clanswer     },
{   NULL,                       S, obisnew      },
{   NULL,                       S, obclass      },
{   NULL,                       S, obshow       },
{   NULL,                       S, obprin1      },

/* Empty slots not needed beyond this point */

    /* evaluator functions */
{   "EVAL",                     SM, xeval       },
{   "APPLY",                    SM, xapply      },
{   "FUNCALL",                  SM, xfuncall    },
{   "QUOTE",                    F, xquote       },
{   "IDENTITY",                 S, xquote       },/*IDENTITY is same as QUOTE*/
{   "FUNCTION",                 F, xfunction    },
{   "COMPLEMENT",               S, xcomplement  },
{   "BACKQUOTE",                F, xbquote      },
{   "LAMBDA",                   F, xlambda      },

    /* symbol functions */
{   "SET",                      S, xset         },
{   "SETQ",                     F, xsetq        },
{   "SETF",                     F, xsetf        },
{   "DEFUN",                    F, xdefun       },
{   "DEFMACRO",                 F, xdefmacro    },
{   "GENSYM",                   S, xgensym      },
{   "MAKE-SYMBOL",              S, xmakesymbol  },
{   "INTERN",                   SM, xintern     },
{   "SYMBOL-NAME",              S, xsymname     },
{   "SYMBOL-VALUE",             S, xsymvalue    },
{   "SYMBOL-PLIST",             S, xsymplist    },
{   "GET",                      S, xget         },
{   "GETF",                     S, xgetf        },
{   "PUTPROP",                  S, xputprop     },
{   "REMPROP",                  S, xremprop     },
{   "HASH",                     S, xhash        },

    /* array functions */
{   "MAKE-ARRAY",               S, xmkarray     },
{   "AREF",                     S, xaref        },
            
    /* list functions */
{   "CAR",                      S, xcar         },
{   "CDR",                      S, xcdr         },
            
{   "CAAR",                     S, xcaar        },
{   "CADR",                     S, xcadr        },
{   "CDAR",                     S, xcdar        },
{   "CDDR",                     S, xcddr        },

{   "CAAAR",                    S, xcaaar       },
{   "CAADR",                    S, xcaadr       },
{   "CADAR",                    S, xcadar       },
{   "CADDR",                    S, xcaddr       },
{   "CDAAR",                    S, xcdaar       },
{   "CDADR",                    S, xcdadr       },
{   "CDDAR",                    S, xcddar       },
{   "CDDDR",                    S, xcdddr       },

{   "CAAAAR",                   S, xcaaaar      },
{   "CAAADR",                   S, xcaaadr      },
{   "CAADAR",                   S, xcaadar      },
{   "CAADDR",                   S, xcaaddr      },
{   "CADAAR",                   S, xcadaar      },
{   "CADADR",                   S, xcadadr      },
{   "CADDAR",                   S, xcaddar      },
{   "CADDDR",                   S, xcadddr      },
{   "CDAAAR",                   S, xcdaaar      },
{   "CDAADR",                   S, xcdaadr      },
{   "CDADAR",                   S, xcdadar      },
{   "CDADDR",                   S, xcdaddr      },
{   "CDDAAR",                   S, xcddaar      },
{   "CDDADR",                   S, xcddadr      },
{   "CDDDAR",                   S, xcdddar      },
{   "CDDDDR",                   S, xcddddr      },

{   "CONS",                     S, xcons        },
{   "LIST",                     S, xlist        },
{   "LIST*",                    S, xliststar    },
{   "APPEND",                   S, xappend      },
{   "REVERSE",                  S, xreverse     },
{   "LAST",                     S, xlast        },
{   "NTH",                      S, xnth         },
{   "NTHCDR",                   S, xnthcdr      },
{   "MEMBER",                   S, xmember      },
{   "ASSOC",                    S, xassoc       },
{   "SUBST",                    S, xsubst       },
{   "SUBLIS",                   S, xsublis      },
{   "NSUBST",                   S, xnsubst      },
{   "NSUBST-IF",                S, xnsubstif    },
{   "NSUBST-IF-NOT",            S, xnsubstifnot },
{   "NSUBLIS",                  S, xnsublis     },
{   "REMOVE",                   S, xremove      },
{   "LENGTH",                   S, xlength      },
{   "LIST-LENGTH",              S, xlistlength  },
{   "MAPC",                     S, xmapc        },
{   "MAPCAR",                   S, xmapcar      },
{   "MAPL",                     S, xmapl        },
{   "MAPLIST",                  S, xmaplist     },
{   "MAPCAN",                   S, xmapcan      },
{   "MAPCON",                   S, xmapcon      },

            
    /* destructive list functions */
{   "RPLACA",                   S, xrplca       },
{   "RPLACD",                   S, xrplcd       },
{   "NCONC",                    S, xnconc       },
{   "DELETE",                   S, xdelete      },

    /* predicate functions */
{   "ATOM",                     S, xatom        },
{   "SYMBOLP",                  S, xsymbolp     },
{   "NUMBERP",                  S, xnumberp     },
{   "BOUNDP",                   S, xboundp      },
{   "NULL",                     S, xnull        },
{   "LISTP",                    S, xlistp       },
{   "CONSP",                    S, xconsp       },
{   "MINUSP",                   S, xminusp      },
{   "ZEROP",                    S, xzerop       },
{   "PLUSP",                    S, xplusp       },
{   "EVENP",                    S, xevenp       },
{   "ODDP",                     S, xoddp        },
{   "EQ",                       S, xeq          },
{   "EQL",                      S, xeql         },
{   "EQUAL",                    S, xequal       },

    /* special forms */
{   "COND",                     FM, xcond       },
{   "CASE",                     FM, xcase       },
{   "AND",                      FM, xand        },
{   "OR",                       FM, xor         },
{   "LET",                      FM, xlet        },
{   "LET*",                     FM, xletstar    },
{   "IF",                       FM, xif         },
{   "PROG",                     FM, xprog       },
{   "PROG*",                    FM, xprogstar   },
{   "PROG1",                    F, xprog1       },
{   "PROG2",                    F, xprog2       },
{   "PROGN",                    FM, xprogn      },
{   "GO",                       F, xgo          },
{   "RETURN",                   F, xreturn      },
{   "DO",                       FM, xdo         },
{   "DO*",                      FM, xdostar     },
{   "DOLIST",                   FM, xdolist     },
{   "DOTIMES",                  FM, xdotimes    },
{   "CATCH",                    FM, xcatch      },
{   "THROW",                    F, xthrow       },
    
    /* debugging and error handling functions */
{   "ERROR",                    S, xerror       },
{   "CERROR",                   S, xcerror      },
{   "BREAK",                    S, xbreak       },
{   "CLEAN-UP",                 S, xcleanup     },
{   "TOP-LEVEL",                S, xtoplevel    },
{   "CONTINUE",                 S, xcontinue    },
{   "ERRSET",                   F, xerrset      },
{   "BAKTRACE",                 S, xbaktrace    },
{   "EVALHOOK",                 SM, xevalhook   },

    /* arithmetic functions */
{   "TRUNCATE",                 SM, xfix        },
{   "FLOAT",                    S, xfloat       },
{   "+",                        S, xadd         },
{   "-",                        S, xsub         },
{   "*",                        S, xmul         },
{   "/",                        S, xdiv         },
{   "1+",                       S, xadd1        },
{   "1-",                       S, xsub1        },
{   "REM",                      S, xrem         },
{   "MIN",                      S, xmin         },
{   "MAX",                      S, xmax         },
{   "ABS",                      S, xabs         },
{   "SIN",                      S, xsin         },
{   "COS",                      S, xcos         },
{   "TAN",                      S, xtan         },
{   "EXPT",                     S, xexpt        },
{   "EXP",                      S, xexp         },
{   "SQRT",                     S, xsqrt        },
{   "RANDOM",                   S, xrand        },
            
    /* bitwise logical functions */
{   "LOGAND",                   S, xlogand      },
{   "LOGIOR",                   S, xlogior      },
{   "LOGXOR",                   S, xlogxor      },
{   "LOGNOT",                   S, xlognot      },
#ifdef BIGNUMS
{   "LOGEQV",                   S, xlogeqv      },
{   "LOGNAND",                  S, xlognand     },
{   "LOGNOR",                   S, xlognor      },
{   "LOGANDC1",                 S, xlogandc1    },
{   "LOGANDC2",                 S, xlogandc2    },
{   "LOGORC1",                  S, xlogorc1     },
{   "LOGORC2",                  S, xlogorc2     },
{   "LOGTEST",                  S, xlogtest     },
{   "LOGBITP",                  S, xlogbitp     },
{   "LOGCOUNT",                 S, xlogcount    },
{   "INTEGER-LENGTH",           S, xintlen      },
#endif
#ifdef COMPLX
{   "ASH",                      S, xash         },
#endif

    /* numeric comparison functions */
{   "<",                        S, xlss         },
{   "<=",                       S, xleq         },
{   "=",                        S, xequ         },
{   "/=",                       S, xneq         },
{   ">=",                       S, xgeq         },
{   ">",                        S, xgtr         },
            
    /* string functions */

{   "CONCATENATE",              S, xconcatenate },
{   "SUBSEQ",                   S, xsubseq      },
{   "STRING",                   S, xstring      },
{   "CHAR",                     S, xchar        },

    /* I/O functions */
{   "READ",                     S, xread        },
{   "READ-PRESERVING-WHITESPACE",S, xreadpw     },
{   "PRINT",                    S, xprint       },
{   "PRIN1",                    S, xprin1       },
{   "PRINC",                    S, xprinc       },
{   "TERPRI",                   S, xterpri      },
{   "FLATSIZE",                 S, xflatsize    },
{   "FLATC",                    S, xflatc       },
            
    /* file I/O functions */
{   "OPEN",                     S, xopen        },
{   "FORMAT",                   S, xformat      },
{   "CLOSE",                    S, xclose       },
{   "READ-CHAR",                S, xrdchar      },
{   "PEEK-CHAR",                S, xpkchar      },
{   "WRITE-CHAR",               S, xwrchar      },
{   "READ-LINE",                S, xreadline    },

    /* system functions */
{   "LOAD",                     S, xload        },
{   "DRIBBLE",                  S, xtranscript  },

/* functions specific to xldmem.c */
{   "GC",                       S, xgc          },
{   "EXPAND",                   S, xexpand      },
{   "ALLOC",                    S, xalloc       },
{   "ROOM",                     S, xmem         },
#ifdef SAVERESTORE
{   "SAVE",                     S, xsave        },
{   "RESTORE",                  S, xrestore     },
#endif
/* end of functions specific to xldmem.c */

{   "TYPE-OF",                  S, xtype        },
{   "EXIT",                     S, xexit        },
{   "PEEK",                     S, xpeek        },
{   "POKE",                     S, xpoke        },
{   "ADDRESS-OF",               S, xaddrs       },

    /* new functions and special forms */
{   "VECTOR",                   S, xvector      },
{   "BLOCK",                    FM, xblock      },
{   "RETURN-FROM",              F, xrtnfrom     },
{   "TAGBODY",                  F, xtagbody     },
{   "PSETQ",                    F, xpsetq       },
{   "PSETF",                    F, xpsetf       },
{   "FLET",                     FM, xflet       },
{   "LABELS",                   FM, xlabels     },
{   "MACROLET",                 FM, xmacrolet   },
{   "UNWIND-PROTECT",           FM, xunwindprotect},
{   "PPRINT",                   SM, xpp         },
{   "STRING<",                  S, xstrlss      },
{   "STRING<=",                 S, xstrleq      },
{   "STRING=",                  S, xstreql      },
{   "STRING/=",                 S, xstrneq      },
{   "STRING>=",                 S, xstrgeq      },
{   "STRING>",                  S, xstrgtr      },
{   "STRING-LESSP",             S, xstrilss     },
{   "STRING-NOT-GREATERP",      S, xstrileq     },
{   "STRING-EQUAL",             S, xstrieql     },
{   "STRING-NOT-EQUAL",         S, xstrineq     },
{   "STRING-NOT-LESSP",         S, xstrigeq     },
{   "STRING-GREATERP",          S, xstrigtr     },
{   "INTEGERP",                 S, xintegerp    },
{   "FLOATP",                   S, xfloatp      },
{   "STRINGP",                  S, xstringp     },
{   "ARRAYP",                   S, xarrayp      },
{   "STREAMP",                  S, xstreamp     },
{   "OBJECTP",                  S, xobjectp     },
{   "STRING-UPCASE",            S, xupcase      },
{   "STRING-DOWNCASE",          S, xdowncase    },
{   "STRING-CAPITALIZE",        S, xcapcase     },
{   "NSTRING-UPCASE",           S, xnupcase     },
{   "NSTRING-DOWNCASE",         S, xndowncase   },
{   "NSTRING-CAPITALIZE",       S, xncapcase    },
{   "STRING-TRIM",              S, xtrim        },
{   "STRING-LEFT-TRIM",         S, xlefttrim    },
{   "STRING-RIGHT-TRIM",        S, xrighttrim   },
{   "WHEN",                     FM, xwhen       },
{   "UNLESS",                   FM, xunless     },
{   "LOOP",                     FM, xloop       },
{   "SYMBOL-FUNCTION",          S, xsymfunction },
{   "FBOUNDP",                  S, xfboundp     },
{   "SEND",                     SM, xsend       },
{   "SEND-SUPER",               SM, xsendsuper  },
{   "PROGV",                    FM, xprogv      },
{   "CHARACTERP",               S, xcharp       },
{   "CHAR-INT",                 S, xcharint     },
{   "INT-CHAR",                 S, xintchar     },
{   "READ-BYTE",                S, xrdbyte      },
{   "WRITE-BYTE",               S, xwrbyte      },
{   "MAKE-STRING-INPUT-STREAM", S, xmkstrinput  },
{   "MAKE-STRING-OUTPUT-STREAM",S, xmkstroutput },
{   "GET-OUTPUT-STREAM-STRING", S, xgetstroutput},
{   "GET-OUTPUT-STREAM-LIST",   S, xgetlstoutput},
{   "GCD",                      S, xgcd         },
{   "GET-LAMBDA-EXPRESSION",    S, xgetlambda   },
{   "MACROEXPAND",              SM, xmacroexpand},
{   "MACROEXPAND-1",            SM,x1macroexpand},
{   "CHAR<",                    S, xchrlss      },
{   "CHAR<=",                   S, xchrleq      },
{   "CHAR=",                    S, xchreql      },
{   "CHAR/=",                   S, xchrneq      },
{   "CHAR>=",                   S, xchrgeq      },
{   "CHAR>",                    S, xchrgtr      },
{   "CHAR-LESSP",               S, xchrilss     },
{   "CHAR-NOT-GREATERP",        S, xchrileq     },
{   "CHAR-EQUAL",               S, xchrieql     },
{   "CHAR-NOT-EQUAL",           S, xchrineq     },
{   "CHAR-NOT-LESSP",           S, xchrigeq     },
{   "CHAR-GREATERP",            S, xchrigtr     },
{   "UPPER-CASE-P",             S, xuppercasep  },
{   "LOWER-CASE-P",             S, xlowercasep  },
{   "BOTH-CASE-P",              S, xbothcasep   },
{   "DIGIT-CHAR-P",             S, xdigitp      },
{   "ALPHANUMERICP",            S, xalphanumericp},
{   "ALPHA-CHAR-P",             S, xalphacharp  },
{   "CHAR-UPCASE",              S, xchupcase    },
{   "CHAR-DOWNCASE",            S, xchdowncase  },
{   "DIGIT-CHAR",               S, xdigitchar   },
{   "CHAR-CODE",                S, xcharcode    },
{   "CODE-CHAR",                S, xcodechar    },
{   "ENDP",                     S, xendp        },
{   "REMOVE-IF",                S, xremif       },
{   "REMOVE-IF-NOT",            S, xremifnot    },
{   "DELETE-IF",                S, xdelif       },
{   "DELETE-IF-NOT",            S, xdelifnot    },
{   "TRACE",                    F, xtrace       },
{   "UNTRACE",                  F, xuntrace     },
{   "SORT",                     S, xsort        },
{   "STABLE-SORT",              S, xsort        },
#ifdef SUBSTITUTE
{   "SUBSTITUTE",               S, xsubstitute  },
{   "SUBSTITUTE-IF",            S, xsubstituteif},
{   "SUBSTITUTE-IF-NOT",        S, xsubstituteifnot},
{   "NSUBSTITUTE",              S, xnsubstitute },
{   "NSUBSTITUTE-IF",           S, xnsubstituteif},
{   "NSUBSTITUTE-IF-NOT",       S, xnsubstituteifnot},
#endif
#ifdef ADDEDTAA
{   "GENERIC",                  S, xgeneric     },
#endif
#ifdef TIMES
{   "TIME",                     FM, xtime       },
{   "GET-INTERNAL-RUN-TIME",    S, xruntime     },
{   "GET-INTERNAL-REAL-TIME",   S, xrealtime    },
#endif
/* extra table entries */
#ifdef POSFCNS
{   "COUNT",                    S, xcount       },
{   "FIND",                     S, xfind        },
{   "POSITION",                 S, xposition    },
{   "COUNT-IF",                 S, xcountif     },
{   "FIND-IF",                  S, xfindif      },
{   "POSITION-IF",              S, xpositionif  },
{   "COUNT-IF-NOT",             S, xcountifnot  },
{   "FIND-IF-NOT",              S, xfindifnot   },
{   "POSITION-IF-NOT",          S, xpositionifnot},
#endif
{   "COERCE",                   S, xcoerce      },
{   "ELT",                      S, xelt         },
#ifdef SRCHFCN
{   "SEARCH",                   S, xsearch      },
#endif
#ifdef MAPFCNS
{   "MAP",                      S, xmap         },
{   "MAP-INTO",                 S, xmapinto     },
{   "SOME",                     S, xsome        },
{   "EVERY",                    S, xevery       },
{   "NOTANY",                   S, xnotany      },
{   "NOTEVERY",                 S, xnotevery    },
#endif
{   "FILE-POSITION",            S, xfileposition},
{   "FILE-LENGTH",              S, xfilelength  },
{   "FRESH-LINE",               S, xfreshline   },
{   "OPEN-STREAM-P",            S, xopenstreamp },
{   "INPUT-STREAM-P",           S, xinputstreamp},
{   "OUTPUT-STREAM-P",          S, xoutputstreamp},
#ifdef FILETABLE
{   "TRUENAME",                 S, xtruename    },
{   "DELETE-FILE",              S, xdeletefile  },
#endif
{   "DEFSTRUCT",                F, xdefstruct   },
{   "%STRUCT-TYPE-P",           S, xstrtypep    },
{   "%MAKE-STRUCT",             S, xmkstruct    },
{   "%COPY-STRUCT",             S, xcpystruct   },
{   "%STRUCT-REF",              S, xstrref      },
{   "%STRUCT-SET",              S, xstrset      },
{   "ASIN",                     S, xasin        },
{   "ACOS",                     S, xacos        },
{   "ATAN",                     S, xatan        },
#ifdef APPLYHOOK
{   "APPLYHOOK",                SM, xapplyhook  },
#endif
{   "NREVERSE",                 S, xnreverse    },
{   "BUTLAST",                  S, xbutlast     },
{   "TYPEP",                    S, xtypep       },
#ifdef REDUCE
{   "REDUCE",                   S, xreduce      },
#endif
#ifdef REMDUPS
{   "REMOVE-DUPLICATES",        S, xremove_duplicates },
#endif

#ifdef SETS
{   "ADJOIN",                   S, xadjoin          },
{   "UNION",                    S, xunion           },
{   "INTERSECTION",             S, xintersection    },
{   "SET-DIFFERENCE",           S, xset_difference  },
{   "SUBSETP",                  S, xsubsetp         },
#endif

#ifdef HASHFCNS
{   "GETHASH",                  SM, xgethash        },
{   "REMHASH",                  S, xremhash         },
{   "MAKE-HASH-TABLE",          S, xmakehash        },
{   "CLRHASH",                  S, xclrhash         },
{   "MAPHASH",                  S, xmaphash         },
{   "HASH-TABLE-COUNT",         S, xhashcount       },
#endif

#ifdef COMPLX
{   "COMPLEXP",                 S, xcomplexp        },
{   "COMPLEX",                  S, xcomplex         },
{   "CONJUGATE",                S, xconjugate       },
{   "REALPART",                 S, xrealpart        },
{   "IMAGPART",                 S, ximagpart        },
{   "LOG",                      S, xlog             },
{   "FLOOR",                    SM, xfloor          },
{   "CEILING",                  SM, xceil           },
{   "ROUND",                    SM, xround          },
{   "PHASE",                    S, xphase           },
{   "LCM",                      S, xlcm             },
{   "MOD",                      S, xmod             },
#endif

#ifdef BIGNUMS
{   "RATIONALP",                S, xrationalp       },
{   "NUMERATOR",                S, xnumerator       },
{   "DENOMINATOR",              S, xdenominator     },
{   "RATIONAL",                 S, xrational        },
#endif

{   "DEFCONSTANT",              F, xdefconstant     },
{   "CONSTANTP",                S, xconstantp       },
{   "DEFPARAMETER",             F, xdefparameter    },
{   "DEFVAR",                   F, xdefvar          },
{   "MAKUNBOUND",               S, xmakunbound      },
{   "FMAKUNBOUND",              S, xfmakunbound     },
{   "SPECIALP",                 S, xspecialp        },
{   "MARK-AS-SPECIAL",          S, xmarkspecial     },

{   "TOP-LEVEL-LOOP",           S, xtoplevelloop    },
{   "RESET-SYSTEM",             S, xresetsystem     },

#ifdef RANDOM
{   "MAKE-RANDOM-STATE",        S, xmakerandom      },
#endif
#ifdef STSZ
{   "SET-STACK-MARK",           S, xsetmark         },
#endif

#ifdef PACKAGES
{   "MAKE-PACKAGE",             S, xmakepackage     },
{   "IN-PACKAGE",               F, xinpackage       },
{   "EXPORT",                   S, xexport          },
{   "UNEXPORT",                 S, xunexport        },
{   "IMPORT",                   S, ximport          },
{   "SYMBOL-PACKAGE",           S, xsympackage      },
{   "USE-PACKAGE",              S, xusepackage      },
{   "UNUSE-PACKAGE",            S, xunusepackage    },
{   "PACKAGE-USE-LIST",         S, xpackageuselist  },
{   "PACKAGE-USED-BY-LIST",     S, xpackageusedbylist},
{   "PACKAGE-SHADOWING-SYMBOLS", S, xpackageshadows },
{   "PACKAGE-NAME",             S, xpackagename     },
{   "PACKAGE-NICKNAMES",        S, xpackagenicknames},
{   "PACKAGE-OBARRAY",          S, xpackageobarray  },
{   "PACKAGE-VALID-P",          S, xpackagevalidp   },
{   "LIST-ALL-PACKAGES",        S, xlistallpackages },
{   "UNINTERN",                 S, xunintern        },
{   "SHADOW",                   S, xshadow          },
{   "SHADOWING-IMPORT",         S, xshadowingimport },
{   "FIND-PACKAGE",             S, xfindpackage     },
{   "FIND-SYMBOL",              SM, xfindsymbol     },
{   "DELETE-PACKAGE",           S, xdeletepackage   },
{   "RENAME-PACKAGE",           S, xrenamepackage   },
{   "FIND-ALL-SYMBOLS",         S, xfindallsymbols  },
#endif
#ifdef MULVALS
{   "VALUES",                   SM, xvalues         },
{   "MULTIPLE-VALUE-CALL",      FM, xmulvalcall     },
{   "MULTIPLE-VALUE-PROG1",     FM, xmulvalprog1        },
{   "NTH-VALUE",                F, xnthvalue        },
#endif

/* include system dependent function pointers */
#include "osptrs.h"
#ifdef COMPILER
#include "compftab.h"
#endif
    /* Two patches here to promote module portability to xscheme:*/
    /* $putpatch.c$: "MODULE_XLFTAB_C_FUNTAB_S" */
    /* $putpatch.c$: "MODULE_XLFTAB_C_FUNTAB_F" */
{0,0,0} /* end of table marker */

};          

int ftabsize = sizeof(funtab); /* TAA MOD -- added validity check */
