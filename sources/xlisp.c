/* xlisp.c - a small implementation of lisp with object-oriented programming */
/*      Copyright (c) 1987, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

/* For full credits see file xlisp.h */

#include "xlisp.h"

/* define the banner line string */
#define BANNER  "XLISP-PLUS version %d.%02d\n\
Portions Copyright (c) 1988, by David Betz.\n\
Modified by Thomas Almy and others."


/* global variables */
#ifdef SAVERESTORE
jmp_buf top_level;
#endif

/* local variables */
jmp_buf exit_xlisp;

LOCAL VOID toplevelloop _((void));

/* usage - print command line usage, then quit TAA addition */

VOID usage _((void)) {

#ifdef WINDOWS
    xoserror("invalid command line argument(s)");
#else
#ifdef SAVERESTORE
    fprintf(stderr,"Valid Arguments:\n\t-?\tThis help\n\
\t-tfname\tOpen transcript (dribble) file fname\n\
\t-v\tLoad verbosely\n\
\t-b\tBatch mode (non-interactive)\n\
\t-w\tDon't restore from xlisp.wks\n\
\t-wfname\tRestore from fname\n\
\tfname\tLoad file fname\n");
#else
    fprintf(stderr,"Valid Arguments:\n\t-?\tThis help\n\
\t-tfname\tOpen transcript (dribble) file fname\n\
\t-v\tLoad verbosely\n\
\t-b\tBatch mode (non-interactive)\n\
\tfname\tLoad file fname\n");
#endif
#endif
    longjmp(exit_xlisp, 2);
}

#ifdef FILETABLE
#ifdef ANSI
static void init_filetab(void)  /* dynamic initialization of filetab for all
                               versions added 3/98. Previously it was coded
                               in two separate ways for SASC and WIN32 */
#else
static VOID init_filetab()
#endif
{
    filetab[0].fp = stdin;
    filetab[0].tname = "(stdin)";
    filetab[1].fp = stdout;
    filetab[1].tname = "(stdout)";
    filetab[2].fp = stderr;
    filetab[2].tname = "(console)";
/*    filetab[3].fp = NULL;
    filetab[3].tname = ""; */
}
#endif

/* main - the main routine */
#ifdef USEQT
/* In the case of QT, this isn't the program's main function */
int cmain(int argc, char *argv[])
#else
int CDECL main(argc,argv)
  int argc; char *argv[];
#endif
{
    char *transcript;
    CONTXT cntxt;
    int verbose,i;
#ifdef SAVERESTORE
    char *resfile = "xlisp.wks";    /* TAA mod -- command line restore file */
#endif

#ifdef FILETABLE
    init_filetab();
#endif

    /* The way out on errors */
    if ((i = setjmp(exit_xlisp)) != 0)
        return i-1;

    /* setup default argument values */
    transcript = NULL;
    verbose = FALSE;

    /* parse the argument list switches */
#ifndef LSC
    for (i = 1; i < argc; ++i)
        if (argv[i][0] == '-')
            switch(isupper(argv[i][1])?tolower(argv[i][1]):argv[i][1]) {
            case '?':   /* TAA MOD: added help */
                usage();
            case 't':
                transcript = &argv[i][2];
                break;
            case 'b':
                batchmode = TRUE;
                break;
            case 'v':
                verbose = TRUE;
                break;
#ifdef SAVERESTORE
            case 'w':
                resfile = &argv[i][2];
                break;
#endif
            default: /* Added to print bad switch message */
#ifndef WINDOWS
                fprintf(stderr,"Bad switch: %s\n",argv[i]);
#endif
                usage();
            }
#endif

    /* initialize and print the banner line */
    {
      char buf[256];
      sprintf(buf, BANNER, MAJOR_VERSION, MINOR_VERSION);
      osinit(buf);
    }

    /* setup initialization error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
    if (setjmp(cntxt.c_jmpbuf))
        xlfatal("fatal initialization error");
#ifdef SAVERESTORE
    if (setjmp(top_level))
        xlfatal("RESTORE not allowed during initialization");
#endif
    /* initialize xlisp */
#ifdef SAVERESTORE
    i = xlinit(resfile);
#else
    i = xlinit(NULL);
#endif

    /* reset the error handler, since we know what "true" is */
    xlend(&cntxt);
    xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, s_true);

    /* open the transcript file */
    if (transcript!=NULL && (tfp = OSAOPEN(transcript,CREATE_WR)) == CLOSED) {
        /* TAA Mod -- quote name so "-t foo" will indicate no file name */
        sprintf(buf,"error: can't open transcript file: \"%s\"",transcript);
        stdputstr(buf);
    }

    /* enter the command line (L. Tierney 9/93) */
    if (setjmp(cntxt.c_jmpbuf) == 0) {
        LVAL line;
        int j;
        xlsave1(line);
        line = NIL;
        for (j = argc - 1; j >= 0; j--)
            line = cons(cvstring(argv[j]), line);
        xlpop();
        setsvalue(s_command_line, line);
    }

    /* load "init.lsp" */
    if (i && (setjmp(cntxt.c_jmpbuf) == 0))
        xlload("init.lsp",TRUE,FALSE);

    /* run any startup functions (L. Tierney 9/93) */
    if (setjmp(cntxt.c_jmpbuf) == 0) {
        LVAL funs = getvalue(s_startup_functions);
        FRAMEP newfp;

        for (; consp(funs); funs = cdr(funs)) {
            newfp = xlsp;
            pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
            pusharg(car(funs));
            pusharg(cvfixnum((FIXTYPE) 0));
            xlfp = newfp;
            xlapply(0);
        }
    }

    /* load any files mentioned on the command line */
    if (!null(getvalue(s_loadfileargs)) && setjmp(cntxt.c_jmpbuf) == 0)
        for (i = 1; i < argc; i++)
            if (argv[i][0] != '-' && !xlload(argv[i],TRUE,verbose))
                xlerror("can't load file",cvstring(argv[i]));

    /* target for restore */
#ifdef SAVERESTORE
    if (setjmp(top_level))
        xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, s_true);
#endif
    /* main command processing loop */
    for (;;) {

        /* setup the error return */
        if (setjmp(cntxt.c_jmpbuf)) {
            setvalue(s_evalhook,NIL);
            setvalue(s_applyhook,NIL);
            xltrcindent = 0;
            xldebug = 0;
            xlflush();
        }

#ifdef STSZ
        stackwarn = FALSE;
#endif

        if (boundp(s_toplevelloop)) {
            FRAMEP newfp;

            newfp = xlsp;
            pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
            pusharg(getvalue(s_toplevelloop));
            pusharg(cvfixnum((FIXTYPE) 0));
            xlfp = newfp;
            xlapply(0);
        }
        else 
            toplevelloop();
    }   /* never exit from here */
}



/* xtoplevelloop - lisp-callable top level loop */
/* Luke Tierney 9/93 */
LVAL xtoplevelloop()
{
    xllastarg();
  
    toplevelloop();
    return(NIL); /* doesn't return */
}

/* toplevelloop - the default command loop */
LOCAL VOID toplevelloop()
{
    LVAL expr;
#ifdef MULVALS
    int i;
#endif

    /* protect some pointers */
    xlsave1(expr);

    for (;;) {

        /* print a prompt */
#ifdef PACKAGES
        if (!redirectin) {
            LVAL pack = getvalue(s_package);
            if (pack != xluserpack && goodpackagep(pack)) {
#ifdef MEDMEM
                STRCPY(buf, getstring(xlpackagename(pack)));
                dbgputstr(buf);
#else
                dbgputstr(getstring(xlpackagename(pack)));
#endif
            }
            dbgputstr("> ");
        }
#else
        if (!redirectin) dbgputstr("> ");
#endif /* PACKAGES */

        /* read an expression */
        if (!xlread(getvalue(s_stdin),&expr,FALSE,FALSE)) {
            /* clean up */
            wrapup();
            break;
        }

        /* save the input expression */
        xlrdsave(expr);

        /* evaluate the expression */
        expr = xleval(expr);

        /* save the result */
        xlevsave(expr);

        /* Show result on a new line -- TAA MOD to improve display */
        xlfreshline(getvalue(s_stdout));

        /* print it */
#ifdef MULVALS
        /* Fix from Luke Tierney, 1/97, to save multiple values in case
           a structure print function is called */
        switch (xlnumresults) {
            case 0: break;
            case 1: stdprint(expr); break;
            default:
            {
                LVAL vals;
                xlsave1(vals);
                for (i = xlnumresults; i-- > 0; )
                    vals = cons(xlresults[i], vals);
                for (; consp(vals); vals = cdr(vals))
                    stdprint(car(vals));
                xlpop();
            }
        }
#else
        stdprint(expr);
#endif /* MULVALS */
    }
}

/* xlrdsave - save the last expression returned by the reader */
VOID xlrdsave(expr)
  LVAL expr;
{
    setvalue(s_3plus,getvalue(s_2plus));
    setvalue(s_2plus,getvalue(s_1plus));
    setvalue(s_1plus,getvalue(s_minus));
    setvalue(s_minus,expr);
}

/* xlevsave - save the last expression returned by the evaluator */
VOID xlevsave(expr)
  LVAL expr;
{
    setvalue(s_3star,getvalue(s_2star));
    setvalue(s_2star,getvalue(s_1star));
    setvalue(s_1star,expr);
}

/* xlfatal - print a fatal error message and exit */
VOID xlfatal(msg)
  char *msg;
{
    xoserror(msg);
    wrapup();
}

/* wrapup - clean up and exit to the operating system */
VOID wrapup()
{
    /* $putpatch.c$: "MODULE_XLISP_C_WRAPUP" */
    if (tfp != CLOSED)
        OSCLOSE(tfp);
    osfinish();
    longjmp(exit_xlisp, 1);
}

/* xresetsystem - reset system for user top-levels */
LVAL xresetsystem()
{
  xlflush();
  return(NIL);
}
