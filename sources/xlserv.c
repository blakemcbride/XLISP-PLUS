/* XLSERV.C -- Xlisp server replacement for XLISP.C */

/* Written by Tom Almy */

#include "xlisp.h"

/* define the banner line string */
#define BANNER  "XLISP-PLUS version 3.04, Copyright (c) 1988, by David Betz\n\
As modified by Thomas Almy"

jmp_buf sysFailure;

#ifdef SAVERESTORE

jmp_buf top_level;

VOID freeimage _((void));
LVAL getstroutput _((LVAL stream));

#endif

int execXlisp _((char *str, int restype, char FAR * FAR * resstr, LVAL *resval));

/* The Xlisp server must be initialized via a call to initXlisp.
   Since it could be restoring from a workspace, the name of that workspace
   is passed as an argument */

/* The initialization function returns non-zero on initialization failure:
    1 - failure during initialization
    2 - failure reading init.lsp
    3 - OS Failure (typically not enough memory)
        */

int initXlisp(resfile)
char *resfile;
{
    CONTXT cntxt;
    int i;

    /* Operating system initialization code will probably need changing
        from the original, non-server version */

    osinit(BANNER);

    /* setup initialization error handler with phoney, non NIL "true" */

    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
    if (setjmp(cntxt.c_jmpbuf)) return (1);
    if (setjmp(sysFailure)) return (3);

    /* initialize xlisp */

#ifdef SAVERESTORE
    i = xlinit(resfile);
#else
    i = xlinit(NULL);
#endif
    xlend(&cntxt);

    if (i) {    /* need to load init.lsp */
        xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, s_true);

        if (setjmp(cntxt.c_jmpbuf) != 0) {
            xlend(&cntxt);
            return (2);
        }
        else
            xlload("init.lsp",TRUE,FALSE);

        xlend(&cntxt);
    }

    return 0;
}

/* execXlisp -- execute an Xlisp expression */
/* Return code: 1 "error failure" 2 "total failure" 3 "restore happened" */

int execXlisp(str, restype, resstr, resval)
char *str;  /* string to execute */
int restype;    /* Nonzero for string return, else value return */
char FAR * FAR *resstr; /* result string will be disposed on next execXlisp call*/
LVAL *resval; /* pointer to result LVAL, disposed on next call */
{
    CONTXT cntxt;
    LVAL expr, instream;
    unsigned i, len;
    
    xlbegin(&cntxt, CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL, s_true);

    if (setjmp(sysFailure)) {
        xlend(&cntxt);
        xlpopn(2);
        return 2;
    }

    if (setjmp(top_level)) {
        xlend(&cntxt);
        xlpopn(2);
        return 3;
    }
    
    /* setup the error return */
    if (setjmp(cntxt.c_jmpbuf)) {
        xlend(&cntxt);
        setvalue(s_evalhook,NIL);
        setvalue(s_applyhook,NIL);
        xltrcindent = 0;
        xldebug = 0;
        xlpopn(2);
        return 1;
    }

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(instream);
    xlsave1(expr);

    /* create input stream from input */
    instream = newustream();
    len = strlen(str);
    for (i=0; i < len; i++) xlputc(instream, str[i]);
    
    /* main processing loop */
    for (;;) {
        /* TAA MOD 3/98 -- added missing fourth argument */
        if (!xlread(instream, &expr, FALSE, FALSE)) break;
        
        xlrdsave(expr);

        expr = xleval(expr);
        
        xlevsave(expr);
        
        /* NO PRINTING! */
    }
    
    if (restype) {
        expr = getstroutput(expr);
        *resstr = getstring(expr);
    }
        
    else *resval = expr;    /* return expression */
    
    xlend(&cntxt);
    xlpopn(2);

    return (0);

}



/* wrapupXlisp - clean up -- we are done */
VOID wrapupXlisp()
{
    if (tfp != CLOSED)
        OSCLOSE(tfp);
#ifdef SAVERESTORE  /* should really be defined for this */
    freeimage();
#endif
    osfinish();
}

/* xlfatal - print a fatal error message and exit */
VOID xlfatal(msg)
  char *msg;
{
    xoserror(msg);
    wrapupXlisp();
    longjmp(sysFailure,1);
}

/* Terminate execution */
VOID wrapup()
{
    wrapupXlisp();
    longjmp(sysFailure,1);
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



VOID CDECL main() {
    char far *foo, ch;
    int i;
    
    if (initXlisp("win.wks")!= 0) {
        fprintf(stderr, "Init failure");
        return;
    }
    
    fprintf(stderr,"Hello there!");

    if ((i = execXlisp("(room)", 1, &foo, NULL)) != 0) 
        fprintf(stderr, "Exec failure #%d", i);
    else 
        while ((ch = *foo++) != 0) putchar(ch);

    wrapupXlisp();
    fprintf(stderr,"Finished!");
    return;
}
