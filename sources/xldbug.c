/* xldebug - xlisp debugging support */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

/* forward declarations */
LOCAL VOID NEAR breakloop _((char *hdr, char FAR *cmsg,
                             char FAR *emsg, LVAL arg,  int cflag));


/* xlabort - xlisp serious error handler */
VOID xlabort(emsg)
  char *emsg;
{
    xlsignal(emsg,s_unbound);
    xlerrprint("error",NULL,emsg,s_unbound);
    xlbrklevel();
}

/* xlbreak - enter a break loop */
VOID xlbreak(emsg,arg)
  char FAR *emsg; LVAL arg;
{
#ifdef USEQT
    xrevertToText();
#endif
    breakloop("break","return from BREAK",emsg,arg,TRUE);
}

/* xlfail - xlisp error handler */
VOID xlfail(emsg)
  char *emsg;
{
    xlerror(emsg,s_unbound);
}

/* xlerror - handle a fatal error */
LVAL xlerror(emsg,arg)
  char FAR *emsg; LVAL arg;
{
#ifdef USEQT
    xrevertToText();
#endif
    if (!null(getvalue(s_breakenable)))
        breakloop("error",NULL,emsg,arg,FALSE);
    else {
        xlsignal(emsg,arg);
        xlerrprint("error",NULL,emsg,arg);
        xlbrklevel();
    }
        return NIL;     /* actually doesn't return */
}

/* xlcerror - handle a recoverable error */
VOID xlcerror(cmsg,emsg,arg)
  char FAR *cmsg, FAR *emsg; LVAL arg;
{
#ifdef USEQT
    xrevertToText();
#endif
    if (!null(getvalue(s_breakenable)))
        breakloop("error",cmsg,emsg,arg,TRUE);
    else {
        xlsignal(emsg,arg);
        xlerrprint("error",NULL,emsg,arg);
        xlbrklevel();
    }
}

/* xlerrprint - print an error message */
VOID xlerrprint(hdr,cmsg,emsg,arg)
  char *hdr, FAR *cmsg, FAR *emsg; LVAL arg;
{
/* TAA MOD -- start error message on a fresh line */
    xlfreshline(getvalue(s_stderr));

    /* print the error message */
#ifdef MEDMEM
    sprintf(buf,"%s: ",hdr);
    STRCAT(buf, emsg);
#else
    sprintf(buf,"%s: %s",hdr,emsg);
#endif
    errputstr(buf);

    /* print the argument */
    if (arg != s_unbound) {
        errputstr(" - ");
        errprint(arg);
    }

    /* no argument, just end the line */
    else
        errputstr("\n");

    /* print the continuation message */
    if (cmsg != NULL) {
#ifdef MEDMEM
        strcpy(buf,"if continued: ");
        STRCAT(buf, cmsg);
        strcat(buf, "\n");
#else
        sprintf(buf,"if continued: %s\n",cmsg);
#endif
        errputstr(buf);
    }
}

#ifdef NEED_TO_REPLACE_BREAKLOOP
/* $putpatch.c$: "MODULE_XLDBUG_C_BREAKLOOP_REPLACEMENT" */
#else

/* breakloop - the debug read-eval-print loop */
LOCAL VOID NEAR breakloop(hdr,cmsg,emsg,arg,cflag)
  char *hdr, FAR *cmsg, FAR *emsg; LVAL arg; int cflag;
{
    LVAL expr,val;
    CONTXT cntxt;
    int type;

    /* print the error message */
    xlerrprint(hdr,cmsg,emsg,arg);

    /* handle running in batch mode */
    if (batchmode) xlfatal("uncaught error");

    /* flush the input buffer */
    xlflush();

    /* do the back trace */
    if (!null(getvalue(s_tracenable))) {
        val = getvalue(s_tlimit);
        xlbaktrace(fixp(val) ? (int)getfixnum(val) : -1);
    }

    /* protect some pointers */
    xlsave1(expr);

    /* increment the debug level */
    ++xldebug;

    /* debug command processing loop */
    xlbegin(&cntxt, CF_BRKLEVEL|CF_CLEANUP|CF_CONTINUE, s_true);
    for (type = 0; type == 0; ) {

        /* setup the continue trap */
        if ((type = setjmp(cntxt.c_jmpbuf)) != 0)
            switch (type) {
            case CF_CLEANUP:
                continue;
            case CF_BRKLEVEL:
                type = 0;
                break;
            case CF_CONTINUE:
                if (cflag) {
                    dbgputstr("[ continue from break loop ]\n");
                    continue;
                }
                else xlabort("this error can't be continued");
            }

        /* print a prompt */
#ifdef USEQT
        xrevertToText();
#endif
#ifdef PACKAGES
        {
            LVAL pack = getvalue(s_package);
            if (pack != xluserpack && goodpackagep(pack)) {
#ifdef MEDMEM
                STRCPY(buf, getstring(xlpackagename(pack)));
                dbgputstr(buf);
#else
                dbgputstr(getstring(xlpackagename(pack)));
#endif /* MEDMEM */
                dbgputstr(" ");
            }
        }
#endif /* PACKAGES */
        sprintf(buf,"%d> ",xldebug);
        dbgputstr(buf);

        /* read an expression and check for eof */
        if (!xlread(getvalue(s_debugio),&expr,FALSE,FALSE)) {
            type = CF_CLEANUP;
            break;
        }

        /* save the input expression */
        xlrdsave(expr);

        /* evaluate the expression */
        expr = xleval(expr);

        /* save the result */
        xlevsave(expr);

        /* Show result on a new line -- TAA MOD to improve display */
        xlfreshline(getvalue(s_debugio));

        /* print it */
#ifdef MULVALS
        {
            int i;
            for (i = 0; i < xlnumresults; i++)
                dbgprint(xlresults[i]);
        }
#else
        dbgprint(expr);
#endif /* MULVALS */
    }
    xlend(&cntxt);

    /* decrement the debug level */
    --xldebug;

    /* restore the stack */
    xlpop();

    /* check for aborting to the previous level */
    if (type == CF_CLEANUP)
        xlbrklevel();
}
#endif

/* baktrace - do a back trace */
VOID xlbaktrace(n)
  int n;
{
    FRAMEP fp, p;
    int argc;
    for (fp = xlfp; (n < 0 || n--) && !null(*fp); fp = fp - (int)getfixnum(*fp)) {
        p = fp + 1;
        errputstr("Function: ");
        errprint(*p++);
        if ((argc = (int)getfixnum(*p++)) != 0)
            errputstr("Arguments:\n");
        while (--argc >= 0) {
            errputstr("  ");
            errprint(*p++);
        }
    }
}

/* xldinit - debug initialization routine */
VOID xldinit()
{
    xlsample = 0;
    xldebug = 0;
}

