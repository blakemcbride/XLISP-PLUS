/* xlsubr - xlisp builtin function support routines */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

/* Function prototypes */
LOCAL int NEAR stringcmp _((LVAL arg1, LVAL arg2));

/* xlsubr - define a builtin function */

LVAL xlsubr(sname,type,fcn,offset)
  char *sname; int type; LVAL (*fcn) _((void)); int offset;
{
    LVAL sym;
    sym = xlenter(sname);
#ifdef MULVALS
    setfunction(sym,cvsubr(fcn, type&TYPEFIELD, offset));
    setmulvalp(getfunction(sym), (char)((type & (TYPEFIELD+1))?TRUE:FALSE));
#else
    setfunction(sym,cvsubr(fcn,type,offset));
#endif
    return (sym);
}

#ifdef AOKKEY
/* TAA Addition 9/93 */
/* xllastkey - no more keys expected, check for :allow-other-keys */
VOID xllastkey()
{
    LVAL *argv=xlargv;
    int argc=xlargc;
    
    if (argc==0) return; /* no more keys */

    for (; (argc -=2) >= 0; argv +=2) {
        if (*argv == k_allow_other_keys) {
            if (null(argv[1])) break; /* why anyone would do this is beyond me */
            xlargv += xlargc;
            xlargc = 0;
            return; /* good news */
        }
    }
    xltoomany(); /* bad news */
}
#endif

/* xlgetkeyarg - get a keyword argument */
int xlgetkeyarg(key,pval)
  LVAL key,*pval;
{
#if 0
    LVAL *argv=xlargv;
    int argc=xlargc;
    for (argv = xlargv, argc = xlargc; (argc -= 2) >= 0; argv += 2) {
        if (*argv == key) {
            *pval = *++argv;

            /* delete the used argument */
            if (argc>0) memcpy(argv-1, argv+1, argc*sizeof(LVAL));
            xlargc -=2;

            return (TRUE);
        }
    }
    return (FALSE);
#else
    int argc = xlargc;
    int result = FALSE;
    int i, first;
    LVAL *argv = xlargv;

    /* First check if we have an even number of arguments remaining */
    if (( argc & 1 )) {
        xlfail( "keyword value missing" );
        return FALSE;
    }
    /*
     * Argument count is ok, now we traverse the remaining key-value-pairs to
     * search for the requested key. During the traversal, we move the
     * matching argument to the front of the remaining arguments, so used
     * arguments get popped off the stack only at the front, no entries are
     * overwritten. 
     * Tricky: keyword processing is specified to take only the leftmost
     * occurence of a key-value-pair in the argument list and ignore the
     * others - to allow the C-macro xllastarg to work correctly, we search
     * all remaining arguments and move the unused pairs to the front of the
     * remaining arguments but just behind the used one. This method allows the
     * function xlbaktrace to print a correct arguments during a backtrace as
     * no arguments are overwritten (this has been the case in the old
     * implementation above) but only their order gets changed. Remind: the
     * sorting is stable (the leftmost key od a duplicate remains the
     * leftmost).
     * FIXME:
     * Still untreated is the occurence of :allow_other_keys in the argument
     * list: xllastarg should be in closer contact with this function to do
     * the correct thing. However: I do not expect functions having so complex
     * argument-lists implemented in C, so the provided macros will work fine.
     * From: wolfgang@pr-wiesbaden.de (Wolfgang Kechel - Patzschke + Rasp GmbH)
     */
     for ( i = first = 0; i < argc; i += 2 ) {
         if ( argv[i] == key ) {
             if ( ! result ) {
                 *pval = xlargv[i+1];
                 result = TRUE;
             }
             if ( i != first ) {
                 LVAL temp[2];
                 int j;

                 temp[0] = argv[i];
                 temp[1] = argv[i+1];
                 for ( j = i+1; j > first; --j )
                     argv[j] = argv[j-2];
                 argv[first] = temp[0];
                 argv[first+1] = temp[1];
             }
             first += 2;
         }
     }
     xlargc -= first;
     xlargv += first;
     return result;
#endif
}

/* xlgkfixnum - get a fixnum keyword argument */
int xlgkfixnum(key,pval)
  LVAL key,*pval;
{
    if (xlgetkeyarg(key,pval)) {
        if (!fixp(*pval))
            xlbadtype(*pval);
        return (TRUE);
    }
    return (FALSE);
}

/* xltest - get the :test or :test-not keyword argument */
VOID xltest(pfcn,ptresult)
  LVAL *pfcn; int *ptresult;
{
    if (xlgetkeyarg(k_test,pfcn))       /* :test */
        *ptresult = TRUE;
    else if (xlgetkeyarg(k_tnot,pfcn))  /* :test-not */
        *ptresult = FALSE;
    else {
        *pfcn = getfunction(s_eql);
        *ptresult = TRUE;
    }
}

/* xlgetfile - get a file or stream */
LVAL xlgetfile(outflag)
  int outflag;
{
    LVAL arg;

    /* get a file or stream (cons) or nil */
    if (null(arg = xlgetarg()))
        return getvalue(outflag ? s_stdout: s_stdin);
    else if (streamp(arg)) {
        if (getfile(arg) == CLOSED)
            xlfail("file not open");
#ifdef BIGNUMS
        if (arg->n_sflags & S_BINARY)
            xlfail("binary file");
#endif
    }
    else if (arg == s_true)
        return getvalue(s_termio);
    else if (!ustreamp(arg))
        xlbadtype(arg);
    return arg;
}

/* xlgetfname - get a filename */
LVAL xlgetfname()
{
    LVAL name;

    /* get the next argument */
    name = xlgetarg();

    /* get the filename string */
#ifdef FILETABLE
    if (streamp(name) && getfile(name) > CONSOLE)
        /* "Steal" name from file stream */
        name = cvstring(filetab[getfile(name)].tname);
    else
#endif
    if (symbolp(name))
        name = getpname(name);
    else if (!stringp(name))
        xlbadtype(name);

    if (getslength(name) >= FNAMEMAX)
        xlerror("file name too long", name);

    /* return the name */
    return (name);
}

/* needsextension - check if a filename needs an extension */
int needsextension(name)
  char *name;
{
    char *p;

#ifdef NO_EXTENSIONS    /* for systems not using filename extensions */
    return (FALSE);
#else
    /* check for an extension */
    for (p = &name[strlen(name)]; --p >= &name[0]; )
        if (*p == '.')
            return (FALSE);
        else if (!islower(*p) && !isupper(*p) && !isdigit(*p))
            return (TRUE);

    /* no extension found */
    return (TRUE);
#endif
}

/* xlbadtype - report a "bad argument type" error */
LVAL xlbadtype(arg)
  LVAL arg;
{
    return xlerror("bad argument type",arg);
}

/* xltoofew - report a "too few arguments" error */
LVAL xltoofew()
{
    xlfail("too few arguments");
    return (NIL);   /* never returns */
}

/* xltoomany - report a "too many arguments" error */
VOID xltoomany()
{
    xlfail("too many arguments");
}

/* xltoolong - report a "too long to process" error */
VOID xltoolong()
{
    xlfail("too long to process");
}

/* xlnoassign - report a "can't assign/bind to constant" error */
VOID xlnoassign(arg)
   LVAL arg;
{
    xlerror("can't assign/bind to constant", arg);
}

#ifdef COMPLX
#define comparecomplex(arg1, arg2) \
  (eql(getreal(arg1), getreal(arg2)) && eql(getimag(arg1), getimag(arg2)))
#endif
#ifdef BIGNUMS
#define compareratio(arg1, arg2) \
  (eql(getnumer(arg1), getnumer(arg2)) && eql(getdenom(arg1), getdenom(arg2)))
#endif

/* eql - internal eql function */
int eql(arg1,arg2)
  LVAL arg1,arg2;
{
    /* compare the arguments */
    if (arg1 == arg2)
        return (TRUE);
    else if (arg1 != NIL) {
        switch (ntype(arg1)) {
        case FIXNUM:
            return (fixp(arg2) ? getfixnum(arg1)==getfixnum(arg2) : FALSE);
#ifdef BIGNUMS
        case RATIO:
            return (ratiop(arg2) ? compareratio(arg1, arg2) : FALSE);
        case BIGNUM:
            return (bignump(arg2) ? comparebignum(arg1, arg2) == 0 : FALSE);
#endif
        case FLONUM:
            return (floatp(arg2) ? getflonum(arg1)==getflonum(arg2) : FALSE);
#ifdef COMPLX
        case COMPLEX:
            return (complexp(arg2) ? comparecomplex(arg1,arg2) : FALSE);
#endif
        default:
            return (FALSE);
        }
    }
    else
        return (FALSE);
}

LOCAL int NEAR stringcmp(arg1, arg2) /* compare two strings for equal */
LVAL arg1, arg2;                    /* Written by TAA. Compares strings */
                                    /* with embedded nulls */
{
    char FAR *s1 = getstring(arg1), FAR *s2 = getstring(arg2);
    unsigned l = getslength(arg1);

    if (l != getslength(arg2)) return FALSE;

    while (l-- > 0) if (*s1++ != *s2++) return FALSE;

    return TRUE;
}

/* equal- internal equal function */
int equal(arg1,arg2)
  LVAL arg1,arg2;
{
    FIXTYPE n=0;    /* for circularity check -- 6/93 */
    
    /* compare the arguments */
isItEqual:  /* turn tail recursion into iteration */
    if (arg1 == arg2)
        return (TRUE);
    else if (arg1 != NIL) {
        switch (ntype(arg1)) {
        case FIXNUM:
            return (fixp(arg2) ? getfixnum(arg1)==getfixnum(arg2) : FALSE);
#ifdef BIGNUMS
        case RATIO:
            return (ratiop(arg2) ? compareratio(arg1, arg2) : FALSE);
        case BIGNUM:
            return (bignump(arg2) ? comparebignum(arg1, arg2) == 0 : FALSE);
#endif
        case FLONUM:
            return (floatp(arg2) ? getflonum(arg1)==getflonum(arg2) : FALSE);
#ifdef COMPLX
        case COMPLEX:
            return (complexp(arg2) ? comparecomplex(arg1,arg2) : FALSE);
#endif
        case STRING:
            return (stringp(arg2) ? stringcmp(arg1,arg2) : FALSE); /* TAA MOD */
        case CONS:  /* TAA MOD turns tail recursion into iteration */
                    /* Not only is this faster, but greatly reduces chance */
                    /* of stack overflow */
#ifdef STSZ
            if (consp(arg2) && (stchck(), equal(car(arg1),car(arg2))))
#else
            if (consp(arg2) && equal(car(arg1),car(arg2))) 
#endif
            {
                arg1 = cdr(arg1);
                arg2 = cdr(arg2);
                if (++n > nnodes) xlfail("circular list");
                goto isItEqual;
            }
            return FALSE;
        default:
            return (FALSE);
        }
    }
    else
        return (FALSE);
}


#ifdef KEYARG
/* TAA Addition */
/* xlkey - get the :key keyword argument */

LVAL xlkey()
{
    LVAL kfcn;

/* TAA MOD, 7/93, so if key is IDENTITY, it is ignored */
    if (xlgetkeyarg(k_key,&kfcn) && kfcn != s_identity) return kfcn;
    return NIL;
}

/* xlapp1 - apply a function of a single argument */
LVAL xlapp1(fun,arg)
  LVAL fun,arg;
{
    FRAMEP newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)1));
    pusharg(arg);
    xlfp = newfp;

    /* return the result of applying the function */
    return xlapply(1);

}


/* dotest1 - call a test function with one argument */
int dotest1(arg,fun,kfun)
  LVAL arg,fun,kfun;
{
    FRAMEP newfp;

    if (kfun != NIL) arg = xlapp1(kfun,arg);

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)1));
    pusharg(arg);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(1) != NIL);

}

/* dotest2 - call a test function with two arguments */
int dotest2(arg1,arg2,fun,kfun)
  LVAL arg1,arg2,fun,kfun;
{
    FRAMEP newfp;

    if (kfun != NIL) arg2 = xlapp1(kfun,arg2);

    /* Speedup for default case TAA MOD */
    if (fun == getfunction(s_eql))
        return (eql(arg1,arg2));

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(arg1);
    pusharg(arg2);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(2) != NIL);

}

/* dotest2s - call a test function with two arguments, symmetrical */
int dotest2s(arg1,arg2,fun,kfun)
  LVAL arg1,arg2,fun,kfun;
{
    FRAMEP newfp;

    if (kfun != NIL) {
        arg1 = xlapp1(kfun,arg1);
        arg2 = xlapp1(kfun,arg2);
    }

    /* Speedup for default case TAA MOD */
    if (fun == getfunction(s_eql))
        return (eql(arg1,arg2));

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(arg1);
    pusharg(arg2);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(2) != NIL);

}

#else
/* dotest1 - call a test function with one argument */
int dotest1(arg,fun)
  LVAL arg,fun;
{
    FRAMEP newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)1));
    pusharg(arg);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(1) != NIL);

}

/* dotest2 - call a test function with two arguments */
int dotest2(arg1,arg2,fun)
  LVAL arg1,arg2,fun;
{
    FRAMEP newfp;

    /* Speedup for default case TAA MOD */
    if (fun == getfunction(s_eql))
        return (eql(arg1,arg2));

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(arg1);
    pusharg(arg2);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(2) != NIL);

}

#endif

#ifdef COMPLX
/* return value of a number coerced to a FLOTYPE */
FLOTYPE makefloat(x)
     LVAL x;
{
    switch (ntype(x)) {
        case FIXNUM: return ((FLOTYPE) getfixnum(x));
        case FLONUM: return getflonum(x);
#ifdef BIGNUMS
        case BIGNUM: return cvtbigflonum(x);
        case RATIO:  return cvtratioflonum(x);
#endif
    }
    xlerror("not a number", x);
    return 0.0; /* never reached */
}
#endif

#ifdef STSZ

VOID stchck(VOID) {
    int dummy;
    int stackleft = STACKREPORT(dummy);
    
    if (stackleft < (stackwarn ?  MARGLO : marghi)) {
        stackwarn = TRUE;
        if (stackleft>MARGLO) 
            xlcerror("use full stack",
                     "system stack is low, bytes left", cvfixnum(stackleft));
        else {
            xlabort("system stack overflow");
        }
    }
}

LVAL xsetmark(VOID) {
    FIXTYPE n, oldval=marghi;
    FIXTYPE left = STACKREPORT(n);

    n = getfixnum(xlgafixnum()) + MARGLO;
    
    if (n > left-MARGLO) n = left;  /* can't request more than is available */
    if (n <= MARGLO) n = MARGLO;    /* can't be less than low margin */
    marghi = (int)n;
    return cvfixnum(oldval - MARGLO);
}
#endif
