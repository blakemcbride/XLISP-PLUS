/* xlbfun.c - xlisp basic built-in functions */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

/* forward declarations */
static LVAL NEAR makesymbol _((int iflag));

/* xeval - the built-in function 'eval' */
LVAL xeval()
{
    LVAL expr,oldenv,oldfenv;

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(oldenv);
    xlprotect(oldfenv);

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    /*establish global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;

    /* evaluate the expression */
    expr = xleval(expr);

    /* restore environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(2);

    /* return evaluated expression */
    return (expr);
}

/* xapply - the built-in function 'apply' */
/* Algorithm based on Luke Tierney's XLISP-STAT */

LVAL xapply()
{
    LVAL fun,arglist;
    int n;

    if (xlargc < 2) xltoofew();
    if (! listp(xlargv[xlargc - 1])) xlfail("last argument must be a list");

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(arglist);
    xlprotect(fun);

    fun = xlgetarg();
    n = xlargc - 1;
    arglist = xlargv[n];
    while (n-- > 0) arglist = cons(xlargv[n], arglist);

    /* restore the stack */
    xlpopn(2);

    return xlapply(pushargs(fun, arglist));
}

/* xfuncall - the built-in function 'funcall' */
LVAL xfuncall()
{
    FRAMEP newfp;
    int argc;

    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(xlgetarg());
    pusharg(NIL); /* will be argc */

    /* push each argument */
    for (argc = 0; moreargs(); ++argc)
        pusharg(nextarg());

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* apply the function to the arguments */
    return (xlapply(argc));
}

/* xmacroexpand - expand a macro call repeatedly */
LVAL xmacroexpand()
{
    LVAL form;
#ifdef MULVALS
    LVAL val;
#endif
    form = xlgetarg();
    xllastarg();
#ifdef MULVALS
    val = xlexpandmacros(form);
    xlnumresults = 2;
    xlresults[0] = val;
    xlresults[1] = (val == form) ? NIL : s_true;
    return(val);
#else
    return (xlexpandmacros(form));
#endif /* MULVALS */
}

/* x1macroexpand - expand a macro call */
LVAL x1macroexpand()
{
    LVAL form,fun,args;
#ifdef MULVALS
    int expanded = FALSE;
#endif /* MULVALS */

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(args);

    /* get the form */
    form = xlgetarg();
    xllastarg();

    /* expand until the form isn't a macro call */
    if (consp(form)) {
        fun = car(form);                /* get the macro name */
        args = cdr(form);               /* get the arguments */
        if (symbolp(fun) && (fun = xlxgetfunction(fun)) != s_unbound) {
#ifdef MULVALS
            expanded = macroexpand(fun,args,&form);
#else
            macroexpand(fun,args,&form);
#endif /* MULVALS */
        }
    }

    /* restore the stack and return the expansion */
    xlpopn(2);
#ifdef MULVALS
    xlnumresults = 2;
    xlresults[0] = form;
    xlresults[1] = (expanded) ? s_true : NIL;
#endif /* MULVALS */
    return (form);
}

/* xatom - is this an atom? */
LVAL xatom()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (atom(arg) ? s_true : NIL);
}

/* xsymbolp - is this an symbol? */
LVAL xsymbolp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (symbolp(arg) ? s_true : NIL);
}

/* xnumberp - is this a number? */
LVAL xnumberp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (numberp(arg) ? s_true : NIL);
}

#ifdef COMPLX
/* xcomplexp - is this a complex number? */
LVAL xcomplexp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (complexp(arg) ? s_true : NIL);
}
#endif

/* xintegerp - is this an integer? */
LVAL xintegerp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (integerp(arg) ? s_true : NIL);
}

/* xfloatp - is this a float? */
LVAL xfloatp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (floatp(arg) ? s_true : NIL);
}

#ifdef BIGNUMS
LVAL xrationalp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (rationalp(arg) ? s_true : NIL);
}

LVAL xnumerator()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (fixp(arg)) return cvfixnum(getfixnum(arg));
    if (ratiop(arg)) return getnumer(arg);
    xlbadtype(arg);
    return NIL; /* never executes */
}

LVAL xdenominator()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (fixp (arg)) return cvfixnum((FIXTYPE)1);
    if (ratiop(arg)) return getdenom(arg);
    xlbadtype(arg);
    return NIL; /* never executes */
}
#endif

/* xcharp - is this a character? */
LVAL xcharp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (charp(arg) ? s_true : NIL);
}

/* xstringp - is this a string? */
LVAL xstringp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (stringp(arg) ? s_true : NIL);
}

/* xarrayp - is this an array? */
LVAL xarrayp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (vectorp(arg) ? s_true : NIL);
}

/* xstreamp - is this a stream? */
LVAL xstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (streamp(arg) || ustreamp(arg) ? s_true : NIL);
}

/* xopenstreamp - is this an open stream? */
LVAL xopenstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return s_true;
    if (streamp(arg)) return (getfile(arg) != CLOSED ? s_true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}

/* xinputstreamp - is this an input stream? */
LVAL xinputstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return s_true;
    if (streamp(arg))
        return (getfile(arg)!=CLOSED && (arg->n_sflags&S_FORREADING)?
            s_true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}

/* xoutputstreamp - is this an output stream? */
LVAL xoutputstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return s_true;
    if (streamp(arg))
        return (getfile(arg)!=CLOSED && (arg->n_sflags&S_FORWRITING)?
            s_true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}


/* xobjectp - is this an object? */
LVAL xobjectp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (objectp(arg) ? s_true : NIL);
}

/* xboundp - is this a value bound to this symbol? */
LVAL xboundp()
{
    LVAL sym;
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();
    return (boundp(sym) ? s_true : NIL);
}

/* xfboundp - is this a functional value bound to this symbol? */
LVAL xfboundp()
{
    LVAL sym;
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();
    return (fboundp(sym) ? s_true : NIL);
}

/* xconstantp - is this constant? TAA addition*/
LVAL xconstantp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();

    if ((!null(arg)) &&
        (((ntype(arg)==CONS) && (car(arg) != s_quote)) ||
         ((ntype(arg)==SYMBOL) && (!constantp(arg)))))
        return (NIL);
    return (s_true);
}

/* xspecialp - is the symbol marked special?  Luke Tierney 9/93 */
LVAL xspecialp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return((symbolp(arg) && specialp(arg)) ? s_true : NIL);
}

/* xmarkspecial - mark the symbol as special Luke Tierney 9/93 */
LVAL xmarkspecial()
{
    LVAL arg;
    int constant;
    arg = xlgasymbol();
    constant = moreargs() ? !null(nextarg()) : FALSE;
    xllastarg();

    if (constant)
        setsflags(arg, F_CONSTANT | F_SPECIAL);
    else
        setsflags(arg, F_SPECIAL);
    return(NIL);
}

/* xnull - is this null? */
LVAL xnull()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (null(arg) ? s_true : NIL);
}

/* xlistp - is this a list? */
LVAL xlistp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (listp(arg) ? s_true : NIL);
}

/* xendp - is this the end of a list? */
LVAL xendp()
{
    LVAL arg;
    arg = xlgalist();
    xllastarg();
    return (null(arg) ? s_true : NIL);
}

/* xconsp - is this a cons? */
LVAL xconsp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (consp(arg) ? s_true : NIL);
}

/* xeq - are these equal? */
LVAL xeq()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (arg1 == arg2 ? s_true : NIL);
}

/* xeql - are these equal? */
LVAL xeql()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (eql(arg1,arg2) ? s_true : NIL);
}

/* xequal - are these equal? (recursive) */
LVAL xequal()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (equal(arg1,arg2) ? s_true : NIL);
}

/* xset - built-in function set */
LVAL xset()
{
    LVAL sym,val;

    /* get the symbol and new value */
    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    if (constantp(sym)) {
        xlnoassign(sym);
    }

    /* assign the symbol the value of argument 2 and the return value */
    setvalue(sym,val);

    /* return the result value */
    return (val);
}

/* xgensym - generate a symbol */
LVAL xgensym()
{
#ifdef BETTERGENSYM
    char prefix[STRMAX+22];
    FIXTYPE suffix;
    int storeflag = TRUE;
    if ((!fixp(getvalue(s_gensymcounter))) || (suffix = getfixnum(getvalue(s_gensymcounter))) < 0 )
        xlerror("*gensym-counter* isn't non-negative fixnum", getvalue(s_gensymcounter));
    strcpy(prefix, "G");
    if (moreargs()) {
        LVAL x = xlgetarg();
        switch (null(x)? CONS : ntype(x)) {
            case STRING:
                STRNCPY(prefix, getstring(x), STRMAX);
                prefix[STRMAX] = '\0';
                break;
            case FIXNUM:
                suffix = getfixnum(x);
                storeflag = FALSE;
                if (suffix > 0) break;
            default:
                xlerror("Invalid argument", x);
        }
    }
    sprintf(&prefix[strlen(prefix)], "%ld", suffix);
    if (storeflag) setvalue(s_gensymcounter, cvfixnum((suffix == MAXFIX) ? 0 : suffix+1));
    return (xlmakesym(prefix));

#else

    char sym[STRMAX+22]; /* enough space for prefix and number */
    LVAL x;

    /* get the prefix or number */
    if (moreargs()) {
        x = xlgetarg();
        switch (null(x)? CONS : ntype(x)) { /* was ntype(x)   TAA Mod */
        case SYMBOL:
                x = getpname(x);
        case STRING:
                STRNCPY(gsprefix,getstring(x),STRMAX);
                gsprefix[STRMAX] = '\0';
                break;
        case FIXNUM:
                gsnumber = getfixnum(x);
                break;
        default:
                xlbadtype(x);
        }
    }
    xllastarg();

    /* create the pname of the new symbol */
    sprintf(sym,"%s%ld",gsprefix,(long)gsnumber++); /* TAA Fix 2/94 --
                                                originally considered gsnumber
                                                to be an int */

    /* make a symbol with this print name */
    return (xlmakesym(sym));
#endif
}

/* xmakesymbol - make a new uninterned symbol */
LVAL xmakesymbol()
{
    return (makesymbol(FALSE));
}

/* xintern - make a new interned symbol */
LVAL xintern()
{
    return (makesymbol(TRUE));
}

/* makesymbol - make a new symbol */
LOCAL LVAL NEAR makesymbol(iflag)
  int iflag;
{
    LVAL pname;
#ifdef PACKAGES
    LVAL pack;
#ifdef MULVALS
    LVAL sym;
    int found;
#endif /* MULVALS */
#endif /* PACKAGES */
    int i;

    /* get the print name of the symbol to intern */
    pname = xlgastring();
#ifdef PACKAGES
    if (iflag)
        pack = xlgetpackage((moreargs()) ? xlgetarg() : getvalue(s_package));
#endif /* PACKAGES */
    xllastarg();

    /* check for containing only printable characters */
    i = getslength(pname);
    if (i >= STRMAX)
	    xlerror("too long", pname);
    /* June 2011 -- I modified the following to allow printing
     * characters >128 and disallow the rubout character. Call this an
     * old oversight.  Tom Almy */
#ifdef ANSI8
    while (i-- > 0) if ((pname->n_string[i] & 0x7f) < 32 || (pname->n_string[i]& 0x7f) == 127 )
#else
    while (i-- > 0) if (pname->n_string[i] < 32 || pname->n_string[i] == 127)
#endif
	    xlerror("non-printing characters",pname);

    /* make the symbol */
#ifdef PACKAGES
#ifdef MULVALS
    if (iflag) {
        /* TAA fix, 7/97, change order of next two statements and
         * added if condition */
        found = xlfindsymbol(getstring(pname), pack, &sym);
        if (found == SYM_NOT_FOUND)
            sym = xlintern(getstring(pname), pack);
        xlnumresults = 2;
        xlresults[0] = sym;
        switch (found) {
            case SYM_INTERNAL: xlresults[1] = k_internal; break;
            case SYM_EXTERNAL: xlresults[1] = k_external; break;
            case SYM_INHERITED: xlresults[1] = k_inherited; break;
            default: xlresults[1] = NIL;
        }
        return(sym);
    }
    else return(xlmakesym(getstring(pname)));
#else
    return (iflag ? xlintern(getstring(pname), pack)
                  : xlmakesym(getstring(pname)));
#endif /* MULVALS */
#else
#ifdef MEDMEM
    STRCPY(buf, getstring(pname));
    return (iflag ? xlenter(buf)
                  : xlmakesym(buf));
#else
    return (iflag ? xlenter(getstring(pname))
                  : xlmakesym(getstring(pname)));
#endif /* MEDMEM */
#endif /* PACKAGES */
}

/* xsymname - get the print name of a symbol */
LVAL xsymname()
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();

    /* return the print name */
    return (getpname(sym));
}

/* xsymvalue - get the value of a symbol */
LVAL xsymvalue()
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();

    /* get the global value */
    while ((val = getvalue(sym)) == s_unbound)
        xlunbound(sym);

    /* return its value */
    return (val);
}

/* xsymfunction - get the functional value of a symbol */
LVAL xsymfunction()
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymornil();       /* TAA fix */
    xllastarg();

    /* get the global value */
    while ((val = getfunction(sym)) == s_unbound)
        xlfunbound(sym);

    /* return its value */
    return (val);
}

/* xsymplist - get the property list of a symbol */
LVAL xsymplist()
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();

    /* return the property list */
    return (getplist(sym));
}

/* xget - get the value of a property */
/* TAA MOD 7/93 -- added default argument */
LVAL xget()
{
    LVAL sym,prp,dflt=NIL;

    /* get the symbol and property */
    sym = xlgasymbol();
    prp = xlgetarg();
    if (moreargs()) dflt = xlgetarg();
    xllastarg();

    /* retrieve the property value */
    return (null(prp = findprop(getplist(sym),prp)) ? dflt : car(prp));
}

/* xgetf - get the value of a property  NEW 7/93 */
LVAL xgetf()
{
    LVAL plist,prp,dflt=NIL;

    /* get the plist and property */
    plist = xlgetarg();
    prp = xlgetarg();
    if (moreargs()) dflt = xlgetarg();
    xllastarg();

    /* retrieve the property value */
    return (null(prp = findprop(plist,prp)) ? dflt : car(prp));
}
    

/* xputprop - set the value of a property */
LVAL xputprop()
{
    LVAL sym,val,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    val = xlgetarg();
    prp = xlgetarg();
    xllastarg();

    /* set the property value */
    xlputprop(sym,val,prp);

    /* return the value */
    return (val);
}

/* xremprop - remove a property value from a property list */
LVAL xremprop()
{
    LVAL sym,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    prp = xlgetarg();
    xllastarg();

    /* remove the property */
    xlremprop(sym,prp);

    /* return nil */
    return (NIL);
}

/* xhash - compute the hash value of a string or symbol */
/* TAA Modified to hash anything */
LVAL xhash()
{
    LVAL len,val;
    int n;

    /* get the object and the table length */
    val = xlgetarg();
    len = xlgafixnum(); n = (int)getfixnum(len);
    xllastarg();

    /* check for hash arg out of range */
    if (n <= 0) xlbadtype(len);

    /* return the hash index */
    return (cvfixnum((FIXTYPE)xlhash(val,n)));
}



/* xaref - array reference function */
LVAL xaref()
{
    LVAL array,index;
    FIXTYPE i;          /* TAA fix */

    /* get the array (may be a string) and the index */
    array = xlgetarg();
    index = xlgafixnum();
    i = getfixnum(index);       /* TAA fix */
    xllastarg();

    if (stringp(array)) {   /* extension -- allow fetching chars from string*/
        if (i < 0 || i >= (FIXTYPE)getslength(array))
            xlerror("string index out of bounds",index);
        return (cvchar(getstringch(array,(int)i)));
    }

    if (!vectorp(array)) xlbadtype(array);  /* type must be array */

    /* range check the index */
    if (i < 0 || i >= getsize(array))
        xlerror("array index out of bounds",index);

    /* return the array element */
    return (getelement(array,(int)i));  /* TAA fix -- casting */
}

/* xmkarray - make a new array */
LVAL xmkarray()
{
    LVAL val, initelem = NIL, initcont = NIL;
    unsigned int i;
    FIXTYPE n;

    /* get the size of the array */
    val = xlgafixnum() ; n = getfixnum(val);
    if (n < 0 || n > MAXSLEN )
        xlerror("out of range", val);
    if (!xlgetkeyarg(k_initcont, &initcont))
        xlgetkeyarg(k_initelem, &initelem);
    xllastkey();

    /* create the array */
    val = newvector((unsigned) n);

    if (null(initcont)) {
        if (!null(initelem)) 
            for (i = (unsigned int)n; i-- > 0;)
                setelement(val, i, initelem);
    }
    else switch (ntype(initcont)) {
        case CONS:  /* initialize from a list */
            for (i = 0; i < (unsigned int) n; i++, initcont = cdr(initcont)) {
                if (!consp(initcont)) goto badlength;
                setelement(val, i, car(initcont));
            }
            if (consp(initcont)) goto badlength;
            break;
        case VECTOR:  /* initialize from an array */
            if (n != getsize(initcont)) goto badlength;
            for (i = (unsigned int) n; i-- > 0;)
                setelement(val, i, getelement(initcont, i));
            break;
        case STRING: /* initialize from a string */
            if (n != (FIXTYPE)getslength(initcont)) goto badlength;
            for (i = (unsigned int) n; i-- > 0;)
                setelement(val, i, cvchar(getstringch(initcont, i)));
            break;
        default: xlbadtype(initcont);
    }
    
    return val;
    
    badlength: xlerror("initial contents sequence wrong length", s_unbound);
    return NIL;
}

/* xvector - make a vector */
LVAL xvector()
{
    LVAL val;
    int i;

    /* make the vector */
    val = newvector(xlargc);

    /* store each argument */
    for (i = 0; moreargs(); ++i)
        setelement(val,i,nextarg());
    xllastarg();

    /* return the vector */
    return (val);
}

#ifdef OLDERRORS    /* Normally we don't want to use this code! */
/* xerror - special form 'error' */
LVAL xerror()
{
    LVAL emsg,arg;

    /* get the error message and the argument */
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    return (xlerror(getstring(emsg),arg));
}

/* xcerror - special form 'cerror' */
LVAL xcerror()
{
    LVAL cmsg,emsg,arg;

    /* get the correction message, the error message, and the argument */
    cmsg = xlgastring();
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    xlcerror(getstring(cmsg),getstring(emsg),arg);

    /* return nil */
    return (NIL);
}

/* xbreak - special form 'break' */
LVAL xbreak()
{
    LVAL emsg,arg;

    /* get the error message */
    emsg = (moreargs() ? xlgastring() : NIL);
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* enter the break loop */
    xlbreak((!null(emsg) ? getstring(emsg) : (char FAR *)"**BREAK**"),arg);

    /* return nil */
    return (NIL);
}

#else

/* xerror - special form 'error' */
LVAL xerror()
{
    LVAL emsg;
    LVAL val = newustream();
    
    xlprot1(val);
    
    /* get the error message */
    emsg = xlgastring();

    xlformat(emsg, val);

    val = getstroutput(val);
    
    /* we don't need xlpop here because we don't return! */

    /* signal the error */
    return (xlerror(getstring(val),s_unbound));
}

/* xcerror - special form 'cerror' */
LVAL xcerror()
{
    LVAL cmsg, emsg, val1, val2;
    LVAL NEAR *origargv;
    int origargc;

    /* create the string streams */
    xlstkcheck(2);
    xlprotect(val1);
    val1 = newustream();
    xlprotect(val2);
    val2 = newustream();

    /* get the correction message and the error message */
    cmsg = xlgastring();
    emsg = xlgastring();

    /* process the message strings */
    origargv = xlargv;
    origargc = xlargc;
    xlformat(cmsg, val1);
    val1 = getstroutput(val1);
    xlargv = origargv;
    xlargc = origargc;
    xlformat(emsg, val2);
    val2 = getstroutput(val2);

    /* signal the error */
    xlcerror(getstring(val1),getstring(val2),s_unbound);

    xlpopn(2);

    /* return nil */
    return (NIL);
}

/* xbreak - special form 'break' */
LVAL xbreak()
{
    LVAL emsg;
    LVAL val = newustream();
    
    xlprot1(val);
    
    /* get the error message */
    emsg = xlgastring();

    xlformat(emsg, val);

    val = getstroutput(val);
    
    /* enter the break loop */
    xlbreak(getstring(val), s_unbound);

    /* restore stack */
    xlpop();

    /* return nil */
    return (NIL);
}
#endif

/* xcleanup - special form 'clean-up' */
LVAL xcleanup()
{
    xllastarg();
    xlcleanup();
    return (NIL);
}

/* xtoplevel - special form 'top-level' */
LVAL xtoplevel()
{
    xllastarg();
    xltoplevel();
    return (NIL);
}

/* xcontinue - special form 'continue' */
LVAL xcontinue()
{
    xllastarg();
    xlcontinue();
    return (NIL);
}

/* xevalhook - eval hook function */
LVAL xevalhook()
{
    LVAL expr,newehook,newahook,newenv,oldenv,oldfenv,olddenv,val;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(newenv);

    /* get the expression, the new hook functions and the environment */
    expr = xlgetarg();
    newehook = xlgetarg();
    newahook = xlgetarg();
    newenv = (moreargs() ? xlgalist() : NIL);
    xllastarg();

    /* bind *evalhook* and *applyhook* to the hook functions */
    olddenv = xldenv;
    xldbind(s_evalhook,newehook);
    xldbind(s_applyhook,newahook);

    /* establish the environment for the hook function */
#if 0   /* old way, if env is NIL then uses current environment */
    if (!null(newenv)) {
        oldenv = xlenv;
        oldfenv = xlfenv;
        xlenv = car(newenv);
        xlfenv = cdr(newenv);
    }
#else   /* TAA MOD -- if env is NIL then uses global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    if (!null(newenv)) {
        xlenv = car(newenv);
        xlfenv = cdr(newenv);
    }
    else {
        xlenv = xlfenv = NIL;
    }
#endif
    /* evaluate the expression (bypassing *evalhook*) */
    val = xlxeval(expr);

    /* restore the old environment */
    xlunbind(olddenv);
#if 0
    if (!null(newenv)) {
        xlenv = oldenv;
        xlfenv = oldfenv;
    }
#else
    xlenv = oldenv;
    xlfenv = oldfenv;
#endif

    /* restore the stack */
    xlpopn(3);

    /* return the result */
    return (val);
}

#ifdef APPLYHOOK
/* xapplyhook - apply hook function */
LVAL xapplyhook()
{
    LVAL fcn,args,newehook,newahook,olddenv,val;

    /* get the function, arguments, and the new hook functions */
    fcn = xlgetarg();
    args = xlgetarg();
    newehook = xlgetarg();
    newahook = xlgetarg();
    xllastarg();

    /* bind *evalhook* and *applyhook* to the hook functions */
    olddenv = xldenv;
    xldbind(s_evalhook,newehook);
    xldbind(s_applyhook,newahook);

    /* apply function (apply always bypasses hooks) */
    val = xlapply(pushargs(fcn,args));

    /* restore the old environment */
    xlunbind(olddenv);

    /* return the result */
    return (val);
}

#endif

#ifdef MULVALS
LVAL xvalues()
{
    int i;
    if (xlargc > MULVALLIMIT)
        xlfail("too many values ");
    for (i = 0; i < xlargc; i++)
        xlresults[i] = xlargv[i];
    xlnumresults = xlargc;
    return(xlargc > 0 ? xlargv[0] : NIL);
}
#endif /* MULVALS */

