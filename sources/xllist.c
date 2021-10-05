/* xllist.c - xlisp built-in list functions */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

struct nsubargs { /* TAA added 7/93 */
    LVAL to;    /* and ALIST */
    LVAL from;
    LVAL fcn;
#ifdef KEYARG
    LVAL kfcn;
#endif
    int tresult;
    int expr;
    int subst;
};

struct substargs {  /* TAA MOD - 7/93 to reduce stack usage */
    LVAL to;
    LVAL from;
    LVAL fcn;
#ifdef KEYARG
    LVAL kfcn;
#endif
    int tresult;
};

struct sublargs {   /* TAA MOD - 7/93 to reduce stack usage */
    LVAL alist;
    LVAL fcn;
#ifdef KEYARG
    LVAL kfcn;
#endif
    int tresult;
};

/* forward declarations */
LOCAL LVAL NEAR cxr _((char *adstr));
LOCAL LVAL NEAR nth _((int charflag));
LOCAL LVAL NEAR subst _((LVAL expr, struct substargs *args));
LOCAL LVAL NEAR sublis _((LVAL expr, struct sublargs *args));
#ifdef KEYARG
LOCAL LVAL NEAR assoc _((LVAL expr, LVAL alist, LVAL fcn, LVAL kfcn, int tresult));
LOCAL LVAL NEAR sublis _((LVAL expr, struct sublargs *args));
#ifdef SETS
LOCAL LVAL NEAR membr _((LVAL expr,LVAL list,LVAL fcn,LVAL kfcn,int tresult));
#endif
#else /* KEYARG */
LOCAL LVAL NEAR assoc _((LVAL expr, LVAL alist, LVAL fcn, int tresult));
#ifdef SETS
LOCAL LVAL NEAR membr _((LVAL expr,LVAL list,LVAL fcn,int tresult));
#endif
#endif
LOCAL LVAL NEAR nsub _((int subst, int tresult, int expr));
LOCAL VOID NEAR nsub1 _((LVAL FAR * tree, struct nsubargs *args));
LOCAL LVAL NEAR map _((int carflag, int valflag));
LOCAL LVAL NEAR set_op _((int which));

/* xlcircular -- circular list error */
VOID xlcircular _((VOID))
{
    xlfail("circular list");
}

/* xcar - take the car of a cons cell */
LVAL xcar()
{
    LVAL list;
    list = xlgalist();
    xllastarg();
    return (null(list) ? NIL : car(list));
}

/* xcdr - take the cdr of a cons cell */
LVAL xcdr()
{
    LVAL list;
    list = xlgalist();
    xllastarg();
    return (null(list) ? NIL : cdr(list));
}

/* cxxr functions */
LVAL xcaar() { return (cxr("aa")); }
LVAL xcadr() { return (cxr("da")); }
LVAL xcdar() { return (cxr("ad")); }
LVAL xcddr() { return (cxr("dd")); }

/* cxxxr functions */
LVAL xcaaar() { return (cxr("aaa")); }
LVAL xcaadr() { return (cxr("daa")); }
LVAL xcadar() { return (cxr("ada")); }
LVAL xcaddr() { return (cxr("dda")); }
LVAL xcdaar() { return (cxr("aad")); }
LVAL xcdadr() { return (cxr("dad")); }
LVAL xcddar() { return (cxr("add")); }
LVAL xcdddr() { return (cxr("ddd")); }

/* cxxxxr functions */
LVAL xcaaaar() { return (cxr("aaaa")); }
LVAL xcaaadr() { return (cxr("daaa")); }
LVAL xcaadar() { return (cxr("adaa")); }
LVAL xcaaddr() { return (cxr("ddaa")); }
LVAL xcadaar() { return (cxr("aada")); }
LVAL xcadadr() { return (cxr("dada")); }
LVAL xcaddar() { return (cxr("adda")); }
LVAL xcadddr() { return (cxr("ddda")); }
LVAL xcdaaar() { return (cxr("aaad")); }
LVAL xcdaadr() { return (cxr("daad")); }
LVAL xcdadar() { return (cxr("adad")); }
LVAL xcdaddr() { return (cxr("ddad")); }
LVAL xcddaar() { return (cxr("aadd")); }
LVAL xcddadr() { return (cxr("dadd")); }
LVAL xcdddar() { return (cxr("addd")); }
LVAL xcddddr() { return (cxr("dddd")); }

/* cxr - common car/cdr routine */
LOCAL LVAL NEAR cxr(adstr)
  char *adstr;
{
    LVAL list;

    /* get the list */
    list = xlgalist();

    xllastarg();

    /* perform the car/cdr operations */
    while (*adstr && consp(list))
        list = (*adstr++ == 'a' ? car(list) : cdr(list));

    /* make sure the operation succeeded */
    if (*adstr && !null(list))
        xlfail("bad argument");

    /* return the result */
    return (list);
}

/* xcons - construct a new list cell */
LVAL xcons()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* construct a new list element */
    return (cons(arg1,arg2));
}

/* xlist - built a list of the arguments */
/* Rewritten by TAA for compactness and speed */
LVAL xlist()
{
    LVAL val;
    int i=xlargc;

    /* protect a pointer */
    xlsave1(val);

    /* do the work */
    while (i-- > 0)
        val = cons(xlargv[i],val);

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}

/* xliststar - built a list of the arguments */
/* by TAA */
LVAL xliststar()
{
    LVAL val;
    int i=xlargc;

    if (i==0) xltoofew();   /* must have at least one argument */

    /* protect a pointer */
    xlprot1(val);

    /* last argument is list tail */

    val = xlargv[--i];

    /* do the work */
    while (i-- > 0)
        val = cons(xlargv[i],val);

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}

/* xbutlast -- copy list for all but last n */
/* Added function TAA */

LVAL xbutlast()
{
    LVAL val,list,last,next;
    FIXTYPE n=1,l=0;

    /* get argument(s) */
    list = xlgalist();
    if (moreargs()) {
        n = getfixnum(next=xlgafixnum());
        if (n<0) xlerror("bad index",next);
        xllastarg();
    }

    /* get length */
    for (next=list; consp(next);) {
        next=cdr(next);
        l++;
        if (l > nnodes) xlcircular();
    }

    /* calc final length */
    l-=n;
    if (l <= 0) return (NIL);   /* nothing left */

    /* do the first cons */

    val = consa(car(list));
    if (l-- == 1) return val;

    /* protect a pointer */
    xlprot1(val);

    /* do remaining conses */
    last = val;
    while (l-- > 0) {
        list = cdr(list);
        next = consa(car(list));
        rplacd(last,next);
        last = next;
    }


    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}


/* xappend - built-in function append */
LVAL xappend()
{
    LVAL list,last=NIL,next,val;
    long n;

    /* protect some pointers */
    xlsave1(val);

    /* append each argument */
    if (moreargs()) {
        while (xlargc > 1) {
            /* check for circular list (Added 5/6/94) */
            next = list = nextarg();
            for (n = 0; consp(next); next=cdr(next)) {
                if (n++ > nnodes) xlcircular(); /*DIRTY, but we loose anyway!*/
            }
            /* append each element of this list to the result list */
            for (; consp(list); list = cdr(list)) {
                next = consa(car(list));
                if (!null(val)) rplacd(last,next);
                else val = next;
                last = next;
            }
            if (!null(list)) xlbadtype(*--xlargv);  /*TAA added errormessage*/
        }

        /* handle the last argument */
        if (!null(val)) rplacd(last,nextarg());
        else val = nextarg();
    }

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}


/* xlast - return the last cons of a list */
LVAL xlast()
{
    LVAL list;
    long l=0;

    /* get the list */
    list = xlgalist();
    xllastarg();

    /* find the last cons */
    if (consp(list))            /* TAA fix */
        while (consp(cdr(list))) {
            list = cdr(list);
            if (l++ > nnodes) xlcircular();
        }

    /* return the last element */
    return (list);
}

/* xmember - built-in function 'member' */
LVAL xmember()
{
    LVAL x,list,slist,fcn,val;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    /* protect some pointers */
    xlsave1(fcn);
#endif

    /* get the expression to look for and the list */
    x = xlgetarg();
    slist = list = xlgalist();
    xltest(&fcn,&tresult);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

    /* look for the expression */
    for (val = NIL; consp(list); list = cdr(list), slist = cdr(slist)) {
        /* do a pair per iteration */

#ifdef KEYARG
        if (dotest2(x,car(list),fcn,kfcn) == tresult)
#else
        if (dotest2(x,car(list),fcn) == tresult)
#endif
        {
            val = list;
            break;
        }

        if (!consp(list = cdr(list))) {
            if (null(list)) {
                break;
            }
            else
                xlerror("not a proper list", list);
        }
        
#ifdef KEYARG
        if (dotest2(x,car(list),fcn,kfcn) == tresult)
#else
        if (dotest2(x,car(list),fcn) == tresult)
#endif
        {
            val = list;
            break;
        }

        if (list == slist)
            xlerror("not a proper list", car (list));   /* list must be circular */
    }

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the result */
    return (val);
}

/* xassoc - built-in function 'assoc' */
LVAL xassoc()
{
    LVAL x,alist,slist,fcn,pair,val;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    /* protect some pointers */
    xlsave1(fcn);
#endif

    /* get the expression to look for and the association list */
    x = xlgetarg();
    slist = alist = xlgalist();
    xltest(&fcn,&tresult);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

    /* look for the expression */
    for (val = NIL; consp(alist); alist = cdr(alist), slist = cdr(slist)) {
        /* do two iterations per loop */
        if ((!null(pair = car(alist))) && consp(pair))
#ifdef KEYARG
            if (dotest2(x,car(pair),fcn,kfcn) == tresult)
#else
            if (dotest2(x,car(pair),fcn) == tresult)
#endif
            {
                val = pair;
                break;
            }

        if (!consp(alist = cdr(alist))) break;

        if ((!null(pair = car(alist))) && consp(pair))
#ifdef KEYARG
            if (dotest2(x,car(pair),fcn,kfcn) == tresult)
#else
            if (dotest2(x,car(pair),fcn) == tresult)
#endif
            {
                val = pair;
                break;
            }
        
        if (slist == alist) break;  /* circular alist */
    }

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return result */
    return (val);
}




/* xnsubst,xnsublis - destructive versions of subst and sublis */
/* ADDED 7/93 */
LOCAL VOID NEAR nsub1(tree, args)
LVAL FAR *tree; struct nsubargs *args;
{
    LVAL pair;
    FIXTYPE n=0;

tailrecursion:

#ifdef KEYARG
    if (args->subst? 
        (args->expr?
            (dotest2(args->from,*tree,args->fcn,args->kfcn)==args->tresult): 
            (dotest1(*tree, args->fcn, args->kfcn)==args->tresult)) :
        !null(pair=assoc(args->kfcn!=NIL?xlapp1(args->kfcn,*tree):*tree,args->to,args->fcn,NIL,args->tresult)))
#else
    if (args->subst? 
        (args->expr?
            (dotest2(args->from,*tree,args->fcn)==args->tresult): 
            (dotest1(*tree, args->fcn)==args->tresult)) :
        !null(pair=assoc(*tree,args->to,args->fcn,args->tresult)))
#endif
    {
        *tree = (args->subst ? args->to : cdr(pair));
    }
    else if (consp(*tree)) {
#ifdef STSZ         /* This function is a good candidate for stack ov */
        stchck();
#endif
        nsub1(&car(*tree), args);
        tree = &cdr(*tree);
        if (++n > nnodes) 
            xlfail("circular list");    /* only the tip of the iceburg */
        goto tailrecursion;
    }
    else return;
}
            


LOCAL LVAL NEAR nsub(subst, tresult, expr)
int subst, tresult, expr;
{
    struct nsubargs args;
    LVAL tree;
        /* protect some pointers */
#ifdef KEYARG
    xlstkcheck(2);
    xlsave(args.fcn);
    xlsave(args.kfcn);
#else
    xlsave1(args.fcn);
#endif

    args.subst = subst;
    args.tresult = tresult;
    args.expr = expr;

    if (expr) { /* get the expressions and the tree */
        args.to = xlgetarg();
        if (subst) args.from = xlgetarg();
        tree = xlgetarg();
        xltest(&args.fcn, &args.tresult);
    }
    else {
        /* get the result expression, the function and the tree */
        args.to = xlgetarg();
        args.fcn = xlgetarg();
        tree = xlgetarg();
    }
    
#ifdef KEYARG
    args.kfcn = xlkey();
#endif

    xllastkey();

    nsub1(&tree, &args);
    
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    return (tree);
}



LVAL xnsubst() { return nsub(TRUE, TRUE, TRUE);}
LVAL xnsubstif() { return nsub(TRUE, TRUE, FALSE); }
LVAL xnsubstifnot() { return nsub(TRUE, FALSE, FALSE); }
LVAL xnsublis() { return nsub(FALSE, TRUE, TRUE);}

/* xsubst - substitute one expression for another */
LVAL xsubst()
{
    struct substargs args;
    LVAL expr;
    /* protect some pointers */
#ifdef KEYARG
    xlstkcheck(2);
    xlsave(args.fcn);
    xlsave(args.kfcn);
#else
    xlsave1(args.fcn);
#endif

    /* get the to value, the from value and the expression */
    args.to = xlgetarg();
    args.from = xlgetarg();
    expr = xlgetarg();
    xltest(&args.fcn,&args.tresult);

#ifdef KEYARG
    args.kfcn = xlkey();
#endif

    xllastkey();

    /* do the substitution */
    expr = subst(expr,&args);

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the result */
    return (expr);
}

/* subst - substitute one expression for another */
LOCAL LVAL NEAR subst(expr,args)
  LVAL expr; struct substargs *args;
{
    LVAL carval,cdrval;

#ifdef KEYARG
    if (dotest2(args->from,expr,args->fcn,args->kfcn) == args->tresult)
#else
    if (dotest2(args->from,expr,args->fcn) == args->tresult)
#endif
        return (args->to);
    else if (consp(expr)) {
#ifdef STSZ         /* This function is a good candidate for stack ov */
        stchck();
#endif
        xlsave1(carval);
        carval = subst(car(expr),args);
        cdrval = subst(cdr(expr),args);
        xlpop();

/* the following TAA mod makes subst like COMMON LISP */

        if ((carval == car(expr)) && (cdrval == cdr(expr)))
            return expr; /* no change */
        else
            return (cons(carval,cdrval));
    }
    else
        return (expr);
}

/* xsublis - substitute using an association list */
LVAL xsublis()
{
    struct sublargs args;
    LVAL expr;

    /* protect some pointers */
#ifdef KEYARG
    xlstkcheck(2);
    xlsave(args.fcn);
    xlsave(args.kfcn);
#else
    xlsave1(args.fcn);
#endif

    /* get the assocation list and the expression */
    args.alist = xlgalist();
    expr = xlgetarg();
    xltest(&args.fcn,&args.tresult);

#ifdef KEYARG
    args.kfcn = xlkey();
#endif

    xllastkey();

    /* do the substitution */
    expr = sublis(expr,&args);

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the result */
    return (expr);
}

/* sublis - substitute using an association list */
LOCAL LVAL NEAR sublis(expr,args)
  LVAL expr; struct sublargs *args;
{
    LVAL carval,cdrval,pair;

#ifdef KEYARG
    if (!null(pair = assoc(args->kfcn!=NIL?
                           xlapp1(args->kfcn,expr):
                           expr,
                           args->alist,
                           args->fcn,
                           NIL,
                           args->tresult)))
#else
    if (!null(pair = assoc(expr,args->alist,args->fcn,args->tresult)))
#endif
        return (cdr(pair));
    else if (consp(expr)) {
#ifdef STSZ         /* This function is a good candidate for stack ov */
        stchck();
#endif
        xlsave1(carval);
        carval = sublis(car(expr),args);
        cdrval = sublis(cdr(expr),args);
        xlpop();
/* TAA MOD for making like common lisp */
        if ((car(expr) == carval) && (cdr(expr) == cdrval))
            return (expr);
        else
            return (cons(carval,cdrval));
    }
    else
        return (expr);
}

/* assoc - find a pair in an association list */
#ifdef KEYARG
LOCAL LVAL NEAR assoc(expr,alist,fcn,kfcn,tresult)
  LVAL expr,alist,fcn,kfcn; int tresult;
#else
LOCAL LVAL NEAR assoc(expr,alist,fcn,tresult)
  LVAL expr,alist,fcn; int tresult;
#endif
{
    LVAL pair;

    for (; consp(alist); alist = cdr(alist))
        if ((!null((pair = car(alist)))) && consp(pair))
#ifdef KEYARG
            if (dotest2(expr,car(pair),fcn,kfcn) == tresult)
#else
            if (dotest2(expr,car(pair),fcn) == tresult)
#endif
                return (pair);
    return (NIL);
}

/* xnth - return the nth element of a list */
LVAL xnth()
{
    return (nth(TRUE));
}

/* xnthcdr - return the nth cdr of a list */
LVAL xnthcdr()
{
    return (nth(FALSE));
}

/* nth - internal nth function */
LOCAL LVAL NEAR nth(carflag)
  int carflag;
{
    LVAL list,num;
    FIXTYPE n;

    /* get n and the list */
    num = xlgafixnum();
/*  list = xlgacons(); */
    list = xlgalist();      /* TAA fix */

    xllastarg();

    /* make sure the number isn't negative */
    if ((n = getfixnum(num)) < 0)
        xlfail("bad argument");

    /* find the nth element */
    while (consp(list) && --n >= 0)
        list = cdr(list);

    /* return the list beginning at the nth element */
    return (carflag && consp(list) ? car(list) : list);
}

/* xlength - return the length of a list or string */
LVAL xlength()
{
    FIXTYPE n;
    LVAL arg;

    /* get the list or string */
    arg = xlgetarg();
    xllastarg();

    /* find the length of a list */
    if (listp(arg))
        for (n = 0; consp(arg);) {
            arg = cdr(arg);
            n++;
            if (n > nnodes) xlcircular();   /*DIRTY, but we loose anyway!*/
        }

    /* find the length of a string */
    else if (stringp(arg))
        n = (FIXTYPE)getslength(arg);

    /* find the length of a vector */
    else if (vectorp(arg))
        n = (FIXTYPE)getsize(arg);

    /* otherwise, bad argument type */
    else
                xlbadtype(arg);

    /* return the length */
    return (cvfixnum(n));
}

/* xlistlength -- return the length of a list */
LVAL xlistlength()
{
    FIXTYPE n = 0;
    LVAL arg, sarg;
    
    /* get the list */
    arg = sarg = xlgalist();
    xllastarg();
    
    while (consp(arg)) {
        arg = cdr(arg);
        if (!consp(arg)) { n++; break; }
        if (sarg == arg) return NIL;    /* circular list */
        arg = cdr(arg);
        sarg = cdr(sarg);
        n += 2;
    }
    
    /* return the length */
    return (cvfixnum(n));
}


/* map - internal mapping function */
#define CONCAT 2    /* third choice for valflag */

LOCAL LVAL NEAR map(carflag,valflag)
  int carflag,valflag;
{
    FRAMEP newfp;
    LVAL fun,lists,val,last,p,x,y;
    int argc;
    long n=0, nmax=nnodes;


    /* protect some pointers */
    xlstkcheck(3);
    xlsave(fun);
    xlsave(lists);
    xlsave(val);

    /* get the function to apply and the first list */
    fun = xlgetarg();
    lists = xlgalist();

    /* initialize the result list */
    val = (valflag ? NIL : lists);

    /* build a list of argument lists */
    argc = 1;
    for (lists = last = consa(lists); moreargs(); last = cdr(last)) {
        argc++;
        rplacd(last,cons(xlgalist(),NIL));
    }

    /* loop through each of the argument lists */
    for (;;) {

        if (n++ > nmax) xlcircular();

        /* build an argument list from the sublists */
        newfp = xlsp;
        pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
        pusharg(fun);
        pusharg(cvfixnum((FIXTYPE)argc));
        for (x = lists; (!null(x)) && (!null(y = car(x))) && consp(y); x = cdr(x)) {
            pusharg(carflag ? car(y) : y);
            rplaca(x,cdr(y));
        }

        /* quit if any of the lists were empty */
        if (!null(x)) {
            xlsp = newfp;
            break;
        }

        /* apply the function to the arguments */
        xlfp = newfp;
        switch (valflag) {
        case CONCAT:
            p = xlapply(argc);
            if (!null(p)) {
                if (!consp(p)) xlerror("non-list to concatenate", p);
                if (null(val)) val = p;
                else rplacd(last, p);
                while (consp(cdr(p))) p = cdr(p); /* find end--no circular check */
                last = p;
            }
            break;

        case TRUE:
            p = consa(xlapply(argc));
            if (!null(val)) rplacd(last,p);
            else val = p;
            last = p;
            break;

        case FALSE:
            xlapply(argc);
            break;
        }
    }

    /* restore the stack */
    xlpopn(3);

    /* return the last test expression value */
    return (val);
}

/* xmapc - built-in function 'mapc' */
LVAL xmapc()
{
    return (map(TRUE,FALSE));
}

/* xmapcar - built-in function 'mapcar' */
LVAL xmapcar()
{
    return (map(TRUE,TRUE));
}

/* xmapl - built-in function 'mapl' */
LVAL xmapl()
{
    return (map(FALSE,FALSE));
}

/* xmaplist - built-in function 'maplist' */
LVAL xmaplist()
{
    return (map(FALSE,TRUE));
}

/* xmapcan - built-in function 'mapcan' */
LVAL xmapcan()
{
    return (map(TRUE,CONCAT));
}

/* xmapcon - built-in function 'mapcon' */
LVAL xmapcon()
{
    return (map(FALSE,CONCAT));
}



/* xrplca - replace the car of a list node */
LVAL xrplca()
{
    LVAL list,newcar;

    /* get the list and the new car */
    list = xlgacons();
    newcar = xlgetarg();
    xllastarg();

    /* replace the car */
    rplaca(list,newcar);

    /* return the list node that was modified */
    return (list);
}

/* xrplcd - replace the cdr of a list node */
LVAL xrplcd()
{
    LVAL list,newcdr;

    /* get the list and the new cdr */
    list = xlgacons();
    newcdr = xlgetarg();
    xllastarg();

    /* replace the cdr */
    rplacd(list,newcdr);

    /* return the list node that was modified */
    return (list);
}

/* xnconc - destructively append lists */
LVAL xnconc()
{
    LVAL next,last=NIL,val=NIL;
    long l; /* TAA MOD */

    /* concatenate each argument */
    if (moreargs()) {
        while (xlargc > 1) {

            /* TAA mod -- give error message if not a list */
            if ((!null(next = nextarg())) && consp(next)) {

                /* concatenate this list to the result list */
                if (!null(val)) rplacd(last,next);
                else val = next;

                /* find the end of the list */
                l = 0;
                while (consp(cdr(next))) {
                    next = cdr(next);
                    if (l++ > nnodes) xlcircular();
                }
                last = next;
            }
            else if (!null(next)) xlbadtype(*--xlargv); /* TAA -- oops! */
        }

        /* handle the last argument */
        if (!null(val)) rplacd(last,nextarg());
          else val = nextarg();
     }

     /* return the list */
     return (val);
}


/* xsort - built-in function 'sort' */

#ifdef KEYARG
LOCAL LVAL NEAR mergesortk _((LVAL list, LVAL sortfcn, LVAL sortkey));
 
LOCAL LVAL NEAR mergesortk(list,sortfcn,sortkey)
LVAL list,sortfcn,sortkey;
{
    /* Strategy: divide into two parts, (recurse) to sort each, then
        merge them together */
    LVAL left, right;
    
    /* less than 2 cells needn't be sorted */ 
    if (!(consp(list) && consp(cdr(list))))
        return list;

    xlstkcheck(5);  /* Only two are used at recursion */
    xlprotect(left);
    xlprotect(right);

    /* Find the center of the list */
    {
        unsigned i=0;
        LVAL temp;
        left = right = list;
        while (consp(list) && consp(list=cdr(list))) {
            list = cdr(list);
            right = cdr(temp=right);
            if ((i += 2) > MAXSLEN) xltoolong();
        }
        rplacd(temp, NIL);  /* split left and right parts */
    }
    
    left = mergesortk(left, sortfcn, sortkey);
    right = mergesortk(right, sortfcn, sortkey);

    {
        LVAL result, resultt, leftarg, rightarg;
        leftarg = xlapp1(sortkey, car(left));
        xlprotect(leftarg);
        rightarg = xlapp1(sortkey, car(right));
        xlprotect(rightarg);
        xlsave(result); /* set to NIL */

        while (TRUE) {
            if (!dotest2(leftarg, rightarg, sortfcn, NIL) &&
                dotest2(rightarg, leftarg, sortfcn, NIL)) {
                /* right is smaller */
                if (null(result)) {
                    result = resultt = right;
                }
                else {
                    rplacd(resultt, right);
                    resultt = right;
                }
                right = cdr(right);
                if (null(right)) { /* finished the merge */
                    rplacd(resultt, left);
                    break;
                }
                rightarg=xlapp1(sortkey,car(right));
            }
            else  { /* left is smaller */
                if (null(result)) {
                    result = resultt = left;
                }
                else {
                    rplacd(resultt, left);
                    resultt = left;
                }
                left = cdr(left);
                if (null(left)) { /* finished the merge */
                    rplacd(resultt, right);
                    break;
                }
                leftarg=xlapp1(sortkey,car(left));
            }
        }
        xlpopn(5);
        return result;
    }
}

#endif

LOCAL LVAL NEAR xmergesort _((LVAL list, LVAL sortfcn));

LOCAL LVAL NEAR xmergesort(list,sortfcn)
LVAL list,sortfcn;
{
    /* Strategy: divide into two parts, (recurse) to sort each, then
       merge them together */
    LVAL left, right;
    
    /* less than 2 cells needn't be sorted */ 
    if (!(consp(list) && consp(cdr(list))))
        return list;

    xlstkcheck(3);
    xlprotect(left);
    xlprotect(right);

    /* Find the center of the list */
    {
        unsigned i=0;
        LVAL temp;
        left = right = list;
        while (consp(list) && consp(list=cdr(list))) {
            list = cdr(list);
            right = cdr(temp=right);
            if ((i += 2) > MAXSLEN) xltoolong();
        }
        rplacd(temp, NIL);  /* split left and right parts */
    }
    left = xmergesort(left, sortfcn);
    right = xmergesort(right, sortfcn);

    {
        LVAL result, resultt;
        xlsave(result); /* set to NIL */

        while (TRUE) {
            if (
#ifdef KEYARG
                !dotest2(car(left), car(right), sortfcn, NIL) &&
                dotest2(car(right), car(left), sortfcn, NIL)
#else
                !dotest2(car(left), car(right), sortfcn) &&
                dotest2(car(right), car(left), sortfcn)
#endif
                ) { /* right is smaller */
                if (null(result)) {
                    result = resultt = right;
                }
                else {
                    rplacd(resultt, right);
                    resultt = right;
                }
                right = cdr(right);
                if (null(right)) { /* finished the merge */
                    rplacd(resultt, left);
                    break;
                }
            }
            else  { /* left is smaller */
                if (null(result)) {
                    result = resultt = left;
                }
                else {
                    rplacd(resultt, left);
                    resultt = left;
                }
                left = cdr(left);
                if (null(left)) { /* finished the merge */
                    rplacd(resultt, right);
                    break;
                }
            }
        }
    xlpopn(3);
    return result;
    }
}

LVAL xsort()
{
    LVAL list, sortfcn;

    /* protect some pointers */
#ifdef KEYARG
    LVAL sortkey;
    xlstkcheck(3);
    xlsave(sortkey);
#else
    xlstkcheck(2);
#endif
    xlsave(list);
    xlsave(sortfcn);

    /* get the list to sort and the comparison function */
    list = xlgetarg();
    sortfcn = xlgetarg();
#ifdef KEYARG
    sortkey = xlkey();
#endif
    xllastkey();

    /* sort the list */
    if (!null(list)) switch (ntype(list)) {
        case VECTOR:
            list = listify(list);
#ifdef KEYARG
            list = null(sortkey) ? xmergesort(list, sortfcn)
                                : mergesortk(list, sortfcn, sortkey);
#else
            list = xmergesort(list, sortfcn);
#endif
            list = vectify(list);
            break;
        case STRING:
            list = listify(list);
#ifdef KEYARG
            list = null(sortkey) ? xmergesort(list, sortfcn)
                                : mergesortk(list, sortfcn, sortkey);
#else
            list = xmergesort(list, sortfcn);
#endif
            list = stringify(list);
            break;
        case CONS:
#ifdef KEYARG
            list = null(sortkey) ? xmergesort(list, sortfcn)
                                : mergesortk(list, sortfcn, sortkey);
#else
            list = xmergesort(list, sortfcn);
#endif
            break;
        default: xlbadtype(list);
    }

    /* restore the stack and return the sorted list */
#ifdef KEYARG
    xlpopn(3);
#else
    xlpopn(2);
#endif
    return (list);
}


#ifdef SETS
/* These functions have the following copyright notice: */
/* XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney                  */
/*      All Rights Reserved                                            */
/*      Permission is granted for unrestricted non-commercial use      */

/* membr - internal MEMBER for set functions TAA */
#ifdef KEYARG
LOCAL LVAL NEAR membr(expr,list,fcn,kfcn,tresult)
  LVAL expr,list,fcn,kfcn; int tresult;
{
    xlprot1(expr);
    if (!null(kfcn)) expr = xlapp1(kfcn,expr);
    for (; consp(list); list = cdr(list))
        if (dotest2(expr,car(list),fcn,kfcn) == tresult) {
            xlpop();
            return (list);
        }
    xlpop();
    return (NIL);
}

#else
LOCAL LVAL NEAR membr(expr,list,fcn,tresult)
  LVAL expr,list,fcn; int tresult;
{
    for (; consp(list); list = cdr(list))
        if (dotest2(expr,car(list),fcn) == tresult)
                return (list);
    return (NIL);
}
#endif

/* Common Lisp ADJOIN function */
LVAL xadjoin()
{
    LVAL x, list, fcn;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    xlsave1(fcn);
#endif

    /* get the lists and key arguements, if any */
    x = xlgetarg();
    list = xlgalist();
    xltest(&fcn,&tresult);
#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

#ifdef KEYARG
    if (null(membr(x,list,fcn,kfcn,tresult))) list = cons(x,list) ;
    xlpopn(2);
#else
    if (null(membr(x,list,fcn,tresult))) list = cons(x,list) ;
    xlpop();
#endif

    return list;
}

LOCAL LVAL NEAR set_op(which)
        int which;
{
    LVAL x, list1, list2, result, fcn;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(kfcn);
#else

    /* protect some pointers */
    xlstkcheck(2);
#endif
    xlsave(fcn);
    xlsave(result);

    /* get the lists and key arguements, if any */
    list1 = xlgalist();
    list2 = xlgalist();
    xltest(&fcn,&tresult);
#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

    switch(which) {
        case 'U':
            for (result = list1; consp(list2); list2 = cdr(list2)) {
                x = car(list2);
#ifdef KEYARG
                if (null(membr(x,list1,fcn,kfcn,tresult)))
#else
                if (null(membr(x,list1,fcn,tresult)))
#endif
                    result = cons(x, result);
            }
            break;
        case 'I':
            for (result = NIL; consp(list2); list2 = cdr(list2)) {
                x = car(list2);
#ifdef KEYARG
                if (!null(membr(x,list1,fcn,kfcn,tresult)))
#else
                if (!null(membr(x,list1,fcn,tresult)))
#endif
                    result = cons(x, result);
            }
            break;
        case 'D':
            for (result = NIL; consp(list1); list1 = cdr(list1)) {
                x = car(list1);
#ifdef KEYARG
                if (null(membr(x,list2,fcn,kfcn,tresult)))
#else
                if (null(membr(x,list2,fcn,tresult)))
#endif
                    result = cons(x, result);
            }
            break;
        case 'S':
            for (result = s_true; consp(list1); list1 = cdr(list1)) {
                x = car(list1);
#ifdef KEYARG
                if (null(membr(x,list2,fcn,kfcn,tresult)))
#else
                if (null(membr(x,list2,fcn,tresult)))
#endif
                {
                    result = NIL;
                    break;
                }
            }
            break;
    }

#ifdef KEYARG
    xlpopn(3);
#else
    xlpopn(2);
#endif
    return(result);
}

LVAL xunion()          { return(set_op('U')); }
LVAL xintersection()   { return(set_op('I')); }
LVAL xset_difference() { return(set_op('D')); }
LVAL xsubsetp()        { return(set_op('S')); }

#endif


/* HASH TABLES ARE IMPLEMENTED AS STRUCTS, WITHOUT ACCESSING FCNS */

#ifdef HASHFCNS

/* Hash table functions from Ken Whedbee */
LVAL xmakehash()    /* rewritten by TAA */
{
    LVAL size, testfcn, result;
    FIXTYPE len;

    if (xlgetkeyarg(k_size,&size)) {
        if (!fixp(size) || (len=getfixnum(size)) < 1)
            xlbadtype(size);
    }
    else len = 31;

    if (!xlgetkeyarg(k_test,&testfcn))
        testfcn = getfunction(s_eql);

    xllastkey();

    xlprot1(testfcn);

    result = newstruct(a_hashtable,(int)len+1);

    setelement(result, 1, testfcn);

    xlpop();

    return result;
}

LVAL xgethash()
{
    LVAL alist,val,key,table,def=NIL;

    key = xlgetarg();
    table = xlgastruct();
    if (moreargs()) {
        def = xlgetarg();
        xllastarg();
    }
    if (getelement(table, 0) != a_hashtable)
        xlbadtype(table);

    alist = getelement(table,
        xlhash(key,getsize(table)-2) + 2);

#ifdef KEYARG
    val = assoc(key,alist,getelement(table,1),NIL,TRUE);
#else
    val = assoc(key,alist,getelement(table,1),TRUE);
#endif

    /* return result */
#ifdef MULVALS
    xlnumresults = 2;
    if (null(val)) {
        xlresults[0] = def;
        xlresults[1] = NIL;
    }
    else {
        xlresults[0] = cdr(val);
        xlresults[1] = s_true;
    }
    return(xlresults[0]);
#else
    return (null(val) ? def : cdr(val));
#endif /* MULVALS */
}

LVAL xremhash()
/* By TAA -- can't use assoc here*/
{
    LVAL alist,key,table,last;

    int idx;

    key = xlgetarg();
    table = xlgastruct();
    xllastarg();

    if (getelement(table, 0) != a_hashtable)
        xlbadtype(table);

    idx = xlhash(key,getsize(table)-2) + 2;

    alist = getelement(table,idx);

    if (null(alist))
        return NIL;

#ifdef KEYARG
    else if (dotest2(key,car(car(alist)),getelement(table,1),NIL)==TRUE)
#else
    else if (dotest2(key,car(car(alist)),getelement(table,1))==TRUE)
#endif
        {
        setelement(table,idx,cdr(alist));   /* matches first element */
        return s_true;
    }
    else {
        last = alist;
        alist = cdr(alist);
        while (consp(alist)) {
#ifdef KEYARG
            if (dotest2(key,car(car(alist)),getelement(table,1),NIL)==TRUE)
#else
            if (dotest2(key,car(car(alist)),getelement(table,1))==TRUE)
#endif
            {
                rplacd(last,cdr(alist));
                return s_true;
            }
            last = alist;
            alist = cdr(alist);
        }
    }

    return NIL;
}

VOID xlsetgethash(key,table,value)
LVAL key,table,value;
{
    LVAL alist,oldval;
    int idx;

    if (getelement(table, 0) != a_hashtable)
        xlbadtype(table);

    idx = xlhash(key,getsize(table)-2) + 2;

    alist = getelement(table,idx);

#ifdef KEYARG
    if (!null(oldval = assoc(key,alist,getelement(table,1),NIL,TRUE)))
#else
    if (!null(oldval = assoc(key,alist,getelement(table,1),TRUE)))
#endif
        rplacd(oldval,value);
    else {
        alist = cons(cons(key,value),alist);
        setelement(table,idx,alist);
    }
}

/* function clrhash  TAA */

LVAL xclrhash()
{
    LVAL table;
    int i;

    table = xlgastruct();
    xllastarg();

    if (getelement(table, 0) != a_hashtable)
        xlbadtype(table);

    for (i = getsize(table)-1; i > 1; i--) setelement(table,i,NIL);

    return (table);

}

/* function hash-table-count  TAA */

LVAL xhashcount()
{
    LVAL table, element;
    int i;
    FIXTYPE cnt = 0;

    table = xlgastruct();
    xllastarg();

    if (getelement(table, 0) != a_hashtable)
        xlbadtype(table);

    for (i = getsize(table)-1; i > 1; i--)
        for (element=getelement(table,i);consp(element);element=cdr(element))
            cnt++;

    return (cvfixnum(cnt));
}

/* function maphash  TAA */

LVAL xmaphash()
{
    FRAMEP newfp;
    LVAL fun, table, arg, element;
    int i;

    fun = xlgetarg();
    table = xlgastruct();
    xllastarg();

    if (getelement(table, 0) != a_hashtable)
        xlbadtype(table);

    xlstkcheck(2);
    xlprotect(fun);
    xlprotect(table);

    for (i = getsize(table)-1; i > 1; i--)
        for (element=getelement(table,i); consp(element);) {
            arg = car(element);
            element = cdr(element); /* in case element is deleted */
            newfp =xlsp;
            pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
            pusharg(fun);
            pusharg(cvfixnum((FIXTYPE) 2));
            pusharg(car(arg));
            pusharg(cdr(arg));
            xlfp = newfp;
            xlapply(2);
        }

    xlpopn(2);

    return (NIL);
}

#endif
