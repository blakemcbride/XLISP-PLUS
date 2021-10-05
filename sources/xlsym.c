/* xlsym - symbol handling routines */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

/* forward declarations */
#ifdef PACKAGES
LOCAL FIXTYPE NEAR llength _((LVAL list));
LOCAL VOID NEAR fail_message _((char *msg, LVAL arg1, LVAL arg2, LVAL arg3, LVAL arg4));
LOCAL LVAL NEAR copylist _((LVAL list));
LOCAL LVAL NEAR mklist _((int n, LVAL elem));
LOCAL LVAL delete1 _((LVAL x, LVAL list));
LOCAL LVAL adjoin _((LVAL x, LVAL list));
LOCAL int  check_export_conflicts _((LVAL sym, LVAL pack));
LOCAL VOID check_use_conflicts _((LVAL pack_to_use, LVAL pack));
LOCAL VOID check_unintern_conflicts _((LVAL sym, LVAL pack));
LOCAL LVAL makepackage _((char FAR *name));
LOCAL VOID check_nicknames _((LVAL list, LVAL pack));
LOCAL VOID add_nickname _((char FAR *name, LVAL pack));
LOCAL VOID set_nicknames _((LVAL pack, LVAL names));
LOCAL VOID xlunexport _((LVAL sym, LVAL pack));
LOCAL VOID use_package _((LVAL pack_to_use, LVAL pack));
LOCAL VOID unuse_package _((LVAL pack_to_use, LVAL pack));
LOCAL int  xlunintern _((LVAL sym, LVAL pack));
LOCAL int  findsym _((char FAR *name, LVAL list, LVAL *psym));
LOCAL VOID xlshadow _((char FAR *name, LVAL pack));
LOCAL VOID xlshadowingimport _((LVAL sym, LVAL pack));
LOCAL LVAL intern _((char FAR *name, LVAL pack, int export));
LOCAL VOID unintern _((LVAL sym, LVAL pack, int external));
LOCAL LVAL packop _((int which));
#endif /* PACKAGES */

#ifdef PACKAGES
/* xlenter - enter an external symbol in the current package */
/* TAA MOD 10/96 -- makes keywords external. Merged in xlextern, which
   wasn't called anywhere else */
LVAL xlenter(name)
     char *name;
{
    LVAL sym, pack ;

    /***** should this also shadow if there is a conflict? */
    if (name[0] == ':') { /* keyword package */
        name++; /* skip colon */
        pack = xlkeypack;
    }
    else {
        pack = getvalue(s_package);
    }

    if (xlfindsymbol(name, pack, &sym) == SYM_NOT_FOUND)
        sym = intern(name, pack, TRUE);

    return sym;
}
#else
/* xlenter - enter a symbol into the obarray */
LVAL xlenter(name)
  char *name;
{
    LVAL sym,array;
    int i;

    /* check for symbol already in table */
    array = getvalue(obarray);
    i = hash(name,HSIZE);
    for (sym = getelement(array,i); !null(sym); sym = cdr(sym))
        if (STRCMP(name,getstring(getpname(car(sym)))) == 0)
            return (car(sym));

    /* make a new symbol node and link it into the list */
    xlsave1(sym);
    sym = consd(getelement(array,i));
    rplaca(sym,xlmakesym(name));
    setelement(array,i,sym);
    xlpop();

    /* return the new symbol */
    return (car(sym));
}
#endif  /* PACKAGES */

/* xlmakesym - make a new symbol node */
LVAL xlmakesym(name)
  char FAR *name;
{
    LVAL sym;
    sym = cvsymbol(name);
#ifdef PACKAGES
    setsflags(sym, F_NORMAL);
#else
    if (*name == ':') {
        setvalue(sym,sym);
        setsflags(sym, F_CONSTANT | F_SPECIAL);
    }
    else setsflags(sym, F_NORMAL);
#endif /* PACKAGES */
    return (sym);
}

/* xlgetvalue - get the value of a symbol (with check) */
LVAL xlgetvalue(sym)
  LVAL sym;
{
    LVAL val;

    /* look for the value of the symbol */
    while ((val = xlxgetvalue(sym)) == s_unbound)
        xlunbound(sym);

    /* return the value */
    return (val);
}

/* xlxgetvalue - get the value of a symbol */
LVAL xlxgetvalue(sym)
  LVAL sym;
{
    register LVAL fp,ep;
    LVAL val;

    /* check the environment list */
    for (fp = xlenv; !null(fp); fp = cdr(fp))

        /* check for an instance variable */
        if (!null(ep = car(fp)) && objectp(car(ep))) {
            if (xlobgetvalue(ep,sym,&val))
                return (val);
        }

        /* check an environment stack frame */
        else {
            for (; !null(ep); ep = cdr(ep))
                if (sym == car(car(ep)))
                    return (cdr(car(ep)));
        }

    /* return the global value */
    return (getvalue(sym));
}

/* xlsetvalue - set the value of a symbol */
VOID xlsetvalue(sym,val)
  LVAL sym,val;
{
    register LVAL fp,ep;

    if (constantp(sym)) {
        xlnoassign(sym);
        /* never returns */
    }

    /* look for the symbol in the environment list */
    for (fp = xlenv; !null(fp); fp = cdr(fp))

        /* check for an instance variable */
        if (!null(ep = car(fp)) && objectp(car(ep))) {
            if (xlobsetvalue(ep,sym,val))
                return;
        }

        /* check an environment stack frame */
        else {
            for (; !null(ep); ep = cdr(ep))
                if (sym == car(car(ep))) {
                    rplacd(car(ep),val);
                    return;
                }
        }

    /* store the global value */
    setvalue(sym,val);
}

/* xlgetfunction - get the functional value of a symbol (with check) */
LVAL xlgetfunction(sym)
  LVAL sym;
{
    LVAL val;

    /* look for the functional value of the symbol */
    while ((val = xlxgetfunction(sym)) == s_unbound)
        xlfunbound(sym);

    /* return the value */
    return (val);
}

/* xlxgetfunction - get the functional value of a symbol */
LVAL xlxgetfunction(sym)
  LVAL sym;
{
    register LVAL fp,ep;

    /* check the environment list */
    for (fp = xlfenv; !null(fp); fp = cdr(fp))
        for (ep = car(fp); !null(ep); ep = cdr(ep))
            if (sym == car(car(ep)))
                return (cdr(car(ep)));

    /* return the global value */
    return (getfunction(sym));
}

/* xlgetprop - get the value of a property */
LVAL xlgetprop(sym,prp)
  LVAL sym,prp;
{
    LVAL p;
    return (null(p = findprop(getplist(sym),prp)) ? NIL : car(p));
}

/* xlputprop - put a property value onto the property list */
VOID xlputprop(sym,val,prp)
  LVAL sym,val,prp;
{
    LVAL pair;
    if (!null(pair = findprop(getplist(sym),prp)))
        rplaca(pair,val);
    else
        setplist(sym,cons(prp,cons(val,getplist(sym))));
}

/* xlremprop - remove a property from a property list */
VOID xlremprop(sym,prp)
  LVAL sym,prp;
{
    LVAL last,p;
    last = NIL;
    for (p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(last)) {
        if (car(p) == prp) {
            if (!null(last)) {
                rplacd(last,cdr(cdr(p)));
            }
            else {
                setplist(sym,cdr(cdr(p)));
            }
        }
        last = cdr(p);
    }
}

/* findprop - find a property pair */
LVAL findprop(p,prp)
  LVAL p,prp;
{
    for (; consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
        if (car(p) == prp)
            return (cdr(p));
    return (NIL);
}

/* hash - hash a symbol name string */
int hash(str,len)
  char FAR *str;
  int len;
{
    int i;
    for (i = 0; *str; )
        i = (i << 1) ^ *str++;
    i %= len;
    return (i < 0 ? -i : i);
}

/* xlhash -- hash any xlisp object */
/* TAA extension */
int xlhash(obj,len)
    LVAL obj;
    int len;
{
    int i;
    unsigned long tot;
    union {FIXTYPE i; float j; unsigned FIXTYPE k;} swizzle;

    hashloop:   /* iterate on conses */
    switch (ntype(obj)) {
        case SYMBOL:
            obj = getpname(obj);
        case STRING:
            return hash(getstring(obj),len);
        case SUBR: case FSUBR:
            return getoffset(obj) % len;
        case FIXNUM:
            swizzle.i = getfixnum(obj);
            return (int) (swizzle.k % len);
        case FLONUM:
            swizzle.j = (float)(getflonum(obj));
            return (int) (swizzle.k % len);
        case CHAR:
            return getchcode(obj) % len;
        case CONS: case USTREAM:
            obj = car(obj);     /* just base on CAR */
            goto hashloop;
        case STREAM:
            return 0;   /* nothing we can do on this */
#ifdef COMPLX
        case COMPLEX:
            return (xlhash(getreal(obj), len)+xlhash(getimag(obj), len) % len);
#endif
#ifdef BIGNUMS
        case RATIO:
            return (xlhash(getnumer(obj),len)+xlhash(getdenom(obj),len) % len);
        case BIGNUM:
        {
            BIGNUMDATA FAR *xd = getbignumarray(obj);
            for (i = getbignumsize(obj), tot = 0; i-- > 0;)
                tot += xd[i];
            return (int)(tot % len);
        }
#endif
        case OBJECT:
            /* Bandaid: class contains a reference to itself - JK */
            if (obj == cls_class)
                return 123 % len; /* pick a random value */
            /* else fall through... */
        default:    /* all array types */
            for (i = getsize(obj), tot = 0; i-- > 0;)
                tot += (unsigned)xlhash(getelement(obj,i),len);
            return (int)(tot % len);
    }
}

/* unbind a variable/constant */
LVAL xmakunbound()
{
    LVAL sym;

    sym = xlgasymbol();
    xllastarg();

    if (constantp(sym))
        xlerror("can't unbind constant", sym);

    setvalue(sym, s_unbound);
    setsflags(sym, F_NORMAL);
    return(sym);
}

/* unbind a function */
LVAL xfmakunbound()
{
    LVAL sym;
    
    sym = xlgasymbol();
    xllastarg();
    
    setfunction(sym, s_unbound);
    return (sym);
}


/* define a constant -- useful in initialization */

VOID defconstant(sym, val)
  LVAL sym, val;
{
    setvalue(sym, val);
    setsflags(sym, F_CONSTANT | F_SPECIAL);
}

/* DEFCONSTANT DEFPARAMETER and DEFVAR */

LVAL xdefconstant()
{
    LVAL sym, val;

    sym = xlgasymbol();
    val = xlgetarg();
    if (moreargs()) {
        (void)xlgetarg(); /* optional doc string - ignore for now */
        xllastarg();
    }

    /* evaluate constant value */
    val = xleval(val);

    if (null(sym)) xlfail("can't redefine NIL");

    if (specialp(sym)) {
        if (constantp(sym)) {
            if (!eql(getvalue(sym),val)) {
                errputstr("WARNING-- redefinition of constant ");
                errprint(sym);
            }
        }
        else xlerror("can't make special variable into a constant", sym);
    }

    defconstant(sym, val);

    return(sym);
}


LVAL xdefparameter()
{
    LVAL sym, val;

    sym = xlgasymbol();
    val = xlgetarg();
    if (moreargs()) {
        (void)xlgetarg(); /* optional doc string - ignore for now */
        xllastarg();
    }

    if (constantp(sym)) xlnoassign(sym);

    setvalue(sym, xleval(val));
    setsflags(sym, F_SPECIAL);
    return(sym);
}

LVAL xdefvar()
{
    LVAL sym, val=NIL;

    sym = xlgasymbol();
    if (moreargs()) {
        val = xlgetarg();
        if (moreargs()) {
            (void)xlgetarg(); /* optional doc string - ignore for now */
            xllastarg();
        }
    }

    if (constantp(sym)) xlnoassign(sym);

    if (getvalue(sym) == s_unbound) setvalue(sym, xleval(val));
    setsflags(sym, F_SPECIAL);
    return(sym);
}


/* xlsinit - symbol initialization routine */
VOID xlsinit()
{
#ifdef PACKAGES
    /* initialize the package list */
    obarray = NIL;

    /* make the system packages */
    xlisppack = makepackage("XLISP");
    obarray = cons(xlisppack, obarray);
    add_nickname("LISP", xlisppack);
    add_nickname("COMMON-LISP", xlisppack);
    add_nickname("CL", xlisppack);
    xlkeypack = makepackage("KEYWORD");
    obarray = cons(xlkeypack, obarray);
/*  TAA 10/96 -- don't want to do this! 
    setintsyms(xlkeypack, getextsyms(xlkeypack));*/ /* only exernal symbols */
    xluserpack = makepackage("USER");
    obarray = cons(xluserpack, obarray);
    add_nickname("COMMON-LISP-USER", xluserpack);
    add_nickname("CL-USER", xluserpack);
    use_package(xlisppack, xluserpack);

    /* add the package symbol */
    s_package = xlmakesym("*PACKAGE*");
    setpackage(s_package, xlisppack);
    setsvalue(s_package,xlisppack);
    xlimport(s_package, xlisppack);
    xlexport(s_package, xlisppack);
#else
    LVAL array,p;

    /* initialize the obarray */
    obarray = xlmakesym("*OBARRAY*");
    array = newvector(HSIZE);
    setsvalue(obarray,array);

    /* add the symbol *OBARRAY* to the obarray */
    p = consa(obarray);
    setelement(array,hash("*OBARRAY*",HSIZE),p);
#endif /* PACKAGES */
}

/* added - L. Tierney */
#ifdef PACKAGES
LOCAL LVAL delete1(x, list)
     LVAL x, list;
{
    LVAL val, next;
    if (consp(list)) {
        if (x == car(list)) {
            val = cdr(list);
        }
        else {
            val = list;
            for (next=cdr(list); consp(next); list = next, next = cdr(next)) {
                if (x == car(next)) {
                    rplacd(list, cdr(next));
                    break;
                }
            }
        }
    }
    else val = NIL;
    return(val);
}

LOCAL LVAL adjoin(x, list)
     LVAL x, list;
{
  LVAL next;

  for (next = list; consp(next); next = cdr(next))
      if (x == car(next))
          return(list);
  return(cons(x, list));
}

LOCAL VOID NEAR fail_message(msg, arg1, arg2, arg3, arg4)
    char *msg;
    LVAL arg1, arg2, arg3, arg4;
{
    FRAMEP oldargv = xlargv;
    int oldargc = xlargc;
    LVAL msgarg;
    
    /* check if there's room for the new call frame (4 slots needed) */
    if (xlsp >= (xlargstktop-4)) xlargstkoverflow();

    xlprot1(msgarg);
    msgarg = cvstring(msg);

    xlargv = xlsp;  /* We will cheat badly with this call */
    xlargc = 4;
    *xlsp++ = arg1;
    *xlsp++ = arg2;
    *xlsp++ = arg3;
    *xlsp++ = arg4;
    xlformat(msgarg, getvalue(s_debugio));
    xlsp -= 4;
    xlargv = oldargv;
    xlargc = oldargc;

    xlpop();
}
    

/* TAA MOD 10/96 -- don't do xlcerror here anymore, but return failure flag
   so that calling routine can do xlcerror */

LOCAL int check_export_conflicts(sym, pack)
     LVAL sym, pack;
{
    LVAL list, fsym;
    char FAR *name;
    int failure = FALSE;

    name = getstring(getpname(sym));

    for (list = getusedby(pack); consp(list); list = cdr(list))
        if (xlfindsymbol(name, car(list), &fsym) &&
            sym != fsym &&
            ! findsym(name, getshadowing(car(list)), &fsym)) {
            fail_message(
                "~&Name conflict with ~s in ~s~%  when exporting ~s from ~s",
                fsym, car(list), sym, pack);
                failure = TRUE;
        }
    return failure;
}

LOCAL VOID check_use_conflicts(pack_to_use, pack)
     LVAL pack_to_use, pack;
{
    LVAL array, list, sym, fsym;
    char FAR *name;
    int i;
    int failure = FALSE;

    do {
        if (failure) {
            xlcerror("recheck for conflicts", "name conflicts", s_unbound);
            failure = FALSE;
        }
        for (i = 0; i < HSIZE; i++) {
            array = getextsyms(pack_to_use);
            for (list = getelement(array, i); consp(list); list = cdr(list)) {
                sym = car(list);
                name = getstring(getpname(sym));
                if (xlfindsymbol(name, pack, &fsym) &&
                    sym != fsym &&
                    ! findsym(name, getshadowing(pack), &fsym)) {
                    fail_message(
        "~&Name conflict of ~s and ~s~%  when using ~s in ~s",
                                 sym, fsym, pack_to_use, pack);
                    failure = TRUE;
                }
            }
        }
    } while (failure);
}

LOCAL VOID check_unintern_conflicts(sym, pack)
     LVAL sym, pack;
{
    LVAL uselist, list, fsym1, fsym2;
    int found, i;
    char FAR *name;

    name = getstring(getpname(sym));
    if (findsym(name, getshadowing(pack), &fsym1)) {
        i = hash(name,HSIZE);
        found = FALSE;
        for (uselist = getuses(pack); consp(uselist); uselist = cdr(uselist)) {
            list = getelement(getextsyms(car(uselist)), i);
            if (findsym(name, list, &fsym2)) {
                if (found) {
                    if (fsym1 != fsym2) {
                        fail_message(
            "~&Name conflict of ~s and ~s~%  when uninterning ~s from ~s",
                                     fsym1, fsym2, sym, pack);
                        xlerror("name conflict", s_unbound);
                    }
                }
                else {
                    found = TRUE;
                    fsym2 = fsym1;
                }
            }
        }
    }
}

/* make and install a new package -- does not check for an existing package */
LOCAL LVAL makepackage(name)
     char FAR *name;
{
    LVAL pack;

    xlsave1(pack);
    pack = newpackage();
/* TAA MOD 10/96 -- the actual linking of the package into the package list
   needs to be defered in xmakepackage(), so we can't do it here, but must do
   it everywhere makepackage() is called */
/*  obarray = cons(pack, obarray); */
    setpacknames(pack, consa(cvstring(name)));
    xlpop();
    return(pack);
}

VOID xlimport(sym, pack)
     LVAL sym, pack;
{
    LVAL list, fsym;
    int found, i;

    found = xlfindsymbol(getstring(getpname(sym)), pack, &fsym);

    if (found == SYM_NOT_FOUND || found == SYM_INHERITED) {
        /* enter the symbol as an internal symbol */
        i = hash(getstring(getpname(sym)),HSIZE);
        list = cons(sym, getelement(getintsyms(pack),i));
        setelement(getintsyms(pack),i,list);
    }
}

VOID xlexport(sym, pack)
     LVAL sym, pack;
{
    LVAL fsym, list;
    int found, i;

    found = xlfindsymbol(getstring(getpname(sym)), pack, &fsym);
    if (found == SYM_NOT_FOUND || fsym != sym)
        /* (error "~s not accessible from ~s" sym pack) */
        xlerror("symbol not accessible", sym);
    else if (found == SYM_INTERNAL) {
        /* move it from internal to external */
        i = hash(getstring(getpname(sym)),HSIZE);
        list = delete1(sym, getelement(getintsyms(pack), i));
        setelement(getintsyms(pack), i, list);
        list = cons(sym, getelement(getextsyms(pack), i));
        setelement(getextsyms(pack),i,list);
    }
    else if (found == SYM_INHERITED) {
        /* import it and make it external */
        i = hash(getstring(getpname(sym)),HSIZE);
        list = cons(sym,getelement(getextsyms(pack), i));
        setelement(getextsyms(pack),i,list);
    }
}

LOCAL VOID xlunexport(sym, pack)
     LVAL sym, pack;
{
    LVAL fsym, list;
    int i;
    char FAR *name;

    name = getstring(getpname(sym));
    if (xlfindsymbol(name, pack, &fsym) == SYM_EXTERNAL && sym == fsym) {
        i = hash(name, HSIZE);
        list = delete1(sym, getelement(getextsyms(pack), i));
        setelement(getextsyms(pack), i, list);
        list = cons(sym, getelement(getintsyms(pack), i));
        setelement(getintsyms(pack),i,list);
    }

    /***** We should check for internal and sym != fsym and give an error
           message in that case *********/

}

LOCAL LVAL intern(name, pack, export)
     char FAR *name;
     LVAL pack;
     int export;
{
    LVAL sym, list, array;
    int i;

    if (pack == xlkeypack) export = TRUE;

    /* make a new symbol and enter it as an internal or external symbol */
    xlsave1(sym);
    sym = xlmakesym(name);
    if (export)
        while(check_export_conflicts(sym, pack))
            xlcerror("recheck for conflicts", "name conflict", s_unbound);
    i = hash(name,HSIZE);
    array = (export) ? getextsyms(pack) : getintsyms(pack);
    list = cons(sym, getelement(array,i));
    setelement(array,i,list);
    setpackage(sym,pack);
    xlpop();

    if (pack == xlkeypack) {
        setvalue(sym,sym);
        setsflags(sym, F_CONSTANT | F_SPECIAL);     
    }

    return(sym);
}

LOCAL VOID unintern(sym, pack, external)
     LVAL sym, pack;
     int external;
{
    LVAL array, list;
    int i;

    array = (external) ? getextsyms(pack) : getintsyms(pack);
    i = hash(getstring(getpname(sym)), HSIZE);
    list = getelement(array, i);
    setelement(array, i, delete1(sym, list));
    if (pack == getpackage(sym))
        setpackage(sym, NIL);
    setshadowing(pack, delete1(sym, getshadowing(pack)));
}

LVAL xlintern(name, pack)
     char FAR *name;
     LVAL pack;
{
    LVAL sym;
    if (! goodpackagep(pack)) sym = NIL;
    else if (! xlfindsymbol(name, pack, &sym))
        sym = intern(name, pack, FALSE);
    return(sym);
}

LOCAL int xlunintern(sym, pack)
     LVAL sym, pack;
{
    LVAL fsym;
    char FAR *name;
    int found, val;

    name = getstring(getpname(sym));
    found = xlfindsymbol(name, pack, &fsym);
    if (sym == fsym && (found == SYM_INTERNAL || found == SYM_EXTERNAL)) {
        check_unintern_conflicts(sym, pack);
        unintern(sym, pack, (found == SYM_EXTERNAL) ? TRUE : FALSE);
        val = TRUE;
    }
    else val = FALSE;
    return(val);
}

LVAL xlfindpackage(name)
     char FAR *name;
{
    LVAL list, names, pack;

    for (list = obarray; consp(list); list = cdr(list)) {
        pack = car(list);
        for (names = getpacknames(pack); consp(names); names = cdr(names)) {
            if (STRCMP(name,getstring(car(names))) == 0)
                return(pack);
        }
    }
    return(NIL);
}

LOCAL int findsym(name, list, psym)
     char FAR *name;
     LVAL list, *psym;
{
    for (; consp(list); list = cdr(list))
        if (STRCMP(name,getstring(getpname(car(list)))) == 0) {
            if (psym != NULL) *psym = car(list);
            return(TRUE);
        }
    return(FALSE);
}

int xlfindsymbol(name, pack, psym)
     char FAR *name;
     LVAL pack, *psym;
{
    LVAL list;
    int i;

    i = hash(name,HSIZE);
    if (findsym(name, getelement(getintsyms(pack),i), psym))
        return(SYM_INTERNAL);
    else if (findsym(name, getelement(getextsyms(pack), i), psym))
        return(SYM_EXTERNAL);
    else {
        for (list = getuses(pack); consp(list); list = cdr(list))
            if (findsym(name, getelement(getextsyms(car(list)),i), psym))
                return(SYM_INHERITED);
        return(SYM_NOT_FOUND);
    }
}

LOCAL VOID check_nicknames(list, pack)
     LVAL list, pack;
{
    LVAL s, p;

    if (!listp(list))   /* TAA added error check 10/93 */
        xlerror("must be a list", list);

    for (; consp(list); list = cdr(list)) {
        s = car(list);
        if (! stringp(s) && ! symbolp(s))
            xlerror("not a string or symbol", s);
        p = xlfindpackage(getstring(symbolp(s) ? getpname(s) : s));
        if (!null(p) && p != pack)
            xlerror("package already exists", s);
    }
}

LOCAL VOID add_nickname(name, pack)
     char FAR *name;
     LVAL pack;
{
    LVAL nlist, rest;
    nlist = getpacknames(pack);
    if (consp(nlist)) {
        rest = cdr(nlist);
        rplacd(nlist, consa(cvstring(name)));
        rplacd(cdr(nlist), rest);
    }
}

LOCAL VOID set_nicknames(pack, names)
     LVAL pack, names;
{
    LVAL nlist, s;
    nlist = getpacknames(pack);
    if (consp(nlist)) {
        rplacd(nlist, NIL);
        for (; consp(names); names = cdr(names)) {
            s = car(names);
            add_nickname(getstring((symbolp(s) ? getpname(s) : s)), pack);
        }
    }
}

LVAL xlpackagename(pack)
     LVAL pack;
{
    if (goodpackagep(pack))
        return(car(getpacknames(pack)));
    else
        return(getpname(NIL));
}

LOCAL VOID use_package(pack_to_use, pack)
     LVAL pack_to_use, pack;
{
    if (pack != pack_to_use && pack != xlkeypack && pack_to_use != xlkeypack) {
        check_use_conflicts(pack_to_use, pack);
        setuses(pack, adjoin(pack_to_use, getuses(pack)));
        setusedby(pack_to_use, adjoin(pack, getusedby(pack_to_use)));
    }
}

LOCAL VOID unuse_package(pack_to_unuse, pack)
     LVAL pack_to_unuse, pack;
{
    if (pack != pack_to_unuse) {
        setuses(pack, delete1(pack_to_unuse, getuses(pack)));
        setusedby(pack_to_unuse, delete1(pack, getusedby(pack_to_unuse)));
    }
}

LVAL xlgetpackage(arg)
     LVAL arg;
{
    LVAL pack;
    if (stringp(arg)) pack = xlfindpackage(getstring(arg));
    else if (symbolp(arg)) pack = xlfindpackage(getstring(getpname(arg)));
    else pack = arg;
    if (! packagep(pack))
        xlerror("package not found", arg);
    if (null(getpacknames(pack)))
        xlfail("deleted package");
    return(pack);
}

LOCAL VOID xlshadow(name, pack)
     char FAR *name;
     LVAL pack;
{
    LVAL sym;
    int found;

    found = xlfindsymbol(name, pack, &sym);
    if (found == SYM_NOT_FOUND || found == SYM_INHERITED)
        sym = intern(name, pack, FALSE);
    setshadowing(pack, adjoin(sym, getshadowing(pack)));
}

LOCAL VOID xlshadowingimport(sym, pack)
     LVAL sym, pack;
{
    LVAL fsym, array;
    int found, i;
    char FAR *name;

    name = getstring(getpname(sym));
    found = xlfindsymbol(name, pack, &fsym);
    if ((found == SYM_INTERNAL || found == SYM_EXTERNAL) && fsym != sym)
        unintern(fsym, pack, (found == SYM_EXTERNAL) ? TRUE : FALSE);
    if (found == SYM_INHERITED || found == SYM_NOT_FOUND || fsym != sym) {
        i = hash(name, HSIZE);
        array = getintsyms(pack);
        setelement(array, i, cons(sym, getelement(array, i)));
    }
    setshadowing(pack, adjoin(sym, getshadowing(pack)));
}

/* TAA MOD 10/96 -- modified to not create package if there is any error
   function setuselist() no longer needed */
LVAL xmakepackage()
{
    LVAL name, nicknames, uselist, pack;
    char FAR *str;

    name = xlgastrorsym();
    if (xlgetkeyarg(k_nicknames, &nicknames))
        check_nicknames(nicknames, NIL);
    else nicknames = NIL;
    if (xlgetkeyarg(k_use, &uselist)) {
        LVAL temp;
        /* TAA added 10/93 to catch error */
        if (!listp(uselist)) xlerror("must be a list", uselist);
        for (temp = uselist; consp(temp); temp = cdr(temp))
            if (xlgetpackage(car(temp)) == xlkeypack)
                xlfail("can't explicitly use KEYWORD package");
    }
    else
        uselist = NIL;
    xllastkey();

    str = getstring(symbolp(name) ? getpname(name) : name);
    if (!null(xlfindpackage(str))) xlerror("package already exists", name);

    pack = makepackage(str);
    xlprot1(pack);
    
    set_nicknames(pack, nicknames);
    for (; consp(uselist); uselist = cdr(uselist))
        use_package(xlgetpackage(car(uselist)), pack);
    /* All is ok -- make package real */
    obarray = cons(pack, obarray);

    xlpop();
    return(pack);
}

LVAL xinpackage()
{
    LVAL name, pack;

    name = xlgastrorsym();
    xllastarg();

    pack = xlfindpackage(getstring(symbolp(name) ? getpname(name) : name));
    if (null(pack)) xlerror("package not found", name);
    if (! goodpackagep(pack)) xlerror ("bad package", pack);
    setvalue(s_package, pack);
    return(pack);
}

enum {
  EXPORT_POP,
  UNEXPORT_POP,
  IMPORT_POP,
  SHADOW_POP,
  SHADOWING_IMPORT_POP,
  USE_PACKAGE_POP,
  UNUSE_PACKAGE_POP
  };

LOCAL LVAL packop(which)
     int which;
{
    LVAL arg, arglist, pack;

    arglist = xlgetarg();
    pack = xlgetpackage(moreargs() ? xlgetarg() : getvalue(s_package));
    xllastarg();

    if (! goodpackagep(pack)) xlerror ("bad package", pack);

    xlprot1(arglist);
    if (! consp(arglist)) arglist = consa(arglist);

    /* TAA MOD 10/96 -- do error checking in advance as much as is practical */

    for (arg = arglist; consp(arg); arg = cdr(arg)) {
        if (which == USE_PACKAGE_POP || which == UNUSE_PACKAGE_POP) {
            /* signals error if invalid */
            if (xlgetpackage(car(arg))==xlkeypack && which==USE_PACKAGE_POP)
                xlfail("can't explicitly use KEYWORD package"); 
        }
        else if (!(symbolp(car(arg)) ||
                   (which == SHADOW_POP && stringp(car(arg)))))
            xlbadtype(car(arg));
    }

    if (pack == xlkeypack) {
        if (which == UNEXPORT_POP) xlfail("can't unexport in KEYWORD package");
        /* ignore in other cases */
        xlpop();
        return(s_true);
    }

    if (which == IMPORT_POP) {
        LVAL fsym;
        int failflag = FALSE;
        do {
            if (failflag) {
                xlcerror("try importing again", "name conflict", s_unbound);
                failflag = FALSE;
            }
            for (arg = arglist; consp(arg); arg = cdr(arg)) {
                if (xlfindsymbol(getstring(getpname(car(arg))), pack, &fsym) &&
                    fsym != car(arg)) {
                    fail_message("~&Name conflict importing ~s into ~s",
                                 car(arg),pack,NIL,NIL);
                    failflag = TRUE;
                }
            }
        } while (failflag);
    }

    else if (which == EXPORT_POP) {
        int failflag = FALSE;
        do {
            if (failflag) {
                xlcerror("recheck for conflicts", "name conflict", s_unbound);
                failflag = FALSE;
            }
            for (arg = arglist; consp(arg); arg = cdr(arg)) {
                if (check_export_conflicts(car(arg), pack))
                    failflag = TRUE;
            }
        } while (failflag);
    }

    /* TAA Mod 10/96 -- remainder of code doesn't have error checks */

    for (; consp(arglist); arglist = cdr(arglist)) {
        arg = car(arglist);
        switch (which) {
            case EXPORT_POP:
                xlexport(arg, pack);
                break;
            case UNEXPORT_POP:
                xlunexport(arg, pack);
                break;
            case IMPORT_POP:
                xlimport(arg, pack);
                break;
            case SHADOW_POP:
                xlshadow(getstring(symbolp(arg) ? getpname(arg) : arg), pack);
                break;
            case SHADOWING_IMPORT_POP:
                xlshadowingimport(arg, pack);
                break;
            case USE_PACKAGE_POP:
                use_package(xlgetpackage(arg), pack);
                break;
            case UNUSE_PACKAGE_POP:
                unuse_package(xlgetpackage(arg), pack);
                break;
        }
    }
    xlpop();
    return(s_true);
}

LVAL xexport()          { return(packop(EXPORT_POP)); }
LVAL xunexport()        { return(packop(UNEXPORT_POP)); }
LVAL ximport()          { return(packop(IMPORT_POP)); }
LVAL xshadow()          { return(packop(SHADOW_POP)); }
LVAL xshadowingimport() { return(packop(SHADOWING_IMPORT_POP)); }
LVAL xusepackage()      { return(packop(USE_PACKAGE_POP)); }
LVAL xunusepackage()    { return(packop(UNUSE_PACKAGE_POP)); }

LVAL xfindpackage()
{
    LVAL name, pack;
    name = xlgetarg();
    xllastarg();
    if (stringp(name)) pack = xlfindpackage(getstring(name));
    else if (symbolp(name)) pack = xlfindpackage(getstring(getpname(name)));
    else pack = name;
    return(goodpackagep(pack) ? pack : NIL);
}

LVAL xfindsymbol()
{
    LVAL name, pack, sym;
    int found;

    name = xlgastring();
    pack = xlgetpackage(moreargs() ? xlgetarg() : getvalue(s_package));
    xllastarg();
    found = xlfindsymbol(getstring(name), pack, &sym);
    if (! found) sym = NIL;
#ifdef MULVALS
    xlnumresults = 2;
    xlresults[0] = sym;
    switch (found) {
        case SYM_INTERNAL: xlresults[1] = k_internal; break;
        case SYM_EXTERNAL: xlresults[1] = k_external; break;
        case SYM_INHERITED: xlresults[1] = k_inherited; break;
        default: xlresults[1] = NIL;
    }
#endif /* MULVALS */
    return(sym);
}

/* find length of a list */
LOCAL FIXTYPE NEAR llength(x)
     LVAL x;
{
  FIXTYPE n;

  for (n = 0; consp(x); n++, x = cdr(x))
    if (n > nnodes) xlcircular();

  return(n);
}

/* Internal version of MAKE-LIST */
LOCAL LVAL NEAR mklist(n, elem)
     int n;
     LVAL elem;
{
  LVAL result;

  xlsave1(result);
  for (result = NIL; n > 0; n--)
    result = cons(elem, result);
  xlpop();
  return(result);
}

/* internal version of COPYLIST */
LOCAL LVAL NEAR copylist(list)
     LVAL list;
{
    LVAL result, nextl, nextr;

    if (! listp(list)) xlbadtype(list);

    /* protect the result pointer */
    xlsave1(result);

    result = mklist((int)llength(list), NIL);
    for (nextl = list, nextr = result; consp(nextl);
         nextl = cdr(nextl), nextr = cdr(nextr)) {
         rplaca(nextr, car(nextl));
    }
    if (! null(nextl)) {
        for (nextr = result; consp(cdr(nextr)); nextr = cdr(nextr));
        rplacd(nextr, nextl);
    }

    /* restore the stack frame */
    xlpop();

    return(result);
}

LVAL xpackageuselist()
{
    LVAL pack;
    pack = xlgetpackage(xlgetarg());
    xllastarg();
    return(copylist(getuses(pack)));
}

LVAL xpackageusedbylist()
{
    LVAL pack;
    pack = xlgetpackage(xlgetarg());
    xllastarg();
    return(copylist(getusedby(pack)));
}

LVAL xpackageshadows()
{
    LVAL pack;
    pack = xlgetpackage(xlgetarg());
    xllastarg();
    return(copylist(getshadowing(pack)));
}

LVAL xpackagename()
{
    LVAL pack;
    pack = xlgetpackage(xlgetarg());
    xllastarg();
    return(car(getpacknames(pack)));
}

LVAL xpackagenicknames()
{
    LVAL pack;
    pack = xlgetpackage(xlgetarg());
    xllastarg();
    return(cdr(copylist(getpacknames(pack))));
}

LVAL xpackageobarray()
{
    LVAL pack;
    int external;
    pack = xlgetpackage(xlgetarg());
    if (moreargs())
        external = (null(xlgetarg())) ? FALSE : TRUE;
    else
        external = TRUE;
    xllastarg();
    return(external ? getextsyms(pack) : getintsyms(pack));
}

LVAL xpackagevalidp()
{
    LVAL pack;
    pack = xlgetarg();
    return(goodpackagep(pack) ? s_true : NIL);
}

LVAL xunintern()
{
    LVAL sym, pack;
    sym =xlgasymbol();
    pack = xlgetpackage(moreargs() ? xlgetarg() : getvalue(s_package));
    xllastarg();
    return(xlunintern(sym, pack) ? s_true : NIL);
}

LVAL xlistallpackages()
{
    LVAL val, list;
    xllastarg();
    xlsave1(val);
    for (val = NIL, list = obarray; consp(list); list = cdr(list))
        val = cons(car(list), val);
    xlpop();
    return(val);
}

LVAL xdeletepackage()
{
    LVAL pack, list, array;
    int i;

    pack = xlgetpackage(xlgetarg());
    xllastarg();

    if (pack == xlisppack || pack == xlkeypack)
        return(NIL);

    if (pack == getvalue(s_package))
        xlfail("can't delete the current package");
    if (! null(getusedby(pack)))
        xlerror("package is used", getusedby(pack));
    if (! null(getuses(pack)))
        for (list = getuses(pack); consp(list); list = cdr(list))
            unuse_package(car(list), pack);
    for (i = 0; i < HSIZE; i++) {
        array = getintsyms(pack);
        for (list = getelement(array, i); consp(list); list = cdr(list))
            if (getpackage(car(list)) == pack)
                setpackage(car(list), NIL);
        setelement(array, i, NIL);
        array = getextsyms(pack);
        for (list = getelement(array, i); consp(list); list = cdr(list))
            if (getpackage(car(list)) == pack)
                setpackage(car(list), NIL);
        setelement(array, i, NIL);
    }
    setshadowing(pack, NIL);
    setpacknames(pack, NIL);

    for (list = obarray; consp(list); list = cdr(list))
        if (pack == car(list)) {
            obarray = delete1(pack, obarray);
            return(s_true);
        }
    return(NIL);
}

LVAL xrenamepackage()
{
    LVAL pack, name, nicknames, p;
    pack = xlgetpackage(xlgetarg());
    name = xlgastrorsym();
    nicknames = (moreargs()) ? xlgalist() : NIL;
    xllastarg();

    if (symbolp(name)) name = getpname(name);
    if (pack == xlisppack || pack == xlkeypack || pack == xluserpack) {
        name = car(getpacknames(pack));
    }
    p = xlfindpackage(getstring(name));
    if (!null(p) && p != pack)
        xlerror("packages already exists", name);
    check_nicknames(nicknames, pack);
    setpacknames(pack, consa(name));
    set_nicknames(pack, nicknames);
    return(pack);
}

LVAL xfindallsymbols()
{
    LVAL arg, val, sym, packs;
    char FAR *name;

    arg = xlgastrorsym();
    xllastarg();

    name = getstring(symbolp(arg) ? getpname(arg) : arg);

    xlsave1(val);
    for (val = NIL, packs = obarray; consp(packs); packs = cdr(packs))
        if (xlfindsymbol(name, car(packs), &sym))
            val = adjoin(sym, val);
    xlpop();
    return(val);
}

/* xsympackage - get the package name of a symbol */
LVAL xsympackage()
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymbol();
    xllastarg();

    /* return the print name */
    return (getpackage(sym));
}
#endif /* PACKAGES */
