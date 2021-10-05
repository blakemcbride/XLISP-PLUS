/* xlsys.c - xlisp builtin system functions */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

/* $putpatch.c$: "MODULE_XLSYS_C_GLOBALS" */

/* Function prototypes */
LOCAL int NEAR xltypep _((LVAL arg, LVAL typ));
LOCAL int is_member _((LVAL x, LVAL list));

/* xload - read and evaluate expressions from a file */
LVAL xload()
{
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;
#endif
    int vflag,pflag;
    LVAL oldenv,oldfenv;    /* TAA MOD-- code sections using these variables
                               forces global environment on LOAD
                               Change based on Luke Tierney's XLISP-STAT */
    LVAL arg;

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(oldenv);
    xlprotect(oldfenv);

    /* establish global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;


    /* get the file name */
#ifdef MEDMEM
    _fstrncpy(name, getstring(xlgetfname()), STRMAX);
    name[STRMAX-1] = '\0';
#else
    name = getstring(xlgetfname());
#endif
    /* get the :verbose flag */ /* TAA MOD to simplify */
    vflag = xlgetkeyarg(k_verbose,&arg) ? (arg != NIL) : TRUE;

    /* get the :print flag */ /* TAA MOD to simplify */
    pflag = xlgetkeyarg(k_print,&arg) ? (arg != NIL) : FALSE;

    xllastkey();

    /* load the file, check for success */
    arg = xlload(name,vflag,pflag) ? s_true : NIL;

    /* restore environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(2);

    /* return success flag */
    return arg;

}

/* xtranscript - open or close a transcript file */
LVAL xtranscript()
{
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;
#endif

    /* get the transcript file name */
#ifdef MEDMEM
    if (moreargs()) {
        _fstrncpy(name, getstring(xlgetfname()), STRMAX);
        name[STRMAX-1] = '\0';
    }
    else {
        name[0] = '\0';
    }
#else
    name = (moreargs() ? getstring(xlgetfname()) : NULL);
#endif
    xllastarg();

    /* close the current transcript */
    if (tfp != CLOSED) OSCLOSE(tfp);

    /* open the new transcript */
#ifdef MEDMEM
    tfp = (name[0] != '\0' ? OSAOPEN(name,CREATE_WR) : CLOSED);
#else
    tfp = (name != NULL ? OSAOPEN(name,CREATE_WR) : CLOSED);
#endif

    /* return T if a transcript is open, NIL otherwise */
    return (tfp != CLOSED ? s_true : NIL);
}

/* xtype - return type of a thing */
LVAL xtype()
{
    LVAL arg;

    arg = xlgetarg();
    xllastarg();    /* TAA MOD -- this was missing */

    switch (ntype(arg)) {
    case SUBR:          return (a_subr);
    case FSUBR:         return (a_fsubr);
    case CONS:          return (a_cons);
    case SYMBOL:        return (null(arg) ? a_list : a_symbol); /* different
                                            from XLISP 2.1 */
    case FIXNUM:        return (a_fixnum);
    case FLONUM:        return (a_flonum);
    case STRING:        return (a_string);
#ifdef BIGNUMS
    case RATIO:         return (a_ratio);
    case BIGNUM:        return (a_bignum);
#endif
    case OBJECT:        return (a_object);
    case STREAM:        return (a_stream);
    case VECTOR:        return (a_vector);
    case CLOSURE:       return (a_closure);
    case CHAR:          return (a_char);
    case USTREAM:       return (a_ustream);
    case STRUCT:        return (getelement(arg,0));
#ifdef COMPLX
    case COMPLEX:       return (a_complex);
#endif
#ifdef PACKAGES
    case PACKAGE:       return (a_package); /* L. Tierney fell through to bad node type */
#endif /* PACKAGES */
    /* $putpatch.c$: "MODULE_XLSYS_C_XTYPE" */
    default:            xlfail("bad node type");
                        return (NIL); /* eliminate warning message */
    }
}

int xlcvttype(arg)  /* find type of argument and return it */
LVAL arg;
{
/*sorted into roughly most-likely-used-first order*/
    if (arg == a_cons)      return CONS;
    if (arg == a_list)      return CONS;    /* Synonym here */
    if (arg == a_vector)    return VECTOR;
    if (arg == a_string)    return STRING;
    if (arg == a_symbol)    return SYMBOL;
    if (arg == a_subr)      return SUBR;
    if (arg == a_fsubr)     return FSUBR;
    if (arg == a_fixnum)    return FIXNUM;
    if (arg == a_flonum)    return FLONUM;
#ifdef BIGNUMS
    if (arg == a_ratio)     return RATIO;
    if (arg == a_bignum)    return BIGNUM;
#endif
    if (arg == a_object)    return OBJECT;
    if (arg == a_stream)    return STREAM;
    if (arg == a_closure)   return CLOSURE;
    if (arg == a_char)      return CHAR;
    if (arg == a_ustream)   return USTREAM;
    if (arg == a_struct)    return STRUCT;
#ifdef COMPLX
    if (arg == a_complex)   return COMPLEX;
#endif
#ifdef PACKAGES
    if (arg == a_package)   return PACKAGE;
#endif /* PACKAGES */
    if (arg == s_true)      return -1;  /* Fix for coerce */
    return 0;
}

/* typep -- check type of thing */
LOCAL int NEAR xltypep(arg, typ)
  LVAL arg, typ;
{

    if (symbolp(typ)) {

        /* everything is type T */

        if (typ == s_true) return TRUE;

        /* only NIL is NULL */

        if (typ == a_null) return null(arg);

        /* only atoms are ATOM */

        if (typ == a_atom) return atom(arg);

        /* two types of streams */

        if (typ == a_anystream)
            return (streamp(arg) || ustreamp(arg));

        /* many ways to be a function */

        if (typ == s_function)
            return (subrp(arg) || closurep(arg) || symbolp(arg) ||
                (consp(arg) && car(arg) == s_lambda));

        /* NIL is type LIST or SYMBOL */

        if (null(arg)) return (typ==a_list || typ==a_symbol);

        /* Structures are type STRUCT or the structure type */

        if (ntype(arg) == STRUCT)
            return ((typ == a_struct
#ifdef HASHFCNS
                && getelement(arg,0) != a_hashtable
#endif
                )|| getelement(arg,0) == typ);


        /* If typename is NUMBER, then arg can be any numeric type */

        if (typ == a_number)
            return (numberp(arg));

        /* if typename is INTEGER, then arg can be fixnum (possibly) bignum */

        if (typ == a_integer)
            return (integerp(arg));

        /* if typename is REAL, then arg can be any non-complex number */

        if (typ == a_real)
            return (realp(arg));

#ifdef BIGNUMS
        /* if typename is RATIONAL then arg can be integer or ratio */

        if (typ == a_rational)
            return (rationalp(arg));

#endif

        /* otherwise the typename must be the same as the type of the
                object (as would be returned by TYPE-OF) */

        return (ntype(arg) == xlcvttype(typ));
    }
    /* type specifier is a list */
    if (consp(typ)) {
        LVAL fn = car(typ);
        LVAL lst = cdr(typ);

        if (fn == s_not) {  /* (not spec) */
            if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
            return !xltypep(arg, car(lst));
        }
        if (fn == s_satisfies) { /* (satisfies predicatefn) */
            if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
#ifdef KEYARG
            return dotest1(arg, car(lst), NIL);
#else
            return dotest1(arg, car(lst));
#endif
        }
        if (fn == a_object) { /* (object class) */
            if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
            lst = car(lst);
            return (objectp(arg) &&
                (symbolp(lst) ? getvalue(lst) : lst) == getclass(arg));
        }
        if (fn == s_and) {  /* (and {spec}) */
            for (; consp(lst); lst = cdr(lst))
                if (!xltypep(arg,car(lst))) return FALSE;
            return TRUE;
        }
        if (fn == s_or) {   /* (or {spec}) */
            for (; consp(lst); lst = cdr(lst))
                if (xltypep(arg,car(lst))) return TRUE;
            return FALSE;
        }
        if (fn == s_member) {   /* (member {args}) */
            for (; consp(lst); lst = cdr(lst))
                if (eql(car(lst),arg)) return TRUE;
            return FALSE;
        }
    }
bad_type:
    xlerror("bad type specifier", typ);
    return FALSE; /* keep compilers happy */
}

LVAL xtypep()
{
    LVAL arg, typ;

    arg = xlgetarg();
    typ = xlgetarg();
    xllastarg();

    return (xltypep(arg, typ) ? s_true : NIL);
}




LVAL listify(arg) /* arg must be vector or string */
LVAL arg;
{
    LVAL val;
    unsigned i;

    xlsave1(val);

    if (ntype(arg) == VECTOR) {
        for (i = getsize(arg); i-- > 0; )
            val = cons(getelement(arg,i),val);
    }
    else {  /* a string */
        for (i = getslength(arg); i-- > 0; )
            val = cons(cvchar(getstringch(arg,i)),val);
    }

    xlpop();
    return (val);
}

LVAL vectify(arg) /* arg must be string or cons */
LVAL arg;
{
    LVAL val,temp;
    unsigned i,l;

    if (ntype(arg) == STRING) {
        l = getslength(arg);
        val = newvector(l);
        for (i=0; i < l; i++) setelement(val,i,cvchar(getstringch(arg,i)));
    }
    else {  /* a cons */
        val = arg;
        for (l = 0; consp(val);) { /* get length */
            val = cdr(val);
            l++;
            if (l > MAXSLEN) xltoolong();
        }
        val = newvector(l);
        temp = arg;
        for (i = 0; i < l; i++) {
            setelement(val,i,car(temp));
            temp = cdr(temp);
        }
    }
        return val;
}

LVAL stringify(arg)   /* arg must be vector or cons */
LVAL arg;
{
    LVAL val,temp;
    unsigned i,l;

    if (ntype(arg) == VECTOR) {
        l = getsize(arg);
        val = newstring(l);
        for (i=0; i < l; i++) {
            temp = getelement(arg,i);
            if (ntype(temp) != CHAR) goto failed;
                val->n_string[i] = (char) getchcode(temp);
        }
        val->n_string[l] = 0;
        return val;
    }
    else {  /* must be cons */
        val = arg;
        for (l = 0; consp(val);) {
            if (ntype(car(val)) != CHAR) goto failed;
            val = cdr(val); /* get length */
            l++;
            if (l > MAXSLEN) xltoolong();
        }

        val = newstring(l);
        temp = arg;
        for (i = 0; i < l; i++) {
            val->n_string[i] = (char) getchcode(car(temp));
            temp = cdr(temp);
        }
        val->n_string[l] = 0;
        return val;
    }
failed:
    xlerror("can't make into string", arg);
    return (NIL);   /* avoid compiler warnings */
}



/* coerce function */
LVAL xcoerce()
{
    LVAL type, arg, temp;
    int newtype,oldtype;

    arg = xlgetarg();
    type = xlgetarg();
    xllastarg();

    if ((newtype = xlcvttype(type)) == 0) goto badconvert;

    oldtype = (arg==NIL? CONS: ntype(arg)); /* TAA fix */

    if (newtype == -1 || oldtype == newtype) return (arg);  /* easy case! */

    switch (newtype) {
        case CONS:
            if ((oldtype == STRING)||(oldtype == VECTOR))
                return (listify(arg));
            break;
        case STRING:
            if ((oldtype == CONS)||(oldtype == VECTOR))
                return (stringify(arg));
            break;
        case VECTOR:
            if ((oldtype == STRING)||(oldtype == CONS))
                return (vectify(arg));
            break;
        case CHAR:
            if (oldtype == FIXNUM) return cvchar((int)getfixnum(arg));
            else if ((oldtype == STRING) && (getslength(arg) == 1))
                return cvchar(getstringch(arg,0));
            else if (oldtype == SYMBOL) {
                temp = getpname(arg);
                if (getslength(temp) == 1) return cvchar(getstringch(temp,0));
            }
            break;
        case FLONUM:
            if (oldtype == FIXNUM)
                return (cvflonum((FLOTYPE) getfixnum(arg)));
#ifdef BIGNUMS
            else if (oldtype == RATIO) {
                return cvflonum(cvtratioflonum(arg));
            }
            else if (oldtype == BIGNUM) 
                return cvflonum(cvtbigflonum(arg));
#endif
            break;
#ifdef COMPLX
        case COMPLEX:
            if (oldtype == FIXNUM 
#ifdef BIGNUMS
                || oldtype == BIGNUM || oldtype == RATIO
#endif
                )
                return (arg);   /* nothing happens */
            else if (oldtype == FLONUM)
                return newdcomplex(getflonum(arg), (FLOTYPE) 0.0);
            break;
#endif
    }


badconvert:
    xlerror("illegal coersion",arg);
    return (NIL);   /* avoid compiler warnings */
}



#ifdef ADDEDTAA
/* xgeneric - get generic representation of thing */
/* TAA addition */
LVAL xgeneric()
{
    LVAL arg,acopy;

    arg = xlgetarg();
    xllastarg();

    switch (ntype(arg)) {
    case CONS: case USTREAM:
        return (cons(car(arg),cdr(arg)));
    case SYMBOL: case OBJECT: case VECTOR: case CLOSURE:
    case STRUCT:
#ifdef COMPLX
    case COMPLEX:
#endif
#ifdef PACKAGES
    case PACKAGE:
#endif
        acopy = newvector(getsize(arg));
        MEMCPY(acopy->n_vdata, arg->n_vdata, getsize(arg)*sizeof(LVAL));
        return (acopy);
    case STRING: /* make a copy of the string */
        acopy = newstring(getslength(arg));
        MEMCPY(getstring(acopy), getstring(arg), getslength(arg)+1);
        return (acopy);
#ifdef BIGNUMS
    case BIGNUM:
        acopy = newstring((getbignumsize(arg)+1)*sizeof(BIGNUMDATA));
        MEMCPY(getstring(acopy), getbignumarray(arg), getslength(acopy));
        return (acopy);
    case RATIO:
#endif
    case FIXNUM: case FLONUM: case CHAR:
        return (arg); /* it hardly matters to copy these */
    default:    xlbadtype(arg);
        return (NIL);   /* avoid compiler warnings */
    }
}

#endif


/* xbaktrace - print the trace back stack */
LVAL xbaktrace()
{
    LVAL num;
    int n;

    if (moreargs()) {
        num = xlgafixnum();
        n = (int)getfixnum(num);
    }
    else
        n = -1;
    xllastarg();
    xlbaktrace(n);
    return (NIL);
}

/* xexit - get out of xlisp */
LVAL xexit()
{
    xllastarg();
    wrapup();
    return (NIL); /* never returns */
}

/* xpeek - peek at a location in memory */
LVAL xpeek()
{
    LVAL num;
    OFFTYPE *adr;   /* TAA MOD so that data fetched is sizeof(LVAL *) */

    /* get the address */
    num = xlgafixnum(); adr = (OFFTYPE *)getfixnum(num);
    xllastarg();

    /* return the value at that address */
    return (cvfixnum((FIXTYPE)*adr));
}

/* xpoke - poke a value into memory */
LVAL xpoke()
{
    LVAL val;
    OFFTYPE *adr;   /* TAA MOD so that data fetched is sizeof(LVAL *) */

    /* get the address and the new value */
    val = xlgafixnum(); adr = (OFFTYPE *)getfixnum(val);
    val = xlgafixnum();
    xllastarg();

    /* store the new value */
    *adr = (OFFTYPE)getfixnum(val);

    /* return the new value */
    return (val);
}

/* xaddrs - get the address of an XLISP node */
LVAL xaddrs()
{
    LVAL val;

    /* get the node */
    val = xlgetarg();
    xllastarg();

    /* return the address of the node */
    return (cvfixnum((FIXTYPE)val));
}

#ifdef RANDOM

LVAL newrandom(seed)
 long seed;
{
    LVAL result;

    result = newstruct(a_randomstate, 1);
    xlprot1(result);

    setelement(result, 1, cvfixnum((FIXTYPE)seed));

    xlpop();

    return result;
}


/* make-random-state function */
LVAL xmakerandom()
{
    LVAL arg;

    /*argument is either random state, t for randomize, or nil/absent
        to use *random-state* */

    /* secret agenda: there could also be no regular arguments but a
        single keyword argument (:DATA) which is the seed!
        I'll leave it to the curious to figure out why. */

    if (moreargs()) {
        arg = xlgetarg();
        if (arg == k_data) {
            arg = xlgafixnum();
            xllastarg();
            return newrandom((long)getfixnum(arg));
        }
        xllastarg();
        if (arg == s_true) return newrandom(real_tick_count());
        if (null(arg)) arg = getvalue(s_randomstate);
    }
    else arg = getvalue(s_randomstate);

    if ((!structp(arg)) || getelement(arg,0) != a_randomstate
        || !fixp(getelement(arg,1))) {
        xlbadtype(arg);
    }

    return newrandom((long)getfixnum(getelement(arg,1)));
}

/* RANDOM Function */

LVAL xrand()
{
    LVAL state, value;
    long rand;
    int isfixed = FALSE; /* Initialzied to get rid of warning */

    value = xlgetarg();

    if (fixp(value)) {
        isfixed = TRUE;
        if (getfixnum(value) <= 0) xlerror("range error", value);
    }
    else if (floatp(value)) {
        isfixed = FALSE;
        if (getflonum(value) <= 0.0) xlerror("range error", value);
    }
    else xlbadtype(value);

    if (moreargs()) {   /* seed provided */
        state = xlgetarg();
        xllastarg();
    }
    else {  /* use global seed */
        state = getvalue(s_randomstate);
    }

    if ((!structp(state)) || getelement(state,0) != a_randomstate
        || !fixp(getelement(state,1))) {
        xlbadtype(state);
    }

    rand = osrand((long)getfixnum(getelement(state,1))); /* generate number*/

    setelement(state, 1, cvfixnum((FIXTYPE)rand)); /* put seed away */

    if (isfixed)
        return cvfixnum((FIXTYPE)rand % getfixnum(value));
    else
        /* I'm tossing the upper 7 bits which, while it increases granularity,
            will make the numbers more "random", I hope */
        return cvflonum((FLOTYPE)(rand&0xffffffL)/(FLOTYPE)0x1000000L*getflonum(value));
}
#endif
/***********************************************************************/
/**                                                                   **/
/**                  Features Maintenance Functions                   **/
/**                                                                   **/
/* XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney                  */
/*      All Rights Reserved                                            */
/*      Permission is granted for unrestricted non-commercial use      */
/***********************************************************************/

LOCAL int is_member(x, list)
        LVAL x, list;
{
    for (; consp(list); list = cdr(list))
        if (x == car(list)) return TRUE;
    return FALSE;
}

int checkfeatures(arg, which)
        LVAL arg;
        int which;
{
    int has_feature;
    LVAL features = getvalue(s_features);
  
    if (consp(arg)) {
        if (car(arg) == s_and)
            for (has_feature = TRUE, arg = cdr(arg);
                 consp(arg) && has_feature;
                 arg = cdr(arg)) {
                                         /* + was which, changed 10/93 */
                has_feature = has_feature && checkfeatures(car(arg), '+');
            }
        else if (car(arg) == s_or)
            for (has_feature = FALSE, arg = cdr(arg);
                consp(arg) && ! has_feature;
                arg = cdr(arg)) {
                                         /* + was which, changed 10/93 */
                has_feature = has_feature || checkfeatures(car(arg), '+');
            }
        else if (car(arg) == s_not && consp(cdr(arg)))
                                         /* + was which, changed 10/93 */
            has_feature = ! checkfeatures(car(cdr(arg)), '+');
        else xlerror("bad feature", arg);
    }
    else has_feature = is_member(arg, features);
    
    if (which == '-') has_feature = ! has_feature;
    return(has_feature);
}

