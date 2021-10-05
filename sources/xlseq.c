/* xlseq.c - xlisp sequence functions */
/*  Written by Thomas Almy, based on code:
    Copyright (c) 1985, by David Michael Betz
    All Rights Reserved
    Permission is granted for unrestricted non-commercial use   */
/* TAA MOD 3/98 -- added casting to eliminate many warning messages when using
   some 32bit integer compilers */

#include "xlisp.h"

/* this is part of the COMMON LISP extension: */
/* (elt seq index)  -- generic sequence reference function */
/* (map type fcn seq1 [seq2 ...]) -- generic sequence mapping function */
/*   type is one of cons, array, string, or nil */
/* (some fcn seq1 [seq2 ...]) -- apply fcn until non-nil */
/*    also every notany and notevery */
/* (concatenate type seq1 [seq2 ...]) -- sequence concatenation function */
/*    type is one of cons, array, or string. */
/* (search seq1 seq1 &key :test :test-not :start1 :end1 :start2 :end2) --
    generic sequence searching function. */
/* subseq reverse remove remove-if remove-if-not delete delete-if 
   delete-if-not -- rewritten to allow all sequence types */
/* Position, position-if, position-if-not, count, count-if, count-if-not,
   find, find-if, find-if-not */
/* the keyword arguments :start and :end are now valid for the remove, delete,
   find position and count functions */
/* the keyword argument :key is also valid where appropriate */

/* The author, Tom Almy, appologizes for using "goto" several places in
   this code. */

/* Function prototypes */
LOCAL unsigned NEAR getlength _((LVAL seq));
LOCAL VOID NEAR getseqbounds _((unsigned *start, unsigned *end, 
                    unsigned length, LVAL startkey, LVAL endkey));
LOCAL LVAL NEAR map _((int into));
LOCAL LVAL NEAR xlmapwhile _((int cond));
LOCAL LVAL NEAR remif _((int tresult, int expr));
LOCAL LVAL NEAR delif _((int tresult, int expr));
LOCAL LVAL NEAR substituteif _((int tresult, int expr));
LOCAL LVAL NEAR nsubstituteif _((int tresult, int expr));
LOCAL LVAL NEAR xlkitchensink _((int ftype, int tresult, int expr));



LOCAL unsigned NEAR getlength(seq)
LVAL seq;
{
    unsigned len;
    
    if (null(seq)) return 0;
    
    switch (ntype(seq)) {
        case STRING: 
            return (unsigned)(getslength(seq));
        case VECTOR: 
            return (unsigned)(getsize(seq));
        case CONS: 
            len = 0;
            while (consp(seq)) {
                len++;
                if (len > MAXSLEN) xltoolong();
                seq = cdr(seq);
            }
            return len;
        default: 
            xlbadtype(seq);
            return (0); /* ha ha */
        }
}


LOCAL VOID NEAR getseqbounds(start,end,length,startkey,endkey)
unsigned *start, *end, length;
LVAL startkey, endkey;
{
    LVAL arg;
    FIXTYPE temp;

    if (xlgkfixnum(startkey,&arg)) {
        temp = getfixnum(arg);
        if (temp < 0 || temp > (FIXTYPE)length ) goto rangeError;
        *start = (unsigned)temp;
    }
    else *start = 0;
    
    if (xlgetkeyarg(endkey, &arg) && !null(arg)) {
        if (!fixp(arg)) xlbadtype(arg);
        temp = getfixnum(arg);
        if (temp < (FIXTYPE)*start  || temp > (FIXTYPE)length) goto rangeError;
        *end = (unsigned)temp;  
    }
    else *end = length;
    
    return;
    /* else there is a range error */
    
rangeError:
    xlerror("range error",arg);
}
        

/* xelt - sequence reference function */
LVAL xelt()
{
    LVAL seq,index;
    FIXTYPE i;
    
    /* get the sequence and the index */

    seq = xlgetarg();

    index = xlgafixnum(); i = getfixnum(index); 
    if (i < 0) goto badindex;
    
    xllastarg();

    if (listp(seq)) { /* do like nth, but check for in range */
        /* find the ith element */
        while (consp(seq)) {
            if (i-- == 0) return (car(seq));
            seq = cdr(seq);
        }
        goto badindex;  /* end of list reached first */
    }
        

    if (ntype(seq) == STRING) { 
        if (i >= (FIXTYPE)getslength(seq)) goto badindex;
        return (cvchar(getstringch(seq,(int)i)));
    }
    
    if (ntype(seq)!=VECTOR) xlbadtype(seq); /* type must be array */

    /* range check the index */
    if (i >= getsize(seq)) goto badindex;

    /* return the array element */
    return (getelement(seq,(int)i));
    
badindex:
    xlerror("index out of bounds",index);
    return (NIL);   /* eliminate warnings */
}

#ifdef MAPFCNS
LOCAL LVAL NEAR map(into)
int into;
{
    FRAMEP newfp;
    LVAL fun, lists, val, last, x, y, y2;
    unsigned len,temp, i;
    int argc, typ;
    
    /* protect some pointers */
    xlstkcheck(3);
    xlsave(fun);
    xlsave(lists);
    xlsave(val);

    /* get the type of resultant, and resultant for map-into */
    val = xlgetarg();
    if (null(val))
        typ = 0;    /* return nothing */
    else {
        typ = (into ? ntype(val) : xlcvttype(val));
        if (!(typ==CONS || typ==STRING || typ==VECTOR))
            xlerror("invalid result type", val);
    }
   
    /* get the function to apply and argument sequences */
    fun = xlgetarg();
    /* Initialization code bug fixed, Luke Tierney 3/94 */
    if (into) { /* MAP-INTO */
        len = getlength(val);
        if (moreargs()) { /* handle first argument */
            x = xlgetarg();
            if ((temp = getlength(x)) < len) len = temp;
            argc = 1;
            lists = last = consa(x);
        }
        else {
            lists = NIL;
            argc = 0;
        }
    }
    else { /* MAP */
        val = NIL;
        lists = xlgetarg();
        len = getlength(lists);
        lists = last = consa(lists);
        argc = 1;
    }

    /* build a list of argument lists */
    for (; moreargs(); last = cdr(last)) {
        x = xlgetarg();
        if ((temp = getlength(x)) < len) len = temp;
        argc++;
        rplacd(last,(consa(x)));
    }
    
    /* initialize the result list */
    if (!into) switch (typ) {
        case VECTOR: 
            val = newvector(len); 
            break;
        case STRING: 
            val = newstring(len); 
            val->n_string[len] = 0;
            break;
        default:    
            val = NIL; 
            break;
    }
    
    y2 = val;   /* for map-into a list */
    
    /* loop through each of the argument lists */
    for (i=0;i<len;i++) {

        /* build an argument list from the sublists */
        newfp = xlsp;
        pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
        pusharg(fun);
        pusharg(cvfixnum((FIXTYPE)argc));
        for (x = lists; !null(x) ; x = cdr(x)) {
            y = car(x);
            switch (ntype(y)) {
                case CONS: 
                    pusharg(car(y));
                    rplaca(x,cdr(y));
                    break;
                case VECTOR:
                    pusharg(getelement(y,i));
                    break;
                case STRING:
                    pusharg(cvchar(getstringch(y,i)));
                    break;
            }
        }

        /* apply the function to the arguments */
        xlfp = newfp;
        x = xlapply(argc);
        
        switch (typ) {
            case CONS:
                if (into) {
                    rplaca(y2, x);
                    y2 = cdr(y2);
                }
                else {
                    y = consa(x);
                    if (!null(val)) rplacd(last,y);
                    else val = y;
                    last = y;
                }
                break;
            case VECTOR:
                setelement(val,i,x);
                break;
            case STRING:
                if (!charp(x)) 
                    xlerror("map function returned non-character",x);
                     val->n_string[i] = (char) getchcode(x);
                break;
        }
            
    }

    /* restore the stack */
    xlpopn(3);

    /* return the last test expression value */
    return (val);
}

/* xmap -- map function */

LVAL xmap()     {return map(FALSE);}
LVAL xmapinto() {return map(TRUE);}


/* every, some, notany, notevery */

#define EVERY 0
#define SOME 1
#define NOTEVERY 2
#define NOTANY 3

LOCAL LVAL NEAR xlmapwhile(cond)
int cond;
{
    int exitcond;
    FRAMEP newfp;
    LVAL fun, lists, val, last, x, y;
    unsigned len,temp,i;
    int argc;
    
    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(lists);

    /* get the function to apply and argument sequences */
    fun = xlgetarg();
    lists = xlgetarg();
    len = getlength(lists);
    argc = 1;

    /* build a list of argument lists */
    for (lists = last = consa(lists); moreargs(); last = cdr(last)) {
        val = xlgetarg();
        if ((temp = getlength(val)) < len) len = temp;
        argc++;
        rplacd(last,(cons(val,NIL)));
    }
    
    switch (cond) {
        case SOME:
        case NOTANY:
            exitcond = TRUE;
            val = NIL;
            break;
        case EVERY:
        case NOTEVERY:
            exitcond = FALSE;
            val = s_true;
            break;
    }


    /* loop through each of the argument lists */
    for (i=0;i<len;i++) {

        /* build an argument list from the sublists */
        newfp = xlsp;
        pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
        pusharg(fun);
        pusharg(cvfixnum((FIXTYPE)argc));
        for (x = lists; !null(x); x = cdr(x)) {
            y = car(x);
            switch (ntype(y)) {
                case CONS: 
                    pusharg(car(y));
                    rplaca(x,cdr(y));
                    break;
                case VECTOR:
                    pusharg(getelement(y,i));
                    break;
                case STRING:
                    pusharg(cvchar(getstringch(y,i)));
                    break;
            }
        }

        /* apply the function to the arguments */
        xlfp = newfp;
        val = xlapply(argc);
        if (null(val) ^ exitcond) break;
    }

    if ((cond == NOTANY) | (cond == NOTEVERY))
        val = (null(val) ? s_true : NIL);
    

    /* restore the stack */
    xlpopn(2);

    /* return the last test expression value */
    return (val);
    }


LVAL xevery()
{
    return xlmapwhile(EVERY);
}

LVAL xsome()
{
    return xlmapwhile(SOME);
}

LVAL xnotany()
{
    return xlmapwhile(NOTANY);
}

LVAL xnotevery()
{
    return xlmapwhile(NOTEVERY);
}
#endif

/* xconcatenate - concatenate a bunch of sequences */
/* replaces (and extends) strcat, now a macro */
LOCAL unsigned NEAR calclength _((void))
{
    LVAL tmp;
    FRAMEP saveargv;
    int saveargc;
    long len;

    /* save the argument list */
    saveargv = xlargv;
    saveargc = xlargc;

    /* find the length of the new string or vector */
    for (len = 0; moreargs(); ) {
        tmp = xlgetarg();
        len += getlength(tmp);

        if (len>MAXSLEN) xltoolong();  /*check for overflow*/
    }

    /* restore the argument list */
    xlargv = saveargv;
    xlargc = saveargc;

    return (unsigned)len;
}


LOCAL LVAL NEAR cattostring _((void))
{
     LVAL tmp,temp,val;
     char FAR *str;
     unsigned len,i;

     /* find resulting length -- also validates argument types */
     len = calclength();

     /* create the result string */
     val = newstring(len);
     str = getstring(val);

     /* combine the strings */
     while (moreargs()) {
          tmp = nextarg();
          if (!null(tmp)) switch (ntype(tmp)) {
                case STRING:
                     len = getslength(tmp);
                     MEMCPY(str, getstring(tmp), len);
                     str += len;
                     break;
                case VECTOR:
                     len = getsize(tmp);
                     for (i = 0; i < len; i++) {
                          temp = getelement(tmp,i);
                          if (!charp(temp)) goto failed;
                          *str++ = (char) getchcode(temp);
                     }
                     break;
                case CONS:
                     while (consp(tmp)) {
                          temp = car(tmp);
                          if (!charp(temp)) goto failed;
                          *str++ = (char) getchcode(temp);
                          tmp = cdr(tmp);
                     }
                break;
        }
    }

    *str = 0;   /* delimit string */

    /* return the new string */
    return (val);

failed:
    xlerror("can't make into string", tmp);
    return (NIL);   /* avoid warning message */
}


LOCAL LVAL NEAR cattovector _((void))
{
    LVAL tmp,val;
    LVAL FAR *vect;
    unsigned len,i;
    
    /* find resulting length -- also validates argument types */
    len = calclength();

    /* create the result vector */
    val = newvector(len);
    vect = &val->n_vdata[0];

    /* combine the vectors */
    while (moreargs()) {
        tmp = nextarg();
        if (!null(tmp)) switch (ntype(tmp)) {
            case VECTOR: 
                len = getsize(tmp);
                MEMCPY(vect, &getelement(tmp,0), len*sizeof(LVAL));
                vect += len;
                break;
            case STRING:
                len = getslength(tmp);
                for (i = 0; i < len; i++) {
                    *vect++ = cvchar(getstringch(tmp,i));
                }
                break;
            case CONS:
                while (consp(tmp)) {
                    *vect++ = car(tmp);
                    tmp = cdr(tmp);
                }
                break;
        }
    }

    /* return the new vector */
    return (val);
}


LOCAL LVAL NEAR cattocons _((void))
{
    LVAL val,tmp,next,last=NIL;
    unsigned len,i;
    long n;
    
    xlsave1(val);       /* protect against GC */
    
    /* combine the lists */
    while (moreargs()) {
        tmp = nextarg();
        if (!null(tmp)) switch (ntype(tmp)) {
            case CONS:
                /* check for circular list (Added 5/6/94) */
                next = tmp;
                for (n = 0; consp(next); next=cdr(next)) {
                    if (n++ > nnodes)
                        xlcircular();   /*DIRTY, but we loose anyway!*/
                }
                while (consp(tmp)) {
                    next = consa(car(tmp));
                    if (!null(val)) rplacd(last,next);
                    else val = next;
                    last = next;
                    tmp = cdr(tmp);
                }
                break;
            case VECTOR:
                len = getsize(tmp);
                for (i = 0; i<len; i++) {
                    next = consa(getelement(tmp,i));
                    if (!null(val)) rplacd(last,next);
                    else val = next;
                    last = next;
                }
                break;
            case STRING:
                len = getslength(tmp);
                for (i = 0; i < len; i++) {
                    next = consa(cvchar(getstringch(tmp,i)));
                    if (!null(val)) rplacd(last,next);
                    else val = next;
                    last = next;
                }
                break;
            default: 
                xlbadtype(tmp); break; /* need default because no precheck*/
        }
    }
    
    xlpop();
    
    return (val);

}
    

LVAL xconcatenate()
{
    LVAL tmp;
    
    switch (xlcvttype(tmp = xlgetarg())) {  /* target type of data */
        case CONS:      return cattocons();
        case STRING:    return cattostring();           
        case VECTOR:    return cattovector();
        default:        xlerror("invalid result type", tmp);
                        return (NIL);   /* avoid warning */
    }
}

/* xsubseq - return a subsequence -- new version */

LVAL xsubseq()
{
    unsigned start,end=0,len;
    FIXTYPE temp;
    int srctype;
    LVAL src,dst;
    LVAL next,last=NIL;

    /* get sequence */
    src = xlgetarg();
    if (listp(src)) srctype = CONS;
    else srctype=ntype(src);

    
    /* get length */
    switch (srctype) {
        case STRING:
            len = getslength(src);
            break;
        case VECTOR:
            len = getsize(src);
            break;
        case CONS:      /* BADLY INEFFICIENT! */
            dst = src;  /* use dst as temporary */
            len = 0;
            while (consp(dst)) {
                dst = cdr(dst);
                len++; 
                if (len > MAXSLEN) xltoolong();
            }
            break;
        default:
            xlbadtype(src);
    }

    /* get the starting position */
    dst = xlgafixnum(); temp = getfixnum(dst);
    if (temp < 0 || temp > (FIXTYPE)len) 
        xlerror("sequence index out of bounds",dst);
    start = (unsigned) temp;

    /* get the ending position */
    if (moreargs()) {
        dst = nextarg();
        if (null(dst)) end = len;
        else if (fixp(dst)) {
            temp = getfixnum(dst);
            if (temp < (FIXTYPE)start || temp > (FIXTYPE)len)
                xlerror("sequence index out of bounds",dst);
            end = (unsigned) temp;
        }
        else xlbadtype(dst);
    }
    else
        end = len;
    xllastarg();

    len = end - start;
    
    switch (srctype) {  /* do the subsequencing */
        case STRING:
            dst = newstring(len);
            MEMCPY(getstring(dst), getstring(src)+start, len);
            dst->n_string[len] = 0;
            break;
        case VECTOR:
            dst = newvector(len);
            MEMCPY(dst->n_vdata, &src->n_vdata[start], sizeof(LVAL)*len);
            break;
        case CONS:
            xlsave1(dst);
            while (start--) src = cdr(src);
            while (len--) {
                next = consa(car(src));
                if (!null(dst)) rplacd(last,next);
                else dst = next;
                last = next;
                src = cdr(src);
            }
            xlpop();
            break;
    }

    /* return the substring */
    return (dst);
}


/* xnreverse -- built-in function nreverse (destructive reverse) */
LVAL xnreverse()
{
    LVAL seq,val=NIL,next; /* Val initialized to eliminate warning 10/2015 */
    unsigned int i,j;
    int ival;

    /* get the sequence to reverse */
    seq = xlgetarg();
    xllastarg();

    if (null(seq)) return (NIL);    /* empty argument */
    
    switch (ntype(seq)) {
        case CONS:
            val = NIL;
            while (consp(seq)) {
                next = cdr(seq);
                rplacd(seq,val);
                val = seq;
                seq = next;
            }
            break;
        case VECTOR:
            if (getsize(seq) > 1)
                for (i = 0, j = getsize(seq)-1; i < j; i++, j--) {
                    val = getelement(seq,i);
                    setelement(seq,i,getelement(seq,j));
                    setelement(seq,j,val);
                }
            return seq;
        case STRING:
            if (getslength(seq) > 1)  /* TAA MOD 7/18/98 Changed from 2 */
                for (i = 0, j=getslength(seq)-1 ; i < j; i++, j--) {
                    ival = seq->n_string[i];
                    seq->n_string[i] = seq->n_string[j];
                          seq->n_string[j] = (char) ival;
                }
            return seq;
        default: 
            xlbadtype(seq); break;
    }

    /* return the sequence */
    return (val);
}

/* xreverse - built-in function reverse -- new version */
LVAL xreverse()
{
    LVAL seq,val=NIL; /* val initialized to eliminate warning 10/2015 */
    unsigned i,len;

    /* get the sequence to reverse */
    seq = xlgetarg();
    xllastarg();

    if (null(seq)) return (NIL);    /* empty argument */
    
    switch (ntype(seq)) {
        case CONS:
            /* protect pointer */
            xlsave1(val);

            /* append each element to the head of the result list */
            for (val = NIL; consp(seq); seq = cdr(seq))
                val = cons(car(seq),val);

            /* restore the stack */
            xlpop();
            break;
        case VECTOR:
            len = getsize(seq);
            val = newvector(len);
            for (i = 0; i < len; i++)
                setelement(val,i,getelement(seq,len-i-1));
            break;
        case STRING:
            len = getslength(seq);
            val = newstring(len);
            for (i = 0; i < len; i++)
                val->n_string[i] = seq->n_string[len-i-1];
            val->n_string[len] = 0;
            break;
        default: 
            xlbadtype(seq); break;
    }

    /* return the sequence */
    return (val);
}


/* remif - common code for 'remove', 'remove-if', and 'remove-if-not' */
LOCAL LVAL NEAR remif(tresult,expr)
  int tresult,expr;
{
    LVAL x,seq,fcn,val,next;
    LVAL last=NULL;
    unsigned i,j,l;
    unsigned start,end;
    long s;

#ifdef KEYARG
    LVAL kfcn;
#endif
#ifdef FROMEND
    int fromend=FALSE;  /* process from the end */
    FIXTYPE count=-1;
#endif


    if (expr) {
        /* get the expression to remove and the sequence */
        x = xlgetarg();
        seq = xlgetarg();
        xltest(&fcn,&tresult);
    }
    else {
        /* get the function and the sequence */
        fcn = xlgetarg();
        seq = xlgetarg();
    }

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef FROMEND
    if (xlgetkeyarg(k_fromend, &val) && !null(val)) fromend=TRUE;
    if (xlgetkeyarg(k_count, &val)) {
        if (!null(val)) {
            if (!fixp(val)) xlbadtype(val);
            count = getfixnum(val);
            if (count < 0) count = 0;
        }
    }
    if (count <= 0) fromend=FALSE; /* save some time */
#endif

#ifdef KEYARG
    kfcn=xlkey();
#endif

    xllastkey();

    if (null(seq)) return NIL;

    /* protect some pointers */

#ifdef KEYARG
    xlstkcheck(3);
    xlprotect(kfcn);
#else
    xlstkcheck(2);
#endif
    xlprotect(fcn);
    xlsave(val);

    /* remove matches */
    
#ifdef FROMEND
    if (fromend) {
    switch (ntype(seq)) {
        case CONS:
            l = getlength(seq);
            val = NIL;
            for (i=l-1; (signed)i != -1; i--) {
                for (next=seq, j=i; j>0; j--) next = cdr(next);
                next = car(next);
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
                    count== 0 ||
                    (expr?dotest2(x,next,fcn,kfcn)
                    :dotest1(next,fcn,kfcn)) != tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
                    count== 0 ||
                    (expr?dotest2(x,next,fcn)
                    :dotest1(next,fcn)) != tresult)
#endif
                    val = cons(next, val);
                else
                    count--;
            }
            break;
        case VECTOR:
            val = newvector(l=getsize(seq));
            for (i=j=l-1; (signed)i != -1; i--) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
                    count == (FIXTYPE)(j-i) ||
                    (expr?dotest2(x,getelement(seq,i),fcn,kfcn)
                    :dotest1(getelement(seq,i),fcn,kfcn)) != tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
                    count == (FIXTYPE)(j-i) ||
                    (expr?dotest2(x,getelement(seq,i),fcn)
                    :dotest1(getelement(seq,i),fcn)) != tresult)
#endif
                {
                    setelement(val,j--,getelement(seq,i));
                }
            }
            if (++j != 0) { /* need new, shorter result -- too bad */
                fcn = val; /* save value in protected cell */
                l -= j; /* new length */
                val = newvector(l);
                MEMCPY(val->n_vdata, fcn->n_vdata+j, l*sizeof(LVAL));
            }
            break;
        case STRING:
            l = getslength(seq);
            val = newstring(l);
            for (i=j=l-1; (signed)i !=-1; i--) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
                    count == (FIXTYPE)(j-i) ||
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))!=tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
                    count == (FIXTYPE)(j-i) ||
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn)) != tresult)
#endif
                {
                    val->n_string[j--] = seq->n_string[i];
                }
            }
            if (++j != 0) { /* need new, shorter result -- too bad */
                fcn = val; /* save value in protected cell */
                l -= j;
                val = newstring(l);
                MEMCPY(val->n_string, fcn->n_string+j, l*sizeof(char));
                val->n_string[l] = 0;
            }
            break;
        default:
            xlbadtype(seq); break;
    }
    }
    else {
#endif
    switch (ntype(seq)) {
        case CONS:
#ifdef FROMEND
            for (s=start; end-- > 0 && count != 0; seq = cdr(seq)) {
#else
            for (s=start; end-- > 0; seq = cdr(seq)) {
#endif
                        /* check to see if this element should be deleted */

#ifdef KEYARG
                if (s-- > 0 || 
                    (expr?dotest2(x,car(seq),fcn,kfcn)
                    :dotest1(car(seq),fcn,kfcn)) != tresult)
#else
                if (s-- > 0 || 
                    (expr?dotest2(x,car(seq),fcn)
                    :dotest1(car(seq),fcn)) != tresult)
#endif
                {
                    next = consa(car(seq));
                    if (!null(val)) rplacd(last,next);
                    else val = next;
                    last = next;
                }
#ifdef FROMEND
                else count--;
#endif
            }
            /* copy to end */
            while (consp(seq)) {
                next = consa(car(seq));
                if (!null(val)) rplacd(last,next);
                else val = next;
                last = next;
                seq = cdr(seq);
            }
            break;
        case VECTOR:
            val = newvector(l=getsize(seq));
            for (i=j=0; i < l; i++) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count == (FIXTYPE)(i-j) ||
#endif
                    (expr?dotest2(x,getelement(seq,i),fcn,kfcn)
                    :dotest1(getelement(seq,i),fcn,kfcn)) != tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count == (FIXTYPE)(i-j) ||
#endif
                    (expr?dotest2(x,getelement(seq,i),fcn)
                    :dotest1(getelement(seq,i),fcn)) != tresult)
#endif
                {
                    setelement(val,j++,getelement(seq,i));
                }
            }
            if (l != j) { /* need new, shorter result -- too bad */
                fcn = val; /* save value in protected cell */
                val = newvector(j);
                MEMCPY(val->n_vdata, fcn->n_vdata, j*sizeof(LVAL));
            }
            break;
        case STRING:
            l = getslength(seq);
            val = newstring(l);
            for (i=j=0; i < l; i++) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count == (FIXTYPE)(i-j) ||
#endif
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))!=tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count == (FIXTYPE)(i-j) ||
#endif
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn)) != tresult)
#endif
                {
                    val->n_string[j++] = seq->n_string[i];
                }
            }
            if (l != j) { /* need new, shorter result -- too bad */
                fcn = val; /* save value in protected cell */
                val = newstring(j);
                MEMCPY(val->n_string, fcn->n_string, j*sizeof(char));
                val->n_string[j] = 0;
            }
            break;
        default:
            xlbadtype(seq); break;
    }
#ifdef FROMEND
    }
#endif
            
    /* restore the stack */
#ifdef KEYARG
    xlpopn(3);
#else
    xlpopn(2);
#endif

    /* return the updated sequence */
    return (val);
}

/* xremif - built-in function 'remove-if' -- enhanced version */
LVAL xremif()
{
    return (remif(TRUE,FALSE));
}

/* xremifnot - built-in function 'remove-if-not' -- enhanced version */
LVAL xremifnot()
{
    return (remif(FALSE,FALSE));
}

/* xremove - built-in function 'remove' -- enhanced version */

LVAL xremove()
{
    return (remif(TRUE,TRUE));
}


/* delif - common code for 'delete', 'delete-if', and 'delete-if-not' */
LOCAL LVAL NEAR delif(tresult,expr)
  int tresult,expr;
{
    LVAL x,seq,fcn,last,val;
    unsigned i,j,l;
    unsigned start,end;

#ifdef KEYARG
    LVAL kfcn;
#endif
#ifdef FROMEND
    int fromend=FALSE;  /* process from the end */
    FIXTYPE count=-1;
#endif

    if (expr) {
        /* get the expression to delete and the sequence */
        x = xlgetarg();
        seq = xlgetarg();
        xltest(&fcn,&tresult);
    }
    else {
        /* get the function and the sequence */
        fcn = xlgetarg();
        seq = xlgetarg();
    }

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef FROMEND
    if (xlgetkeyarg(k_fromend, &val) && !null(val)) fromend=TRUE;
    if (xlgetkeyarg(k_count, &val)) {
        if (!null(val)) {
            if (!fixp(val)) xlbadtype(val);
            count = getfixnum(val);
            if (count < 0) count = 0;
        }
    }
    if (count <= 0) fromend=FALSE; /* save some time */
#endif

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

    if (null(seq)) return NIL;

    /* protect a pointer */

#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
#else
    xlstkcheck(1);
#endif
    xlprotect(fcn);

    /* delete matches */
    
#ifdef FROMEND
    if (fromend) {
    switch (ntype(seq)) {
        case CONS:
            l = getlength(seq);
            for (i=l-1; (i > 0) && (count != 0); i--) {
                if (i < start || i >= end) continue;
                val = seq;
                for (j=i-1; j>0; j--) val = cdr(val); /* preceeding cell */
#ifdef KEYARG
                if ((expr?dotest2(x,car(cdr(val)),fcn,kfcn)
                    :dotest1(car(cdr(val)),fcn,kfcn)) == tresult)
#else
                if ((expr?dotest2(x,car(cdr(val)),fcn)
                    :dotest1(car(cdr(val)),fcn)) == tresult)
#endif
                {
                    rplacd(val, cdr(cdr(val)));
                    count--;
                }
            }
            val = seq;
            /* check for deletion of first element */
            if (count !=0 && start == 0 && end > 0 &&
#ifdef KEYARG
                (expr?dotest2(x,car(val),fcn,kfcn)
                    :dotest1(car(val),fcn,kfcn)) == tresult
#else
                (expr?dotest2(x,car(val),fcn)
                    :dotest1(car(val),fcn)) == tresult
#endif
                ) {
                val = cdr(val);
            }
            break;
        case VECTOR:
            l = getsize(seq);
            for (i=j=l-1; (signed)i != -1; i--) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
                    count == (FIXTYPE)(j-i) ||
                    (expr?dotest2(x,getelement(seq,i),fcn,kfcn)
                    :dotest1(getelement(seq,i),fcn,kfcn)) != tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
                    count == (FIXTYPE)(j-i) ||
                    (expr?dotest2(x,getelement(seq,i),fcn)
                    :dotest1(getelement(seq,i),fcn)) != tresult)
#endif
                {
                    if (i != j) setelement(seq,j,getelement(seq,i));
                    j--;
                }
            }
            if (++j != 0) { /* need new, shorter result -- too bad */
                fcn = seq; /* save value in protected cell */
                l -= j; /* new length */
                seq = newvector(l);
                MEMCPY(seq->n_vdata, fcn->n_vdata+j, l*sizeof(LVAL));
            }
            val = seq;
            break;
        case STRING:
            l = getslength(seq);
            for (i=j=l-1; (signed)i != -1; i--) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
                    count == (FIXTYPE)(j-i) ||
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))!=tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
                    count == (FIXTYPE)(j-i) ||
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn)) != tresult)
#endif
                {
                    if (i != j) seq->n_string[j] = seq->n_string[i];
                    j--;
                }
            }
            if (++j != 0) { /* need new, shorter result -- too bad */
                fcn = seq; /* save value in protected cell */
                l -= j; /* new length */
                seq = newstring(l);
                MEMCPY(seq->n_string, fcn->n_string+j, l*sizeof(char));
                seq->n_string[l] = 0;
            }
            val = seq;
            break;
        default:
            xlbadtype(seq); break;
    }
    }
    else {
#endif
    switch (ntype(seq)) {
        case CONS:
            end -= start; /* gives length */
            /* delete leading matches, only if start is 0 */
            if (start == 0)
                while (consp(seq) && end > 0) {
                    end--;
#ifdef FROMEND
                    if (count == 0) break;
#endif
#ifdef KEYARG
                    if ((expr?dotest2(x,car(seq),fcn,kfcn)
                        :dotest1(car(seq),fcn,kfcn)) != tresult)
#else
                    if ((expr?dotest2(x,car(seq),fcn)
                        :dotest1(car(seq),fcn)) != tresult)
#endif
                            break;
                    seq = cdr(seq);
#ifdef FROMEND
                    count--;
#endif
                }

            val = last = seq;

            /* delete embedded matches */
            if (consp(seq) && end > 0) {

                /* skip the first non-matching element, start == 0 */
                if (start == 0) seq = cdr(seq);

                /* skip first elements if start > 0, correct "last" */
                for (;consp(seq) && start-- > 0;last=seq, seq=cdr(seq)) ;

                /* look for embedded matches */
                while (consp(seq) && end-- > 0) {

                    /* check to see if this element should be deleted */
#ifdef FROMEND
                    if (count != 0) {
#endif
#ifdef KEYARG
                    if ((expr?dotest2(x,car(seq),fcn,kfcn)
                        :dotest1(car(seq),fcn,kfcn)) == tresult)
#else
                    if ((expr?dotest2(x,car(seq),fcn)
                        :dotest1(car(seq),fcn)) == tresult)
#endif
                    {
                        rplacd(last,cdr(seq));
#ifdef FROMEND
                        count--;
#endif
                    }
                    else {
                        last = seq;
                    }
                    /* move to the next element */
                    seq = cdr(seq);
#ifdef FROMEND
                    }
#endif
                }
            }
            break;
        case VECTOR:
            l = getsize(seq);
            for (i=j=0; i < l; i++) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count == (FIXTYPE)(i-j) ||
#endif
                    (expr?dotest2(x,getelement(seq,i),fcn,kfcn)
                    :dotest1(getelement(seq,i),fcn,kfcn)) != tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count == (FIXTYPE)(i-j) ||
#endif
                    (expr?dotest2(x,getelement(seq,i),fcn)
                    :dotest1(getelement(seq,i),fcn)) != tresult)
#endif
            {
                    if (i != j) setelement(seq,j,getelement(seq,i));
                    j++;
                }
            }
            if (l != j) { /* need new, shorter result -- too bad */
                fcn = seq; /* save value in protected cell */
                seq = newvector(j);
                MEMCPY(seq->n_vdata, fcn->n_vdata, j*sizeof(LVAL));
            }
            val = seq;
            break;
        case STRING:
            l = getslength(seq);
            for (i=j=0; i < l; i++) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count == (FIXTYPE)(i-j) ||
#endif
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))!=tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count == (FIXTYPE)(i-j) ||
#endif
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn)) != tresult)
#endif
                {
                    if (i != j) seq->n_string[j] = seq->n_string[i];
                    j++;
                }
            }
            if (l != j) { /* need new, shorter result -- too bad */
                fcn = seq; /* save value in protected cell */
                seq = newstring(j);
                MEMCPY(seq->n_string, fcn->n_string, j*sizeof(char));
                seq->n_string[j] = 0;
            }
            val = seq;
            break;
        default:
            xlbadtype(seq); break;
    }
#ifdef FROMEND
    }
#endif
    
    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the updated sequence */
    return (val);
}

/* xdelif - built-in function 'delete-if' -- enhanced version */
LVAL xdelif()
{
    return (delif(TRUE,FALSE));
}

/* xdelifnot - built-in function 'delete-if-not' -- enhanced version */
LVAL xdelifnot()
{
    return (delif(FALSE,FALSE));
}

/* xdelete - built-in function 'delete' -- enhanced version */

LVAL xdelete()
{
    return (delif(TRUE,TRUE));
}

#ifdef SUBSTITUTE
/* substituteif - common code for 'substitute*' functions */
LOCAL LVAL NEAR substituteif(tresult,expr)
  int tresult,expr;
{
    LVAL x,seq,fcn,val,next,repl;
    LVAL last=NULL;
    char ch;
    unsigned i,j,l;
    unsigned start,end;
    long s;

#ifdef KEYARG
    LVAL kfcn;
#endif
#ifdef FROMEND
    int fromend=FALSE;  /* process from the end */
    FIXTYPE count=-1;
#endif

    repl = xlgetarg();  /* replacement expression */

    if (expr) {
        /* get the expression to remove and the sequence */
        x = xlgetarg();
        seq = xlgetarg();
        xltest(&fcn,&tresult);
    }
    else {
        /* get the function and the sequence */
        fcn = xlgetarg();
        seq = xlgetarg();
    }

    if (stringp(seq)) {
        if (charp(repl)) ch = (char) getchcode(repl);
        else xlerror("substitution invalid for string", repl);
    }
    getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef FROMEND
    if (xlgetkeyarg(k_fromend, &val) && !null(val)) fromend=TRUE;
    if (xlgetkeyarg(k_count, &val)) {
        if (!null(val)) {
            if (!fixp(val)) xlbadtype(val);
            count = getfixnum(val);
            if (count < 0) count = 0;
        }
    }
    if (count <= 0) fromend=FALSE; /* save some time */
#endif

#ifdef KEYARG
    kfcn=xlkey();
#endif

    xllastkey();

    if (null(seq)) return NIL;

    /* protect some pointers */

#ifdef KEYARG
    xlstkcheck(3);
    xlprotect(kfcn);
#else
    xlstkcheck(2);
#endif
    xlprotect(fcn);
    xlsave(val);

    /* substitute matches */
    
#ifdef FROMEND
    if (fromend) {
    switch (ntype(seq)) {
        case CONS:
            l = getlength(seq);
            val = NIL;
            for (i=l-1; (signed)i != -1; i--) {
                for (next=seq, j=i; j>0; j--) next = cdr(next);
                next = car(next);
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
                    count== 0 ||
                    (expr?dotest2(x,next,fcn,kfcn)
                    :dotest1(next,fcn,kfcn)) != tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
                    count== 0 ||
                    (expr?dotest2(x,next,fcn)
                    :dotest1(next,fcn)) != tresult)
#endif
                    val = cons(next, val);
                else {
                    val = cons(repl, val);
                    count--;
                }
            }
            break;
        case VECTOR:
            val = newvector(l=getsize(seq));
            for (i=l-1; (signed)i != -1; i--) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
                    count== 0 ||
                    (expr?dotest2(x,getelement(seq,i),fcn,kfcn)
                    :dotest1(getelement(seq,i),fcn,kfcn)) != tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
                    count== 0 ||
                    (expr?dotest2(x,getelement(seq,i),fcn)
                    :dotest1(getelement(seq,i),fcn)) != tresult)
#endif
                {
                    setelement(val,i,getelement(seq,i));
                }
                else
                {
                    setelement(val,i,repl);
                    count--;
                }
            }
            break;
        case STRING:
            l = getslength(seq);
            val = newstring(l);
            for (i=l-1; (signed)i !=-1; i--) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
                    count== 0 ||
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))!=tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
                    count== 0 ||
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn)) != tresult)
#endif
                {
                    val->n_string[i] = seq->n_string[i];
                }
                else
                {
                    val->n_string[i] = ch;
                    count--;
                }
            }
            break;
        default:
            xlbadtype(seq); break;
    }
    }
    else {
#endif
    switch (ntype(seq)) {
        case CONS:
#ifdef FROMEND
            for (s=start; end-- > 0 && count != 0; seq = cdr(seq)) {
#else
            for (s=start; end-- > 0; seq = cdr(seq)) {
#endif
            /* check to see if this element should be replaced */

#ifdef KEYARG
                if (s-- > 0 || 
                    (expr?dotest2(x,car(seq),fcn,kfcn)
                    :dotest1(car(seq),fcn,kfcn)) != tresult)
#else
                if (s-- > 0 || 
                    (expr?dotest2(x,car(seq),fcn)
                    :dotest1(car(seq),fcn)) != tresult)
#endif
                {
                    next = consa(car(seq));
                }
                else {
                    next = consa(repl);
#ifdef FROMEND
                    count--;
#endif
                }
                if (!null(val)) rplacd(last,next);
                else val = next;
                last = next;
            }
            /* copy to end */
            while (consp(seq)) {
                next = consa(car(seq));
                if (!null(val)) rplacd(last,next);
                else val = next;
                last = next;
                seq = cdr(seq);
            }
            break;
        case VECTOR:
            val = newvector(l=getsize(seq));
            for (i=0; i < l; i++) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count== 0 ||
#endif
                    (expr?dotest2(x,getelement(seq,i),fcn,kfcn)
                    :dotest1(getelement(seq,i),fcn,kfcn)) != tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count== 0 ||
#endif
                    (expr?dotest2(x,getelement(seq,i),fcn)
                    :dotest1(getelement(seq,i),fcn)) != tresult)
#endif
                {
                    setelement(val,i,getelement(seq,i));
                }
                else
                {
                    setelement(val,i,repl);
#ifdef FROMEND
                    count--;
#endif
                }
            }
            break;
        case STRING:
            l = getslength(seq);
            val = newstring(l);
            for (i=0; i < l; i++) {
#ifdef KEYARG
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count== 0 ||
#endif
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))!=tresult)
#else
                if (i < start || i >= end ||    /* copy if out of range */
#ifdef FROMEND
                    count== 0 ||
#endif
                    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn)) != tresult)
#endif
                {
                    val->n_string[i] = seq->n_string[i];
                }
                else
                {
                    val->n_string[i] = ch;
#ifdef FROMEND
                    count--;
#endif
                }

            }
            break;
        default:
            xlbadtype(seq); break;
    }
#ifdef FROMEND
    }
#endif
            
    /* restore the stack */
#ifdef KEYARG
    xlpopn(3);
#else
    xlpopn(2);
#endif

    /* return the updated sequence */
    return (val);
}

/* xsubstituteif - built-in function 'substitute-if' -- enhanced version */
LVAL xsubstituteif()
{
    return (substituteif(TRUE,FALSE));
}

/* xsubstituteifnot - built-in function 'substitute-if-not' -- enhanced version */
LVAL xsubstituteifnot()
{
    return (substituteif(FALSE,FALSE));
}

/* xsubstitute - built-in function 'substitute' -- enhanced version */

LVAL xsubstitute()
{
    return (substituteif(TRUE,TRUE));
}


/* nsubstituteif - common code for 'nsubstitute*' family */
LOCAL LVAL NEAR nsubstituteif(tresult,expr)
  int tresult,expr;
{
    LVAL x,seq,fcn,val,repl;
    char ch;
    unsigned i,j,l;
    unsigned start,end;

#ifdef KEYARG
     LVAL kfcn;
#endif
#ifdef FROMEND
    int fromend=FALSE;  /* process from the end */
    FIXTYPE count=-1;
#endif

    repl = xlgetarg();  /* replacement expression */

    if (expr) {
        /* get the expression to substitute and the sequence */
        x = xlgetarg();
        seq = xlgetarg();
        xltest(&fcn,&tresult);
    }
    else {
        /* get the function and the sequence */
        fcn = xlgetarg();
        seq = xlgetarg();
    }

    if (stringp(seq)) {
        if (charp(repl)) ch = (char) getchcode(repl);
        else xlerror("substitution invalid for string", repl);
    }
    getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef FROMEND
    if (xlgetkeyarg(k_fromend, &val) && !null(val)) fromend=TRUE;
    if (xlgetkeyarg(k_count, &val)) {
        if (!null(val)) {
            if (!fixp(val)) xlbadtype(val);
            count = getfixnum(val);
            if (count < 0) count = 0;
        }
    }
    if (count <= 0) fromend=FALSE; /* save some time */
#endif

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

    if (null(seq)) return NIL;

    /* protect a pointer */

#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
#else
    xlstkcheck(1);
#endif
    xlprotect(fcn);

    /* substitute matches */
    
#ifdef FROMEND
    if (fromend) {
    switch (ntype(seq)) {
        case CONS:
            l = getlength(seq);
            for (i=l-1; (signed)i != -1 && (i >= start) && (count != 0); i--) {
                if (i >= end) continue;
                val = seq;
                for (j=i; j>0; j--) val = cdr(val); /* find cell */
#ifdef KEYARG
                if ((expr?dotest2(x,car(val),fcn,kfcn)
                    :dotest1(car(val),fcn,kfcn)) == tresult)
#else
                if ((expr?dotest2(x,car(val),fcn)
                    :dotest1(car(val),fcn)) == tresult)
#endif
                {
                    rplaca(val, repl);
                    count--;
                }
            }
            val = seq;
            break;
        case VECTOR:
            l = getsize(seq);
            for (i=l-1; (signed)i != -1 && (i >= start) && (count != 0); i--) {
                if (i >= end) continue;
#ifdef KEYARG
                if ((expr?dotest2(x,getelement(seq,i),fcn,kfcn)
                    :dotest1(getelement(seq,i),fcn,kfcn)) == tresult)
#else
                if ((expr?dotest2(x,getelement(seq,i),fcn)
                    :dotest1(getelement(seq,i),fcn)) == tresult)
#endif
                {
                    setelement(seq, i, repl);
                    count--;
                }
            }
            val = seq;
            break;
        case STRING:
            l = getslength(seq);
            for (i=l-1; (signed)i != -1 && (i >= start) && (count != 0); i--) {
                if (i >= end) continue;
#ifdef KEYARG
                if ((expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))==tresult)
#else
                if ((expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn)) == tresult)
#endif
                {
                    seq->n_string[i] = ch;
                    count--;
                }
            }
            val = seq;
            break;
        default:
            xlbadtype(seq); break;
    }
    }
    else {
#endif
    switch (ntype(seq)) {
        case CONS:
            val = seq;
            end = end - start; /* number of elements to check */

            /* substitute embedded matches */
            if (consp(seq) && end > 0) {

                /* skip first elements if start > 0 */
                for (;consp(seq) && start-- > 0; seq=cdr(seq)) ;

                /* look for embedded matches */
#ifdef FROMEND
                while (consp(seq) && end-- > 0 && count != 0)
#else
                while (consp(seq) && end-- > 0)
#endif
                {
                    /* check to see if this element should be replaced */
#ifdef KEYARG
                    if ((expr?dotest2(x,car(seq),fcn,kfcn)
                        :dotest1(car(seq),fcn,kfcn)) == tresult)
#else
                    if ((expr?dotest2(x,car(seq),fcn)
                        :dotest1(car(seq),fcn)) == tresult)
#endif
                    {
                        rplaca(seq,repl);
#ifdef FROMEND
                        count--;
#endif
                    }

                    /* move to the next element */
                    seq = cdr(seq);
                }
            }
            break;
        case VECTOR:
            l = getsize(seq);
#ifdef KEYARG
#ifdef FROMEND
            for (i=start; (i < l) && (i < end) && (count != 0); i++)
#else
            for (i=start; (i < l) && (i < end); i++)
#endif
#else
            for (i=0; i < l; i++)
#endif
            {
#ifdef KEYARG
                if ((expr?dotest2(x,getelement(seq,i),fcn,kfcn)
                    :dotest1(getelement(seq,i),fcn,kfcn)) == tresult)
#else
                if ((expr?dotest2(x,getelement(seq,i),fcn)
                    :dotest1(getelement(seq,i),fcn)) == tresult)
#endif
                    {
                    setelement(seq, i, repl);
#ifdef FROMEND
                    count--;
#endif
                    }
            }
            val = seq;
            break;
        case STRING:
            l = getslength(seq);
#ifdef KEYARG
#ifdef FROMEND
            for (i=start; (i < l) && (i < end) && (count != 0); i++)
#else
            for (i=start; (i < l) && (i < end); i++)
#endif
#else
            for (i=0; i < l; i++)
#endif
            {
#ifdef KEYARG
                if ((expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn)) == tresult)
#else
                if ((expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
                    :dotest1(cvchar(getstringch(seq,i)),fcn)) == tresult)
#endif
                {
                    seq->n_string[i] = ch;
#ifdef FROMEND
                    count--;
#endif
                }
            }
            val = seq;
            break;
        default:
            xlbadtype(seq); break;
    }
#ifdef FROMEND
    }
#endif
    
    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the updated sequence */
    return (val);
}

/* xnsubstituteif - built-in function 'nsubstitute-if' -- enhanced version */
LVAL xnsubstituteif()
{
    return (nsubstituteif(TRUE,FALSE));
}

/* xnsubstituteifnot - built-in function 'nsubstitute-if-not' -- enhanced version */
LVAL xnsubstituteifnot()
{
    return (nsubstituteif(FALSE,FALSE));
}

/* xnsubstitute - built-in function 'nsubstitute' -- enhanced version */

LVAL xnsubstitute()
{
    return (nsubstituteif(TRUE,TRUE));
}

#endif

#ifdef POSFCNS
/* TAA MOD -- This is a rewrite done 6/93 to incorporate missing variations */

#define CNTFCN 0    /* three different function types */
#define FNDFCN 1
#define POSFCN 2

/* This is the test expression for all cases */

#ifdef KEYARG
#define bigtest(i) ((expr?dotest2(x,i,fcn,kfcn):dotest1(i,fcn,kfcn)) == tresult)
#else
#define bigtest(i) ((expr?dotest2(x,i,fcn):dotest1(i,fcn)) == tresult)
#endif


/* count*, position*, and find* are all done by the following function */
LOCAL LVAL NEAR xlkitchensink(ftype,tresult,expr)
  int ftype,tresult,expr;
{
    LVAL seq, fcn;          /* sequence and function */
    LVAL x;                 /* expression (when expr is TRUE) */
    unsigned start, end;    /* start and end positions */
    unsigned counter=0;     /* for CNTFCN */
    unsigned count;         /* for POSFCN */
    LVAL val;               /* for FNDFCN */
    
#ifdef KEYARG
    LVAL kfcn;
#endif
#ifdef FROMEND
    int fromend = FALSE; /* process from end */
    unsigned int matchpos = (unsigned)(-1);
#endif
    
    if (expr) {
        x = xlgetarg();         /* get expression */
        seq = xlgetarg();       /* get sequence */
        xltest(&fcn, &tresult); /* get test function and invert from keyargs*/
    }
    else {
        fcn = xlgetarg();       /* get function */
        seq = xlgetarg();       /* get sequence */
    }
    
    getseqbounds(&start, &end, getlength(seq), k_start, k_end);
    
#ifdef FROMEND
    if (xlgetkeyarg(k_fromend, &val) && !null(val)) {
        fromend=TRUE;
        val = NIL;
    }
#endif
#ifdef KEYARG
    kfcn = xlkey();             /* get :key keyword arg */
#endif


    xllastkey();

    if (null(seq))      /* nothing to do, return default result */
        return (ftype==CNTFCN ? cvfixnum((FIXTYPE)0) : NIL);
    
#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
    xlprotect(fcn);
#else
    xlprot1(fcn);
#endif

    count = start;

    /* examine arg and count */
    switch (ntype(seq)) {
        case CONS:
            end -= start;
            for (; consp(seq) && start-- > 0; seq = cdr(seq)) ;
            start = count;
            for (; end-- > 0; seq = cdr(seq), start++)
                if (bigtest(car(seq))) {
                    if (ftype==CNTFCN) counter++;
                    else {
                        val = car(seq); 
#ifdef FROMEND
                        if (fromend) matchpos=start;
                        else goto fin;
#endif
                    }
                }
#ifdef FROMEND
            if (fromend && matchpos != (unsigned)-1) {
                start = matchpos;
                goto fin;
            }
#endif
            break;
        case VECTOR:
#ifdef FROMEND
            if (fromend) {
                for (; start < end;)
                    if (bigtest(getelement(seq,--end))) {
                        if (ftype==CNTFCN) counter++;
                        else {
                            val = getelement(seq,(start=end));
                            goto fin;
                        }
                    }
            }
            else
#endif
            for (; start < end; start++)
                if (bigtest(getelement(seq,start))) {
                    if (ftype==CNTFCN) counter++;
                    else {
                        val = getelement(seq,start);
                        goto fin;
                    }
                }
            break;
        case STRING:
#ifdef FROMEND
            if (fromend) {
                for (;start < end;)
                    if (bigtest((val=cvchar(getstringch(seq,--end))))) {
                        if (ftype==CNTFCN) counter++;
                        else {
                            start = end;
                            goto fin;
                        }
                    }
            }
            else
#endif
            for (; start < end; start++)
                if (bigtest((val=cvchar(getstringch(seq,start))))) {
                    if (ftype==CNTFCN) counter++;
                    else goto fin;
                }
            break;
        default:
            xlbadtype(seq); break;
    }

#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    return (ftype==CNTFCN ? cvfixnum((FIXTYPE)counter) : NIL);

fin:

#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    return (ftype==POSFCN ? cvfixnum((FIXTYPE)start) : val);
}


/* nine different functions are done by xlkitchensink */
LVAL xcount() {             /* count */
    return(xlkitchensink(CNTFCN,TRUE,TRUE));
} 
LVAL xcountif() {           /* count-if */
    return(xlkitchensink(CNTFCN,TRUE,FALSE));
} 
LVAL xcountifnot() {        /* count-if-not */
    return(xlkitchensink(CNTFCN,FALSE,FALSE));
} 
LVAL xposition() {          /* position */
    return(xlkitchensink(POSFCN,TRUE,TRUE));
} 
LVAL xpositionif() {        /* position-if */
    return(xlkitchensink(POSFCN,TRUE,FALSE));
} 
LVAL xpositionifnot() {     /* position-if-not */
    return(xlkitchensink(POSFCN,FALSE,FALSE));
} 
LVAL xfind() {              /* find */
    return(xlkitchensink(FNDFCN,TRUE,TRUE));
} 
LVAL xfindif() {            /* find-if */
    return(xlkitchensink(FNDFCN,TRUE,FALSE));
} 
LVAL xfindifnot() {         /* find-if-not */
    return(xlkitchensink(FNDFCN,FALSE,FALSE));
} 
#endif

#ifdef SRCHFCN
/* xsearch -- search function */

LVAL xsearch()
{
    LVAL seq1, seq2, fcn, temp1, temp2;
    unsigned start1, start2, end1, end2, len1, len2;
    unsigned i,j;
    int tresult,typ1, typ2;
#ifdef KEYARG
    LVAL kfcn;
#endif
    
    /* get the sequences */
    seq1 = xlgetarg();  
    len1 = getlength(seq1);
    seq2 = xlgetarg();
    len2 = getlength(seq2);

    /* test/test-not args? */
    xltest(&fcn,&tresult);

    /* check for start/end keys */
    getseqbounds(&start1,&end1,len1,k_1start,k_1end);
    getseqbounds(&start2,&end2,len2,k_2start,k_2end);
    
#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

    /* calculate the true final search string location that needs to
        be checked (end2) */

    if (end2 - start2 < end1 - start1       /* nothing to compare */
        || end2 - start2 == 0) 
            return (NIL); 

    len1 = end1 - start1;   /* calc lengths of sequences to test */
    end2 -= len1;           /* we don't need to compare with start loc
                                beyond this value */

    typ1 = ntype(seq1);
    typ2 = ntype(seq2);
    
#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
#else
    xlstkcheck(1);
#endif
    xlprotect(fcn);

    if (typ1 == CONS) { /* skip leading section of sequence 1 if a cons */
        j = start1;
        while (j--) seq1 = cdr(seq1);
    }

    if (typ2 == CONS) { /* second string is cons */
        i = start2;     /* skip leading section of string 2 */
        while (start2--) seq2 = cdr(seq2);

        for (;i<=end2;i++) {
            temp2 = seq2;
            if (typ1 == CONS) {
                temp1 = seq1;
                for (j = start1; j < end1; j++) {
#ifdef KEYARG
                    if (dotest2s(car(temp1),car(temp2),fcn,kfcn) != tresult)
                        goto next1;
#else
                    if (dotest2(car(temp1),car(temp2),fcn) != tresult)
                        goto next1;
#endif
                    temp1 = cdr(temp1);
                    temp2 = cdr(temp2);
                }
            }
            else {
                for (j = start1; j < end1; j++) {
#ifdef KEYARG
                    if (dotest2s(typ1 == VECTOR ? getelement(seq1,j) :
                        cvchar(getstringch(seq1,j)), car(temp2), fcn, kfcn)
                        !=tresult)
#else
                    if (dotest2(typ1 == VECTOR ? getelement(seq1,j) :
                       cvchar(getstringch(seq1,j)), car(temp2), fcn)!=tresult)
#endif
                        goto next1;
                    temp2 = cdr(temp2);
                }
            }
#ifdef KEYARG
            xlpopn(2);
#else
            xlpop();
#endif
            return cvfixnum(i);
            next1: /* continue */
            seq2 = cdr(seq2);
        }
    }
                
    else for (i = start2; i <= end2 ; i++) { /* second string is array/string */
        if (typ1 == CONS) { 
            temp1 = seq1;
            for (j = 0; j < len1; j++) {
#ifdef KEYARG
                if (dotest2s(car(temp1), 
                            typ2 == VECTOR ? getelement(seq2,i+j) 
                                           : cvchar(getstringch(seq2,i+j)),
                            fcn,kfcn) != tresult)
#else
                if (dotest2(car(temp1), 
                            typ2 == VECTOR ? getelement(seq2,i+j) 
                                           : cvchar(getstringch(seq2,i+j)),
                            fcn) != tresult)
#endif
                    goto next2;
                temp1 = cdr(temp1);
            }
        }
        else for (j=start1; j < end1; j++) {
#ifdef KEYARG
            if (dotest2s(typ1 == VECTOR ? 
                getelement(seq1,j) : 
                cvchar(getstringch(seq1,j)),
                typ2 == VECTOR ? 
                getelement(seq2,i+j-start1) : 
                cvchar(getstringch(seq2,i+j-start1)), fcn, kfcn) 
                != tresult)
#else
            if (dotest2(typ1 == VECTOR ? 
                getelement(seq1,j) : 
                cvchar(getstringch(seq1,j)),
                typ2 == VECTOR ? 
                getelement(seq2,i+j-start1) : 
                cvchar(getstringch(seq2,i+j-start1)), fcn) 
                != tresult)
#endif
                    goto next2;
        }
#ifdef KEYARG
        xlpopn(2);
#else
        xlpop();
#endif
        return cvfixnum(i);
        next2:; /* continue */
    }
    
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    return (NIL);   /*no match*/

}
#endif

#ifdef REDUCE

/* The following is based on code with the following copyright message: */
/* XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney                  */
/*      All Rights Reserved                                            */
/*      Permission is granted for unrestricted non-commercial use      */

/* Extended by Tom Almy to put in a single C function, allow :start and 
   :end keywords, correctly  handle case of null(seq), and case where
   sequence is a string */

/* Common Lisp REDUCE function */
LVAL xreduce()
{
    LVAL fcn, seq, initial_value;
    LVAL next, args, result;
    int has_init;
    unsigned start, end;

    fcn = xlgetarg();
    seq = xlgetarg();
    has_init = xlgetkeyarg(k_ivalue, &initial_value);
    getseqbounds(&start, &end, getlength(seq), k_start, k_end);
    xllastkey();

    /* protect some pointers */
    xlstkcheck(4);
    xlsave(next);
    xlsave(args);
    xlsave(result);
    xlprotect(fcn);

    args = cons(NIL, cons(NIL,NIL));

    if (null(seq) || start==end) {
        result = has_init ? initial_value : xlapply(pushargs(fcn, NIL));
    }
    else switch (ntype(seq)) {
        case CONS:
            end -= start;
            while (start-- > 0) seq = cdr(seq); /* skip to start */
            next = seq;
            if (has_init) result = initial_value;
            else {
                result = car(next);
                next = cdr(next);
                end--;
            }
            for (; end-- > 0; next = cdr(next)) {
                rplaca(args, result);
                rplaca(cdr(args), car(next));
                result = xlapply(pushargs(fcn, args));
            }
            break;
        case VECTOR:
            if (has_init) 
                result = initial_value;
            else {
                result = getelement(seq, start);
                start++;
            }
            for (; start < end; start++) {
                rplaca(args, result);
                rplaca(cdr(args), getelement(seq, start));
                result = xlapply(pushargs(fcn, args));
            }
            break;
        case STRING:    /* for completeness, darned if I can think of a use */
            if (has_init) 
                result = initial_value;
            else {
                result = cvchar(getstringch(seq, start));
                start++;
            }
            for (; start < end; start++) {
                rplaca(args, result);
                rplaca(cdr(args), cvchar(getstringch(seq, start)));
                result = xlapply(pushargs(fcn, args));
            }
            break;
        default:
            xlbadtype(seq);
        }
    
    /* restore the stack frame */
    xlpopn(4);
    
    return(result);
}


#endif

#ifdef REMDUPS

/* Common Lisp REMOVE-DUPLICATES function */
/* by Tom Almy */
/* unlike xllist.c version, this one works on all sequences and 
   allows the :start and :end keywords. */

LVAL xremove_duplicates()
{
    LVAL seq,fcn,val,next,tmp;
    LVAL last=NULL;
    unsigned i,j,l,k;
    unsigned start,end;
    int tresult;

#ifdef KEYARG
    LVAL kfcn,item;
#endif

    /* get the sequence */
    seq = xlgetarg();

    /* get any optional args */
    xltest(&fcn,&tresult);

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);
    
#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastkey();

    if (null(seq)) return NIL;

    /* protect some pointers */
#ifdef KEYARG
    xlstkcheck(4);
    xlprotect(kfcn);
    xlsave(item);
#else
    xlstkcheck(2);
#endif
    xlprotect(fcn);
    xlsave(val);

    /* remove matches */
    
    switch (ntype(seq)) {
        case CONS:
            end -= start;   /* length of valid subsequence */
            while (start-- > 0) {   /* copy leading part intact */
                next = consa(car(seq));
                if (!null(val)) rplacd(last,next);
                else val=next;
                last= next;
            }
            
            for (; end-- > 1; seq = cdr(seq)) {
                /* check to see if this element should be deleted */
#ifdef KEYARG
                item = car(seq);
                if (!null(kfcn)) item = xlapp1(kfcn,item);
                for (l=end,tmp=cdr(seq); l-- >0; tmp = cdr(tmp))
                    if (dotest2(item,car(tmp),fcn,kfcn)==tresult)
                        goto cons_noxfer;
#else
                for (l=end,tmp=cdr(seq); l-- >0; tmp = cdr(tmp))
                    if (dotest2(car(seq),car(tmp),fcn)==tresult)
                        goto cons_noxfer;
#endif              
                next = consa(car(seq));
                if (!null(val)) rplacd(last,next);
                else val = next;
                last = next;
                cons_noxfer:;
            }
            /* now copy to end */
            while (consp(seq)) {
                next = consa(car(seq));
                if (!null(val)) rplacd(last,next);
                else val = next;
                last = next;
                seq = cdr(seq);
            }
            break;
        case VECTOR:
            /* generate new vector */
            val = newvector(l=getsize(seq));

            /* copy starting sequence */
            if (start>0)
                MEMCPY(&getelement(val,0),&getelement(seq,0),start*sizeof(LVAL));

            /* copy non-duplicate elements, i indexes src, j indexes dest */
            for (i=j=start; i < end; i++) {
#ifdef KEYARG
                item = getelement(seq,i);
                if (!null(kfcn)) item = xlapp1(kfcn,item);
                for (k=i+1; k<end; k++)
                    if (dotest2(item,getelement(seq,k),fcn,kfcn)==tresult)
                        goto vector_noxfer;
#else
                for (k=i+1; k<end; k++)
                    if (dotest2(getelement(seq,i),getelement(seq,k),fcn)==tresult)
                        goto vector_noxfer;
#endif
                setelement(val,j++,getelement(seq,i));
                vector_noxfer:;
            }

            if (l-end > 0) { /* elements at end to copy */
                MEMCPY(&getelement(val,j),
                       &getelement(seq,end),
                       (l-end)*sizeof(LVAL));
                j += l - end;
            }
            
            if (l != j) { /* need new, shorter result -- too bad */
                fcn = val; /* save value in protected cell */
                val = newvector(j);
                MEMCPY(val->n_vdata, fcn->n_vdata, j*sizeof(LVAL));
            }
            break;
        case STRING:
            l = getslength(seq);
            val = newstring(l);

            MEMCPY(&val->n_string,&seq->n_string,start*sizeof(char));
            
            for (i=j=start; i < end; i++) {
#ifdef KEYARG
                item = cvchar(getstringch(seq,i));
                if (!null(kfcn)) item = xlapp1(kfcn,item);
                for (k=i+1; k<end; k++)
                    if (dotest2(item,cvchar(getstringch(seq,k)),fcn,kfcn)==tresult)
                        goto string_noxfer;
#else
                tmp = cvchar(getstringch(seq,i));
                for (k=i+1; k<end; k++)
                    if (dotest2(tmp,cvchar(getstringch(seq,k)),fcn)==tresult)
                        goto string_noxfer;
#endif
                setstringch(val,j++,getstringch(seq,i));
                string_noxfer:;
            }

            MEMCPY(&val->n_string[end],&seq->n_string[end],(l-end)*sizeof(char));

            if (l != j) { /* need new, shorter result -- too bad */
                fcn = val; /* save value in protected cell */
                val = newstring(j);
                MEMCPY(val->n_string, 
                       fcn->n_string, 
                       j*sizeof(char));
                val->n_string[j] = 0;
            }
            break;
        default:
            xlbadtype(seq); break;
    }
        
            
    /* restore the stack */
#ifdef KEYARG
    xlpopn(4);
#else
    xlpopn(2);
#endif

    /* return the updated sequence */
    return (val);
}

#endif


