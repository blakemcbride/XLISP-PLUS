/* xldmem - xlisp dynamic memory management routines */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"
#ifndef DLDMEM
/* node flags */
#define MARK    0x20
#define LEFT    0x40

/* macro to compute the size of a segment */
#define segsize(n) (sizeof(SEGMENT)+((n)-1)*sizeof(struct node))

#ifdef TIMES
static unsigned long gctime;    /* calcuate time in garbage collector */
#endif

/* variables local to xldmem.c and xlimage.c */
SEGMENT *segs,*lastseg,*fixseg,*charseg;
int anodes,nsegs;
LVAL fnodes = NIL;

/* forward declarations */
#ifdef JMAC
LOCAL LVAL NEAR Newnode _((int type));
#else
LOCAL LVAL NEAR newnode _((int type));
#endif
LOCAL char * NEAR stralloc _((unsigned int size));
LOCAL VOID NEAR mark _((LVAL ptr));
LOCAL VOID NEAR sweep _((void));
LOCAL VOID NEAR findmem _((void));
LOCAL int  NEAR addseg _((void));
LOCAL VOID NEAR stats _((void));


#ifdef JMAC
LVAL _nnode = NIL;
FIXTYPE _tfixed = 0;
int _tint = 0;

#define newnode(type) (((_nnode = fnodes) != NIL) ? \
            ((fnodes = cdr(_nnode)), \
             nfree--, \
                 (_nnode->n_type = (char)type), \
             rplacd(_nnode,NIL), \
             _nnode) \
            : Newnode(type))
 
#endif

/* $putpatch.c$: "MODULE_XLDMEM_C_GLOBALS" */

#ifdef VMEM
LOCAL VOID gcq(size)
long size;
{
    if ((total+size)/VMEM > total/VMEM) gc();
}
#endif

/* xlminit - initialize the dynamic memory module */
VOID xlminit()
{
    LVAL p;
    int i;

    /* initialize our internal variables */
    segs = lastseg = NULL;
    nnodes = nfree = total = gccalls = 0L;
    nsegs = 0;
    anodes = NNODES;
    fnodes = NIL;

    /* allocate the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
        xlfatal("insufficient memory");

    /* initialize the fixnum segment */
    p = &fixseg->sg_nodes[0];
    for (i = SFIXMIN; i <= SFIXMAX; ++i) {
        p->n_type = FIXNUM;
        p->n_fixnum = i;
        ++p;
    }

    /* allocate the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
        xlfatal("insufficient memory");

    /* initialize the character segment */
    p = &charseg->sg_nodes[0];
    for (i = CHARMIN; i <= CHARMAX; ++i) {
        p->n_type = CHAR;
        p->n_chcode = i;
        ++p;
    }

    /* initialize structures that are marked by the collector */
    obarray = NULL;
    xlenv = xlfenv = xldenv = NIL;
    s_gcflag = s_gchook = NULL;

    /* $putpatch.c$: "MODULE_XLDMEM_C_XLMINIT" */

    /* allocate the evaluation stack */
    xlstack = xlstktop;

    /* allocate the argument stack */
    xlfp = xlsp = xlargstkbase;
    *xlsp++ = NIL;

#ifdef MULVALS
#if 0
    /* is this needed?? */
    for (i = 0; i < MULVALLIMIT; i++)
      xlresults[i] = NIL;
#endif
    xlnumresults = 0;
#endif /* MULVALS */

    /* we have to make a NIL symbol before continuing */

    p = xlmakesym("NIL");
    memcpy(NIL, p, sizeof(struct node));    /* we point to this! */
    defconstant(NIL, NIL);
    p->n_type = FREE;                       /* don't collect "garbage" */

}

/* cons - construct a new cons node */
LVAL cons(x,y)
  LVAL x,y;
{
    LVAL nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
        xlstkcheck(2);
        xlprotect(x);
        xlprotect(y);
        findmem();
        if ((nnode = fnodes) == NIL)
            xlabort("insufficient node space");
        xlpop();
        xlpop();
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    --nfree;

    /* initialize the new node */
    nnode->n_type = CONS;
    rplaca(nnode,x);
    rplacd(nnode,y);

    /* return the new node */
    return (nnode);
}

/* cvstring - convert a string to a string node */
LVAL cvstring(str)
  char *str;
{
    LVAL val;
    xlsave1(val);
    val = newnode(STRING);
    val->n_strlen = strlen(str);
    val->n_string = stralloc(getslength(val)+1);
    strcpy((char *)getstring(val),str);
    xlpop();
    return (val);
}

/* newstring - allocate and initialize a new string */
LVAL newstring(size)
  unsigned size;
{
    LVAL val;
    xlsave1(val);
    val = newnode(STRING);
    val->n_strlen = size;
    val->n_string = stralloc(size+1);
    val->n_string[0] = 0;
    xlpop();
    return (val);
}

#ifdef BIGNUMS
/* newbignum - allocate a new bignum */
LVAL newbignum(size)
  unsigned size;
{
    /* size of the sign field not included in n_vsize */
    BIGNUMDATA FAR *x;
    LVAL val;
    xlsave1(val);
    val = newnode(BIGNUM);
    val->n_bsize = size;
    x = val->n_bdata = (BIGNUMDATA FAR *)stralloc((size+1)*sizeof(BIGNUMDATA));
    size++;
    while (size--) *x++ = 0;    /* set value to zero */
    xlpop();
    return val;
}
#endif  

/* cvsymbol - convert a string to a symbol */
LVAL cvsymbol(pname)
  char *pname;
{
    LVAL val;
    xlsave1(val);
    val = newvector(SYMSIZE);
    val->n_type = SYMBOL;
    setvalue(val,s_unbound);
    setfunction(val,s_unbound);
    setpname(val,cvstring(pname));
    xlpop();
    return (val);
}

/* cvsubr - convert a function to a subr or fsubr */
LVAL cvsubr(fcn,type,offset)
  LVAL (*fcn) _((void)); int type,offset;
{
    LVAL val;
     val = newnode(type);
    val->n_subr = fcn;
     val->n_offset = (short) offset;
    return (val);
}

/* cvfile - convert a file pointer to a stream */
LVAL cvfile(fp, iomode)
  FILEP fp;
  int  iomode;
{
    LVAL val;
    val = newnode(STREAM);
    setfile(val,fp);
    setsavech(val,'\0');
     val->n_sflags = (char)iomode;
    val->n_cpos = 0;
    return (val);
}

#ifdef JMAC
 
/* cvfixnum - convert an integer to a fixnum node */
LVAL Cvfixnum(n)
  FIXTYPE n;
{
    LVAL val;
    val = newnode(FIXNUM);
    val->n_fixnum = n;
    return (val);
}
#else
/* cvfixnum - convert an integer to a fixnum node */
LVAL cvfixnum(n)
  FIXTYPE n;
{
    LVAL val;
    if (n >= SFIXMIN && n <= SFIXMAX)
        return (&fixseg->sg_nodes[(int)n-SFIXMIN]);
    val = newnode(FIXNUM);
    val->n_fixnum = n;
    return (val);
}
#endif

/* cvflonum - convert a floating point number to a flonum node */
LVAL cvflonum(n)
  FLOTYPE n;
{
    LVAL val;
    val = newnode(FLONUM);
    val->n_flonum = n;
    return (val);
}

/* cvchar - convert an integer to a character node */
#ifdef JMAC
LVAL Cvchar(n)
  int n;
{
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return(NIL);    /* never executed */
}
#else
LVAL cvchar(n)
  int n;
{
    if (n >= CHARMIN && n <= CHARMAX)
        return (&charseg->sg_nodes[n-CHARMIN]);
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return 0;   /* never executed but gets rid of warning message */
}
#endif

#ifdef BIGNUMS
/* cvbratio - convert a pair of bignums into a ratio node */
LVAL cvbratio(num, denom)
LVAL num, denom;
{
    FIXTYPE nu, d;
    int fixtyped;
    LVAL n,m,r;

    if (cvtbigfixnum(num, &nu) && cvtbigfixnum(denom, &d))
        return cvratio(nu,d);
    xlstkcheck(5);
    xlprotect(num);
    xlprotect(denom);
    xlsave(n);
    xlsave(m);
    xlsave(r);

    if (zeropbignum(num)) { /* zero is fixnum zero */
        xlpopn(5);
        return cvfixnum((FIXTYPE) 0);
    }
    if (getbignumsign(denom)) { /* denominator must be positive */
        denom = copybignum(denom, 0);
        num = copybignum(num,!getbignumsign(num));  /* final sign */
    }
    n = copybignum(num, 0); /* abs of numerator */
    m = denom;
    for (;;) { /* get gcd */
        divbignum(m, n, &r); /* use remainder only */
        if (zeropbignum(r)) break;
        m = n;
        n = r;
    }
    if ((!cvtbigfixnum(n, &d)) || d != 1) { /* can reduce */
        denom = divbignum(denom, n, &r);
        num = divbignum(num, n, &r);
    }
    if ((fixtyped = cvtbigfixnum(denom, &d)) != 0 && d == 1) {
        /* reduced to an integer */
        xlpopn(5);
        if (cvtbigfixnum(num, &nu)) return cvfixnum(nu);
        return num;
    }
    /* got value to return */
    r = newnode(RATIO);
    r->n_denom = r->n_numer = NIL; /* in case of garbage collect */
    r->n_denom = (fixtyped ? cvfixnum(d) : denom);
    r->n_numer = (cvtbigfixnum(num, &nu) ? cvfixnum(nu) : num);
    xlpopn(5);
    return (r);
}
    
    
    
/* cvratio - convert an integer pair to a ratio node */
LVAL cvratio(num, denom)
FIXTYPE num, denom;
{
    LVAL val;
    unsigned long n, m, r, nu, de;
    int sign;

    if (num == 0) return cvfixnum((FIXTYPE) 0); /* zero is int zero */
    if (denom < 0) {    /* denominator must be positive */
        if (denom == -1 && num == MINFIX) {
            xlsave1(val);
            val = cvtulongbignum((unsigned long)MAXFIX+1, FALSE);
            xlpop();
            return val;
        }
        denom = -denom;
        sign = num >= 0;
    }
    else
        sign = num < 0;
    
    if (num < 0) num = -num;
    n = nu = (unsigned long)(long)num;
    m = de = (unsigned long)(long)denom;  /* reduce the ratio: compute GCD */
    for (;;) {
        if ((r = m % n) == 0) break;
        m = n;
        n = r;
    }
    if (n != 1) {
        de /= n;
        nu /= n;
    }
    if (de == 1)
        return cvfixnum(sign ? -(long)nu : (long)nu);   /* reduced to integer */
    xlsave1(val);
    val = newnode(RATIO);
    val->n_denom = val->n_numer = NIL; /* in case of garbage collect */
    if ((nu == (unsigned long)MAXFIX+1 && sign==0))
        val->n_numer = cvtulongbignum(nu, sign);
    else
        val->n_numer = cvfixnum(sign ? -(long)nu : (long)nu);
    if (de == (unsigned long)MAXFIX+1)
        val->n_denom = cvtulongbignum(de, FALSE);
    else
        val->n_denom = cvfixnum(de);
    xlpop();
    return (val);
}
#endif

/* newustream - create a new unnamed stream */
LVAL newustream()
{
    LVAL val;
    val = newnode(USTREAM);
    sethead(val,NIL);
    settail(val,NIL);
    return (val);
}

/* newobject - allocate and initialize a new object */
LVAL newobject(cls,size)
  LVAL cls; int size;
{
    LVAL val;
    val = newvector(size+1);
    val->n_type = OBJECT;
    setelement(val,0,cls);
    return (val);
}

/* newclosure - allocate and initialize a new closure */
LVAL newclosure(name,type,env,fenv)
  LVAL name,type,env,fenv;
{
    LVAL val;
    val = newvector(CLOSIZE);
    val->n_type = CLOSURE;
    setname(val,name);
    settype(val,type);
    setenvi(val,env);
    setfenv(val,fenv);
    return (val);
}


/* newstruct - allocate and initialize a new structure node */
LVAL newstruct(type,size)
 LVAL type; int size;
{
    LVAL val;
    val = newvector(size+1);
    val->n_type = STRUCT;
    setelement(val,0,type);
    return (val);
}

#ifdef PACKAGES
/* newpackage - allocate and initialize a new package */
LVAL newpackage()
{
    LVAL val;
    xlsave1(val);
    val = newvector(PACKSIZE);
    val->n_type = PACKAGE;
    setintsyms(val, newvector(HSIZE));
    setextsyms(val, newvector(HSIZE));
    xlpop();
    return (val);
}
#endif /* PACKAGES */


/* newvector - allocate and initialize a new vector node */
LVAL newvector(size)
  unsigned size;
{
    LVAL vect;
    int i;
    long bsize = size * sizeof(LVAL *);

    if (size > MAXVLEN) xlfail("array too large");

    xlsave1(vect);

    vect = newnode(VECTOR);
    vect->n_vsize = 0;

    if (size != 0) {
        /* We must clear to a nonzero value */
#ifdef VMEM
        gcq(bsize);
#endif
        if ((vect->n_vdata = (LVAL *)MALLOC((unsigned int)bsize)) == NULL) {
            gc();   /*  TAA Mod -- was findmem(), but this would
                        cause undesired memory expansion */
            if ((vect->n_vdata = (LVAL *)MALLOC((unsigned int)bsize)) == NULL)
                xlfail("insufficient vector space");
        }
        for (i = size; i-- > 0;) setelement(vect, i, NIL);
        vect->n_vsize = size;
        total += bsize;
    }
    xlpop();
    return (vect);
}

/* newnode - allocate a new node */
#ifdef JMAC
LOCAL LVAL NEAR Newnode(type)
  int type;
{
    LVAL nnode;

    /* get a free node */
    findmem();
    if ((nnode = fnodes) == NIL)
        xlabort("insufficient node space");

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1L;

    /* initialize the new node */
     nnode->n_type = (char)type;
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
}
#else
LOCAL LVAL NEAR newnode(type)
  int type;
{
    LVAL nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
        findmem();
        if ((nnode = fnodes) == NIL)
            xlabort("insufficient node space");
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1L;

    /* initialize the new node */
    nnode->n_type = (char)type;
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
}
#endif

/* stralloc - allocate memory for a string */
LOCAL char * NEAR stralloc(size)
  unsigned int size;
{
    char *sptr;

#ifdef VMEM
    gcq((long)size);
#endif

    /* allocate memory for the string copy */
    if ((sptr = (char *)MALLOC(size)) == NULL) {
        gc();  
        if ((sptr = (char *)MALLOC(size)) == NULL)
            xlfail("insufficient string space");
    }
    total += (long)size;

    /* return the new string memory */
    return (sptr);
}

/* findmem - find more memory by collecting then expanding */
LOCAL VOID NEAR findmem()
{
    gc();
    if (nfree < (long)anodes)
        addseg();
}

/* gc - garbage collect (only called here and in xlimage.c) */
VOID gc()
{
    register LVAL **p,*ap,tmp;
    FRAMEP newfp;
    LVAL fun;
#ifdef TIMES
    unsigned long gccount = run_tick_count();
#endif

#ifdef STSZ
#if (GCSTMARGIN>0)
    if (STACKREPORT(fun)<GCSTMARGIN) { /* Do not try gc with less */
        dbgputstr("Insufficient stack left for GC  ");
        if (batchmode) xlfatal("uncaught error");
        xltoplevel();
    }
#endif
#endif

    /* print the start of the gc message */
    if (s_gcflag != NULL && getvalue(s_gcflag) != NIL) {
        /* print message on a fresh line */
        xlfreshline(getvalue(s_debugio));
        sprintf(buf,"[ gc: total %ld, ",nnodes);
        dbgputstr(buf); /* TAA MOD -- was std output */
    }

    /* $putpatch.c$: "MODULE_XLDMEM_C_GC" */

    /* mark the obarray, the argument list and the current environment */
    if (obarray != NULL)
        mark(obarray);
    if (xlenv != NIL)
        mark(xlenv);
    if (xlfenv != NIL)
        mark(xlfenv);
    if (xldenv != NIL)
        mark(xldenv);

    mark(NIL);
    mark(s_unbound);    /* TAA Mod 1/92 */

    /* mark the evaluation stack */
    for (p = xlstack; p < xlstktop; ++p)
        if ((tmp = **p) != NIL)
            mark(tmp);

    /* mark the argument stack */
    for (ap = xlargstkbase; ap < xlsp; ++ap)
        if ((tmp = *ap) != NIL)
            mark(tmp);

#ifdef MULVALS
    /* mark the results */
    {
        int i = 0;
        for (; i < xlnumresults; i++)
            if ((tmp = xlresults[i]) != NIL)
                mark(tmp);
    }
#endif /* MULVALS */

    /* sweep memory collecting all unmarked nodes */
    sweep();

    NIL->n_type &= ~MARK;

    /* count the gc call */
    ++gccalls;

    /* call the *gc-hook* if necessary */
    if (s_gchook != NULL && ((fun = getvalue(s_gchook)) != NIL) ) {

        /* rebind hook function to NIL  TAA MOD */
        tmp = xldenv;
        xldbind(s_gchook,NIL);

        newfp = xlsp;
        pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
        pusharg(fun);
        pusharg(cvfixnum((FIXTYPE)2));
        pusharg(cvfixnum((FIXTYPE)nnodes));
        pusharg(cvfixnum((FIXTYPE)nfree));
        xlfp = newfp;
        xlapply(2);

        /* unbind the symbol TAA MOD */
        xlunbind(tmp);
    }

    /* print the end of the gc message */
    if (s_gcflag != NULL && getvalue(s_gcflag) != NIL) {
        sprintf(buf,"%ld free ]\n",nfree);
        dbgputstr(buf); /* TAA MOD -- was std output */
    }

#ifdef TIMES
    gctime += run_tick_count() - gccount;
#endif
}

/* mark - mark all accessible nodes */
LOCAL VOID NEAR mark(ptr)
  LVAL ptr;
{
    register LVAL this,prev,tmp;
    int i,n;

#ifdef STSZ /* can't recover from here */
#if (GCSTMARGIN>0)
    if (STACKREPORT(n) < GCMARGLO)
        xlfatal("Insufficient stack during GC");
#endif
#endif

    /* initialize */
    prev = NIL;
    this = ptr;

    /* mark this list */
    for (;;) {
        /* descend as far as we can */
        while (!(this->n_type & MARK)) {
            i = this->n_type;
            this->n_type |= MARK;
            /* check cons stype nodes */
            if (i >= CONS && i < ARRAY) {
                if ((tmp = car(this)) != NIL) {
                    this->n_type |= LEFT;
                    rplaca(this,prev);
                }
                else if ((tmp = cdr(this)) != NIL)
                    rplacd(this,prev);
                else                /* both sides nil */
                    break;
                prev = this;            /* step down the branch */
                this = tmp;
            }
            /* $putpatch.c$: "MODULE_XLDMEM_C_MARK" */
            else {
                if ((i & ARRAY) != 0) {
                    for (i = 0, n = getsize(this); i < n;) {
                        if ((tmp = getelement(this,i++)) != NIL) {
                            if (!(tmp->n_type&MARK) && tmp->n_type >= CONS)
                                mark(tmp);
                            else
                                tmp->n_type |= MARK;
                        }
                    }
                }
                break;
            }
        }

        /* backup to a point where we can continue descending */
        for (;;)

            /* make sure there is a previous node */
            if (prev != NIL) {
                if (prev->n_type & LEFT) {      /* came from left side */
                    prev->n_type &= ~LEFT;
                    tmp = car(prev);
                    rplaca(prev,this);
                    if ((this = cdr(prev)) != NIL) {
                        rplacd(prev,tmp);                       
                        break;
                    }
                }
                else {                          /* came from right side */
                    tmp = cdr(prev);
                    rplacd(prev,this);
                }
                this = prev;                    /* step back up the branch */
                prev = tmp;
            }
        /* no previous node, must be done */
            else
                return;
    }
}

/* sweep - sweep all unmarked nodes and add them to the free list */
LOCAL VOID NEAR sweep()
{
    SEGMENT *seg;
    LVAL p;
    int n;

    /* empty the free list */
    fnodes = NIL;
    nfree = 0L;

    /* add all unmarked nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        if (seg == fixseg || seg == charseg) {
            /* remove marks from segments */
            p = &seg->sg_nodes[0];
            for (n = seg->sg_size; --n >= 0;)
                (p++)->n_type &= ~MARK;
            continue;
        }
        p = &seg->sg_nodes[0];

        for (n = seg->sg_size; --n >= 0;)
            if (p->n_type & MARK)
                (p++)->n_type &= ~MARK;
            else {
                switch (ntype(p)&TYPEFIELD) {
                case STRING:
                        if (getstring(p) != NULL) {
                            total -= (long)getslength(p)+1;
                            MFREE(getstring(p));
                        }
                        break;
#ifdef BIGNUMS
                case BIGNUM:
                    if (getbignumarray(p) != NULL) {
                        total -= (1+(long)getbignumsize(p))*sizeof(BIGNUMDATA);
                        MFREE(getbignumarray(p));
                    }
                    break;
#endif
                case STREAM:
                        if (getfile(p) != CLOSED
                            && getfile(p) != STDIN
                            && getfile(p) != STDOUT
                            && getfile(p) != CONSOLE)/* taa fix - dont close stdio */
                            OSCLOSE(getfile(p));
                        break;
        /* $putpatch.c$: "MODULE_XLDMEM_C_SWEEP" */
                case SYMBOL:
                case OBJECT:
                case VECTOR:
                case CLOSURE:
                case STRUCT:
                        if (p->n_vsize) {
                            total -= (long)p->n_vsize * sizeof(LVAL);
                            MFREE(p->n_vdata);
                        }
                        break;
                }
                p->n_type = FREE;
                rplaca(p,NIL);
                rplacd(p,fnodes);
                fnodes = p++;
                nfree++;
            }
    }
}

/* addseg - add a segment to the available memory */
LOCAL int NEAR addseg()
{
    SEGMENT *newseg;
    LVAL p;
    int n;

    /* allocate the new segment */
    if (anodes == 0 || (newseg = newsegment(anodes)) == NULL)
        return (FALSE);

    /* add each new node to the free list */
    p = &newseg->sg_nodes[0];
    for (n = anodes; --n >= 0; ++p) {
        rplacd(p,fnodes);
        fnodes = p;
    }
    
    /* return successfully */
    return (TRUE);
}

/* newsegment - create a new segment (only called here and in xlimage.c) */
SEGMENT *newsegment(n)
  int n;
{
    SEGMENT *newseg;

    /* allocate the new segment */
    if ((newseg = (SEGMENT *)CALLOC(1,segsize(n))) == NULL)
        return (NULL);

    /* initialize the new segment */
    newseg->sg_size = n;
    newseg->sg_next = NULL;
    if (segs != NULL)
        lastseg->sg_next = newseg;
    else
        segs = newseg;
    lastseg = newseg;

    /* update the statistics */
    total += (long)segsize(n);
    nnodes += (long)n;
    nfree += (long)n;
    ++nsegs;

    /* return the new segment */
    return (newseg);
}
 
/* stats - print memory statistics */
LOCAL VOID NEAR stats()
{
    sprintf(buf,"Nodes:       %7ld  ", nnodes); stdputstr(buf);
    sprintf(buf,"Free nodes:  %7ld  ", nfree);  stdputstr(buf);
    sprintf(buf,"Segments:  %4d\n", nsegs);   stdputstr(buf);
    sprintf(buf,"Allocate:    %7d  ", anodes);  stdputstr(buf);
    sprintf(buf,"Total:       %7ld\n",total);  stdputstr(buf);
#ifdef TIMES
    sprintf(buf,"Collections: %7ld  ", gccalls); stdputstr(buf);
    sprintf(buf,"Time (sec):   %6ld\n", gctime/ticks_per_second());
    stdputstr(buf);
#else
    sprintf(buf,"Collections:  %ld\n", gccalls); stdputstr(buf);
#endif
}

/* xgc - xlisp function to force garbage collection */
LVAL xgc()
{
    /* make sure there aren't any arguments */
    xllastarg();

    /* garbage collect */
    gc();

    /* return nil */
    return (NIL);
}

/* xexpand - xlisp function to force memory expansion */
LVAL xexpand()
{
    LVAL num;
    FIXTYPE n,i;

    /* get the new number to allocate */
    if (moreargs()) {
        num = xlgafixnum();
        n = getfixnum(num);
        /* make sure there aren't any more arguments */
        xllastarg();
    }
    else
        n = 1;

    /* allocate more segments */
    for (i = 0; i < n; i++)
        if (!addseg())
            break;

    /* return the number of segments added */
    return (cvfixnum((FIXTYPE)i));
}

/* xalloc - xlisp function to set the number of nodes to allocate */
LVAL xalloc()
{
    FIXTYPE n;  /* TAA MOD -- prevent overflow */
    int oldn;

    /* get the new number to allocate */
    n = getfixnum(xlgafixnum());    

    /* make sure there aren't any more arguments */
    if (xlargc > 1) xltoomany();    /* but one more is OK, TAA MOD */

    /* Place limits on argument by clipping to reasonable values  TAA MOD */
    if (n > ((long)MAXSLEN - sizeof(SEGMENT))/sizeof(struct node)) 
        n = ((long)MAXSLEN - sizeof(SEGMENT))/sizeof(struct node);
    else if (n < 1000) 
        n = 1000;   /* arbitrary */

    /* set the new number of nodes to allocate */
    oldn = anodes;
    anodes = (int)n;

    /* return the old number */
    return (cvfixnum((FIXTYPE)oldn));
}

/* xmem - xlisp function to print memory statistics */
LVAL xmem()
{
    /* allow one argument for compatiblity with common lisp */
    if (xlargc > 1) xltoomany();    /* TAA Mod */

    /* print the statistics */
    stats();

    /* return nil */
    return (NIL);
}

#ifdef SAVERESTORE
/* xsave - save the memory image */
LVAL xsave()
{
    char *name;

    /* get the file name, verbose flag and print flag */
    name = getstring(xlgetfname());
    xllastarg();

    /* save the memory image */
    return (xlisave(name) ? s_true : NIL);
}

/* xrestore - restore a saved memory image */
LVAL xrestore()
{
    char *name;

    /* get the file name, verbose flag and print flag */
    name = getstring(xlgetfname());
    xllastarg();

    /* restore the saved memory image */
    if (!xlirestore(name))
        return (NIL);

    /* return directly to the top level */
    dbgputstr("[ returning to the top level ]\n");  /* TAA MOD --was std out*/
    longjmp(top_level,1);
    return (NIL);   /* never executed, but avoids warning message */
}

#endif

#ifdef COMPLX
/* From XLISP-STAT, Copyright (c) 1988 Luke Tierney */

LVAL newicomplex(real, imag)
        FIXTYPE real, imag;
{
    LVAL val;
  
    if (imag == 0) val = cvfixnum(real);
    else {
        xlsave1(val);
        val = newnode(COMPLEX);
        getreal(val) = getimag(val) = NIL; /* in case of garbage collection */
        getreal(val) = cvfixnum(real);
        getimag(val) = cvfixnum(imag);
        xlpop();
    }
    return(val);
}

LVAL newdcomplex(real, imag)
        double real, imag;
{
    LVAL val;

    xlsave1(val);
    val = newnode(COMPLEX);
    getreal(val) = getimag(val) = NIL; /* in case of garbage collection */
    getreal(val) = cvflonum((FLOTYPE) real);
    getimag(val) = cvflonum((FLOTYPE) imag);
    xlpop();
    return(val);
}

#ifdef BIGNUMS
/* newcomplex - allocate and initialize a new object */
LVAL newcomplex (real, imag)
   LVAL real, imag;
{
      LVAL val;
      xlstkcheck(2);
      xlprotect(real);
      xlprotect(imag);

      if (! rationalp(real) || ! rationalp(imag)) {
          if (! floatp(real)) real = cvflonum(makefloat(real));
          if (! floatp(imag)) imag = cvflonum(makefloat(imag));
      }
      if (fixp(imag) && getfixnum(imag) == 0)
          val = real;
      else {
          val = newnode(COMPLEX);
          getreal(val) = real;
          getimag(val) = imag;
      }
      xlpopn(2);
      return(val);
}
#else
/* newcomplex - allocate and initialize a new object */
LVAL newcomplex(real,imag)
  LVAL real,imag;
{
  if (fixp(real) && fixp(imag))
    return(newicomplex(getfixnum(real), getfixnum(imag)));
  else
    return(newdcomplex(makefloat(real), makefloat(imag)));
}
#endif
#endif
#endif
