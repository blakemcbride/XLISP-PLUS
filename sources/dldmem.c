/* dldmem - xlisp dynamic memory management routines */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

/* Modified memory management scheme such that array/string space is
   managed here rather than using malloc/free. The advantage of this is
   the array/string space gets compacted allowing better operation when
   available memory is tight or virtual memory is used. XSCHEME does this,
   but probably needs it more since Xscheme functions are kept as compiled
   code in arrays rather than lists. */

/* When this module is used rather than xldmem (and dlimage is used rather
   than xlimage) then ALLOC and EXPAND take an additional second argument
   for array segment allocation size and array segments to add, respectively.
   The ROOM report is changed to indicate array allocation statistics. */


#include "xlisp.h"

#ifdef WINDOWS  /* for changing the cursor */
void startGC(void);
void endGC(void);
#endif


/* node flags */
#define MARK    0x20
#define LEFT    0x40

/* macro to compute the size of a segment */
#define segsize(n) (sizeof(SEGMENT)+((n)-1)*sizeof(struct node))

#ifdef TIMES
static unsigned long gctime;    /* calcuate time in garbage collector */
#endif

/* For vector memory management */
#define vsegsize(n) (sizeof(VSEGMENT)+((n)-1)*sizeof(LVAL))

#define btow_size(n) (((unsigned)(n)+(sizeof(LVAL)-1))/(unsigned)sizeof(LVAL))
#define stow_size(n) (((unsigned)(n)*sizeof(BIGNUMDATA)+(sizeof(LVAL)-1))/(unsigned)sizeof(LVAL))

typedef struct vsegment {
    struct vsegment FAR *vs_next;   /* next vector segment */
    LVAL FAR *vs_free;              /* next free location in this segment */
    LVAL FAR *vs_top;               /* top of segment (plus one) */
    LVAL vs_data[1];            /* segment data */
} VSEGMENT;

VSEGMENT FAR *vsegments;    /* list of vector segments */
VSEGMENT FAR *vscurrent;    /* current vector segment */
int vscount;            /* number of vector segments */
LVAL FAR *vfree;            /* next free location in vector space */
LVAL FAR *vtop;             /* top of vector space */

static long vgccalls = 0;   /* gc calls forced by vector allocation */
static FIXTYPE vecthresh = 1; /* threshold ratio to expand vector memory */

/* variables local to xldmem.c and xlimage.c */
SEGMENT FAR *segs, FAR *lastseg, FAR *fixseg, FAR *charseg;
int anodes,vnodes,nsegs;
long vsfree;
LVAL fnodes;

/* forward declarations */
LOCAL VOID NEAR compact_vector _((VSEGMENT FAR *vseg));
LOCAL VOID NEAR compact _((void));
LOCAL LVAL NEAR allocvector _((int type, unsigned int size));
VSEGMENT FAR* newvsegment _((unsigned int n));
#ifdef JMAC
LOCAL LVAL NEAR Newnode _((int type));
#else
LOCAL LVAL NEAR newnode _((int type));
#endif
LOCAL VOID NEAR mark _((LVAL ptr));
LOCAL VOID NEAR sweep _((void));
LOCAL VOID NEAR findmem _((void));
LOCAL int  NEAR addseg _((void));
int  scanvmemory _((unsigned int size));
LOCAL long NEAR getvused _((void));
LOCAL VOID NEAR stats _((void));

#ifdef JMAC
LVAL _nnode = NIL;
FIXTYPE _tfixed = 0;
int _tint = 0;

#define newnode(type) (((_nnode = fnodes) != NIL) ? \
                ((fnodes = cdr(_nnode)), \
                 nfree--, \
                 (_nnode->n_type = (char)(type)), \
                 rplacd(_nnode,NIL), \
                 _nnode) \
                : Newnode(type))

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
    vnodes = VSSIZE;
    fnodes = NIL;

    /* initialize vector space */
    vsegments = vscurrent = NULL;
    vscount = 0;
    vfree = vtop = NULL;

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
     obarray = NULL;                 /* will be set to LVAL later */
    xlenv = xlfenv = xldenv = NIL;  /* list heads, initially NIL */
    s_gcflag = s_gchook = NULL;     /* will be set to lval later */

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
    {
        LVAL FAR *vdata;
        p = xlmakesym("NIL");
        MEMCPY(NIL, p, sizeof(struct node));    /* we point to this! */
        defconstant(NIL, NIL);
        p->n_type = FREE;                       /* don't collect "garbage" */
        vdata = p->n_vdata;                     /* correct ptr for compress */
        *--vdata = NIL;
}

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
        xlpopn(2);
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
  char FAR *str;
{
    LVAL val;
    val = newstring(STRLEN(str));
    STRCPY(getstring(val),str);
    return (val);
}

/* newstring - allocate and initialize a new string */
LVAL newstring(size)
  unsigned size;
{
     LVAL val;
     val = allocvector(STRING,btow_size(size+1));
     val->n_strlen = size;
     return (val);
}

#ifdef BIGNUMS
/* newbignum - allocate a new bignum */
LVAL newbignum(size)
  unsigned size;
{
    /* size of the sign field not included in n_bsize */
    LVAL val;
    val = allocvector(BIGNUM,stow_size(size+1));
    val->n_bsize = size;
    return val;
}
#endif

/* cvsymbol - convert a string to a symbol */
LVAL cvsymbol(pname)
  char FAR *pname;
{
     LVAL val;
     xlsave1(val);
     val = allocvector(SYMBOL,SYMSIZE);
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
  int iomode;
{
    LVAL val;
    val = newnode(STREAM);
    setfile(val,fp);
    setsavech(val,'\0');
     val->n_sflags = (char) iomode;
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
    return (NIL);   /* never really returns */
}
#else
LVAL cvchar(n)
  int n;
{
#if (CHARMIN == 0)  /* TAA  MOD eliminating a comparison */
    if (((unsigned)n) <= CHARMAX)
#else
    if (n >= CHARMIN && n <= CHARMAX)
#endif
        return (&charseg->sg_nodes[n-CHARMIN]);
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return (NIL);   /* never really returns */
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
    val = allocvector(OBJECT,size+1);
    setelement(val,0,cls);
    return (val);
}

/* newclosure - allocate and initialize a new closure */
LVAL newclosure(name,type,env,fenv)
  LVAL name,type,env,fenv;
{
    LVAL val;
    val = allocvector(CLOSURE,CLOSIZE);
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
    val = allocvector(STRUCT,size+1);
    setelement(val,0,type);
    return (val);
}


#ifdef PACKAGES
/* newpackage - allocate and initialize a new package */
LVAL newpackage()
{
    LVAL val;
    xlsave1(val);
    val = allocvector(PACKAGE,PACKSIZE);
    setintsyms(val, newvector(HSIZE));
    setextsyms(val, newvector(HSIZE));
    xlpop();
    return (val);
}
#endif /* PACKAGES */

/* newvector - allocate and initialize a new vector */
LVAL newvector(size)
  unsigned size;
{
    return (allocvector(VECTOR,size));
}


/* getvused - get used vector space */
/* also sets vsfree to free space */
LOCAL long NEAR getvused()
{
    long vnu=0L;
    VSEGMENT FAR *vseg;
    
    vsfree = 0L;
    
    if (vscurrent != NULL)
        vscurrent->vs_free = vfree;
    for (vseg = vsegments; vseg != NULL; vseg = vseg->vs_next) {
        vnu += ((long)vseg->vs_free - (long)&vseg->vs_data[0])/sizeof(LVAL FAR *);
        vsfree += ((long)vseg->vs_top - (long)vseg->vs_free)/sizeof(LVAL FAR *);
    }
    return vnu;
}


/* allocvector - allocate and initialize a new vector node */
LOCAL LVAL NEAR allocvector(type,size)
  int type;
  unsigned size;
{
    LVAL val, FAR *p;
    unsigned int i;

    if (size+1 > MAXVLEN) xlfail("array too large");

    xlsave1(val);
     val = newnode(type);

    /* initialize the vector node */
     val->n_type = (char) type;
    val->n_vsize = size;
    val->n_vdata = NULL;

    /* add space for the backpointer */
    ++size;
    
    /* make sure there's enough space */
    if (((unsigned long)vtop-(unsigned long)vfree < size*sizeof(LVAL FAR *)) && 
        !scanvmemory(size)) {
        vgccalls++; /* gc forced by vector allocation */
        gc();   /* try cleaning up and scanning again */
        getvused(); /* calculate free and used space */
        if (!scanvmemory(size) || vsfree < vnodes*vecthresh) 
            newvsegment(size);  /* no memory -- allocate segment */
        if ((unsigned long)vtop-(unsigned long)vfree < size*sizeof(LVAL FAR *))
            xlabort("insufficient vector space");
    }

    /* allocate the next available block */
    p = vfree;
    vfree += size;
    
    /* store the backpointer */
    *p++ = val;
    val->n_vdata = p;

    /* set all the elements to NIL, except for STRINGs or BIGNUMs */
    if (type != STRING
#ifdef BIGNUMS
        && type != BIGNUM
#endif
        ) for (i = size; i > 1; --i) *p++ = NIL;
#ifdef BIGNUMS
    if (type == BIGNUM)
        for (i = size; i > 1; --i) *p++ = 0;
#endif

    /* return the new vector */
    xlpop();
    return (val);
}

/* scanvmemory - look for vector segment with enough space */
/* return success */
int scanvmemory(size)
  unsigned int size;
{
    VSEGMENT FAR *vseg;
    if (vscurrent != NULL)
        vscurrent->vs_free = vfree;
    for (vseg = vsegments; vseg != NULL; vseg = vseg->vs_next)
        if ((unsigned long)vseg->vs_top - (unsigned long)vseg->vs_free > 
            size*sizeof(LVAL FAR *)) {
            vfree = vseg->vs_free;
            vtop = vseg->vs_top;
            vscurrent = vseg;
            return TRUE;
        }
    return FALSE;
}

/* newvsegment - create a new vector segment */
VSEGMENT FAR *newvsegment(n)
  unsigned int n;
{
    VSEGMENT FAR *newseg;
    long reqsize;

    if (n < (unsigned)vnodes) n = vnodes; /* allocate vnodes if larger than request */


    reqsize = vsegsize((long)n);

    if ((unsigned int)reqsize != (unsigned long)reqsize) 
        return (NULL);  /* can't do it */

    /* allocate the new segment */
    if ((newseg = (VSEGMENT FAR *)MALLOC((unsigned int)reqsize)) == NULL)
        return (NULL);

    if (vscurrent != NULL)
        vscurrent->vs_free = vfree;

    /* initialize the new segment */
    vfree = newseg->vs_free = &newseg->vs_data[0];
    vtop = newseg->vs_top = newseg->vs_free + n;
    newseg->vs_next = vsegments;
    vscurrent = vsegments = newseg;

    /* update the statistics */
    total += reqsize;
    ++vscount;

    /* return the new segment */
    return (newseg);
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
    nnode->n_type = (char) type;
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
    nnode->n_type = type;
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
}
#endif

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
    LVAL **p,*ap,tmp;
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
    if (s_gcflag!=NULL && getvalue(s_gcflag) != NIL) {
        /* print message on a fresh line */
        xlfreshline(getvalue(s_debugio));
        sprintf(buf,"[ gc: total %ld, ",nnodes);
        dbgputstr(buf); /* TAA MOD -- was std output */
    }

#ifdef WINDOWS
    startGC();
#endif

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
    mark(s_unbound);    /* TAA MOD 1/93 */

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

    /* compact vector space */
    compact();

        /* sweep memory collecting all unmarked nodes */
    sweep();

    NIL->n_type &= ~MARK;

#ifdef WINDOWS
    endGC();
#endif

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
#ifdef WINDOWS
    oscheck();  /* it's been *so* long since the last check */
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
            /* check cons style nodes */
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
            else {
                if (((i & ARRAY) != 0) && (this->n_vdata != NULL))
                    for (i = 0, n = getsize(this); i < n;)
                        if ((tmp = getelement(this,i++)) != NIL) {
                            if (!(tmp->n_type & MARK) && tmp->n_type >= CONS)
                                mark(tmp);
                            else tmp->n_type |= MARK;
			}
                break;
            }
        }

        /* backup to a point where we can continue descending */
        for (;;)    

            /* make sure there is a previous node */
            if (prev!=NIL) {
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

/* compact - compact vector space */
LOCAL VOID NEAR compact()
{
    VSEGMENT FAR *vseg, FAR *vsold;

    /* store the current segment information */
    if (vscurrent != NULL)
        vscurrent->vs_free = vfree;

    /* compact each vector segment */
    for (vseg = vsegments; vseg != NULL; vseg = vseg->vs_next) {
         compact_vector(vseg);
    }

    /* make the first vector segment current */
    if ((vscurrent = vsegments) != NULL) {
        vfree = vscurrent->vs_free;
        vtop = vscurrent->vs_top;
    }


    getvused(); /* calculate free and used space */

    /*  and free any unused segments if lots of free space (TAA MOD) */
    /*  but don't cut back too far */
    if (vsfree > (vecthresh+1)*(long)vnodes) {
        for (vseg = vsegments, vsold = (VSEGMENT FAR *)&vsegments;
            vseg != NULL && vsfree > (vecthresh+1)*(long)vnodes;
            vsold = vseg, vseg = vseg->vs_next) 
            if (vseg->vs_free == &vseg->vs_data[0]) {   /* empty segment */
                vsold->vs_next = vseg->vs_next;     /* unlink segment */
                vscount--;                          /* adjust tallies */
                total -= sizeof(VSEGMENT)-sizeof(LVAL FAR *)+
                    (unsigned long)vseg->vs_top - (unsigned long)vseg->vs_free;
                vsfree -= (unsigned long)vseg->vs_top - (unsigned long)vseg->vs_free;
                MFREE(vseg);                        /* free segment */
                vseg = vsold;                       /* last becomes current */
            }

        /* make the first vector segment current */
        if ((vscurrent = vsegments) != NULL) {
            vfree = vscurrent->vs_free;
            vtop = vscurrent->vs_top;
        }
    }
}

/* compact_vector - compact a vector segment */
LOCAL VOID NEAR compact_vector(vseg)
  VSEGMENT FAR *vseg;
{
    LVAL FAR *vdata, FAR *vnext, FAR *vfree,vector;
    unsigned vsize;

    vdata = vnext = &vseg->vs_data[0];
    vfree = vseg->vs_free;
    while (vdata < vfree) {
        vector = *vdata;
        if ((vector->n_type & TYPEFIELD) == STRING)
            vsize = btow_size(vector->n_strlen+1) + 1;
#ifdef BIGNUMS
        else if ((vector->n_type & TYPEFIELD) == BIGNUM)
            vsize = stow_size(vector->n_bsize+1) + 1;
#endif
        else
            vsize = vector->n_vsize + 1;
        if (vector->n_type & MARK) {
            if (vdata != vnext) {
                vector->n_vdata = vnext + 1;
                memmove(vnext, vdata, vsize * (unsigned)sizeof(LVAL));
            }
            vnext += vsize;
        }
        vdata += vsize;
    }
    vseg->vs_free = vnext;
}

/* sweep - sweep all unmarked nodes and add them to the free list */
LOCAL VOID NEAR sweep()
{
    SEGMENT FAR *seg;
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
                if (((ntype(p)&TYPEFIELD) == STREAM) 
                    && getfile(p) != CLOSED
                    && getfile(p) != STDIN
                    && getfile(p) != STDOUT
                    && getfile(p) != CONSOLE)/* taa fix - dont close stdio */
                    OSCLOSE(getfile(p));
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
    SEGMENT FAR *newseg;
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
SEGMENT FAR *newsegment(n)
  int n;
{
    SEGMENT FAR *newseg;

    /* allocate the new segment */
    if ((newseg = (SEGMENT FAR *)CALLOC(1,segsize(n))) == NULL)
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
LOCAL VOID NEAR stats ()
{
    long used;
    sprintf(buf,"Nodes:       %7ld  Free nodes:  %7ld  Segments:  %4d\n",
            nnodes, nfree, nsegs);  stdputstr(buf);
    used = getvused();
    sprintf(buf,"Vector nodes:%7ld  Vector free: %7ld  Vector segs:%3d\n",
            used+vsfree, vsfree, vscount); stdputstr(buf);
    sprintf(buf,"Allocate:    %7d  Vec Allocate:%7d  Total:  %7ld\n",
            anodes, vnodes, total);  stdputstr(buf);
#ifdef TIMES
    sprintf(buf,"Collections: %7ld  Vec collect: %7ld  Time (sec):%4ld\n",
            gccalls, vgccalls, gctime/ticks_per_second()); stdputstr(buf);
#else
    sprintf(buf,"Collections:  %ld  Vec collect: %7ld\n",
            gccalls, vgccalls); stdputstr(buf);
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
    FIXTYPE n,vn,vs;   /* TAA MOD -- prevent overflow */
    int oldn;

    /* get the new number to allocate */
    n = getfixnum(xlgafixnum());

    if (moreargs()) {   /* vector allocation */
        vn = getfixnum(xlgafixnum());
        if (moreargs()) { /* expansion sizing */
            vs = getfixnum(xlgafixnum());
            xllastarg();
            /* clip to reasonable values */
            if (vs < 1) vs = 1;
            else if (vs > 100) vs = 100;
            vecthresh = vs;
        }
        /* clip to reasonable values*/
        if (vn > MAXVLEN -(FIXTYPE)(sizeof(VSEGMENT)/sizeof(LVAL)))
            vn = MAXVLEN - (FIXTYPE)(sizeof(VSEGMENT)/sizeof(LVAL));
        else if (vn < 1000) vn = 1000;
        vnodes = (int)vn;
    }


    /* Place limits on argument by clipping to reasonable values  TAA MOD */
    if (n > (FIXTYPE)(((long)MAXSLEN - sizeof(SEGMENT))/sizeof(struct node)))
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
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;
#endif

    /* get the file name */
#ifdef MEDMEM
    _fstrncpy(name, getstring(xlgetfname()), STRMAX);
    name[STRMAX-1] = '\0';
#else
    name = getstring(xlgetfname());
#endif
    xllastarg();

    /* save the memory image */
    return (xlisave(name) ? s_true : NIL);
}

/* xrestore - restore a saved memory image */
LVAL xrestore()
{
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;
#endif

    /* get the file name */
#ifdef MEDMEM
    _fstrncpy(name, getstring(xlgetfname()), STRMAX);
    name[STRMAX-1] = '\0';
#else
    name = getstring(xlgetfname());
#endif
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
