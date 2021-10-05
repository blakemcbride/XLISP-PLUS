/* xlimage - xlisp memory image save/restore functions */
/*  Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use   */
/* modified so that offset is in sizeof(node) units */
#include "xlisp.h"

#ifdef SAVERESTORE

#define FILENIL ((OFFTYPE)0)    /* value of NIL in a file */

/* external variables  from xldmem.c */
extern struct segment *segs,*lastseg,*fixseg,*charseg;
extern int anodes,nsegs;
extern long nnodes;
extern LVAL fnodes;

/* local variables */
static OFFTYPE off,foff;
static FILEP fp;

/* forward declarations */
LOCAL OFFTYPE NEAR readptr _((void));
LOCAL OFFTYPE NEAR cvoptr _((LVAL p));
LOCAL LVAL NEAR cviptr _((OFFTYPE o));
LOCAL VOID NEAR freeimage _((void));
LOCAL VOID NEAR setoffset _((void));
LOCAL VOID NEAR writenode _((LVAL node));
LOCAL VOID NEAR writeptr _((OFFTYPE off));
LOCAL VOID NEAR readnode _((int type, LVAL node));

/* xlisave - save the memory image */
int xlisave(fname)
  char *fname;
{
    char fullname[STRMAX+1];
    SEGMENT *seg;
    int n,i,max;
    LVAL p;

    /* default the extension */
    if (needsextension(fname)) {
        strcpy(fullname,fname);
        strcat(fullname,".wks");
        fname = fullname;
    }

    /* open the output file */

    if ((fp = OSBOPEN(fname,CREATE_WR)) == CLOSED)
        return (FALSE);

    /* first call the garbage collector to clean up memory */
    gc();

    /* write out size of ftab (used as validity check) TAA MOD */
    writeptr((OFFTYPE)ftabsize);

    /* write out the pointer to the unbound marker TAA MOD 1/93 */
    writeptr(cvoptr(s_unbound));

    /* write out the pointer to the *obarray* symbol */
    writeptr(cvoptr(obarray));

    /* write out components of NIL other than value, which must be NIL */
    writeptr(cvoptr(getfunction(NIL)));
    writeptr(cvoptr(getplist(NIL)));
    writeptr(cvoptr(getpname(NIL)));
#ifdef PACKAGES
    writeptr(cvoptr(getpackage(NIL)));
#endif

    /* setup the initial file offsets */
    off = foff = (OFFTYPE)2;

    /* write out all nodes that are still in use */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        p = &seg->sg_nodes[0];
        for (n = seg->sg_size; --n >= 0; ++p, off++)
            switch (ntype(p)) {
            case FREE:
                break;
#ifdef BIGNUMS
            case RATIO:
#endif
#ifdef COMPLX
            case COMPLEX:
#endif
            case CONS:
            case USTREAM:
                setoffset();
                OSPUTC(p->n_type,fp);
                writeptr(cvoptr(car(p)));
                writeptr(cvoptr(cdr(p)));
                foff++;
                break;
            default:
                setoffset();
                writenode(p);
                break;
        }
    }

    /* write the terminator */
    OSPUTC(FREE,fp);
    writeptr((OFFTYPE)0);

    /* write out data portion of SYMBOL/VECTOR/OBJECT/STRING/CLOSURE nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        p = &seg->sg_nodes[0];
        for (n = seg->sg_size; --n >= 0; ++p)
            switch (ntype(p)) {
            /* $putpatch.c$: "MODULE_XLIMAGE_C_XLISAVE" */
            case SYMBOL:
            case OBJECT:
            case VECTOR:
            case CLOSURE:
            case STRUCT:
#ifdef PACKAGES
            case PACKAGE:
#endif
                max = getsize(p);
                for (i = 0; i < max; ++i)
                    writeptr(cvoptr(getelement(p,i)));
                break;
#ifdef BIGNUMS
            case BIGNUM:
                max = (getbignumsize(p)+1)*sizeof(BIGNUMDATA);
                OSWRITE(getbignumarray(p),1,max,fp);
                break;
#endif
            case STRING:
                max = getslength(p)+1;
                OSWRITE(getstring(p),1,max,fp);
                break;
#ifdef FILETABLE
            case STREAM:
                if (getfile(p) > CONSOLE ) {
                    OSWRITE(filetab[getfile(p)].tname,1,FNAMEMAX,fp);
                    *(long *)buf = OSTELL(getfile(p));
                    OSWRITE(buf,1,sizeof(long),fp);
                }
                break;
#endif
        }
    }

    /* close the output file */
    OSCLOSE(fp);

    /* return successfully */
    return (TRUE);
}

/* xlirestore - restore a saved memory image */
int xlirestore(fname)
  char *fname;
{
    char fullname[STRMAX+1];
    int n,i,max,type;
    SEGMENT *seg;
    LVAL p;

    /* default the extension */
    if (needsextension(fname)) {
        strncpy(fullname,fname,STRMAX-4);
        strcat(fullname,".wks");
        fname = fullname;
    }

    /* open the file */
#ifdef PATHNAMES
    if ((fp = ospopen(fname,FALSE)) == CLOSED)
#else
    if ((fp = OSBOPEN(fname,OPEN_RO)) == CLOSED)
#endif
        return (FALSE);

    /* Check for file validity  TAA MOD */
    if (readptr() != (OFFTYPE) ftabsize) {
        OSCLOSE(fp);    /* close it -- we failed */
        return (FALSE);
    }

    /* free the old memory image */
    freeimage();

    /* initialize */
    off = (OFFTYPE)2;
    total = nnodes = nfree = 0L;
    fnodes = NIL;
    segs = lastseg = NULL;
    nsegs = gccalls = 0;
#ifdef BIGNUMS
    n_bigzero=n_bigmone=NULL;   /* TAA fix 3/13/96 -- added */
#endif
    xlenv = xlfenv = xldenv = s_gchook = s_gcflag = NIL;
    xlstack = xlstkbase + EDEPTH;
    xlfp = xlsp = xlargstkbase;
    *xlsp++ = NIL;
    xlcontext = NULL;
#ifdef MULVALS  /* TAA BUG FIX 01/94 */
    xlnumresults = 0;
#endif

    /* create the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
        xlfatal("insufficient memory - fixnum segment");

    /* create the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
        xlfatal("insufficient memory - character segment");

    /* read in the pointer to the unbound marker TAA MOD 1/93 */
    s_unbound = cviptr(readptr());

    /* read the pointer to the *obarray* symbol */
    obarray = cviptr(readptr());

    /* read components of NIL other than value, which must be NIL */
    setfunction(NIL, cviptr(readptr()));
    setplist(NIL, cviptr(readptr()));
    setpname(NIL, cviptr(readptr()));
#ifdef PACKAGES
    setpackage(NIL, cviptr(readptr()));
#endif

    /* read each node */
    while ((type = OSGETC(fp)) >= 0) {
        switch (type) {
        case FREE:
            if ((off = readptr()) == (OFFTYPE)0)
                goto done;
            break;
#ifdef BIGNUMS
        case RATIO:
#endif
#ifdef COMPLX
        case COMPLEX:
#endif
        case CONS:
        case USTREAM:
            p = cviptr(off);
                p->n_type = (char)type;
            rplaca(p,cviptr(readptr()));
            rplacd(p,cviptr(readptr()));
            off++;
            break;
        default:
            readnode(type,cviptr(off));
            off++;
            break;
        }
    }
done:


    /* read the data portion of SYMBOL/VECTOR/OBJECT/STRING/CLOSURE nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
    p = &seg->sg_nodes[0];
    for (n = seg->sg_size; --n >= 0; ++p)
        switch (ntype(p)) {
        /* $putpatch.c$: "MODULE_XLIMAGE_C_XLIRESTORE" */
        case SYMBOL:
        case OBJECT:
        case VECTOR:
        case CLOSURE:
        case STRUCT:
#ifdef PACKAGES
        case PACKAGE:
#endif
            max = getsize(p);
            if ((p->n_vdata = (LVAL *)MALLOC(max * sizeof(LVAL))) == NULL)
                xlfatal("insufficient memory - vector");
            total += (long)(max * sizeof(LVAL));
            for (i = 0; i < max; ++i)
                setelement(p,i,cviptr(readptr()));
            break;
        case STRING:
            max = getslength(p)+1;
            if ((p->n_string = (char *)MALLOC(max)) == NULL)
                xlfatal("insufficient memory - string");
            total += (long)max;
            if (OSREAD(getstring(p),1,max,fp) != max)
                xlfatal("image file corrupted");
            break;
#ifdef BIGNUMS
        case BIGNUM:
            max =  (getbignumsize(p)+1)*sizeof(BIGNUMDATA);
            if ((p->n_vdata = (LVAL *)MALLOC(max)) == NULL)
                xlfatal("insufficient memory - bignum");
            total += (long)max;
            if (OSREAD(getbignumarray(p),1,max,fp)!=max)
                xlfatal("image file corrupted");
            break;
#endif
        case STREAM:
#ifdef FILETABLE
            if (getfile(p) > CONSOLE) { /* actual file to modify */
                unsigned long fpos;
                FILEP f;

                if (OSREAD(buf, 1, FNAMEMAX, fp) != FNAMEMAX ||
                    OSREAD(&fpos, 1, sizeof(long), fp) != sizeof(long))
                        xlfatal("image file corrupted");
                /* open file in same type, file must exist to succeed */
                f = ((p->n_sflags & S_BINARY)? OSBOPEN : OSAOPEN)
                    (buf, (p->n_sflags&S_FORWRITING)? OPEN_UPDATE: OPEN_RO);
                setfile(p, f);
                if (f != CLOSED) {  /* position to same point,
                                        or end if file too short */
                    OSSEEKEND(f);
                    if (OSTELL(f) > fpos) OSSEEK(f, fpos);
                }
            }
            break;
#else
            setfile(p,NULL);
            break;
#endif
        case SUBR:
        case FSUBR:
            p->n_subr = funtab[getoffset(p)].fd_subr;
            break;
        }
    }


    /* close the input file */
    OSCLOSE(fp);

    /* collect to initialize the free space */
    gc();

    /* lookup all of the symbols the interpreter uses */
    xlsymbols();

    /* return successfully */
    return (TRUE);
}

/* freeimage - free the current memory image */
LOCAL VOID NEAR freeimage()
{
    SEGMENT *seg,*next;
    FILEP fp;
    LVAL p;
    int n;

    /* free the data portion of SYMBOL/VECTOR/OBJECT/STRING nodes */
    for (seg = segs; seg != NULL; seg = next) {
    p = &seg->sg_nodes[0];
    for (n = seg->sg_size; --n >= 0; ++p)
        switch (ntype(p)) {
        /* $putpatch.c$: "MODULE_XLIMAGE_C_FREEIMAGE" */
        case SYMBOL:
        case OBJECT:
        case VECTOR:
        case CLOSURE:
#ifdef BIGNUMS
        case BIGNUM:
#endif
        case STRUCT:
            if (p->n_vsize)
                MFREE(p->n_vdata);
            break;
        case STRING:
            if (getstring(p)!=NULL)
                MFREE(getstring(p));
            break;
        case STREAM:
            if (((fp = getfile(p)) != CLOSED) &&
                (fp != STDIN && fp != STDOUT && fp != CONSOLE))  /* TAA BUG FIX */
            OSCLOSE(fp);
            break;
        }
    next = seg->sg_next;
    MFREE(seg);
    }
}

/* setoffset - output a positioning command if nodes have been skipped */
LOCAL VOID NEAR setoffset()
{
    if (off != foff) {
        OSPUTC(FREE,fp);
        writeptr(off);
        foff = off;
    }
}

/* writenode - write a node to a file */
LOCAL VOID NEAR writenode(node)
  LVAL node;
{
    OSPUTC(node->n_type,fp);
    OSWRITE(&node->n_info, sizeof(union ninfo), 1, fp);
#ifdef ALIGN32
    if (node->n_type == SYMBOL) OSPUTC(node->n_spflags,fp);
#endif
    foff++;
}

/* writeptr - write a pointer to a file */
LOCAL VOID NEAR writeptr(off)
  OFFTYPE off;
{
    OSWRITE(&off, sizeof(OFFTYPE), 1, fp);
}

/* readnode - read a node */
LOCAL VOID NEAR readnode(type,node)
  int type; LVAL node;
{
     node->n_type = (char) type;
    if (OSREAD(&node->n_info, sizeof(union ninfo), 1, fp) != 1)
        xlfatal("image file corrupted");
#ifdef ALIGN32
    if (type == SYMBOL) node->n_spflags = (char) OSGETC(fp);
#endif
}

/* readptr - read a pointer */
LOCAL OFFTYPE NEAR readptr()
{
    OFFTYPE off;
    if(OSREAD(&off, sizeof(OFFTYPE), 1, fp) != 1)
        xlfatal("image file corrupted");
    return (off);
}

/* cviptr - convert a pointer on input */
LOCAL LVAL NEAR cviptr(o)
  OFFTYPE o;
{
    OFFTYPE off = (OFFTYPE)2;
    SEGMENT *seg;

    /* check for nil */
    if (o == FILENIL)
        return (NIL);

    /* compute a pointer for this offset */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        if (o < off + (OFFTYPE)seg->sg_size)
            return (seg->sg_nodes + (unsigned int)(o - off));
        off += (OFFTYPE)seg->sg_size;
    }

    /* create new segments if necessary */
    for (;;) {
    /* create the next segment */
        if ((seg = newsegment(anodes)) == NULL)
            xlfatal("insufficient memory - segment");


    /* check to see if the offset is in this segment */
        if (o < off + (OFFTYPE)seg->sg_size)
            return (seg->sg_nodes + (unsigned int)(o - off));
        off += (OFFTYPE)seg->sg_size;
    }
}

/* cvoptr - convert a pointer on output */
LOCAL OFFTYPE NEAR cvoptr(p)
  LVAL p;
{
    OFFTYPE off = (OFFTYPE)2;
    SEGMENT *seg;
    OFFTYPE np = CVPTR(p);

    /* check for nil */
    if (null(p))
        return (FILENIL);

    /* compute an offset for this pointer */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
        if (np >= CVPTR(&seg->sg_nodes[0]) &&
            np <  CVPTR(&seg->sg_nodes[seg->sg_size]))
                return (off+ ((np-CVPTR(seg->sg_nodes))/sizeof(struct node)));
            off += (OFFTYPE)seg->sg_size;
    }

    /* pointer not within any segment */
    xlerror("bad pointer found during image save",p);
    return (0); /* fake out compiler warning */
}
#endif


