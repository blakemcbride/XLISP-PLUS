/* xldmem.h - dynamic memory definitions */
/*      Copyright (c) 1987, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

/* small fixnum range */
#ifndef SFIXMIN
#define SFIXMIN         (-128)
#endif
#ifndef SFIXMAX
#define SFIXMAX         255
#endif
#define SFIXSIZE        (SFIXMAX-SFIXMIN+1)

/* character range */
#define CHARMIN         0
#define CHARMAX         255
#define CHARSIZE        (CHARMAX-CHARMIN+1)

/* new node access macros */
#define ntype(x)        ((x)->n_type)

/* cons access macros */
#define car(x)          ((x)->n_car)
#define cdr(x)          ((x)->n_cdr)
#define rplaca(x,y)     ((x)->n_car = (y))
#define rplacd(x,y)     ((x)->n_cdr = (y))

/* symbol access macros */
#define getvalue(x)      ((x)->n_vdata[0])
#define setvalue(x,v)    ((x)->n_vdata[0] = (v))
#define getfunction(x)   ((x)->n_vdata[1])
#define setfunction(x,v) ((x)->n_vdata[1] = (v))
#define getplist(x)      ((x)->n_vdata[2])
#define setplist(x,v)    ((x)->n_vdata[2] = (v))
#define getpname(x)      ((x)->n_vdata[3])
#define setpname(x,v)    ((x)->n_vdata[3] = (v))
#ifdef PACKAGES
#define getpackage(x)    getelement(x,4)
#define setpackage(x,v)  setelement(x,4,v)
#define SYMSIZE          5
#else
#define SYMSIZE          4
#endif /* PACKAGES */

/* closure access macros */
#define getname(x)      ((x)->n_vdata[0])
#define setname(x,v)    ((x)->n_vdata[0] = (v))
#define gettype(x)      ((x)->n_vdata[1])
#define settype(x,v)    ((x)->n_vdata[1] = (v))
#define getargs(x)      ((x)->n_vdata[2])
#define setargs(x,v)    ((x)->n_vdata[2] = (v))
#define getoargs(x)     ((x)->n_vdata[3])
#define setoargs(x,v)   ((x)->n_vdata[3] = (v))
#define getrest(x)      ((x)->n_vdata[4])
#define setrest(x,v)    ((x)->n_vdata[4] = (v))
#define getkargs(x)     ((x)->n_vdata[5])
#define setkargs(x,v)   ((x)->n_vdata[5] = (v))
#define getaargs(x)     ((x)->n_vdata[6])
#define setaargs(x,v)   ((x)->n_vdata[6] = (v))
#define getbody(x)      ((x)->n_vdata[7])
#define setbody(x,v)    ((x)->n_vdata[7] = (v))
#define getenvi(x)      ((x)->n_vdata[8])
#define setenvi(x,v)    ((x)->n_vdata[8] = (v))
#define getfenv(x)      ((x)->n_vdata[9])
#define setfenv(x,v)    ((x)->n_vdata[9] = (v))
#define getlambda(x)    ((x)->n_vdata[10])
#define setlambda(x,v)  ((x)->n_vdata[10] = (v))
#define CLOSIZE         11

/* vector access macros */
#define getsize(x)      ((x)->n_vsize)
#define getelement(x,i) ((x)->n_vdata[i])
#define setelement(x,i,v) ((x)->n_vdata[i] = (v))

/* object access macros */
#define getclass(x)     ((x)->n_vdata[0])
#define getivar(x,i)    ((x)->n_vdata[i+1])
#define setivar(x,i,v)  ((x)->n_vdata[i+1] = (v))

/* instance variable numbers for the class 'Class' */
#define MESSAGES        0       /* list of messages */
#define IVARS           1       /* list of instance variable names */
#define CVARS           2       /* list of class variable names */
#define CVALS           3       /* list of class variable values */
#define SUPERCLASS      4       /* pointer to the superclass */
#define IVARCNT         5       /* number of class instance variables */
#define IVARTOTAL       6       /* total number of instance variables */
#define PNAME           7       /* print name TAA Mod */
/* number of instance variables for the class 'Class' */
#define CLASSSIZE       8


/* subr/fsubr access macros */
#define getsubr(x)      ((x)->n_subr)
#define getoffset(x)    ((x)->n_offset)
#ifdef MULVALS
#define mulvalp(x)       ((x)->n_mvflag)
#define setmulvalp(x, v) ((x)->n_mvflag = (v))
#endif

/* fixnum/flonum/char access macros */
#define getfixnum(x)    ((x)->n_fixnum)
#define getflonum(x)    ((x)->n_flonum)
#define getchcode(x)    ((x)->n_chcode)

#ifdef COMPLX
/* complex number access macros */
#define getreal(x)      ((x)->n_car)
#define getimag(x)      ((x)->n_cdr)
#endif


#ifdef BIGNUMS
/* rational number access macros */
#define getnumer(x)     ((x)->n_numer)
#define getdenom(x)     ((x)->n_denom)

/* bignum access macros */
typedef unsigned short BIGNUMDATA;
#define getbignumarray(x) ((x)->n_bdata)
#define getbignumsize(x) ((x)->n_bsize)
#define getbignumsign(x) ((x)->n_bdata[0])
#endif

/* string access macros */
#define getstring(x)    ((x)->n_string)
#define getslength(x)   ((x)->n_strlen)
/* the following functions were TAA modifications */
#define getstringch(x,i) (((unsigned char FAR *)((x)->n_string))[i])
#define setstringch(x,i,v) ((x)->n_string[i] = (char)(v))

/* file stream access macros */
#define getfile(x)      ((x)->n_fp)
#define setfile(x,v)    ((x)->n_fp = (v))
#define getsavech(x)    ((x)->n_savech)
#define setsavech(x,v)  ((x)->n_savech = (v))

/* unnamed stream access macros */
#define gethead(x)      ((x)->n_car)
#define sethead(x,v)    ((x)->n_car = (v))
#define gettail(x)      ((x)->n_cdr)
#define settail(x,v)    ((x)->n_cdr = (v))

#ifdef PACKAGES
/* package macros */
#define getintsyms(x)   getelement(x, 0)
#define getextsyms(x)   getelement(x, 1)
#define getshadowing(x) getelement(x, 2)
#define getuses(x)      getelement(x, 3)
#define getusedby(x)    getelement(x, 4)
#define getpacknames(x) getelement(x, 5)
#define setintsyms(x,v) setelement(x, 0, v)
#define setextsyms(x,v) setelement(x, 1, v)
#define setshadowing(x,v) setelement(x, 2, v)
#define setuses(x,v)    setelement(x, 3, v)
#define setusedby(x,v)  setelement(x, 4, v)
#define setpacknames(x,v) setelement(x, 5, v)
#define PACKSIZE 6
#endif /* PACKAGES */

/* node types */
#define FREE    0
#define SUBR    1
#define FSUBR   2
#define FIXNUM  4
#define FLONUM  5
#define STRING  6
#define STREAM  7
#define CHAR    8
#ifdef BIGNUMS
#define BIGNUM  9
#endif
/* Non-array types from CONS up use CAR and CDR fields */
/* This means that all types from CONS up have garbage collectable elements */
#define CONS    10
#ifdef COMPLX
#define COMPLEX 11
#endif
#ifdef BIGNUMS
#define RATIO   12
#endif
#define USTREAM 13
#define ARRAY   16      /* arrayed types */
#define SYMBOL  (ARRAY+1)
#define OBJECT  (ARRAY+2)
#define VECTOR  (ARRAY+3)
#define CLOSURE (ARRAY+4)
#define STRUCT  (ARRAY+5)
#ifdef PACKAGES
#define PACKAGE (ARRAY+7)
#endif
#define TYPEFIELD 0x1f
/* subr/fsubr node */
#define n_subr          n_info.n_xsubr.xs_subr
#define n_offset        n_info.n_xsubr.xs_offset
#define n_mvflag        n_info.n_xsubr.xs_mvflag

/* cons node */
#define n_car           n_info.n_xcons.xc_car
#define n_cdr           n_info.n_xcons.xc_cdr

/* fixnum node */
#define n_fixnum        n_info.n_xfixnum.xf_fixnum

/* flonum node */
#define n_flonum        n_info.n_xflonum.xf_flonum
/* character node */
#define n_chcode        n_info.n_xchar.xc_chcode

/* string node */
#define n_string        n_info.n_xstring.xs_string
#define n_strlen        n_info.n_xstring.xs_length

/* stream node */
#define n_fp            n_info.n_xstream.xs_fp
#define n_savech        n_info.n_xstream.xs_savech

#define S_READING       1   /* File is in reading mode */
#define S_WRITING       2   /* file is in writing mode */
#define S_FORREADING    4   /* File open for reading */
#define S_FORWRITING    8   /* file open for writing */
#define S_BINARY        16  /* file is binary file */
#define S_UNSIGNED      32  /* file is unsigned binary */

#define n_sflags        n_info.n_xstream.xs_flags
#define n_cpos          n_info.n_xstream.xs_cpos    /* position of char file*/
#define n_bsiz          n_info.n_xstream.xs_cpos    /* byte size of bin file*/

#ifdef BIGNUMS
/* rational number node */
#define n_numer         n_info.n_xratio.xf_numer
#define n_denom         n_info.n_xratio.xf_denom

/* bignum node */
#define n_bsize         n_info.n_xbignum.xb_length
#define n_bdata         n_info.n_xbignum.xb_data
#endif

/* vector/object node */
#define n_vsize         n_info.n_xvector.xv_size
#define n_vdata         n_info.n_xvector.xv_data
#ifndef ALIGN32
#define n_spflags       n_info.n_xvector.xv_flags
#endif

/* node structure */
typedef struct node {
/* 32 bit compilers that pack structures will do better with
   these chars at the end  */
#ifndef ALIGN32
    char n_type;                /* type of node */
#endif
    union ninfo {               /* value */
        struct xsubr {          /* subr/fsubr node */
            struct node FAR*(*xs_subr) _((void));   /* function pointer */
            short xs_offset;              /* offset into funtab */
#ifdef MULVALS
            char xs_mvflag;             /* multiple value return */
#endif
        } n_xsubr;
        struct xcons {          /* cons node */
            struct node FAR*xc_car;     /* the car pointer */
            struct node FAR*xc_cdr;     /* the cdr pointer */
        } n_xcons;
        struct xfixnum {        /* fixnum node */
            FIXTYPE xf_fixnum;          /* fixnum value */
        } n_xfixnum;
        struct xflonum {        /* flonum node */
            FLOTYPE xf_flonum;          /* flonum value */
        } n_xflonum;
        struct xchar {          /* character node */
            int xc_chcode;              /* character code */
        } n_xchar;
#ifdef BIGNUMS
        struct xratio {         /* rational number (ratio) node */
            struct node FAR *xf_numer;
            struct node FAR *xf_denom;
        } n_xratio;
        struct xbignum {        /* bignum node */
            unsigned xb_length; /* length of data in #BIGNUMDATAs */
            BIGNUMDATA FAR *xb_data; /* sign BIGNUMDATA followed by xb_length
                                        BIGNUMDATAs */
        } n_xbignum;
#endif
        struct xstring {        /* string node */
            unsigned xs_length;         /* string length */
            char FAR *xs_string;            /* string pointer */
        } n_xstring;
        struct xstream {        /* stream node */
            FILEP xs_fp;                /* the file pointer */
            unsigned char xs_savech;    /* lookahead character */
            char xs_flags;              /* read/write mode flags */
            short xs_cpos;              /* character position in line */
        } n_xstream;
        struct xvector {        /* vector/object/symbol/structure node */
            int xv_size;                /* vector size */
            struct node FAR * FAR *xv_data;     /* vector data */
#ifndef ALIGN32
            char xv_flags;      /* constant and special symbol flags */
#endif
        } n_xvector;
        /* $putpatch.c$: "MODULE_XLDMEM_H_NINFO" */
    } n_info;
#ifdef ALIGN32
    char n_type;                /* type of node */
    char n_spflags;
#endif
} FAR *LVAL;

/* memory segment structure definition */
typedef struct segment {
    int sg_size;
    struct segment FAR *sg_next;
    struct node sg_nodes[1];
} SEGMENT;

/* memory allocation functions */
extern VOID gc _((void));               /* do a garbage collect */
extern SEGMENT FAR *newsegment _((int n));  /* create a new segment */
extern LVAL cons _((LVAL x, LVAL y));   /*  (cons x y) */
extern LVAL cvsymbol _((char FAR *pname));  /* convert a string to a symbol */
extern LVAL cvstring _((char FAR *str));    /* convert a string */
extern LVAL cvfile _((FILEP fp, int flags));    /* convert a FILEP to a file */
extern LVAL cvsubr _((LVAL (*fcn) _((void)), int type, int offset));
                                /* convert a function to a subr/fsubr */
#ifdef JMAC
extern LVAL Cvfixnum _((FIXTYPE n));    /* convert a fixnum */
extern LVAL Cvchar _((int n));          /* convert a character */
#else
extern LVAL cvfixnum _((FIXTYPE n));    /* convert a fixnum */
extern LVAL cvchar _((int n));          /* convert a character */
#endif
extern LVAL cvflonum _((FLOTYPE n));    /* convert a flonum */

#ifdef BIGNUMS
extern LVAL cvratio _((FIXTYPE n, FIXTYPE d));  /* convert a ratio */
extern LVAL cvbratio _((LVAL n, LVAL d));  /* convert a ratio */
#endif

extern LVAL newstring _((unsigned size));   /* create a new string */
extern LVAL newvector _((unsigned size));   /* create a new vector */
extern LVAL newobject _((LVAL cls, int size));  /* create a new object */
extern LVAL newclosure _((LVAL name, LVAL type, LVAL env, LVAL fenv));
                                    /* create a new closure */
extern LVAL newustream _((void));       /* create a new unnamed stream */
extern LVAL newstruct _((LVAL type, int size)); /* create a new structure */
#ifdef COMPLX
extern LVAL newcomplex _((LVAL r, LVAL i));     /* create a new complex number */
extern LVAL newicomplex _((FIXTYPE r, FIXTYPE i));      
extern LVAL newdcomplex _((FLOTYPE r, FLOTYPE i));
#endif

#ifdef BIGNUMS
/* most functions are in xlbignum.c */
extern LVAL newbignum _((unsigned size));
extern LVAL cvtflonum _((LVAL num)); /* convert a rational to a float */
#endif

extern VOID defconstant _((LVAL sym, LVAL val));

#ifdef PACKAGES
extern LVAL newpackage _((void));       /* create a new package */
#endif /* PACKAGES */

#define F_SPECIAL   1
#define F_CONSTANT  2
#define F_NORMAL    0

#define setsvalue(s,v)  (setvalue(s,v), setsflags(s, F_SPECIAL))
#define setsflags(x,c)  ((x)->n_spflags = (c))
#define constantp(x)  ((x)->n_spflags & F_CONSTANT)
#define specialp(x) ((x)->n_spflags & F_SPECIAL)

#ifdef JMAC
/* Speed ups, reduce function calls for fixed characters and numbers       */
/* Speed is exeptionaly noticed on machines with a large instruction cache */
/* No size effects here (JonnyG) */

extern SEGMENT FAR *fixseg, FAR *charseg;
extern FIXTYPE _tfixed;
extern int _tint;
 
#define cvfixnum(n) ((_tfixed = (n)), \
                ((_tfixed > SFIXMIN && _tfixed < SFIXMAX) ? \
                &fixseg->sg_nodes[(int)_tfixed-SFIXMIN] : \
                Cvfixnum(_tfixed)))

#if (CHARMIN == 0)  /* eliminate a comparison */
#define cvchar(c) ((_tint = (c)), \
                (((unsigned)_tint) <= CHARMAX ? \
                        &charseg->sg_nodes[_tint-CHARMIN] : \
                Cvchar(_tint)))
#else
#define cvchar(c) ((_tint = (c)), \
                ((_tint >= CHARMIN && _tint <= CHARMAX) ? \
                        &charseg->sg_nodes[_tint-CHARMIN] : \
                Cvchar(_tint)))
#endif
#endif
/* $putpatch.c$: "MODULE_XLDMEM_H_GLOBALS" */
