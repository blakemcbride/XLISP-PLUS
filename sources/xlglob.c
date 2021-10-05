/* xlglobals - xlisp global variables */
/*  Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use   */

#include "xlisp.h"

/* symbols */
struct node isnil;

LVAL s_true=NULL,obarray=NULL;
LVAL s_features=NULL;
LVAL s_unbound=NIL,s_dot=NULL;
LVAL s_quote=NULL,s_identity=NULL,s_function=NULL;
LVAL s_bquote=NULL,s_comma=NULL,s_comat=NULL;
LVAL s_evalhook=NULL,s_applyhook=NULL,s_tracelist=NULL;
LVAL s_lambda=NULL,s_macro=NULL;
LVAL s_stdin=NULL,s_stdout=NULL,s_stderr=NULL,s_debugio=NULL,s_traceout=NULL;
LVAL s_rtable=NULL;
LVAL a_readpw=NULL;
LVAL s_tracenable=NULL,s_tlimit=NULL,s_breakenable=NULL;
LVAL s_setf=NULL,s_setfl=NULL,s_car=NULL,s_cdr=NULL,s_nth=NULL,s_apply=NULL;
LVAL s_aref=NULL,s_get=NULL,s_getf=NULL;
LVAL s_svalue=NULL,s_sfunction=NULL,s_splist=NULL;
LVAL s_eql=NULL,s_gcflag=NULL,s_gchook=NULL;
#ifdef BIGNUMS
LVAL s_readbase=NULL, s_printbase=NULL;
#else
LVAL s_ifmt=NULL;
#endif
LVAL s_ffmt=NULL;
LVAL s_1plus=NULL,s_2plus=NULL,s_3plus=NULL;
LVAL s_1star=NULL,s_2star=NULL,s_3star=NULL;
LVAL s_minus=NULL,s_printcase=NULL;
LVAL s_printlevel=NULL, s_printlength=NULL;
LVAL s_dispmacros=NULL;
LVAL s_strtypep=NULL, s_mkstruct=NULL, s_cpystruct=NULL;
LVAL s_prntfunc=NULL;
LVAL s_strref=NULL, s_strset=NULL;
LVAL s_x=NULL, s_s=NULL, s_sslots=NULL;
LVAL s_elt = NULL;
LVAL a_list=NULL, a_number=NULL, a_null=NULL, a_atom=NULL, a_anystream=NULL;
LVAL s_and=NULL, s_or=NULL, s_not=NULL, s_satisfies=NULL, s_member=NULL;
LVAL a_struct = NULL;
LVAL s_read_suppress=NULL;
#ifdef DOSINPUT
LVAL s_dosinput=NULL;
#endif
#ifdef HASHFCNS
LVAL s_gethash = NULL, a_hashtable = NULL;
#endif
#ifdef COMPLX
LVAL a_complex = NULL;
#endif
#ifdef RANDOM
LVAL s_randomstate=NULL, a_randomstate=NULL;
#endif
#ifdef READTABLECASE
LVAL s_rtcase=NULL;
#endif
#ifdef PACKAGES
LVAL xlisppack=NULL,xlkeypack=NULL,xluserpack=NULL,s_package=NULL;
#endif /* PACKAGES */


/* keywords */
LVAL k_test=NULL,k_tnot=NULL;
LVAL k_wspace=NULL,k_const=NULL,k_nmacro=NULL,k_tmacro=NULL;
LVAL k_sescape=NULL,k_mescape=NULL;
LVAL k_direction=NULL,k_input=NULL,k_output=NULL;
LVAL k_start=NULL,k_end=NULL,k_1start=NULL,k_1end=NULL;
LVAL k_2start=NULL,k_2end=NULL,k_count=NULL;
LVAL k_verbose=NULL,k_print=NULL;
LVAL k_upcase=NULL,k_downcase=NULL,k_capitalize=NULL;
LVAL k_io=NULL, k_elementtype=NULL;
LVAL s_termio=NULL, k_exist=NULL, k_nexist=NULL, k_error=NULL;
LVAL k_rename=NULL, k_newversion=NULL, k_overwrite=NULL, k_append=NULL;
LVAL k_supersede=NULL, k_rendel=NULL, k_probe=NULL, k_create=NULL;
LVAL k_concname=NULL, k_include=NULL, k_prntfunc=NULL;
LVAL k_initelem=NULL, k_initcont=NULL;
#ifdef AOKKEY
LVAL k_allow_other_keys = NULL;  /* TAA added 9/93 */
#endif
#ifdef REDUCE
LVAL k_ivalue=NULL;
#endif
#ifdef KEYARG
LVAL k_key=NULL;
#endif
#ifdef HASHFCNS
LVAL k_size = NULL;
#endif
#ifdef RANDOM
LVAL k_data=NULL;
#endif
#ifdef READTABLECASE
LVAL k_preserve=NULL,k_invert=NULL;
#endif
#ifdef PACKAGES
LVAL k_nicknames=NULL, k_use=NULL;
#ifdef MULVALS
LVAL k_internal=NULL, k_external=NULL, k_inherited=NULL;
#endif /* MULVALS */
#endif /* PACKAGES */
#ifdef FROMEND
LVAL k_fromend=NULL;
#endif


/* lambda list keywords */
LVAL lk_optional=NULL,lk_rest=NULL,lk_key=NULL,lk_aux=NULL;
LVAL lk_allow_other_keys=NULL;

/* type names */
LVAL a_subr=NULL,a_fsubr=NULL;
LVAL a_cons=NULL,a_symbol=NULL,a_fixnum=NULL,a_flonum=NULL;
LVAL a_string=NULL,a_object=NULL,a_stream=NULL,a_vector=NULL;
LVAL a_closure=NULL,a_char=NULL,a_ustream=NULL;
LVAL a_integer=NULL, a_real=NULL;
#ifdef BIGNUMS
LVAL a_ratio=NULL;
LVAL a_rational=NULL;
LVAL a_bignum=NULL;
LVAL a_unbyte=NULL, a_sbyte=NULL;
LVAL n_bigzero=NULL, n_bigmone=NULL;
#endif
#ifdef PACKAGES
LVAL a_package=NULL;
#endif /* PACKAGES */

/* Object system */
LVAL s_self=NULL,k_new=NULL,k_isnew=NULL;
LVAL k_prin1=NULL;
LVAL cls_class=NULL,cls_object=NULL;

/* evaluation variables */
LVAL * NEAR xlstkbase[EDEPTH];
LVAL * NEAR *xlstack = NULL;
LVAL xlenv=NULL,xlfenv=NULL,xldenv=NULL;

/* argument stack */
LVAL NEAR xlargstkbase[ADEPTH]; /* argument stack */
LVAL NEAR *xlfp = NULL;         /* argument frame pointer */
LVAL NEAR *xlsp = NULL;         /* argument stack pointer */
LVAL NEAR *xlargv = NULL;       /* current argument vector */
int xlargc = 0;         /* current argument count */

/* exception handling variables */
CONTXT *xlcontext = NULL;  /* current exception handler */
CONTXT *xltarget = NULL;   /* target context (for xljump) */
LVAL xlvalue=NULL;      /* exception value (for xljump) */
int xlmask=0;           /* exception type (for xljump) */
#ifdef MULVALS
int xlnumresults = 0;           /* number of values */
LVAL xlresults[MULVALLIMIT] = {0};      /* multiple values array */
#endif /* MULVALS */

/* Garbage collection reporting */
long gccalls=0;
long nfree=0, total=0;

/* debugging variables */
int xldebug = 0;        /* debug level */
int xlsample = 0;       /* control character sample rate */
int xltrcindent = 0;        /* trace indent level */

/* gensym variables */
#ifdef BETTERGENSYM
LVAL s_gensymcounter = NULL;
#else
char gsprefix[STRMAX+1] = { 'G',0 };    /* gensym prefix string */
FIXTYPE gsnumber = 1;       /* gensym number */
#endif

/* i/o variables */
FILEP tfp = CLOSED;     /* transcript file pointer */
int redirectout = FALSE;    /* output is redirected */
int redirectin = FALSE;     /* input is redirected */
int batchmode = FALSE;      /* running a batch process */
int lposition = 0;          /* postition in screen */

/* From Luke Tierney, 9/93 */
/* startup functions and command line symbols */
LVAL s_startup_functions=NULL, s_command_line=NULL;
LVAL s_loadfileargs=NULL, s_toplevelloop=NULL;

/* general purpose string buffer */
char buf[STRMAX*2+1] = { 0 };

/* Number of remaining nodes */
long nnodes;

/* printing level and length */
int plevel, plength;

#ifdef STSZ
/* For stack checking */
int stackwarn=FALSE;    /* is TRUE when warning given */
int marghi=MARGLO;          /* stackleft for warning */
#endif

#ifdef FILETABLE
FILETABLETYPE filetab[FTABSIZE];
#endif

#ifdef ASCII8
#ifdef ANSI8
/* ANSI table different than IBM ASCII */
char ascii8tbl[256]= { 
/* 0x0 - 0x1f */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0,
/* 0x20 - 0x3f */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0,
/* 0x40 - 0x5f */
    0,   UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, 0,   0,   0,   0,   0,
/* 0x60 - 0x7f */
    0,   LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, 0,   0,   0,   0,   0,
/* 0x80 - 0xbf */
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
/* 0xc0 - 0xdf */
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8,
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8,
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, 0,
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, 0,
/* 0xe0 - 0xff */
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8,
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8,
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, 0,
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, 0};

unsigned char ascii8cnv[]= {
         'a', 'b', 'c', 'd', 'e', 'f', 'g',
    'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z', 0, 0, 0, 0, 0,
    0,   'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', 0, 0, 0, 0, 0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0,   0,   0,   0,   0,   0,   0,   0,
    0Xe0,0Xe1,0Xe2,0Xe3,0Xe4,0Xe5,0Xe6,0Xe7,
    0Xe8,0Xe9,0Xea,0Xeb,0Xec,0Xed,0Xee,0Xef,
    0Xf0,0Xf1,0Xf2,0Xf3,0Xf4,0Xf5,0Xf6,0,
    0Xf8,0Xf9,0Xfa,0Xfb,0Xfc,0Xfd,0Xfe,0,
    0Xc0,0Xc1,0Xc2,0Xc3,0Xc4,0Xc5,0Xc6,0Xc7,
    0Xc8,0Xc9,0Xca,0Xcb,0Xcc,0Xcd,0Xce,0Xcf,
    0Xd0,0Xd1,0Xd2,0Xd3,0Xd4,0Xd5,0Xd6,0,
    0Xd8,0Xd9,0Xda,0Xdb,0Xdc,0Xdd,0Xde,0};
    
#else
/* conversion tables for 8 bit ASCII */
char ascii8tbl[256]= { 
/* 0x0 - 0x1f */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0,
/* 0x20 - 0x3f */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0,
/* 0x40 - 0x5f */
    0,   UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, UC8, UC8, UC8, UC8, UC8, 
    UC8, UC8, UC8, 0,   0,   0,   0,   0,
/* 0x60 - 0x7f */
    0,   LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, LU8, LU8, LU8, LU8, LU8, 
    LU8, LU8, LU8, 0,   0,   0,   0,   0,
/* msb set */
    UC8, LU8, LU8, LC8, LU8, LC8, LU8, LU8,
    LC8, LC8, LC8, LC8, LC8, LC8, UC8, UC8,
    UC8, LU8, UC8, LU8, LC8, LC8, LC8, LC8,
    LC8, UC8, UC8, 0,   0,   0,   0,   0,
    LC8, LC8, LC8, LC8, LU8, UC8 };

unsigned char ascii8cnv[101]= {
         'a', 'b', 'c', 'd', 'e', 'f', 'g',
    'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z', 0, 0, 0, 0, 0,
    0,   'A', 'B', 'C', 'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', 0, 0, 0, 0, 0,
    0x87, 0x9a, 0x90, 0x83, 0x8e, 0x85, 0x8f, 0x80,
    0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x84, 0x86,
    0x82, 0x92, 0x91, 0x99, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x93, 0x81, 0,    0,    0,    0,    0,
    0xa0, 0xa1, 0xa2, 0xa3, 0xa5, 0xa4};
#endif
#endif


/* $putpatch.c$: "MODULE_XLGLOB_C_GLOBALS" */
