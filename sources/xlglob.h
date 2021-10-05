/* xlglob.h -- external declarations for global variables */

/* symbols */
extern struct node isnil;

extern LVAL s_true,obarray;
extern LVAL s_features;
extern LVAL s_unbound,s_dot;
extern LVAL s_quote,s_identity,s_function;
extern LVAL s_bquote,s_comma,s_comat;
extern LVAL s_evalhook,s_applyhook,s_tracelist;
extern LVAL s_lambda,s_macro;
extern LVAL s_stdin,s_stdout,s_stderr,s_debugio,s_traceout;
extern LVAL s_rtable;
extern LVAL s_tracenable,s_tlimit,s_breakenable;
extern LVAL s_setf,s_setfl,s_car,s_cdr,s_apply,s_nth;
extern LVAL s_aref,s_get,s_getf;
extern LVAL s_svalue,s_sfunction,s_splist;
extern LVAL s_eql,s_gcflag,s_gchook;
#ifdef BIGNUMS
extern LVAL s_readbase, s_printbase;
#else
extern LVAL s_ifmt;
#endif
extern LVAL s_ffmt;
extern LVAL s_1plus,s_2plus,s_3plus;
extern LVAL s_1star,s_2star,s_3star;
extern LVAL s_minus,s_printcase;
extern LVAL s_printlevel, s_printlength;
extern LVAL s_dispmacros;
extern LVAL s_strtypep, s_mkstruct, s_cpystruct;
extern LVAL s_prntfunc;
extern LVAL s_strref, s_strset;
extern LVAL s_x, s_s, s_sslots;
extern LVAL s_elt;
extern LVAL a_list, a_number, a_null, a_atom, a_anystream;
extern LVAL s_and, s_or, s_not, s_satisfies, s_member;
extern LVAL a_struct;
extern LVAL s_read_suppress;
#ifdef DOSINPUT
extern LVAL s_dosinput;
#endif
#ifdef HASHFCNS
extern LVAL s_gethash, a_hashtable;
#endif
#ifdef COMPLX
extern LVAL a_complex;
#endif
#ifdef RANDOM
extern LVAL s_randomstate, a_randomstate;
#endif
#ifdef READTABLECASE
extern LVAL s_rtcase;
#endif
#ifdef BETTERGENSYM
extern LVAL s_gensymcounter;
#endif
#ifdef PACKAGES
extern LVAL xlisppack, xlkeypack, xluserpack, s_package;
#endif /* PACKAGES */


/* keywords */
extern LVAL k_test,k_tnot;
extern LVAL k_wspace,k_const,k_nmacro,k_tmacro;
extern LVAL k_sescape,k_mescape;
extern LVAL k_direction,k_input,k_output;
extern LVAL k_start,k_end,k_1start,k_1end;
extern LVAL k_2start,k_2end,k_count;
extern LVAL k_verbose,k_print;
extern LVAL k_upcase,k_downcase,k_capitalize;
extern LVAL k_io, k_elementtype;
extern LVAL s_termio, k_exist, k_nexist, k_error;
extern LVAL k_rename, k_newversion, k_overwrite, k_append;
extern LVAL k_supersede, k_rendel, k_probe, k_create;
extern LVAL k_concname, k_include, k_prntfunc;
extern LVAL k_initelem, k_initcont;
#ifdef AOKKEY
extern LVAL k_allow_other_keys;  /* TAA added 9/93 */
#endif
#ifdef REDUCE
extern LVAL k_ivalue;
#endif
#ifdef KEYARG
extern LVAL k_key;
#endif
#ifdef HASHFCNS
extern LVAL k_size;
#endif
#ifdef RANDOM
extern LVAL k_data;
#endif
#ifdef READTABLECASE
extern LVAL k_preserve,k_invert;
#endif
#ifdef PACKAGES
extern LVAL k_nicknames, k_use;
#ifdef MULVALS
extern LVAL k_internal, k_external, k_inherited;
#endif /* MULVALS */
#endif /* PACKAGES */
#ifdef FROMEND
extern LVAL k_fromend;
#endif

/* lambda list keywords */
extern LVAL lk_optional,lk_rest,lk_key,lk_aux;
extern LVAL lk_allow_other_keys;

/* read type -- preserve whitespace?*/
extern LVAL a_readpw;

/* type names */
extern LVAL a_subr,a_fsubr;
extern LVAL a_cons,a_symbol,a_fixnum,a_flonum;
extern LVAL a_string,a_object,a_stream,a_vector;
extern LVAL a_closure,a_char,a_ustream;
extern LVAL a_integer, a_real;
#ifdef BIGNUMS
extern LVAL a_ratio;
extern LVAL a_rational;
extern LVAL a_bignum;
extern LVAL a_unbyte, a_sbyte;
extern LVAL n_bigzero, n_bigmone;
#endif
#ifdef PACKAGES
extern LVAL a_package;
#endif /* PACKAGES */

/* Object system */
extern LVAL s_self,k_new,k_isnew;
extern LVAL k_prin1;
extern LVAL cls_class,cls_object;

/* evaluation variables */
extern LVAL * NEAR xlstkbase[]; /* evaluation stack */
extern LVAL * NEAR *xlstack;    /* evaluation stack pointer */
extern LVAL xlenv,xlfenv,xldenv;    /* environment pointers */

/* argument stack */
extern LVAL NEAR xlargstkbase[]; /* argument stack */
extern LVAL NEAR *xlfp;         /* argument frame pointer */
extern LVAL NEAR *xlsp;         /* argument stack pointer */
extern LVAL NEAR *xlargv;       /* current argument vector */
extern int xlargc;              /* current argument count */

/* exception handling variables */
extern CONTXT *xlcontext;       /* current exception handler */
extern CONTXT *xltarget;        /* target context (for xljump) */
extern LVAL xlvalue;            /* exception value (for xljump) */
extern int xlmask;              /* exception type (for xljump) */
#ifdef MULVALS
extern int xlnumresults;        /* number of values */
extern LVAL xlresults[];        /* multiple values array */
#endif /* MULVALS */

/* Garbage collection reporting variables */
extern long gccalls, nfree, total;

/* debugging variables */
extern int xldebug;        /* debug level */
extern int xlsample;       /* control character sample rate */
extern int xltrcindent;    /* trace indent level */

/* gensym variables */
extern char gsprefix[];     /* gensym prefix string */
extern FIXTYPE gsnumber;    /* gensym number */

/* i/o variables */
extern FILEP tfp;           /* transcript file pointer */
extern int redirectout;     /* output is redirected */
extern int redirectin;      /* input is redirected */
extern int batchmode;       /* running a batch process */
extern int lposition;       /* postition in screen */

/* From Luke Tierney, 9/93 */
/* startup functions and command line symbols */
extern LVAL s_startup_functions, s_command_line;
extern LVAL s_loadfileargs, s_toplevelloop;

/* general purpose string buffer */
extern char buf[];

/* remaining nodes */
extern long nnodes;

/* printing level and length */
extern int plevel, plength;

#ifdef STSZ
/* For stack checking */
extern int stackwarn;       /* is TRUE when warning given */
extern int marghi;          /* stackleft for warning */
#endif

#ifdef MULVALS
extern int xlnumresults;
extern LVAL xlresults[];
#endif

#ifdef FILETABLE
extern FILETABLETYPE filetab[FTABSIZE];
#endif

#ifdef ASCII8
/* conversion tables for 8 bit ASCII */
extern char ascii8tbl[];
extern unsigned char ascii8cnv[];
#endif

/* defined in xlisp.c */
#ifdef SAVERESTORE
extern jmp_buf top_level;
#endif

/* defined in xlftab.c */
/* external variable from xlftab.c */
extern FUNDEF funtab[];
extern int ftabsize;    /* TAA MOD -- added validity check */

/* $putpatch.c$: "MODULE_XLGLOB_H_GLOBALS" */
