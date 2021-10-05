/* xlinit.c - xlisp initialization module */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

/* Forward declarations */
LOCAL VOID NEAR initwks _((void));

/* TAA MOD -- most compilers I use will generate better code calling
   a static function. Because we have many calls of xlenter here, (which
   will only execute once per session), I'm calling xlenter through a
   static function senter() */

#ifdef ANSI
LVAL NEAR senter(char NEAR *str)
{
    return xlenter(str);
}
#else
#define senter(x) xlenter(x)
#endif

#ifdef PACKAGES
/* enter an internal rather than external symbol */
#define ienter(x) xlintern(x, xlisppack);
#else
#define ienter(x) senter(x)
#endif

/* $putpatch.c$: "MODULE_XLINIT_C_GLOBALS" */

#ifdef COMPILER
void xlcompinit _((void));
#endif

/* xlinit - xlisp initialization routine */
int xlinit(resfile) /* TAA Mod -- return true if load of init.lsp needed */
        char *resfile;
{
    /* initialize xlisp (must be in this order) */
    xlminit();  /* initialize xldmem.c */
    xldinit();  /* initialize xldbug.c */

/* finish initializing */
#ifdef SAVERESTORE
    if (*resfile=='\0' || !xlirestore(resfile)) {
        initwks();
        /* $putpatch.c$: "MODULE_XLINIT_C_XLINIT" */
        return TRUE;
    }
    return FALSE;
#else
    initwks();
    /* $putpatch.c$: "MODULE_XLINIT_C_XLINIT" */
    return TRUE;
#endif
}

/* initwks - build an initial workspace */
LOCAL VOID NEAR initwks()
{
    FUNDEF *p;
    int i;
    
    xlsinit();  /* initialize xlsym.c */
    xlsymbols();/* enter all symbols used by the interpreter */
    xlrinit();  /* initialize xlread.c */
    xloinit();  /* initialize xlobj.c */

    /* setup defaults */

    /*can't mark as unbound until #<unbound> created*/
    setfunction(s_unbound, s_unbound);
#ifdef PACKAGES
    setfunction(s_package, s_unbound);
#else
    setfunction(obarray, s_unbound);
#endif /* PACKAGES */
    setfunction(NIL, s_unbound);

    setsvalue(a_readpw, NIL);           /* Don't preserve white space */

    setsvalue(s_evalhook, NIL);         /* no evalhook function */
    setsvalue(s_applyhook, NIL);        /* no applyhook function */
    setsvalue(s_tracelist, NIL);        /* no functions being traced */
    setsvalue(s_tracenable, NIL);       /* traceback disabled */
    setsvalue(s_tlimit, NIL);           /* trace limit infinite */
    setsvalue(s_breakenable, NIL);      /* don't enter break loop on errors */
    setsvalue(s_gcflag, NIL);           /* don't show gc information */
    setsvalue(s_gchook, NIL);           /* no gc hook active */

#ifdef BIGNUMS
    setsvalue(s_readbase, NIL);         /* default read base (decimal) */
    setsvalue(s_printbase, NIL);        /* default print base (decimal) */
#else
    setsvalue(s_ifmt, NIL);             /* default integer print format */
#endif
    setsvalue(s_ffmt, NIL);             /* float print format */

#ifdef RANDOM
    setsvalue(s_randomstate, newrandom(1L));    /* random state */
#endif
    setsvalue(s_printcase, k_upcase);   /* upper case output of symbols */
    setsvalue(s_printlevel, NIL);       /* printing depth is infinite */
    setsvalue(s_printlength, NIL);      /* printing length is infinite */
    setsvalue(s_read_suppress, NIL);    /* do not suppress read operations */
#ifdef READTABLECASE
    setsvalue(s_rtcase, k_upcase);      /* read converting to uppercase */
#endif
#ifdef DOSINPUT
    setsvalue(s_dosinput, NIL);         /* use XLISP line editing */
#endif
    setsvalue(s_dispmacros, NIL);       /* don't displace macros */
    setsvalue(s_startup_functions, NIL);/* no startup functions */
    setsvalue(s_loadfileargs, s_true);  /* load command line files on start */

    /* install the built-in functions and special forms */
    for (i = 0, p = funtab; 
#ifdef ANSI
        (p->fd_subr) != (LVAL(*)(void))NULL;
#else
        (p->fd_subr) != (LVAL(*)())NULL; 
#endif
        ++i, ++p) {
        if (p->fd_name != NULL) {
            xlsubr(p->fd_name,p->fd_type,p->fd_subr,i);
	}
    }
    /* add some synonyms */
    setfunction(senter("NOT"), getfunction(senter("NULL")));
    setfunction(senter("FIRST"), getfunction(senter("CAR")));
    setfunction(senter("SECOND"), getfunction(senter("CADR")));
    setfunction(senter("THIRD"), getfunction(senter("CADDR")));
    setfunction(senter("FOURTH"), getfunction(senter("CADDDR")));
    setfunction(senter("REST"), getfunction(senter("CDR")));

    /* set the initial top level function */
    setsvalue(s_toplevelloop, getfunction(senter("TOP-LEVEL-LOOP")));
#ifdef PACKAGES
    setvalue(s_package, xluserpack);
#endif /* PACKAGES */
#ifdef BETTERGENSYM
    setsvalue(s_gensymcounter, cvfixnum(1));
#endif
}

/* xlsymbols - enter all of the symbols used by the interpreter */
VOID xlsymbols()
{
    LVAL sym;

#ifdef PACKAGES
    LVAL oldpack;

    /* find the system packages */
    xlisppack = xlfindpackage("XLISP");
    xlkeypack = xlfindpackage("KEYWORD");
    xluserpack = xlfindpackage("USER");
    xlfindsymbol("*PACKAGE*", xlisppack, &s_package);
    oldpack = getvalue(s_package);
    setvalue(s_package,xlisppack);
#endif /* PACKAGES */

    /* make the unbound variable indicator (must be first) */
    /* TAA MOD 1/93 -- now not interned */

    if (s_unbound == NIL) { /* don't make twice */
        s_unbound = xlmakesym("U"); /* name doesn't really matter */
        setvalue(s_unbound, s_unbound);
    }

    /* put NIL in oblist */
#ifdef PACKAGES
    setpackage(NIL, xlisppack);
    xlimport(NIL,xlisppack);
    xlexport(NIL,xlisppack);
#else
    {   /* duplicate code in xlenter, with different ending */
        char *name= "NIL";
        LVAL array = getvalue(obarray);
        int i = hash(name, HSIZE);
        
        for (sym = getelement(array,i); !null(sym); sym = cdr(sym))
            if (STRCMP(name, getstring(getpname(car(sym)))) == 0)
                goto noEnterNecessary;

        sym = consd(getelement(array,i));
        rplaca(sym, NIL);
        setelement(array, i, sym);
noEnterNecessary: ;
    }
#endif /* PACKAGES */

    /* enter the 't' symbol */
    s_true = senter("T");
    defconstant(s_true, s_true);        /* TAA mod -- was setvalue */

    /* enter some other constants */

#ifdef TIMES
    sym = senter("INTERNAL-TIME-UNITS-PER-SECOND");
    defconstant(sym, cvfixnum((FIXTYPE) ticks_per_second()));
#endif
#ifdef COMPLX
    sym = senter("PI");
    defconstant(sym, cvflonum((FLOTYPE) PI));
#endif
    
    sym = senter("VERSION");
    /* It should be noted that the following can fail if a GC occurs,
     * and the version number is greater than 255. That's extremely
     * unlikey to occur in the next century. */
    {
        LVAL major, minor;
        major = cvfixnum(MAJOR_VERSION);
        minor = cvfixnum(MINOR_VERSION);
        defconstant(sym, cons(major, minor));
    }
/*    defconstant(sym, cons(cvfixnum(MAJOR_VERSION),
 *    cvfixnum(MINOR_VERSION))); */ /* This was giving warnings. TAA
 *    10/2015 */

    /* enter some important symbols */
    s_dot       = senter(".");
    s_quote     = senter("QUOTE");
    s_identity  = senter("IDENTITY");
    s_function  = senter("FUNCTION");
    s_bquote    = senter("BACKQUOTE");
    s_comma     = senter("COMMA");
    s_comat     = senter("COMMA-AT");
    s_lambda    = senter("LAMBDA");
    s_macro     = senter("MACRO");
    s_eql       = senter("EQL");
    s_features  = senter("*FEATURES*");
#ifdef BIGNUMS
    s_readbase  = senter("*READ-BASE*");
    s_printbase = senter("*PRINT-BASE*");
#else
    s_ifmt      = senter("*INTEGER-FORMAT*");
#endif
    s_ffmt      = senter("*FLOAT-FORMAT*");

    /* symbols set by the read-eval-print loop */
    s_1plus     = senter("+");
    s_2plus     = senter("++");
    s_3plus     = senter("+++");
    s_1star     = senter("*");
    s_2star     = senter("**");
    s_3star     = senter("***");
    s_minus     = senter("-");

    /* enter setf place specifiers */
    s_setf      = senter("*SETF*");
    s_setfl     = senter("*SETF-LAMBDA*");  /* TAA added 7/92 */
    s_getf      = senter("GETF");           /* TAA added 7/93 */
    s_car       = senter("CAR");
    s_cdr       = senter("CDR");
    s_nth       = senter("NTH");
    s_aref      = senter("AREF");
    s_get       = senter("GET");
    s_svalue    = senter("SYMBOL-VALUE");
    s_sfunction = senter("SYMBOL-FUNCTION");
    s_splist    = senter("SYMBOL-PLIST");
    s_elt       = senter("ELT");
    s_apply     = senter("APPLY");
#ifdef HASHFCNS
    s_gethash   = senter("GETHASH");
#endif
    s_read_suppress = xlenter("*READ-SUPPRESS*");

    /* enter the readtable variable and keywords */
    s_rtable    = senter("*READTABLE*");
    k_wspace    = senter(":WHITE-SPACE");
    k_const     = senter(":CONSTITUENT");
    k_nmacro    = senter(":NMACRO");
    k_tmacro    = senter(":TMACRO");
    k_sescape   = senter(":SESCAPE");
    k_mescape   = senter(":MESCAPE");

    /* enter parameter list keywords */
    k_test      = senter(":TEST");
    k_tnot      = senter(":TEST-NOT");

    /* "open" keywords */
    k_direction = senter(":DIRECTION");
    k_input     = senter(":INPUT");
    k_output    = senter(":OUTPUT");
    k_io        = senter(":IO");
    k_probe     = senter(":PROBE");
    k_elementtype = senter(":ELEMENT-TYPE");
    k_exist     = senter(":IF-EXISTS");
    k_nexist    = senter(":IF-DOES-NOT-EXIST");
    k_error     = senter(":ERROR");
    k_rename    = senter(":RENAME");
    k_newversion = senter(":NEW-VERSION");
    k_overwrite = senter(":OVERWRITE");
    k_append    = senter(":APPEND");
    k_supersede = senter(":SUPERSEDE");
    k_rendel    = senter(":RENAME-AND-DELETE");
    k_create    = senter(":CREATE");

    /* enter *print-case* symbol and keywords */
    s_printcase = senter("*PRINT-CASE*");
    k_upcase    = senter(":UPCASE");
    k_downcase  = senter(":DOWNCASE");
    k_capitalize= senter(":CAPITALIZE");

#ifdef READTABLECASE
    /* enter *readtable-case* symbol and keywords */
    s_rtcase    = senter("*READTABLE-CASE*");
    k_preserve  = senter(":PRESERVE");
    k_invert    = senter(":INVERT");
#endif

    /* more printing symbols */
    s_printlevel= senter("*PRINT-LEVEL*");
    s_printlength = senter("*PRINT-LENGTH*");
#ifdef DOSINPUT
    s_dosinput  = senter("*DOS-INPUT*");
#endif
    
    /* other keywords */
    k_start     = senter(":START");
    k_end       = senter(":END");
    k_1start    = senter(":START1");
    k_1end      = senter(":END1");
    k_2start    = senter(":START2");
    k_2end      = senter(":END2");
    k_verbose   = senter(":VERBOSE");
    k_print     = senter(":PRINT");
    k_count     = senter(":COUNT");
    k_concname  = senter(":CONC-NAME"); /* TAA-- added to save xlenters */
    k_include   = senter(":INCLUDE");
    k_prntfunc  = senter(":PRINT-FUNCTION");
    k_initelem  = senter(":INITIAL-ELEMENT");
    k_initcont  = senter(":INITIAL-CONTENTS");
#ifdef AOKKEY
    k_allow_other_keys = senter(":ALLOW-OTHER-KEYS"); /* TAA added 9/93 */
#endif

#ifdef KEYARG   
    k_key       = senter(":KEY");
#endif

#ifdef REDUCE
    k_ivalue    = senter(":INITIAL-VALUE");
#endif

#ifdef HASHFCNS
    k_size = senter(":SIZE");
#endif

#ifdef RANDOM
    k_data = senter(":DATA");
#endif

#ifdef PACKAGES
    k_nicknames = senter(":NICKNAMES");
    k_use = senter(":USE");
#ifdef MULVALS
    k_internal = senter(":INTERNAL");
    k_external = senter(":EXTERNAL");
    k_inherited = senter(":INHERITED");
#endif /* MULVALS */
#endif /* PACKAGES */

#ifdef FROMEND
    k_fromend = senter(":FROM-END");
#endif
    
    /* Startup variables (from L. Tierney 9/93) */
    s_startup_functions = senter("*STARTUP-FUNCTIONS*");
    s_command_line = senter("*COMMAND-LINE*");
    s_loadfileargs = senter("*LOAD-FILE-ARGUMENTS*");
    s_toplevelloop = senter("*TOP-LEVEL-LOOP*");

    /* enter lambda list keywords */
    lk_optional = senter("&OPTIONAL");
    lk_rest     = senter("&REST");
    lk_key      = senter("&KEY");
    lk_aux      = senter("&AUX");
    lk_allow_other_keys = senter("&ALLOW-OTHER-KEYS");

    /* enter *standard-input*, *standard-output* and *error-output* */
    /* TAA Modified so that stderr (CONSOLE) is used if no redirection */

    s_stderr = senter("*ERROR-OUTPUT*");
    setsvalue(s_stderr,cvfile(CONSOLE,S_FORREADING|S_FORWRITING));
    s_termio = senter("*TERMINAL-IO*");
    setsvalue(s_termio,getvalue(s_stderr));
    s_stdin = senter("*STANDARD-INPUT*");
    setsvalue(s_stdin,redirectin ? 
        cvfile(STDIN,S_FORREADING): getvalue(s_stderr));
    s_stdout = senter("*STANDARD-OUTPUT*");
    setsvalue(s_stdout,redirectout ? 
        cvfile(STDOUT,S_FORWRITING): getvalue(s_stderr));

    /* enter *debug-io* and *trace-output* */
    s_debugio = senter("*DEBUG-IO*");
    setsvalue(s_debugio,getvalue(s_stderr));
    s_traceout = senter("*TRACE-OUTPUT*");
    setsvalue(s_traceout,getvalue(s_stderr));

    /* enter the eval and apply hook variables */
    s_evalhook = senter("*EVALHOOK*");
    s_applyhook = senter("*APPLYHOOK*");

    /* enter the symbol pointing to the list of functions being traced */
    s_tracelist = senter("*TRACELIST*");

    /* enter the error traceback and the error break enable flags */
    s_tracenable = senter("*TRACENABLE*");
    s_tlimit = senter("*TRACELIMIT*");
    s_breakenable = senter("*BREAKENABLE*");

    /* enter symbols to control printing of garbage collection messages */
    s_gcflag = senter("*GC-FLAG*");
    s_gchook = senter("*GC-HOOK*");

    /* enter symbol to control displacing of macros with expanded version */
    s_dispmacros = senter("*DISPLACE-MACROS*");

    /* enter a copyright notice into the oblist */
    sym = senter("**Copyright-1988-by-David-Betz**");
    setsvalue(sym,s_true);

    /* enter type names */
    a_subr      = senter("SUBR");
    a_fsubr     = senter("FSUBR");
    a_cons      = senter("CONS");
    a_symbol    = senter("SYMBOL");
    a_fixnum    = senter("FIXNUM");
    a_flonum    = senter("FLONUM");
    a_string    = senter("STRING");
    a_object    = senter("OBJECT");
    a_stream    = senter("FILE-STREAM");
    a_vector    = senter("ARRAY");
    a_closure   = senter("CLOSURE");
    a_char      = senter("CHARACTER");
    a_ustream   = senter("UNNAMED-STREAM");
    a_list      = senter("LIST");
    a_number    = senter("NUMBER");
    a_null      = senter("NULL");
    a_atom      = senter("ATOM");
    a_anystream = senter("STREAM");
    s_and       = senter("AND");
    s_or        = senter("OR");
    s_not       = senter("NOT");
    s_satisfies = senter("SATISFIES");
    s_member    = senter("MEMBER");
    a_struct    = senter("STRUCT");
#ifdef COMPLX
    a_complex   = senter("COMPLEX");
#endif
#ifdef HASHFCNS
    a_hashtable = senter("HASH-TABLE");
#endif
    a_integer   = senter("INTEGER");
    a_real      = senter("REAL");
#ifdef BIGNUMS
    a_ratio     = senter("RATIO");
    a_rational  = senter("RATIONAL");
    a_bignum    = senter("BIGNUM");
    a_unbyte    = senter("UNSIGNED-BYTE");
    a_sbyte     = senter("SIGNED-BYTE");
#endif
#ifdef PACKAGES
    a_package   = senter("PACKAGE");
#endif /* PACKAGES */


    /* struct feature symbols */
    s_strtypep  = ienter("%STRUCT-TYPE-P");
    s_mkstruct  = ienter("%MAKE-STRUCT");
    s_cpystruct = ienter("%COPY-STRUCT");
    s_strref    = ienter("%STRUCT-REF");
    s_strset    = ienter("%STRUCT-SET");
    s_x         = ienter("X");
    s_s         = ienter("S");
    s_prntfunc  = senter("*STRUCT-PRINT-FUNCTION*");
    s_sslots    = senter("*STRUCT-SLOTS*");

    a_readpw    = ienter("*PRESERVE-WHITESPACE*");


#ifdef RANDOM
    s_randomstate = senter("*RANDOM-STATE*");
    a_randomstate = senter("RANDOM-STATE");
    sym = cons(NIL,NIL);    /* add to *struct-slots* property ((data nil)) */
    sym = cons(senter("DATA"),sym);
    sym = consa(sym);
    xlputprop(a_randomstate,sym,s_sslots);
#endif


    /* add the object-oriented programming symbols and os specific stuff */
    obsymbols();        /* object-oriented programming symbols */
    ossymbols();        /* os specific symbols */
#ifdef COMPILER
    xlcompinit();
#endif
    /* $putpatch.c$: "MODULE_XLINIT_C_XLSYMBOLS" */
#ifdef PACKAGES
    setvalue(s_package, oldpack);
#endif
#ifdef BIGNUMS
    {
        LVAL sym = ienter("*big0*");
        if (!boundp(sym)) 
            defconstant(sym, n_bigzero=cvtfixbignum(0L));
        else
            n_bigzero = getvalue(sym);
        sym = ienter("*big-1*");
        if (!boundp(sym))
            defconstant(sym, n_bigmone=cvtfixbignum(-1L));
        else
            n_bigmone = getvalue(sym);
    }
#endif

#ifdef BETTERGENSYM
    s_gensymcounter = senter("*GENSYM-COUNTER*");
#endif
}

