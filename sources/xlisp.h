/* XLISP-PLUS is based on:
*/

/* xlisp - a small subset of lisp */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

/* Public Domain contributors to this modified distribution:
    Tom Almy, Mikael Pettersson, Neal Holtz, Johnny Greenblatt, 
    Ken Whedbee, Blake McBride, Pete Yadlowsky, Hume Smith,
    Wolfgang Kechel, and Richard Zidlicky */

/* Portions of this code from XLISP-STAT Copyright (c) 1988, Luke Tierney */

/* system specific definitions */

#include <stdio.h>
#include <ctype.h>

#define MAJOR_VERSION (3)
#define MINOR_VERSION (5)

#if defined(_Windows)
#define WINDOWS
#endif

#if ((!defined(__TURBOC__)) | (__TURBOC__ < 0x410)) & defined(WINDOWS)
/* Borland finally added catch and throw */
 /* hopefully these won't change :-) */
 typedef int CATCHBUF[9];
 int _far _pascal Catch(int _far*);
 void _far _pascal Throw(const int _far*, int);
 #define setjmp Catch
 #define longjmp Throw
 #define jmp_buf CATCHBUF
#else
#include <setjmp.h>
#endif
#include <string.h>

/************ Notice to anyone attempting modifications ****************/
/* Compared to original XLISP, length of strings in an LVAL exclude the
   terminating null. When appropriate, characters are consistantly treated
   as unsigned, and the null, \0, character is allowed. Don't write any new
   code that assumes NULL and/or NIL are zero */

/* xlenv environment frames may now also contain entries whose car is a fixnum.
   This is to support proper handling of block/return and tagbody/go -- see
   notes below at the definition of xlbindtag */

/********************** PREFERENCE OPTIONS ****************/

/* There used to be many different preference options; if
   you turned them all off you got "standard" xlisp 2.0. But because
   of option proliferation, and the change of name, this is no longer
   true: there are many fewer options, and most functions are now
   standard. */

 /* Costs indicated for Borland Turbo C++ V1.0 (as a C compiler) */

/* Not all permutations of these choices have been tested, but luckily most
   won't interract. */

/* This option uses dynamic array allocation by defining DLDMEM which
 * will use dldmem.c and dlimage.c instead of xldmem.c and xlimage.c.
 * This is recommended for applications that use many arrays, strings,
 * and bignums. Using this alternative adds 1184 bytes of code */
/* #define DLDMEM */

/* This eliminates the problems of fixnums overflowing if no bignums
   The cost is slightly slower performance and 1100 bytes
   requires COMPLX, but no BIGNUMS */
#define NOOVFIXNUM

/* This option modifies performance, but don't affect execution of
   application programs (other than speed) */
#define JMAC        /* performance enhancing macros, Johnny Greenblatt 
                        (7.5K at full config). Don't bother for 16 bit
                        MSDOS compilers. */

/* This option is for IBM PC 8 bit ASCII. To use in other environments,
   you would need to modify the STUFF files, and possibly change the 
   definitions for the macros TOUPPER TOLOWER ISUPPER and ISLOWER.
   Option adds 464 bytes */
#define ASCII8

/* This option, in addition to ASCII8, is used for 8 bit ANSI characters,
   as used in Microsoft Windows. */
/*#define ANSI8 */


/* This option makes CERROR and ERROR work like in earlier versions of
   XLISP-PLUS (2.1e or earlier), instead of like Common Lisp. Note that
   all supplied libraries and demos use the new definition, so using this
   option will require modification of the LSP files. */
/*#define OLDERRORS */

/* This option is necessary for Microsoft Windows 3.0, but can be used
   under MS-DOS as well. Borland C++ and TopSpeed C provide adequate library
   support for MS-DOS use. For other compilers, additional functions would
   need to be written (not supplied). Windows provides the necessary
   functions, so any Windows-compliant compiler should suffice.
   When using this option, you must compile all modules with the medium
   memory model, and you must also use the dldmem/dlimage pair of files
   rather than the xldmem/xlimage pair of files.
   This option is not enabled here; when desired it is enabled from the
   compiler command line. */
/*#define MEDMEM*/      /* Medium memory model */

/* This option is necessary for Microsoft Windows. It handles file
   streams using a local table of file defining structures. For non-windows
   use, the benefits are file streams can print their associated file names
   and files streams are preserved across saves. It also allows the
   functions TRUENAME and DELETE-FILE */
#define FILETABLE

/* This option allows xlisp to be called as a server. There is no outer loop.
   The STUFF file will have to modified appropriately, as well as xldbug. */
/*#define SERVER*/  /* server version */

/* This option adds a *readtable-case* global variable that has the same
   effect as the readtable-case function described in CLtL, 2nd Ed. 
   It is contributed by Blake McBride, root@blakex.raindernet.com, who
   places it in the public domain */
#define READTABLECASE

/* This option adds the :KEY arguments to appropriate functions. It's
   easy to work around when missing (adds about 2k bytes) */
#define KEYARG

/* This option adds the :ALLOW-OTHER-KEYS keyword argument to all functions.
   The mod is simple, but it does make the code size grow and slows things
   down */
#define AOKKEY

/* This option prevents class variable inheritance. The documentation
 * states that class variables are not inherited yet the code implented
 * it. This option makes things match the documentation *
 */
#define NOCLASSVARINH

/* Use environmental variable of same name as a search
    path for LOAD and RESTORE commands. Might not be
    available on some systems */
#define PATHNAMES "XLPATH"

/* The remainder of options solely add various functions. If you are
   pressed for space, you might try eliminating some of these (particularly
   TIMES, COMPLX, and BIGNUMS) */

#define SRCHFCN     /* SEARCH (1040 bytes)*/

#define MAPFCNS     /* SOME EVERY NOTANY NOTEVERY MAP (2352 bytes)*/

#define POSFCNS     /* POSITION- COUNT- FIND-  functions (1168 bytes)*/

#define REMDUPS     /* REMOVE-DUPLICATES (1440 bytes)*/

#define REDUCE      /* REDUCE, by Luke Tierney (with modifications). 
                       (1008 bytes)*/

#define FROMEND     /* adds :FROM-END to REMOVE* DELETE* FIND* POSITION* COUNT*
                       SUBSTITUTE* and NSUBSTITUTE*,
                       and :COUNT to REMOVE* DELETE* SUBSTITUTE* NSUBSTITUTE*
                       (2768 bytes) */

#define SUBSTITUTE  /* adds SUBSTITUTE- and NSUBSTITUTE- functions. */

#define ADDEDTAA    /* added function by TAA: GENERIC (336 bytes) */

#define TIMES       /* time functions TIME GET-INTERNAL-RUN-TIME
                       GET-INTERNAL-REAL-TIME and constant
                       INTERNAL-TIME-UNITS-PER-SECOND (5286 bytes)*/

#define RANDOM      /* Add RANDOM-NUMBER-STATE type, *RANDOM-STATE*, and
                       function MAKE-RANDOM-STATE
                       You must also define TIMES (736 bytes)*/

#define HASHFCNS    /* Hash table functions (Ken Whedbee):
                       SETHASH (SETF (SETHASH..)), MAKE-HASH-TABLE, 
                       TAA's REMHASH, MAPHASH, CLRHASH, HASH-TABLE-COUNT
                       (2608 bytes)*/

#define SETS        /* Luke Tierney's set functions ADJOIN UNION INTERSECTION
                        SET-DIFFERENCE SUBSETP (1328 bytes)*/

#define APPLYHOOK   /* adds applyhook support, strangely missing before 
                       (1312 bytes)*/

#define COMPLX      /* complex numbers&more math from Luke Tierney:
                        COMPLEX, COMPLEXP, IMAGPART, REALPART, CONJUGATE, 
                        PHASE, LOG, FLOOR, CEILING, ROUND, and PI.
                        Also LCM (by Ken Whedbee) and
                        ASH (by Pete Yadlowsky) (15k bytes) */

#define BIGNUMS     /* bignum, ratios, and radix support. Requires COMPLX.
                           31K bytes */

/*#define GENERIC*/ /* Generic bignum-float conversion rather than rely
                       on littlendian IEEE FP. Note that this selection
                       will cost accuracy. */
/*#define BIGENDIAN*/ /* Define this for correct operation of read-byte and
                       write-byte in a big-endian system */
/*#define BIGENDIANFILE*/ /* define this for read-byte and write-byte to
                        utilize bigendian files. This option kept separate
                        so that binary files can be transfered between xlisp
                        systems on different processors */

#define LEXBIND     /* Lexical tag scoping for TAGBODY/GO and BLOCK/RETURN.
                        If not defined, use original dynamic scoping
                        (Code from Luke Tierney) */

#define PACKAGES    /* Changes from using *obarray* to a simplified
                       package implementation (code from Luke Tierney)
                        (11000 bytes) */

#define MULVALS     /* Changes to support multiple value returns
                       (code from Luke Tierney) (3500 bytes) */

#define SAVERESTORE
                    /* SAVE and RESTORE commands (an original option!) 
                        (3936 bytes) */

#define BETTERGENSYM /* GENSYM that meets ANSI Common Lisp instead of definition */
                     /* in CLtL. Also adds special variable
                      * *gensym-counter* */

#define PRINTTOSTRING /* PRIN1-TO-STRING and PRINC-TO-STRING */

/* The following option only available for certain compilers noted
   below */

#define GRAPHICS    /* add graphics commands 
                        MODE COLOR MOVE DRAW MOVEREL DRAWREL
                       and screen commands CLS CLEOL GOTO-XY
                        (3k) */



/************ END OF PREFERENCE OPTIONS **************/


/* handle dependencies */


#ifdef RANDOM
#ifndef TIMES
#define TIMES
#endif
#endif

#ifdef BIGNUMS
#ifndef COMPLX
#define COMPLX
#endif
#endif

#ifdef BIGENDIAN
#ifndef GENERIC
#define GENERIC
#endif
#endif

/*************** COMPILER/ENVIRONMENT OPTIONS ****************/



/* Default compiler options: */
/* NNODES       number of nodes to allocate in each request (2000) */
/* VSSIZE       number of vector nodes to allocate in each request (6000) */
/* EDEPTH       evaluation stack depth (650) */
/* ADEPTH       argument stack depth (1000) */
/* SFIXMIN      minimum static fixnum (-128, in xldmem.h) */
/* SFIXMAX      maximum static fixnum (255, in xldmem.h) */
 /* PLENMAX      maximum value for PRINTLEN (32767) */
 /* PLEVMAX      maximum value for PRINTLEVEL (32767) */
/* FNAMEMAX     Maximum size of file name strings (63) */
/* MULVALLIMIT  Maximum number of returnable values (128) */
/* MAXFIX       maximum positive value of an integer (0x7fffffffL) */
/* MAXSLEN      maximum sequence length, <= maximum unsigned, on 16 bit
                systems should be the maximum string length that can be
                malloc'ed (1000000)*/
/* MAXVLEN      maximum vector length, should normally be MAXSLEN, but on
                16 bit systems needs to be the maximum vector size that can
                be malloc'ed (MAXSLEN) */
/* FORWARD      type of a forward declaration () */
/* LOCAL        type of a local function (static) */
/* NEAR         function is is same segment (8086 processors) () */
/* AFMT         printf format for addresses ("%x") */
/* FIXTYPE      data type for fixed point numbers (long) */
/* ITYPE        fixed point input conversion routine type (long atol()) */
/* ICNV         fixed point input conversion routine (atol) */
/* IFMT         printf format for fixed point numbers ("%ld") (no BIGNUMS)*/
/* FLOTYPE      data type for floating point numbers (double) */
/* OFFTYPE      number the size of an address (int) */
/* CVPTR        macro to convert an address to an OFFTYPE. We have to go
                through hoops for some MS-DOS compilers that like to
                normalize pointers. In these days of Windows, compilers
                seem to be better behaved. Change to default definition
                only after extensive testing. This is no big deal as it
                only effects the SAVE command. (OFFTYPE)(x) */
/* ALIGN32      Compiler has 32 bit ints and 32 bit alignment of struct
                elements */
/* DOSINPUT     OS specific code can read using OS's line input functon */
/* IEEEFP       IEEE FP -- proper printing of +-INF and NAN
                       for compilers that can't hack it.
                       Currently for little-endian systems. */
/* CDECL        C style declaration, for compilers that can also generate
                Pascal style, to allow calling of main() ([nothing])*/
/* ANSI         define for ANSI C compiler */

/* STDIO and MEM and certain STRING calls can be overridden as needed
   for various compilers or environments. By default, the standard
   library functions are used. Any substitute function must mimic the
   standard function in terms of arguments and return values */

/* OSAOPEN      Open ascii file (fopen) */
/* OSBOPEN      Open binary file (fopen) */
/* MODETYPE     Type of open mode (const char *) */
/* OPEN_RO      Open mode for read only ("r") */
/* OPEN_UPDATE  Open mode for update ("r+") */
/* CREATE_WR    Open mode for create for writing ("w") */
/* CREATE_UPDATE Open mode for create update ("w+") */
/* CLOSED       Closed file, or return value when open fails (NULL) */
/* OSGETC       Binary/text Character read (fgetc) */
/* OSPUTC       Binary/text Character write (fputc) */
/* OSREAD       Binary read of file (fread) */
/* OSWRITE      Binary write of file (fwrite) */
/* OSCLOSE      Close the file (fclose) */
/* OSSEEK       Seek in file (fseek(fp,loc,SEEK_SET)) */
/* OSSEEKCUR    Seek for changing direction (fseek(fp,loc,SEEK_CUR)) */
/* OSSEEKEND    Seek to end  (fseek(fp,0L,SEEK_END)) */
/* OSTELL       Tell file location (ftell) */
/* FILEP        File pointer type (FILE *),
                used in all the above functions */
/* STDIN        Standard input (a FILEP) (stdin) */
/* STDOUT       Standard output (stdout) */
/* CONSOLE      Console (stderr) */

/* MALLOC       Memory allocation (malloc) */
/* CALLOC       Memory allocation (calloc) */
/* MFREE        Memory allocation (free) */

/* Systems that differentiate between Ascii and Binary files can either
   handle the "problem" via open (OSAOPEN vs OSBOPEN) or by using the following
   overrides. When these are used, OSGETC and OSPUTC are only used for
   binary files. We can tell the difference because the file objects have
   a binary/ascii bit. We really want to use the following on systems that
   can't handle fseek() properly in ASCII files (such as the GCC compiler
   runtimes). We could do everything this way, but I wanted to leave the
   hook to run older system dependent stuff.c files. */

/* OSAGETC      Text character read, if different from OSGETC */
/* OSAPUTC      Text character write, if different from OSPUTC*/

/* These are needed in case far pointer override is necessary: */

/* STRCMP       String compare (strcmp) */
/* STRCPY       String copy (strcpy) */
/* STRNCPY      String copy (strncpy) */
/* STRCAT       String concatenate (strcat) */
/* STRLEN       String length (strlen) */
/* MEMCPY       Memory copy (memcpy) */

/* The following are when system stack checking is incorporated */
/* This feature from Richard Zidlicky */

/* STSZ         Size of stack (passed from compiler command line) */
/* GCSTMARGIN   Do not try GC with less stack than this (2048) */
/* GCMARGLO     Fatal death if less than this much stack during GC */
/* MARGLO       Goto toplevel when stack below this (512) */

/* The following are definitions for various characters input from the
   keyboard. The default values are for MS-DOS, and match the documentation.
   The characters are only referenced in the "STUFF" file. */
/* C_BREAK      Enter break level (control-B) */
/* C_TOPLEV     Goto top level (control-C) */
/* C_CLEAN      Go up one level (control-G) */
/* C_CONT       Continue (control-P) */
/* C_EOF        End of file (control-Z) */
/* C_PAUSE      Pause (control-S) */
/* C_STATUS     Status message (control-T) */
/* C_TAB        Tab character/name completion */
/* C_BS         Destructive backspace (Control-H or Backspace) */
/* C_ESC        Abort input line (escape) */
/* C_DEL        Delete character at cursor (Delete) */
/* C_LA         Nondestructive backspace (left arrow) */
/* C_RA         Nondestructive space (right arrow) */
/* C_UA         Previous command (up arrow) */
/* C_DA         Next command (down arrow) */
/* C_HOME       Start of line (home) */
/* C_END        End of line (end) */

/* for Zortech C  -- Versions 2.0 and above, please */
/* Works for Large Model, 268PM model (Z), and 386PM model (X) */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/25 */
#ifdef __ZTC__
#ifdef DOS386   /* 80386 compiler */
#define EDEPTH 4000 
#define ADEPTH 6000
#define VSSIZE 20000
#define ALIGN32
#define ANSI
#if __ZTC__ < 0x300
#define IEEEFP      /* they fixed this */
#endif
#define CDECL   _cdecl
#define DOSINPUT
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#else           /* 80286PM or Real mode */
#ifdef DOS16RM
#define EDEPTH          2000
#define ADEPTH          3000
#endif
#define MAXSLEN         (65519U)
#define MAXVLEN         (16379U)
#define ANSI
#define AFMT            "%lx"
#define OFFTYPE         unsigned long
#if __ZTC__ < 0x300
#define IEEEFP      /* they fixed this */
#endif
#define CDECL   _cdecl
#define DOSINPUT
#undef JMAC         /* not worth effort if cramped for space */
#define NEAR _near
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#endif
#undef MEDMEM       /* doesn't work, as of V2.1 */
#endif

/* for the Turbo C compiler - MS-DOS, large or medium model */
/* Version 1.5 and 2.0.  1.5 won't compile with TIMES */
/* Also for Turbo/Borland C++, as a C compiler */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/24 for MEDMEM */
#ifdef __TURBOC__
#ifdef __WIN32__
/* These are for Win32 */
#define FILETABLE
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH ((STSZ-MARGLO)/32)
#define ADEPTH ((STSZ-MARGLO)/24)
#define GCSTMARGIN (5000) /* We put limits with Win32 */
#define MARGLO (2000)      /* We still seem to need this, but not as badly?? */
#define VSSIZE 20000
#define NNODES 10000
#define ALIGN32
#define ANSI
#ifdef BIGNUMS
#define LDEXP myldexp
double myldexp(double, int);
#endif
#undef MEDMEM
#else
#ifdef WINDOWS
 #define MEDMEM     /* force medium memory model */
 #define FILETABLE  /* force the file table */
 #define NNODES 7200    /* These need to be set big for best results */
 #define VSSIZE 16000
 #define GCSTMARGIN (4000)
 #define MARGLO (3000)  /* Windows seems to have serious problems if stack
                                    falls below this */
#endif
#ifdef STSZ
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char _far *)&x))
#define EDEPTH ((STSZ-MARGLO)/22)
#define ADEPTH ((STSZ-MARGLO)/16)
#endif
#define MAXSLEN         (65519U)
#define MAXVLEN         (16383U)
#define ANSI
#define AFMT            "%lx"
#define OFFTYPE         unsigned long
#ifdef MEDMEM
#define CVPTR(x)        (unsigned long)(x)
#else
#define CVPTR(x)        ((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))
#endif
#if __TURBOC__ < 0x297
#define IEEEFP          /* Borland C++ V2.0 or later handles this */
#endif
#define CDECL _Cdecl
#define DOSINPUT
#undef JMAC         /* not worth effort if cramped for space */
#define NEAR near
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _Cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#ifdef BIGNUMS
#define LDEXP myldexp
double myldexp(double, int);
#endif
#endif
#endif
/* for the JPI TopSpeed C Compiler, Medium or Large memory model */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/34 */
#ifdef __TSC__
#pragma data(heap_size=>4096,stack_size=>STSZ)
#ifdef STSZ  /* value of STSZ should be the same as stack_size, above */
extern char *stackbase; /* in theory-- we should use TSC's function */
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/34)
#define ADEPTH (STSZ/24)
#endif
#define IEEEFP
#define MAXSLEN         (65519U)
#define MAXVLEN         (16379U)
#define ANSI
#define AFMT            "%lx"
#define OFFTYPE         unsigned long
#ifdef MEDMEM
#define CVPTR(x)        (unsigned long)(x)
#else
#define CVPTR(x)        ((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))
#endif
#define CDECL           /* don't use CDECL with this compiler */
#define DOSINPUT
#undef JMAC         /* not worth effort if cramped for space */
#define NEAR near
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE *osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#endif

/* For Microsoft Visual C++, version 4.0 or later (32 bit windows) */
/* for the Microsoft C compiler - MS-DOS, large model */
/* Version 5.0.  Avoid optimizations. Should work with earlier as well. */
/* Version 6.0A. Most opts ok. Avoid those that conflict with longjump */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/30 */
#ifdef MSC
#ifdef WIN32
#define __WIN32__
/* These are for Win32 */
#define FILETABLE
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH ((STSZ-MARGLO)/32)
#define ADEPTH ((STSZ-MARGLO)/24)
#define GCSTMARGIN (5000) /* We put limits with Win32 */
#define MARGLO (2000)      /* We still seem to need this, but not as badly?? */
#define VSSIZE 20000
#define NNODES 10000
#define ALIGN32
#define ANSI
#ifdef BIGNUMS
#define LDEXP myldexp
double myldexp(double, int);
#endif
#undef MEDMEM
#else
#ifdef STSZ
/* MSC seems to suck up alot of stack for system use -- set the
    stack size 1k larger than STSZ */
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/30)
#define ADEPTH (STSZ/20)
#endif
#define MAXSLEN         (65519U)
#define MAXVLEN         (16379U)
#define ANSI
#define AFMT            "%lx"
#define OFFTYPE         long
#define CVPTR(x)        ((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))
#define CDECL _cdecl
#define DOSINPUT
#undef JMAC         /* not worth effort if cramped for space */
#define NEAR _near
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#undef MEDMEM       /* Except for Windows, in the future */
#endif
#endif

/* EMX GCC and OS/2 */
#ifdef EMX
#ifdef STSZ /* Stacks can be made very large, but we will keep it in reason.
                The makefile shows how to change the size */
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/32)
#define ADEPTH (STSZ/24)
#define GCSTMARGIN (0)  /* don't GC checking -- allow it to run always! */
#else
#define EDEPTH 4000
#define ADEPTH 6000
#endif
#define VSSIZE 20000
#define NNODES 10000
#define ALIGN32
#define ANSI
#define  SEEK_CUR 1
#define  SEEK_END 2
#define  SEEK_SET 0
/* library improperly handles ASCII files re lseek() */
#define OSBOPEN osopen
#define OSAOPEN osopen
#define OSAGETC osagetc
#define OSAPUTC osaputc
#ifdef FILETABLE
extern int osagetc(int), osaputc(int,int);
extern int osopen(const char *name, const char *mode);
#else /* No FILETABLE */
extern int osagetc(FILE*), osaputc(int,FILE*);
extern FILE *osopen(const char *name, const char *mode);
#endif
#undef MEDMEM
#undef GRAPHICS
#include <sys\param.h>
#define FNAMEMAX MAXPATHLEN
#define STRMAX MAXPATHLEN   /* TAA Mod so buffer will be big enough */
#endif

/* For GCC on MSDOS (see DOSSTUFF.C) */
#ifdef GCC
#ifdef STSZ /* stack can't really overflow here, except with really large
                    stacks which run out of disk swap space -- so we will
                    just use the checking to catch runaway recursions */
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/32)
#define ADEPTH (STSZ/24)
#define GCSTMARGIN (0)  /* don't GC checking -- allow it to run always! */
#else
#define EDEPTH 4000
#define ADEPTH 6000
#endif
#define VSSIZE 20000
#define NNODES 10000
#define ALIGN32
#define ANSI
#define  SEEK_CUR 1
#define  SEEK_END 2
#define  SEEK_SET 0
/* #define IEEEFP   Fixed at release 1.09 */
/* library improperly handles ASCII files re lseek() */
#define OSBOPEN osopen
#define OSAOPEN osopen
#define OSAGETC osagetc
#define OSAPUTC osaputc
/* Turns out fseek SEEK_CUR is buggy as well :-( */
#define OSSEEKCUR(f,pos) OSSEEK(f, OSTELL(f) + (pos))
#ifdef FILETABLE
extern int osagetc(int), osaputc(int,int);
extern int osopen(const char *name, const char *mode);
#else /* No FILETABLE */
extern int osagetc(FILE*), osaputc(int,FILE*);
extern FILE *osopen(const char *name, const char *mode);
#endif
#define DOSINPUT
#undef MEDMEM
#endif

/* for BSD & SYSV Unix. */
/* Also define BSD in BSD or SUNOS systems */
#ifdef UNIX
#define VOID void
#ifdef STSZ /* stack can't really overflow here, except with really large
                    stacks which run out of disk swap space -- so we will
                    just use the checking to catch runaway recursions */
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define EDEPTH (STSZ/32)
#define ADEPTH (STSZ/24)
#define GCSTMARGIN (0)  /* don't GC checking -- allow it to run always! */
#else
#define EDEPTH 4000
#define ADEPTH 6000
#endif
#define VSSIZE 20000
#define NNODES 10000
#define ALIGN32
#define AFMT                    "%lx"
#define OFFTYPE                 unsigned long    /* TAA Added 2/94 */
#ifndef SEEK_SET
#define SEEK_SET                0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR                1
#endif
#ifndef SEEK_END
#define SEEK_END                2
#endif
#undef GRAPHICS
#undef MEDMEM
#undef ASCII8
#define remove unlink   /* not all Unix systems have remove */
#ifdef FILETABLE
extern int osopen();
#define OSAOPEN osopen
#define OSBOPEN osopen
/* use default FILETABLE declaration for OSCLOSE */
#endif
/* Unix filenames can be long! */
#include <sys/param.h>
#define FNAMEMAX MAXPATHLEN
#define STRMAX MAXPATHLEN   /* TAA Mod so buffer will be big enough */
#endif

/* Apple Mac OS X 10.5 or 10.6 */
/* Did define UNIX and BSD */
#ifdef MACOSX
#include <unistd.h>
#define ANSI
#define ASCII8
#define ANSI8 /* Changed to define ANSI8 (ISO Latin 1) */
#define VOID void
#ifndef STSZ
#define STSZ 8192000 /* Huge default stack in OS X */
#endif
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#define setjmp _setjmp
#define longjmp _longjmp
#ifdef __x86_64__
/* 64 bit pointers means stack is larger */
#define EDEPTH (STSZ/68)
#define ADEPTH (STSZ/60)
#define GCSTMARGIN (5000) 
#define MARGLO (4000)     
#else
#define EDEPTH (STSZ/32)
#define ADEPTH (STSZ/24)
#define GCSTMARGIN (5000) 
#define MARGLO (4000)     
#endif
#define VSSIZE 20000
#define NNODES 10000
#define ALIGN32
#define AFMT                    "%lx"
#define OFFTYPE                 unsigned long    /* TAA Added 2/94 */
#ifndef SEEK_SET
#define SEEK_SET                0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR                1
#endif
#ifndef SEEK_END
#define SEEK_END                2
#endif
#ifdef USEQT
#define GRAPHICS
#else
#undef GRAPHICS
#endif
#undef MEDMEM
#define remove unlink   /* not all Unix systems have remove */
#ifdef FILETABLE
extern int osopen();
#define OSAOPEN osopen
#define OSBOPEN osopen
/* use default FILETABLE declaration for OSCLOSE */
#endif
#define DOSINPUT
/* Unix filenames can be long! */
#include <sys/param.h>
#define FNAMEMAX MAXPATHLEN
#define STRMAX MAXPATHLEN   /* TAA Mod so buffer will be big enough */
/* Keybindings are different in MacOSX land */
#define C_BS (127)
#define C_BS2 (8) /* Xterm */
#define C_DEL (1406)
#define C_LA (324)
#define C_RA (323)
#define C_UA (321)
#define C_DA (322)
#define C_HOME (1860)
#define C_HOME2 (328) /* Xterm */
#define C_END (1859)
#define C_END2 (326) /* Xterm */
#define C_EOF (4)
#include <unistd.h>
#endif

/* Linux */
#ifdef LINUX
#include <unistd.h>
#define ANSI
#define ASCII8
#define ANSI8 /* ISO Latin 1 */
#define VOID void
#ifdef STSZ
extern char *stackbase;
#define STACKREPORT(x) (STSZ-(stackbase-(char *)&x))
#ifdef __x86_64__
/* 64 bit pointers means stack is larger -- don't know what these
 * should be*/
#define EDEPTH (STSZ/68)
#define ADEPTH (STSZ/60)
#define GCSTMARGIN (5000) 
#define MARGLO (4000)     
#else
#define EDEPTH (STSZ/56)
#define ADEPTH (STSZ/44)
#define GCSTMARGIN (5000) 
#define MARGLO (4000)     
#endif
#endif
#define VSSIZE 20000
#define NNODES 10000
#define ALIGN32
#define AFMT                    "%lx"
#define OFFTYPE                 unsigned long    /* TAA Added 2/94 */
#ifndef SEEK_SET
#define SEEK_SET                0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR                1
#endif
#ifndef SEEK_END
#define SEEK_END                2
#endif
#ifdef USEQT
#define GRAPHICS
#else
#undef GRAPHICS
#endif
#undef MEDMEM
#define remove unlink   /* not all Unix systems have remove */
#ifdef FILETABLE
extern int osopen();
#define OSAOPEN osopen
#define OSBOPEN osopen
/* use default FILETABLE declaration for OSCLOSE */
#endif
#define DOSINPUT
/* Unix filenames can be long! */
#include <sys/param.h>
#define FNAMEMAX MAXPATHLEN
#define STRMAX MAXPATHLEN   /* TAA Mod so buffer will be big enough */
/* Keybindings are different in UNIX land */
#define C_BS (127)
#define C_DEL (1406)
#define C_LA (324)
#define C_RA (323)
#define C_UA (321)
#define C_DA (322)
#define C_HOME (894)
#define C_END (1662) 
#define C_HOME2 (3144) /* XTERM Variant */
#define C_END2 (3142)  /* XTERM variant */
#define C_EOF (4)
#endif

/* IBM/370 implementations using the SAS/C compiler */
#ifdef __SASC__
#define VOID void
#define EDEPTH 4000 
#define ADEPTH 6000
#define ALIGN32
#define AFMT                    "%lx"
#ifndef SEEK_SET
#define SEEK_SET                0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR                1
#endif
#ifndef SEEK_END
#define SEEK_END                2
#endif
#undef GRAPHICS
#undef MEDMEM
#undef ASCII8
#ifdef FILETABLE
extern int osopen();
#define OSAOPEN osopen
#define OSBOPEN osopen
/* use default FILETABLE declaration for OSCLOSE */
#endif
/* MVS/CMS filenames can be long! */
#include <sys/param.h>
#ifndef MAXPATHLEN
  /* Some versions of SAS/C define this, others don't... */
#define MAXPATHLEN  1024
#endif
#define FNAMEMAX MAXPATHLEN
#define STRMAX MAXPATHLEN
#endif

/* Amiga Lattice 5.04 (From Hume Smith) */
#ifdef AMIGA
#define EDEPTH 4000
#define ADEPTH 6000
#define ALIGN32
#define AFMT         "%lx"
#define SEEK_SET      0
#define SEEK_CUR      1
#define SEEK_END      2
#undef GRAPHICS
#undef MEDMEM
#undef FILETABLE    /* not ported */
#undef ASCII8
#endif

/*>>>>>>> For other systems -- You are on your own! */

/* Take care of VOID default definition */

#ifndef VOID
#define VOID void    
#endif

#ifdef ANSI /* thanks for this trick go to Hume Smith */
#define _(x) x
#else
#define _(x) ()
#endif

/* Handle the FILETABLE specification -- non-windows */
#ifdef FILETABLE
#define FTABSIZE 13
#define FILEP int
#define CLOSED (-1)     /* because FILEP is now table index */
#define STDIN (0)
#define STDOUT (1)
#define CONSOLE (2)
#ifndef OSAOPEN
#define OSAOPEN osaopen
extern FILEP osaopen(const char *name, const char *mode);
#endif
#ifndef OSBOPEN
#define OSBOPEN osbopen
extern FILEP osbopen(const char *name, const char *mode);
#endif
#ifndef OSGETC
#define OSGETC(f) fgetc(filetab[f].fp)
#endif
#ifndef OSPUTC
#define OSPUTC(i,f) fputc(i,filetab[f].fp)
#endif
#ifndef OSREAD
#define OSREAD(x,y,z,f) fread(x,y,z,filetab[f].fp)
#endif
#ifndef OSWRITE
#define OSWRITE(x,y,z,f) fwrite(x,y,z,filetab[f].fp)
#endif
#ifndef OSCLOSE
#define OSCLOSE osclose
extern VOID osclose _((int i)); /* we must define this */
#endif
#ifndef OSSEEK
#define OSSEEK(f,loc) fseek(filetab[f].fp,loc,SEEK_SET)
#endif
#ifndef OSSEEKEND
#define OSSEEKEND(f) fseek(filetab[f].fp,0L,SEEK_END)
#endif
#ifndef OSSEEKCUR
#define OSSEEKCUR(f,loc) fseek(filetab[f].fp,loc,SEEK_CUR)
#endif
#ifndef OSTELL
#define OSTELL(f) ftell(filetab[f].fp)
#endif
#endif

#ifdef ASCII8
/* 8 bit ASCII character handling */
#define LC8 1   /* lower case 8bit */
#define LUC8 2  /* lower case 8bit with upper case version */
#define LU8 (LC8 | LUC8)
#define UC8 4   /* upper case 8bit (always have lower case version) */
/* ISUPPER return true for all upper case characters */
#define ISUPPER(c) (UC8 & ascii8tbl[(unsigned char)(c)])
/* ISLOWER returns true for all lowercase characters which have upper case versions */
#define ISLOWER(c) (LUC8 & ascii8tbl[(unsigned char)(c)])
/* ISLOWERA returns true for all lowercase characters */
#define ISLOWERA(c) (LC8 & ascii8tbl[(unsigned char)(c)])
/* ISLOWER7 returns true for characters a-z only */
#define ISLOWER7(c) (isascii(c) && islower(c))
/* these versions of TOUPPER and TOLOWER only work on characters that
   can be converted in case. The functions are the same, and do a table lookup*/
#define TOLOWER(c) (ascii8cnv[(unsigned char)(c) - 'A'])
#define TOUPPER(c) (ascii8cnv[(unsigned char)(c) - 'A'])
#else
/* We will modify the IS* functions so that they work over the full 8 bit
   character range since these characters can still be generated. */
#ifdef __SASC__
#define ISLOWER(c) (islower(c))
#define ISUPPER(c) (isupper(c))
#define TOUPPER(c) toupper(c)
#define TOLOWER(c) tolower(c)
#define ISLOWER7(c) (islower(c))
#define ISLOWERA(c) (islower(c))
#else
#define ISLOWER(c) (((unsigned)(c)) < 128 && islower(c))
#define ISUPPER(c) (((unsigned)(c)) < 128 && isupper(c))
#define TOUPPER(c) toupper(c)
#define TOLOWER(c) tolower(c)
#define ISLOWER7(c) (((unsigned)(c)) < 128 && islower(c))
#define ISLOWERA(c) (((unsigned)(c)) < 128 && islower(c))
#endif
#endif

#ifndef LDEXP   /* handle bad LDEXP function (Borland) */
#define LDEXP ldexp
#endif

/* Handle the MEDMEM specification */
#ifdef MEDMEM
 #ifdef __ZTC__
  #define FAR _far
 #else
  #include <alloc.h>
  #define FAR far
 #endif
 #define STRCMP _fstrcmp
 #define STRCPY _fstrcpy
 #define STRNCPY _fstrncpy
 #define STRCAT _fstrcat
 #define STRLEN _fstrlen
 #define MEMCPY _fmemcpy
 #ifdef WINDOWS
   /* This isn't done cleanly, and probably would have to be re-hacked
      for future Win versions */
   const void _near*  _far _pascal GlobalAlloc(unsigned, unsigned long);
   void _far * _far _pascal GlobalLock(const void _near*);
   const void _near*  _far _pascal GlobalFree(const void _near *);
   #define HIWORD(l)           ((unsigned int)((unsigned long)(l) >> 16))
   #define MALLOC(n) GlobalLock(GlobalAlloc(2,(n)))
   void _far * wcalloc(unsigned long, unsigned long);
   #define CALLOC wcalloc
   unsigned _far * wfree(void _far *p);
   #define MFREE wfree
/*   #define MFREE(p) GlobalFree((const void _near*)(HIWORD(p)-1)) */
 #else
  #ifdef __TSC__
   #define MALLOC _fmalloc
   #define CALLOC _fcalloc
   #define MFREE  _ffree
  #endif
  #ifdef __TURBOC__
   #define MALLOC farmalloc
   #define CALLOC farcalloc
   #define MFREE farfree
  #endif
 #endif
#endif
/************ DEFAULT DEFINITIONS  ******************/
/* PC bindings */
#ifndef C_BREAK
#define C_BREAK ('\002')
#endif
#ifndef C_TOPLEV
#define C_TOPLEV ('\003')
#endif
#ifndef C_CLEAN
#define C_CLEAN ('\007')
#endif
#ifndef C_CONT
#define C_CONT  ('\020')
#endif
#ifndef C_EOF
#define C_EOF   ('\032')
#endif
#ifndef C_PAUSE
#define C_PAUSE ('\023')
#endif
#ifndef C_STATUS
#define C_STATUS ('\024')
#endif
#ifndef C_TAB
#define C_TAB ('\t')
#endif
#ifndef C_BS
#define C_BS  ('\010')
#endif
#ifndef C_DEL
#define C_DEL (339)
#endif
#ifndef C_ESC
#define C_ESC ('\033')
#endif
#ifndef C_LA
#define C_LA (331)
#endif
#ifndef C_RA
#define C_RA (333)
#endif
#ifndef C_UA
#define C_UA (328)
#endif
#ifndef C_DA
#define C_DA (336)
#endif
#ifndef C_HOME
#define C_HOME (327)
#endif
#ifndef C_END
#define C_END (335)
#endif

#ifndef NNODES
#define NNODES          2000
#endif
#ifndef VSSIZE
#define VSSIZE          6000
#endif
#ifndef EDEPTH
#define EDEPTH          650
#endif
#ifndef ADEPTH
#define ADEPTH          1000
#endif
#ifndef FORWARD
#define FORWARD
#endif
#ifndef LOCAL
#define LOCAL           static
#endif
#ifndef AFMT
#define AFMT            "%x"
#endif
#ifndef FIXTYPE
#define FIXTYPE         long
#endif
#ifdef ANSI /* ANSI C Compilers already define this! */
#include <limits.h>
#include <float.h>
#define MAXFIX  LONG_MAX
#define MINFIX  LONG_MIN
#else
#ifndef DBL_MAX
#define DBL_MAX (1.7976931348623167e+308)
#endif
#ifndef MAXFIX
#define MAXFIX          (0x7fffffffL)
#endif
#ifndef MINFIX
#define MINFIX          ((- MAXFIX) - 1)
#endif
#endif
#ifndef MAXSLEN
#define MAXSLEN         (1000000)   /* no sequences longer than this */
#endif
#ifndef PLENMAX
#define PLENMAX		(32767)
#endif
#ifndef PLEVMAX
#define PLEVMAX		(32767)
#endif
#ifndef MAXVLEN
#define MAXVLEN         MAXSLEN
#endif
#ifndef ITYPE
#define ITYPE           long atol()
#endif
#ifndef ICNV
#define ICNV(n)         atol(n)
#endif
#ifndef IFMT
#define IFMT            "%ld"
#endif
#ifndef FLOTYPE
#define FLOTYPE         double
#endif
#ifndef OFFTYPE
#define OFFTYPE         int
#endif
#ifndef CVPTR
#define CVPTR(x)        ((OFFTYPE)(x))
#endif
#ifndef CDECL
#define CDECL
#endif
#ifndef NEAR
#define NEAR
#endif
#ifndef FAR
#define FAR
#endif
#ifndef FNAMEMAX
#define FNAMEMAX 63
#endif
#ifndef OSAOPEN
#define OSAOPEN fopen
#endif
#ifndef OSBOPEN
#define OSBOPEN fopen
#endif
#ifndef MODETYPE
#define MODETYPE const char *
#endif
#ifndef OPEN_RO
#define OPEN_RO "r"
#endif
#ifndef OPEN_UPDATE
#define OPEN_UPDATE "r+"
#endif
#ifndef CREATE_WR
#define CREATE_WR "w"
#endif
#ifndef CREATE_UPDATE
#define CREATE_UPDATE "w+"
#endif
#ifndef CLOSED
#define CLOSED NULL
#endif
#ifndef OSGETC
#define OSGETC fgetc
#endif
#ifndef OSPUTC
#define OSPUTC fputc
#endif
#ifndef OSREAD
#define OSREAD fread
#endif
#ifndef OSWRITE
#define OSWRITE fwrite
#endif
#ifndef OSCLOSE
#define OSCLOSE fclose
#endif
#ifndef OSSEEK
#define OSSEEK(fp,loc) fseek(fp,loc,SEEK_SET)
#endif
#ifndef OSSEEKEND
#define OSSEEKEND(fp) fseek(fp,0L,SEEK_END)
#endif
#ifndef OSSEEKCUR
#define OSSEEKCUR(fp,loc) fseek(fp,loc,SEEK_CUR)
#endif
#ifndef OSTELL
#define OSTELL ftell
#endif
#ifndef FILEP
#define FILEP FILE *
#endif
#ifndef STDIN
#define STDIN stdin
#endif
#ifndef STDOUT
#define STDOUT stdout
#endif
#ifndef CONSOLE
#define CONSOLE stderr
#endif
#ifndef MALLOC
#define MALLOC malloc
#endif
#ifndef CALLOC
#define CALLOC calloc
#endif
#ifndef MFREE
#define MFREE free
#endif
#ifndef STRCMP
#define STRCMP strcmp
#endif
#ifndef STRCPY
#define STRCPY strcpy
#endif
#ifndef STRNCPY
#define STRNCPY strncpy
#endif
#ifndef STRCAT
#define STRCAT strcat
#endif
#ifndef STRLEN
#define STRLEN strlen
#endif
#ifndef MEMCPY
#define MEMCPY memcpy
#endif
#ifdef STSZ
 #ifndef GCSTMARGIN
 #define GCSTMARGIN (2048)
 #endif
 #ifndef MARGLO
 #define MARGLO (512)
 #endif
 #ifndef GCMARGLO
 #define GCMARGLO (256)
 #endif
 #ifndef STACKREPORT
 #define STACKREPORT(x) stackreport()
 extern int stackreport(VOID);
 #endif
#endif
#ifdef MULVALS
#ifndef MULVALLIMIT
#define MULVALLIMIT (128)
#endif
#endif

/* useful definitions */
#ifndef TRUE
#define TRUE    1
#endif
#ifndef FALSE
#define FALSE   0
#endif

#ifdef COMPLX
#define PI 3.14159265358979323846
#endif

#ifdef ANSI
#include <stdlib.h>
#endif

/************* END OF COMPILER/ENVIRONMENT OPTIONS ************/

/* $putpatch.c$: "MODULE_XLISP_H_PROVIDES" */

/* include the dynamic memory definitions */
#include "xldmem.h"

/* program limits */
#ifndef STRMAX
 #define STRMAX         100             /*maximum length of a string constant*/
#endif
#define HSIZE           199             /* symbol hash table size */
#define SAMPLE          20000            /* control character sample rate */

/* function table offsets for the initialization functions */
#define FT_RMHASH       0
#define FT_RMQUOTE      1
#define FT_RMDQUOTE     2
#define FT_RMBQUOTE     3
#define FT_RMCOMMA      4
#define FT_RMLPAR       5
#define FT_RMRPAR       6
#define FT_RMSEMI       7
#define FT_CLNEW        10
#define FT_CLISNEW      11
#define FT_CLANSWER     12
#define FT_OBISNEW      13
#define FT_OBCLASS      14
#define FT_OBSHOW       15
#define FT_OBPRIN1      16
        
/* macro to push a value onto the argument stack */
#define pusharg(x)      {if (xlsp >= xlargstktop) xlargstkoverflow();\
                         *xlsp++ = (x);}

/* macros to protect pointers */
#define xlstkcheck(n)   {if (xlstack - (n) < xlstkbase) xlstkoverflow();}
#define xlsave(n)       {*--xlstack = &n; n = NIL;}
#define xlprotect(n)    {*--xlstack = &n;}

/* check the stack and protect a single pointer */
#define xlsave1(n)      {if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n; n = NIL;}
#define xlprot1(n)      {if (xlstack <= xlstkbase) xlstkoverflow();\
                         *--xlstack = &n;}

/* macros to pop pointers off the stack */
#define xlpop()         {++xlstack;}
#define xlpopn(n)       {xlstack+=(n);}

/* macros to manipulate the lexical environment */
#define xlframe(e)      cons(NIL,e)
#define xlfbind(s,v)    xlpbind(s,v,xlfenv);
#define xlpbind(s,v,e)  {rplaca(e,cons(cons(s,v),car(e)));}

#ifdef LEXBIND
/* macros for installing tag bindings in xlenv */
/* Added feature from Luke Tierney 09/93 */
/* These are ued to insure that return and go only find lexically visible
   tags. Currently the binding is formed by putting the context pointer
   of the block, as a fixnum, into the car of a binding in xlenv. This
   should work fine as long as nothing tacitly assumes these cars must be
   symbols. If that is a problem, some special symbol can be uses (a gensym
   or s_unbound, for example). */
#define tagentry_p(x) (fixp(car(x)))
#define tagentry_value(x) (cdr(x))
#define tagentry_context(x) ((CONTXT *) getfixnum(car(x)))
#define xlbindtag(c,t,e) xlpbind(cvfixnum((FIXTYPE) (c)),(t),e);
#endif

/* macros to manipulate the dynamic environment */
#define xldbind(s,v)    {xldenv = cons(cons(s,getvalue(s)),xldenv);\
                         setvalue(s,v);}
#define xlunbind(e)     {for (; xldenv != (e); xldenv = cdr(xldenv))\
                           setvalue(car(car(xldenv)),cdr(car(xldenv)));}

/* macro to manipulate dynamic and lexical environment */

#define xlbind(s,v) {if (specialp(s)) xldbind(s,v) else xlpbind(s,v,xlenv)}
#define xlpdbind(s,v,e) {e = cons(cons(s,getvalue(s)),e);\
                         setvalue(s,v);}

/* type predicates */                          
#ifdef __WIN32__
#define null(x)         ((x) == NIL)
#else
#ifdef __BORLANDC__
#define null(x)         (((unsigned)(void _seg *)(x)) == ((unsigned)(void _seg *) NIL))
#else
#ifdef MSC
#define null(x)         (((unsigned)(_segment *)(x)) == ((unsigned)(_segment *) NIL))
#else
#define null(x)         ((x) == NIL)
#endif
#endif
#endif
#define atom(x)         (null(x) || ntype(x) != CONS)
#define listp(x)        (null(x) || ntype(x) == CONS)

#define consp(x)        (ntype(x) == CONS)
#define subrp(x)        (ntype(x) == SUBR)
#define fsubrp(x)       (ntype(x) == FSUBR)
#define stringp(x)      (ntype(x) == STRING)
#define symbolp(x)      (ntype(x) == SYMBOL)
#define streamp(x)      (ntype(x) == STREAM)
#define objectp(x)      (ntype(x) == OBJECT)
#define fixp(x)         (ntype(x) == FIXNUM)
#define floatp(x)       (ntype(x) == FLONUM)
#ifdef COMPLX
#define complexp(x)     (ntype(x) == COMPLEX)
#define numberp(x)      (realp(x) || complexp(x))
#else
#define numberp(x)      (realp(x))
#endif
#ifdef BIGNUMS
#define ratiop(x)       (ntype(x) == RATIO)
#define bignump(x)      (ntype(x) == BIGNUM)
/* "rationalp" checks for rational numeric types */
#define rationalp(x)    (ntype(x) == FIXNUM || ntype(x) == BIGNUM || ntype(x) == RATIO)
/* "integerp" checks for integer numeric types */
#define integerp(x)     (ntype(x) == FIXNUM || ntype(x) == BIGNUM)
/* "realp" checks for non-complex numeric types */
#define realp(x)      (ntype(x) == FIXNUM || ntype(x) == FLONUM || ntype(x) == BIGNUM || ntype(x) == RATIO)
#else
#define realp(x)      (ntype(x) == FIXNUM || ntype(x) == FLONUM)
#define integerp(x)     (ntype(x) == FIXNUM)
#endif
#define vectorp(x)      (ntype(x) == VECTOR)
#define closurep(x)     (ntype(x) == CLOSURE)
#define charp(x)        (ntype(x) == CHAR)
#define ustreamp(x)     (ntype(x) == USTREAM)
#define structp(x)      (ntype(x) == STRUCT)

#ifdef PACKAGES
#define packagep(x)     (ntype(x) == PACKAGE)
#define keywordp(x) (symbolp(x)&&(getpackage(x)==xlkeypack))
#endif /* PACKAGES */

#define boundp(x)       (getvalue(x) != s_unbound)
#define fboundp(x)      (getfunction(x) != s_unbound)

/* shorthand functions */
#define consa(x)        cons(x,NIL)
#define consd(x)        cons(NIL,x)

/* argument list parsing macros */
#define xlgetarg()      (testarg(nextarg()))
#define xllastarg()     {if (xlargc != 0) xltoomany();}

#ifndef AOKKEY
#define xllastkey()     {if (xlargc != 0) xltoomany();}
#endif

#define testarg(e)      (moreargs() ? (e) : xltoofew())
#define typearg(tp)     (tp(*xlargv) ? nextarg() : xlbadtype(*xlargv))
#define nextarg()       (--xlargc, *xlargv++)
#define moreargs()      (xlargc > 0)

/* macros to get arguments of a particular type */
#define xlgacons()      (testarg(typearg(consp)))
#define xlgalist()      (testarg(typearg(listp)))
#define xlgasymbol()    (testarg(typearg(symbolp)))
#define xlgasymornil()  (testarg(typearg(symbolp)))
#define xlgastring()    (testarg(typearg(stringp)))
#define xlgastrorsym()  (testarg(symbolp(*xlargv) ? getpname(nextarg()) : typearg(stringp)))
#define xlgaobject()    (testarg(typearg(objectp)))
#define xlgafixnum()    (testarg(typearg(fixp)))
#define xlgaflonum()    (testarg(typearg(floatp)))
#define xlgachar()      (testarg(typearg(charp)))
#define xlgavector()    (testarg(typearg(vectorp)))
#define xlgastream()    (testarg(typearg(streamp)))
#define xlgaustream()   (testarg(typearg(ustreamp)))
#define xlgaclosure()   (testarg(typearg(closurep)))
#define xlgastruct()    (testarg(typearg(structp)))
#define xlganumber()    (testarg(typearg(numberp)))
#define xlgainteger()   (testarg(typearg(integerp)))
#ifdef PACKAGES
#define xlgapackage()   (testarg(typearg(packagep)))
#endif /* PACKAGES */

/* FILETABLE specification */
#ifdef FILETABLE
typedef struct {
    FILE *fp;
    char *tname;    /* true file name */
#if defined(WINDOWS) | defined(__WIN32__)
    char reopenmode[4];     /* mode to reopen file */
    unsigned long filepos;  /* position of file */
#endif
} FILETABLETYPE;
#endif

/* function definition structure */
typedef struct {
    char *fd_name;      /* function name */
    int fd_type;        /* function type */
#ifdef ANSI
    LVAL (*fd_subr)(void);  /* function entry point */
#else
    LVAL (*fd_subr)();
#endif
} FUNDEF;

/* execution context flags */
#define CF_GO           0x0001
#define CF_RETURN       0x0002
#define CF_THROW        0x0004
#define CF_ERROR        0x0008
#define CF_CLEANUP      0x0010
#define CF_CONTINUE     0x0020
#define CF_TOPLEVEL     0x0040
#define CF_BRKLEVEL     0x0080
#define CF_UNWIND       0x0100

/* execution context */
typedef LVAL NEAR *FRAMEP;
typedef struct context {
    int c_flags;                        /* context type flags */
    LVAL c_expr;                        /* expression (type dependent) */
    jmp_buf c_jmpbuf;                   /* longjmp context */
    struct context *c_xlcontext;        /* old value of xlcontext */
    LVAL * NEAR *c_xlstack;             /* old value of xlstack */
    LVAL NEAR *c_xlargv;                /* old value of xlargv */
    int c_xlargc;                       /* old value of xlargc */
    LVAL NEAR *c_xlfp;                  /* old value of xlfp */
    LVAL NEAR *c_xlsp;                  /* old value of xlsp */
    LVAL c_xlenv;                       /* old value of xlenv */
    LVAL c_xlfenv;                      /* old value of xlfenv */
    LVAL c_xldenv;                      /* old value of xldenv */
} CONTXT;


#define xlstktop (&xlstkbase[EDEPTH])   /* top of the evaluation stack */
#define xlargstktop (&xlargstkbase[ADEPTH]) /* top of the argument stack */

/* OS system interface, *stuff file */
extern VOID oscheck _((void));  /* check for control character during exec */
extern VOID osinit _((char *banner)); /* initialize os interface */
extern VOID osfinish _((void)); /* restore os interface */
extern VOID osflush _((void));  /* flush terminal input buffer */
extern long osrand _((long));   /* next random number in sequence */
#ifdef PATHNAMES
extern FILEP ospopen _((char *name, int ascii)); /* open file using path */
#endif
extern VOID xoserror _((char *msg));/* print an error message */
extern int  ostgetc _((void));      /* get a character from the terminal */
extern VOID ostputc _((int ch));    /* put a character to the terminal */
#ifdef TIMES
extern unsigned long ticks_per_second _((void));
extern unsigned long run_tick_count _((void));
extern unsigned long real_tick_count _((void));
#endif
extern int renamebackup _((char *filename));
#ifdef FILETABLE
extern int truename _((char *name, char *rname));
#endif
#ifdef USEQT
extern VOID xrevertToText _((void));
#endif


#ifdef BIGNUMS
/* for xlbignum.c */
extern LVAL copybignum _((LVAL x, int sign));
extern LVAL normalBignum _((LVAL x));
extern LVAL cvtulongbignum _((unsigned long n, int sign));
extern LVAL cvtfixbignum _((FIXTYPE n));
extern LVAL cvtflobignum _((FLOTYPE n));
extern int cvtbigfixnum _((LVAL x, FIXTYPE *n));
extern int comparebignum _((LVAL x, LVAL y));
extern int zeropbignum _((LVAL x));
extern FLOTYPE cvtbigflonum _((LVAL x));
extern FLOTYPE cvtbigratioflonum _((LVAL num, LVAL denom));
extern FLOTYPE cvtratioflonum _((LVAL ratio));
extern LVAL cvtstrbignum _((char FAR *s, int radix));
extern char FAR *cvtbignumstr _((LVAL x, int radix));
extern LVAL addsubbignum _((LVAL ux, LVAL vx, int subvflag));
extern LVAL multbignum _((LVAL ux, LVAL vx));
extern LVAL divbignum _((LVAL dividend, LVAL divisor, LVAL *remainder));
#endif

/* for xlisp.c */
extern VOID xlrdsave _((LVAL expr));
extern VOID xlevsave _((LVAL expr));
extern VOID xlfatal _((char *msg));
extern VOID wrapup _((void));

/* for xleval */
extern LVAL xlxeval _((LVAL expr));
extern VOID xlabind _((LVAL fun, int argc, LVAL *argv));
extern VOID xlfunbound _((LVAL sym));
extern VOID xlargstkoverflow _((void));
extern int  macroexpand _((LVAL fun, LVAL args, LVAL *pval));
extern int  pushargs _((LVAL fun, LVAL args));
extern LVAL makearglist _((int argc, LVAL *argv));
extern VOID xlunbound _((LVAL sym));
extern VOID xlstkoverflow _((void));

/* for xlio */
extern int xlgetc _((LVAL fptr));
extern VOID xlungetc _((LVAL fptr, int ch));
extern int xlpeek _((LVAL fptr));
extern VOID xlputc _((LVAL fptr, int ch));
extern VOID xlflush _((void));
extern VOID stdprint _((LVAL expr));
extern VOID stdputstr _((char *str));
extern VOID errprint _((LVAL expr));
extern VOID errputstr _((char *str));
extern VOID dbgprint _((LVAL expr));
extern VOID dbgputstr _((char *str));
extern VOID trcprin1 _((LVAL expr));
extern VOID trcputstr _((char *str));

/* for xlprin */
extern VOID xlputstr _((LVAL fptr, char *str));
extern VOID xlprint _((LVAL fptr, LVAL vptr, int flag));
extern VOID xlprintl _((LVAL fptr, LVAL vptr, int flag));
extern int  xlgetcolumn _((LVAL fptr));
extern int  xlfreshline _((LVAL fptr));
extern VOID xlterpri _((LVAL fptr));
extern VOID xlputstr _((LVAL fptr, char* str));

/* for xljump */
extern VOID xljump _((CONTXT *target, int mask, LVAL val));
extern VOID xlbegin _((CONTXT *cptr, int flags, LVAL expr));
extern VOID xlend _((CONTXT *cptr));
extern VOID xlgo _((LVAL label));
extern VOID xlreturn _((LVAL name, LVAL val));
extern VOID xlthrow _((LVAL tag, LVAL val));
extern VOID xlsignal _((char FAR *emsg, LVAL arg));
extern VOID xltoplevel _((void));
extern VOID xlbrklevel _((void));
extern VOID xlcleanup _((void));
extern VOID xlcontinue _((void));

/* for xllist */
extern VOID xlcircular _((void));
#ifdef HASHFCNS
extern VOID xlsetgethash _((LVAL key, LVAL table, LVAL value));
#endif

/* for xlsubr */
extern int xlgetkeyarg _((LVAL key, LVAL *pval));
extern int xlgkfixnum _((LVAL key, LVAL *pval));
extern VOID xltest _((LVAL *pfcn, int *ptresult));
#ifdef AOKKEY
extern VOID xllastkey _((void));
#endif
extern int needsextension _((char *name));
extern int eql _((LVAL arg1, LVAL arg2));
extern int equal _((LVAL arg, LVAL arg2));
#ifdef KEYARG
extern LVAL xlkey _((void));
extern LVAL xlapp1 _((LVAL fun, LVAL arg));
extern int dotest1 _((LVAL arg1, LVAL fun, LVAL kfun));
extern int dotest2 _((LVAL arg1, LVAL arg2, LVAL fun, LVAL kfun));
extern int dotest2s _((LVAL arg1, LVAL arg2, LVAL fun, LVAL kfun));
#else
extern int dotest1 _((LVAL arg1, LVAL fun));
extern int dotest2 _((LVAL arg1, LVAL arg2, LVAL fun));
#endif
#ifdef COMPLX
extern FLOTYPE makefloat _((LVAL arg));
#endif

/* for xlobj */
extern int xlobsetvalue _((LVAL pair, LVAL sym, LVAL val));
extern int xlobgetvalue _((LVAL pair, LVAL sym, LVAL *pval));
extern VOID putobj _((LVAL fptr, LVAL obj));

/* for xlread */
extern LVAL tentry _((int ch));
extern int xlload _((char *fname, int vflag, int pflag));
extern int xlread _((LVAL fptr, LVAL *pval, int rflag, int pwflag));
extern int isanumber _((char *str, LVAL *pval));

/* for xlstruct */
extern LVAL xlrdstruct _((LVAL list));
extern VOID xlprstruct _((LVAL fptr, LVAL vptr, FIXTYPE plevel, int flag));

/* for xlfio */
extern VOID xlformat _((LVAL lfmt, LVAL stream));
extern LVAL getstroutput _((LVAL stream));

/* save/restore functions */
#ifdef SAVERESTORE
extern int xlirestore _((char *fname));
extern int xlisave _((char *fname));
#endif

/* package functions */
#ifdef PACKAGES
#define SYM_NOT_FOUND 0
#define SYM_INTERNAL  1
#define SYM_EXTERNAL  2
#define SYM_INHERITED 3

#define goodpackagep(x) (packagep(x) && ! null(getpacknames(x)))

extern LVAL xlisppack, xlkeypack, xluserpack;
extern VOID xlexport _((LVAL sym, LVAL pack));
extern VOID xlimport _((LVAL sym, LVAL pack));
extern LVAL xlfindpackage _((char FAR *name));
extern int xlfindsymbol _((char FAR *name, LVAL pack, LVAL *psym));
extern LVAL xlpackagename _((LVAL pack));
extern LVAL xlintern _((char FAR *name, LVAL pack));
extern LVAL xlgetpackage _((LVAL arg));
#endif

/* external procedure declarations */
extern VOID obsymbols _((void));    /* initialize oop symbols */
extern VOID ossymbols _((void));    /* initialize os symbols */
extern VOID xlsymbols _((void));    /* initialize interpreter symbols */
extern VOID xloinit _((void));      /* initialize object functions */
extern VOID xlsinit _((void));      /* initialize xlsym.c */
extern VOID xlrinit _((void));      /* initialize xlread.c */
extern VOID xlminit _((void));      /* init xldmem */
extern VOID xldinit _((void));      /* initilaixe debugger */
extern  int xlinit _((char *resfile));  /* xlisp initialization routine */
extern LVAL xleval _((LVAL expr));  /* evaluate an expression */
extern LVAL xlapply _((int argc));  /* apply a function to arguments */
extern LVAL evmethod _((LVAL obj, LVAL msgcls, LVAL method)); /* evaluate a method */
extern LVAL xlsubr _((char *sname, int type, LVAL (*fcn)(void),int offset));
                                /* enter a subr/fsubr */
extern LVAL xlenter _((char *name));/* enter a symbol */
extern LVAL findprop _((LVAL list, LVAL prp)); /* find a property in list */
extern LVAL xlmakesym _((char FAR *name));  /* make an uninterned symbol */
extern LVAL xlgetvalue _((LVAL sym));   /* get value of a symbol (checked) */
extern VOID xlsetvalue _((LVAL sym, LVAL val)); /* set the value of symbol */
extern LVAL xlxgetvalue _((LVAL sym));  /* get value of a symbol */
extern LVAL xlgetfunction _((LVAL sym));/* get functional value of a symbol */
extern LVAL xlxgetfunction _((LVAL sym));
                            /* get functional value of a symbol (checked) */
extern LVAL xlexpandmacros _((LVAL form));      /* expand macros in a form */
extern LVAL xlgetprop _((LVAL sym, LVAL prp));  /* get the value of a property */
extern VOID xlputprop _((LVAL sym, LVAL val, LVAL prp)); /*set value of property*/
extern VOID xlremprop _((LVAL sym, LVAL prp));  /* remove a property */
extern LVAL xlclose _((LVAL name, LVAL type, LVAL fargs, LVAL body, LVAL env, LVAL fenv));
                                /* create a function closure */
extern int hash _((char FAR *str, int len));    /* Hash the string */
extern int xlhash _((LVAL obj, int len));   /* Hash anything */

extern LVAL listify _((LVAL x));
extern LVAL stringify _((LVAL x));
extern LVAL vectify _((LVAL x));

#ifdef RANDOM
extern LVAL newrandom _((long));            /* create a random-state */
#endif

/* argument list parsing functions */
extern LVAL xlgetfile _((int outflag));     /* get a file/stream argument */
extern LVAL xlgetfname _((void));   /* get a filename argument */

/* error reporting functions  (don't *really* return at all) */
extern LVAL xltoofew _((void));     /* report "too few arguments" error */
extern VOID xltoomany _((void));    /* report "too many arguments" error */
extern VOID xltoolong _((void));    /* too long to process error */
extern LVAL xlbadtype _((LVAL arg));/* report "bad argument type" error */
extern LVAL xlerror _((char FAR *emsg, LVAL arg));  /* report arbitrary error */
extern VOID xlcerror _((char FAR *cmsg, char FAR *emsg, LVAL arg)); /*recoverable error*/
extern VOID xlerrprint _((char *hdr,char FAR *cmsg, char FAR *emsg, LVAL arg));
extern VOID xlbaktrace _((int n));  /* do a backtrace */
extern VOID xlabort _((char *emsg));    /* serious error handler */
extern VOID xlfail _((char *emsg));     /* xlisp error handler */
extern VOID xlbreak _((char FAR *emsg, LVAL arg));  /* enter break look */
extern VOID xlnoassign _((LVAL arg));   /* report assignment to constant error */
extern int xlcvttype _((LVAL arg));

#ifdef STSZ
extern VOID stchck _((void));
#endif

#ifdef SERVER
extern int initXlisp _((char *resfile));    /* Initialize, return error code */
extern int execXlisp _((char *cmd, int restype, 
        char FAR * FAR *resstr, LVAL * resval)); /* execute expression */
extern VOID wrapupXlisp _((void));          /* relinquish memory, quit */
#endif

extern int checkfeatures _((LVAL arg, int which));  /* features featuure */

extern int getslot _((void));	/* UNIX/Linux/Mac OS X */

#define NIL (&isnil)

#include "xlftab.h"
#include "xlglob.h"

/* Should be last in file: */
/* $putpatch.c$: "MODULE_XLISP_H_GLOBALS" */
