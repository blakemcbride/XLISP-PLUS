/* dosstuff.c - MS/DOS specific sources */
/* Handles Microsoft C (v4.0 or later), Turbo/Borland C (any version),
   TopSpeed C (any version), and Zortech C (version 2 or later) in large
   memory model. In addition handles Turbo/Borland C and TopSpeed C in
   medium memory model, and the "Z" (286 protected mode) and
   "X" (386 protected mode) models of Zortech C, and GCC (DJ Delorie port) */

/* the former files gccstuff.c and ztcstuff.c have been merged into this
   file so all MSDOS specific code is in one place.  TAA 12/92 */

/* Use at least version 1.12 of DJGCC. This file has been modified to use
   these latest GCC versions so that XLISP will run under DPMI.
   The distributed xlisp386 executable uses a modified go32 that uses
   Ralf Brown's swapping EXEC functions for the DOS shell. I do not
   distribute the modified code, but it is very easy to do yourself. */

#include "xlisp.h"
#include "osdefs.h"

#ifdef GCC
 #include <go32.h>
 #include <sys/farptr.h>
 #include <dos.h>
 #include <math.h>
 #ifdef TIMES
  #include <sys/time.h>
 #endif
#else
 #include <dos.h>
 #include <process.h>
 #include <math.h>
 #include <io.h>
 #include <float.h>
 #ifdef TIMES
  #include <time.h>
 #endif
#endif

#define LBSIZE 77

#if defined( __ZTC__ ) && !defined(DOS386)
#ifdef DOS16RM
extern void * _cdecl D16SegAbsolute(long);  /* undocumented, but necessary, function*/

unsigned _cdecl _stack = 48000;     /* bigger stack in this case */
#else
unsigned _cdecl _stack = 16384;     /* set up reasonable stack */
#endif
#endif
#ifdef __TURBOC__
#ifdef STSZ
unsigned _Cdecl _stklen = STSZ;         /* set up reasonable stack */
#else
unsigned _Cdecl _stklen = 16384;        /* set up reasonable stack */
#endif
#ifdef MEDMEM
unsigned _Cdecl _heaplen = 4096;    /* compress the near heap */
#endif
#endif

char *stackbase;

#ifdef MSC
/* MSC Doesn't define these */
#define MK_FP(seg,ofs) (((unsigned long)(seg)<<16) | (unsigned)(ofs))
#endif

#ifdef GCC
/* external functions */
extern void setfpcw(void);
/* The next three exist here because the first GCC didn't have the
   appropriate functions. I should use the builtins now, but why change? */
extern int doscall(int eax, int ebx, int ecx, int edx);
extern int doscalledx(int eax, int ebx, int ecx, int edx);
#ifdef GRAPHICS 

extern int calldisp(int eax, int ebx, int ecx, int edx);
extern int calldispdx(int eax, int ebx, int ecx, int edx);
extern void setpixel(int x, int y);
extern void setdrawmode(int mode);
extern void unsetdrawmode(void);
#endif
#endif

/* local variables */
/* TAA mod 8/92 made unsigned for high ASCII support */
static unsigned char lbuf[LBSIZE];
static int lcount, lindex;
static unsigned savestate;
static unsigned char savebrk;

#ifdef TIMES
unsigned long kbdtime;
#ifdef GCC
long stsecs;
#endif
#endif

/* Command history */
#define HISTSIZE (20)
static unsigned char *history[HISTSIZE] = {NULL};
int curhist = -1;

#ifdef GCC
static int istty;
extern unsigned short cmem; /* conventional memory selector */
#endif

#ifdef GRAPHICS
static unsigned char origmode;
static unsigned ourmode1=0, ourmode2=0;
#endif

/* forward declarations */
static void NEAR xinfo(void);
static void NEAR xflush(void);
static int  NEAR xgetc(void);
static void NEAR xputc(int ch);
static void NEAR setraw(void);
static void NEAR unsetraw(void);

#ifdef GCC
/* output buffering (not built into GCC) */
#define CHBSIZE 256
static char outbuf[CHBSIZE];
static char *outbufp = &outbuf[0];

void flushbuf()
{
    if (outbufp != &outbuf[0]) {
        doscall(0x4000,2,outbufp - &outbuf[0],(int)&outbuf[0]);
        outbufp = &outbuf[0];
    }
}

/* rewrite of getc and putc to handle CRLF kludge */

#ifdef FILETABLE
int osagetc(FILEP f)
{
    int val;
    FILE *fp = filetab[f].fp;
    
    /* read until a non \r found*/
    do {val = fgetc(fp);}
    while (val == '\r');
    if (val == '\032') 
        val = EOF; /* Fix for ctrl-Z */
    return val;
}
 
int osaputc(int ch, FILEP f)
{
    FILE *fp = filetab[f].fp;
    
    /* Add \r before \n */
    if (ch == '\n') fputc('\r', fp);
    return fputc(ch, fp);
}

#else

int osagetc(FILE *fp)
{
    int val;
    
    /* read until a non \r found*/
    do {val = fgetc(fp);}
    while (val == '\r');
    if (val == '\032') 
        val = EOF; /* Fix for ctrl-Z */
    return val;
}
 
int osaputc(int ch, FILE *fp)
{
    /* Add \r before \n */
    if (ch == '\n') fputc('\r', fp);
    return fputc(ch, fp);
}
#endif

#else /* not GCC */
#define flushbuf() fflush(stderr);
#endif

#ifdef __TURBOC__
#ifdef BIGNUMS
/* We need this because the one that Borland supplies doesn't work for
   exp values out of range */
unsigned char infp[8] = {0,0,0,0,0,0,0xf0, 0x7f};
unsigned char infn[8] = {0,0,0,0,0,0,0xf0, 0xff};

double myldexp(double val, int exp) {
    if (exp > DBL_MAX_EXP)
        return (val > 0 ? *(double *)&infp : *(double *)&infn);
    if (exp < DBL_MIN_EXP) return 0.0;
    return ldexp(val, exp);
}
#endif
#endif

#ifndef GCC
/* math error handler */

#ifdef __TSC__          /* Top Speed wants matherr to be function pointer! */
int newmatherr(struct exception *er)
#else
#if defined(__TURBOC__) && (__TURBOC__ >= 0x400)
int CDECL _matherr(struct exception *er) /* Borland renamed this function */
#else
int CDECL matherr(struct exception *er)
#endif
#endif
{
    char *emsg;

    switch (er->type) {
        case DOMAIN: emsg="domain"; break;
#ifdef BIGNUMS
        case OVERFLOW: return 1; /* xlmath3 will regularly overflow */
#else
        case OVERFLOW: emsg="overflow"; break;
#endif
        case PLOSS: case TLOSS: emsg="inaccurate"; break;
        case UNDERFLOW: return 1;
        default: emsg="????"; break;
    }
    xlerror(emsg,cvflonum(er->arg1));
    return 0; /* never happens */
}
#endif

/* osinit - initialize */

#ifdef GCC
VOID osinit(banner)
  char *banner;
{
#ifdef TIMES
    struct timeval now;
    gettimeofday(&now,0);
    stsecs = now.tv_sec;
#endif
    cmem = _go32_conventional_mem_selector();
#ifdef STSZ
    stackbase = (char *)&banner;    /* find base of stack */
#endif
    fprintf(stderr,"%s\n",banner);
    lposition = 0;
    lindex = 0;
    lcount = 0;
    setfpcw();  /* mask off fp interrupts */
    redirectout = (doscalledx(0x4400,1,0,0) & 0x81) != 0x81;
    redirectin =  (doscalledx(0x4400,0,0,0) & 0x81) != 0x81;
    setraw();
#ifdef GRAPHICS
    origmode = calldisp(0x0f00,0,0,0) & 0xff;   /* get display mode */
#endif
}

#else

VOID osinit(banner)
  char *banner;
{
#ifdef STSZ
    stackbase = (char *)&banner;    /* find base of stack */
#endif
#ifdef TIMES
    kbdtime = real_tick_count();    /* initialize run time to zero */
#endif

#ifdef __TSC__
    matherr = newmatherr;
#endif
    setvbuf(stderr,NULL,_IOFBF,256);

#ifdef DOS386
    redirectout = !isatty(fileno(stdout));
    redirectin = !isatty(fileno(stdin));
#else
    if (*(char FAR *)MK_FP(_psp,0x19) != *(char FAR *)MK_FP(_psp,0x1a))
        redirectout = TRUE;
    if (*(char FAR *)MK_FP(_psp,0x18) != *(char FAR *)MK_FP(_psp,0x1a))
        redirectin = TRUE;
#endif

    fprintf(stderr,"%s\n",banner);
    lposition = 0;
    lindex = 0;
    lcount = 0;
    setraw();

#if defined( __TURBOC__) || defined(MSC) || defined(__TSC__)
    /* let fp overflow pass and domain errors */
    _control87(EM_OVERFLOW|EM_INVALID,EM_OVERFLOW|EM_INVALID);
#endif
#ifdef __TURBOC__
    /* force raw mode for stderr */
    stderr->flags |= _F_BIN;
#endif
}
#endif

/* osfinish - clean up before returning to the operating system */
VOID osfinish()
{
#ifdef GCC
        flushbuf();
#endif
        unsetraw();
}

/* xoserror - print an error message */
VOID xoserror(msg)
  char *msg;
{
    fprintf(stderr, "error: %s\n", msg);
}

/* osrand - return next random number in sequence */
long osrand(rseed)
  long rseed;
{
    long k1;

    /* make sure we don't get stuck at zero */
    if (rseed == 0L) rseed = 1L;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
        rseed += 2147483647L;

    /* return a random number between 0 and MAXFIX */
    return rseed;
}

#ifdef FILETABLE
extern void gc(void);

int truename(char *name, char *rname)
{
    union REGS regs;
#if !(defined(MEDMEM) || defined(DOS386) || defined(GCC))
    struct SREGS sregs;
#endif
    int i;
    char *cp;
    int drive;          /* drive letter */
    char pathbuf[FNAMEMAX+1];   /* copy of path part of name */
    char curdir[FNAMEMAX+1];    /* current directory of drive */
    char *fname;        /* pointer to file name part of name */
    
    /* use backslashes consistantly */
    
    for (cp = name; (cp = strchr(cp, '/')) != NULL; *cp = '\\') ;
    
    /* parse any drive specifier */

    if ((cp = strrchr(name, ':')) != NULL) {
        if (cp != name+1 || !isalpha(*name)) return FALSE;
        drive = toupper(*name);
        name = cp+1;            /* name now excludes drivespec */
    }
    else {
        regs.h.ah = 0x19;   /* get current disk */
        intdos(&regs, &regs);
        drive = regs.h.al + 'A';
    }
    
    /* check for absolute path (good news!) */
    
    if (*name == '\\') {
        sprintf(rname,"%c:%s",drive,name);
    }
    else {
        strcpy(pathbuf, name);
        if ((cp = strrchr(pathbuf, '\\')) != NULL) {    /* path present */
            cp[1] = 0;
            fname = strrchr(name, '\\') + 1;
        }
        else {
            pathbuf[0] = 0;
            fname = name;
        }

        /* get the current directory of the selected drive */
        
        regs.h.ah = 0x47;
        regs.h.dl = drive + 1 - 'A';
#if defined(MEDMEM) || defined(GCC)
        regs.x.si = (unsigned) curdir;
        intdos(&regs, &regs);
#else
#ifdef DOS386
        regs.e.esi = (unsigned) curdir;
        intdos(&regs, &regs);
#else
        regs.x.si = (unsigned) FP_OFF(curdir);
        sregs.ds = (unsigned) FP_SEG(curdir);
        intdosx(&regs, &regs, &sregs);
#endif
#endif
        if (regs.x.cflag != 0) return FALSE;    /* invalid drive */

        /* peel off "..\"s */
        while (strncmp(pathbuf, "..\\", 3) == 0) {
            if (*curdir == 0) return FALSE;     /* already at root */
            strcpy(pathbuf, pathbuf+3);
            if ((cp=strrchr(curdir, '\\')) != NULL)
                *cp = 0;    /* peel one depth of directories */
            else
                *curdir = 0;    /* peeled back to root */
        }
        
        /* allow for a ".\" */
        if (strncmp(pathbuf, ".\\", 2) == 0)
            strcpy(pathbuf, pathbuf+2);
        
        /* final name is drive:\curdir\pathbuf\fname */

        if (strlen(pathbuf)+strlen(curdir)+strlen(fname)+4 > FNAMEMAX) 
            return FALSE;
        
        if (*curdir)
            sprintf(rname, "%c:\\%s\\%s%s", drive, curdir, pathbuf, fname);
        else
            sprintf(rname, "%c:\\%s%s", drive, pathbuf, fname);
    }
    
    /* lowercase the whole string */

    for (cp = rname; (i = *cp) != 0; cp++) {
        if (isupper(i)) *cp = tolower(i);
    }
    
    return TRUE;
}

LOCAL int NEAR getslot(VOID)
{
    int i=0;

    for (; i < FTABSIZE; i++)   /* look for available slot */
        if (filetab[i].fp == NULL) return i;

    gc();   /* is this safe??????? */

    for (i=0; i < FTABSIZE; i++) /* try again -- maybe one has been freed */
        if (filetab[i].fp == NULL) return i;

    xlfail("too many open files");

    return 0;   /* never returns */
}


#ifdef GCC

FILEP osopen(const char *name, const char *mode)
{
    int i=getslot();
    char bmode[10];
    char namebuf[FNAMEMAX+1];
    FILE *fp;
    
    if (!truename((char *)name, namebuf))
        strcpy(namebuf, name);  /* should not happen */

    if ((filetab[i].tname = malloc(strlen(namebuf)+1)) == NULL) {
        xlfail("insufficient memory");
    }
    
    
    strcpy(bmode, mode);
    strcat(bmode, "b");

    if ((fp = fopen(name,bmode)) == NULL) {
        free(filetab[i].tname);
        return CLOSED;
    }

    filetab[i].fp = fp;

    strcpy(filetab[i].tname, namebuf);

    return i;
}
    
#else

FILEP osaopen(const char *name, const char *mode)
{
    int i=getslot();
    char namebuf[FNAMEMAX+1];
    FILE *fp;

    if (!truename((char *)name, namebuf))
        strcpy(namebuf, name);  /* should not happen */

    if ((filetab[i].tname = malloc(strlen(namebuf)+1)) == NULL) {
        xlfail("insufficient memory");
    }
    
    
    if ((fp = fopen(name,mode)) == NULL) {
        free(filetab[i].tname);
        return CLOSED;
    }

    filetab[i].fp = fp;

    strcpy(filetab[i].tname, namebuf);

    return i;
}


FILEP osbopen(const char *name, const char *mode)
{
    char bmode[10];

    strcpy(bmode,mode); strcat(bmode,"b");  

    return osaopen(name, bmode);
}
#endif

VOID osclose(FILEP f)
{
    fclose(filetab[f].fp);
    free(filetab[f].tname);
    filetab[f].tname = NULL;
    filetab[f].fp = NULL;
}

#else

#ifdef GCC
FILE * CDECL osopen(const char *name, const char *mode)
{
    char bmode[10];
    strcpy(bmode,mode); strcat(bmode,"b");
    return (fopen(name,bmode));
}
#else
/* osbopen - open a binary file */
FILE * CDECL osbopen(const char *name, const char *mode)
{
    char bmode[10];
    strcpy(bmode,mode); strcat(bmode,"b");
    return (fopen(name,bmode));
}
#endif
#endif

#ifdef PATHNAMES
/* ospopen - open for reading using a search path */
FILEP ospopen(char *name, int ascii)
{
    FILEP fp;
    char *path = getenv(PATHNAMES);
    char *newnamep;
    char ch;
    char newname[256];

    /* don't do a thing if user specifies explicit path */
#ifdef GCC
    if ((strchr(name,'/') != NULL && strchr(name, '\\') != NULL) ||
        path == NULL)
        return (ascii? OSAOPEN : OSBOPEN)(name, "r");
#else
    if (strchr(name,'/') != NULL && strchr(name, '\\') != NULL)
#ifdef FILETABLE
        return (ascii? osaopen: osbopen)(name,"r");
#else
        return fopen(name,(ascii? "r": "rb"));
#endif
#endif

    do {
        if (*path == '\0')  /* no more paths to check */
            /* check current directory just in case */
#ifdef GCC
            return (ascii? OSAOPEN : OSBOPEN)(name, "r");
#else
#ifdef FILETABLE
            return (ascii? osaopen: osbopen)(name,"r");
#else
            return fopen(name,(ascii? "r": "rb"));
#endif
#endif

        newnamep = newname;
        while ((ch=*path++) != '\0' && ch != ';' && ch != ' ')
            *newnamep++ = ch;

        if (ch == '\0') path--;

        if (newnamep != newname &&
            *(newnamep-1) != '/' && *(newnamep-1) != '\\')
            *newnamep++ = '/';  /* final path separator needed */
        *newnamep = '\0';

        strcat(newname, name);
#ifdef GCC
        fp = (ascii? OSAOPEN : OSBOPEN)(newname, "r");
#else
#ifdef FILETABLE
        fp = (ascii? osaopen: osbopen)(newname,"r");
#else
        fp = fopen(newname, ascii? "r": "rb");
#endif
#endif
    } while (fp == CLOSED); /* not yet found */

    return fp;
}
#endif

/* rename argument file as backup, return success name */
/* For new systems -- if cannot do it, just return TRUE! */

int renamebackup(char *filename) {
    char *bufp, ch=0;

    strcpy(buf, filename);  /* make copy with .bak extension */

    bufp = &buf[strlen(buf)];   /* point to terminator */
    while (bufp > buf && (ch = *--bufp) != '.' && ch != '/' && ch != '\\') ;


    if (ch == '.') strcpy(bufp, ".bak");
    else strcat(buf, ".bak");

    remove(buf);

    return !rename(filename, buf);
}

#define beep() xputc(7)

/* listmatches -- list interned symbols that match to current symbol name */

static void searchobarray(LVAL array, unsigned char *st, int l, int append,
                          int lpos, int *matchcount, LVAL *firstmatch,
                          int *endpos)
{
    unsigned char target[STRMAX+1];
    LVAL sym;       /* obarray accessing */
    int index;      /* obarray index */
    int i;
    unsigned char FAR *stp;  /* string in obarray */
    unsigned char c1;
    int j;
#ifdef READTABLECASE
    LVAL rtcase = getvalue(s_rtcase);
    int low=0, up=0;
    unsigned char *targ;
#endif

    for (i=0; i <= l; i++) {
        c1 = st[i];
#ifdef READTABLECASE
        if (rtcase==k_invert) target[i]=ISLOWER(c1) ? (low++, TOUPPER(c1)):
                                        (ISUPPER(c1) ? (up++, TOLOWER(c1)):c1);
        else if (rtcase==k_downcase) target[i]= ISUPPER(c1) ? TOLOWER(c1) : c1;
        else if (rtcase==k_preserve) target[i] = c1;
        else target[i]= ISLOWER(c1) ? TOUPPER(c1) : c1;
#else
        target[i] = ISLOWER(c1) ? TOUPPER(c1) : c1;
#endif
    }
#ifdef READTABLECASE
    if (low>0 && up>0) { /* invert -- but not inverted! */
        rtcase=k_preserve;
        strcpy((char *)target, (char *)st);
    }
#endif

    for (index = HSIZE; index-- > 0; ) {
        for (sym = getelement(array, index); !null(sym); sym = cdr(sym)) {
            stp = (unsigned char FAR *)getstring(getpname(car(sym)));
#ifdef READTABLECASE
            targ = target;
            if (rtcase==k_invert && (stp[0]==target[0]||stp[0]==st[0])) {
                /* must check for mixed case table entry */
                up=0; low=0;
                for (i=0; (c1 = stp[i]) != '\0' ; i++) {
                    if (ISUPPER(c1)) {
                        up++;
                        if (low>0) {targ=st; break;}
                    }
                    if (ISLOWER(c1)) {
                        low++;
                        if (up>0) {targ=st; break;}
                    }
                }
            }
            for (i = 0; stp[i] == targ[i]; i++) {
#else
            for (i = 0; stp[i] == target[i]; i++) {
#endif
                if (i == l) {
                    if (append) {
                        j = 0;
                        while (TRUE) {
                            c1 = stp[++i];
#ifdef READTABLECASE
                            if (rtcase==k_invert) {
                                if(!(up>0 && low>0)) {
                                    if (ISUPPER(c1)) c1 = TOLOWER(c1);
                                    else if (ISLOWER(c1)) c1 = TOUPPER(c1);
                                }
                            }
                            else if (rtcase!=k_preserve) {
                                if (ISUPPER(c1)) c1 = TOLOWER(c1);
                            }
#else
                            if (ISUPPER(c1)) c1 = TOLOWER(c1);
#endif                          
                            if (*matchcount==0) {
                                lbuf[lpos+j] = c1;
                            }
                            else if (lbuf[lpos+j] != c1) break;
                            if (c1 == '\0' || lpos+j >= LBSIZE-1) break;
                            j++;
                        }
                        lbuf[lpos+j] = ' '; /* put space in buffer */
                        *endpos = lpos+j;
                    }
                    if (++(*matchcount)==1 && append)
                        *firstmatch = car(sym);
                    else {
                        /* first to print? Start new line */
                        if (*matchcount==1 || (*matchcount==2 && append)) {
                            xputc('\r'); xputc('\n');
                        }
                        if (*matchcount==2 && append) {
                            xlprint(getvalue(s_debugio), *firstmatch, TRUE);
                            xputc(' ');
                            lposition++;
                        }
                        STRCPY(buf, (char FAR *)stp);
                        if (lposition + strlen(buf) > 77) {
                            lposition = 0;
                            xputc('\r'); xputc('\n');
                        }
                        xlprint(getvalue(s_debugio), car(sym), TRUE);
                        xputc(' ');
                        lposition++;
                    }
                    break;
                }
            }
        }
    }
}   

static void listmatches(void)
{
    unsigned char *st = &lbuf[lposition-1];     /* string to match */
    int l=0;        /* string length */
    int lpossave = lposition;   /* save to restore later */
    int matchcount = 0;     /* number of matches */
    int matchend = lposition;   /* end of match */
    int append = lposition == lcount; /* append partial matches */
    LVAL firstmatch;
#ifdef PACKAGES
    int i;
    unsigned char *st2 = NULL;
    int external_only = FALSE;
    LVAL pack = getvalue(s_package);
#endif
    
    if (lposition == 0) return; /* no string */

    /* find string start */
    while (l < lposition && !strchr(" \"\'`()\\|", *st)) {
#ifdef PACKAGES
        if (*st == ':') {
            if (st2 == st+1)  /* two in a row */
                external_only = FALSE;
            else
                external_only = TRUE;
            st2 = st;
        }
#endif
        st--;
        l++;
    }

    if (l == 0) return; /* no string */

    st++;
    l--;

#ifdef PACKAGES
    /* check for package name */
    if (st2 != NULL) {
        if (st == st2)
            pack = xlkeypack;   /* zero length name */
        else {
            i=st2-st;
            buf[i] = 0;
            /* cheat and force uppercase */
            while (i-- > 0)
                buf[i] = (ISLOWER(st[i])? TOUPPER(st[i]) : st[i]);
            pack = xlfindpackage(buf);
            if (!packagep(pack))
                return; /* no package of that name */
        }
        if (*(++st2) == ':') st2++; /* start of target string */
        l -= (st2-st);
        st = st2;
        if (l < 0) return; /* no string */
    }
#endif

    lposition = 0;
#ifdef PACKAGES
    searchobarray(getextsyms(pack), st, l, append, lpossave, &matchcount, &firstmatch, &matchend);
    if (!external_only) {
        searchobarray(getintsyms(pack), st, l, append, lpossave, &matchcount, &firstmatch, &matchend);
        pack = getuses(pack);   /* check out imports */
        for (; consp(pack); pack = cdr(pack))
            searchobarray(getextsyms(car(pack)), st, l, append, lpossave, &matchcount, &firstmatch, &matchend);
    }
#else
    searchobarray(getvalue(obarray), st, l, append, lpossave, &matchcount, &firstmatch, &matchend);
#endif
    if (append) lposition = lcount = matchend;
    else lposition = lpossave;
    if (matchcount > 1) beep(); /* signal multiple matches */
    if (matchcount > 1 || !append) {
        xputc('\r');
        xputc('\n');
    }
    else {
        for (l=lpossave; l-- >0; ) xputc(8);
        /* appending and not multiple matches -- overwrite */
    }
}


/* ostgetc - get a character from the terminal */
int ostgetc()
{
    int ch;
#ifndef GCC
    union REGS regs;
#ifndef DOS386
    struct SREGS segregs;
#endif
#endif

    /* check for a buffered character */
    if (lcount-- > 0)
        return (lbuf[lindex++]);

    /* get an input line */
    if (!null(getvalue(s_dosinput)) && !redirectin && !redirectout) {
#ifdef TIMES
        unsigned long kbtime = real_tick_count();
#endif
        flushbuf();
#ifdef GCC   /* Doing this with GCC is significantly different! */
        memset(lbuf, 0, LBSIZE);
        doscall(0x4401,2,0,savestate);  /* restore raw state */
        doscall(0x3f00, 0, LBSIZE-1, (int)lbuf);
        doscall(0x4401,2,0,savestate | 0x20);           /* set raw bit */
        lcount = strlen((char *)lbuf);
        if (tfp!=CLOSED)    /* convoluted because of ugly text mode handling
                                with djgpp (sorry, DJ Delorie) */
            for (lindex=0; lindex < lcount; lindex++) 
                OSAPUTC(lbuf[lindex],tfp);
        lposition = 0;
        lindex = 1;
        lcount--;
#ifdef TIMES
        kbdtime += real_tick_count() - kbtime;
#endif
        return (lbuf[0]);
#else
        lindex = 2;
        lbuf[0] = LBSIZE - 2;
        regs.x.ax = 0x0A00;
#ifdef DOS386
        regs.x.ax = 0x0A00;
        regs.e.edx = (unsigned int) lbuf;
        intdos(&regs,&regs);
#else
        regs.x.dx = FP_OFF(lbuf);
        segregs.ds = FP_SEG(lbuf);
        intdosx(&regs,&regs,&segregs);
#endif
        putchar('\n');
        lcount = lbuf[1];
        lbuf[lcount+2] = '\n';
        if (tfp!=CLOSED) OSWRITE(&lbuf[2],1,lcount+1,tfp);
        lposition = 0;
#ifdef TIMES
        kbdtime += real_tick_count() - kbtime;
#endif
        return (lbuf[lindex++]);
#endif
    }
    else {  /* internal command editing */
    try_again:
    for (lcount = 0, lposition=0, lbuf[0] = '\0'; ; ) 
        switch (ch = xgetc()) {
        case C_TAB:  /* list matches */
            listmatches();
            for (lindex = 0; lindex < lcount;)
                xputc(lbuf[lindex++]);
            for (lindex = lcount - lposition; lindex-- > 0; )
                xputc(8);
            break;
        case '\r':  /* end of line */
        case '\n':
                lbuf[lcount] = 0;
                if (history[0] == NULL || 
                    strcmp((char *)history[curhist < 0?0:curhist], 
                           (char *)lbuf) != 0) {
                    if (lcount > 0) {
                        /* New line to put in history buffer */
                        if (history[HISTSIZE-1] != NULL) 
                            free(history[HISTSIZE-1]);
                        memmove(&history[1], &history[0], 
                            (HISTSIZE-1)*sizeof(char *));
                        history[0] = (unsigned char *)malloc(lcount+1);
                        strcpy((char *)history[0], (char *)lbuf);
                        curhist = -1;   /* reset lookup pointer */
                    }
                }
                else if (curhist >= 0) curhist--; /* TAA MOD 4/95 */
                lbuf[lcount] = '\n';
                xputc('\r'); xputc('\n');
                lposition = 0;
#ifdef GCC
                if (tfp!=CLOSED)
                    for (lindex=0; lindex <= lcount;) 
                        OSAPUTC(lbuf[lindex++],tfp);
#else
                if (tfp!=CLOSED) OSWRITE(lbuf,1,lcount+1,tfp);
#endif
                lindex = 0;
                return (lbuf[lindex++]);
        case C_BS: /* backspace -- delete to right of cursor */
                if (lposition != 0) {
                    lcount--;
                    lposition--;
                    strcpy((char *)&lbuf[lposition], (char *)&lbuf[lposition+1]);
                    xputc(8);   /* back up */
                    for (lindex = lposition; lindex < lcount; )
                        xputc(lbuf[lindex++]);
                    xputc(' '); /* "erase" at end of line */
                    lindex = lcount - lposition + 1;
                    while (lindex--) xputc(8);  /* back up */
                }
                else beep();
                break;
        case C_DEL: /* Delete -- delete at cursor */
                if (lcount > lposition) {
                    lcount--;
                    strcpy((char *)&lbuf[lposition], (char *)&lbuf[lposition+1]);
                    for (lindex = lposition; lindex < lcount; )
                        xputc(lbuf[lindex++]);
                    xputc(' '); /* "erase" at end of line */
                    lindex = lcount - lposition + 1;
                    while (lindex--) xputc(8);  /* back up */
                }
                else beep();
                break;
        case C_LA: /* Left arrow */
                if (lposition != 0) {
                    lposition--;
                    xputc(8);
                }
                else beep();
                break;
        case C_RA: /* Right arrow */
                if (lposition < lcount) 
                    xputc(lbuf[lposition++]);
                else
                    beep();
                break;
        case C_HOME: /* Home -- goto beginning of line */
                while (lposition != 0) {
                    lposition--;
                    xputc(8);
                }
                break;
        case C_END: /* End -- goto end of line */
                while (lposition < lcount) xputc(lbuf[lposition++]);
                break;
        case C_UA: /* Up arrow -- goto previous line */
                if (curhist != HISTSIZE-1 && history[curhist+1] != NULL) {
                    curhist++;
                    goto join_downarrow;
                }
                beep();
                break;
        case C_DA: /* Down arrow -- goto next line */
                if (lcount > 0 || curhist < 0) { /* empty line in history --
                                                    restore current line */
                    if (curhist <= 0) { /* bumped limit */
                        beep();
                        break;
                    }
                    curhist--;  /* restore next line */
                }
        join_downarrow:
                strcpy((char *)lbuf, (char *)history[curhist]);
                if (strlen((char *)lbuf) < lcount) {
                    /* old line longer -- must "erase" it */
                    lindex = lposition - strlen((char *)lbuf);
                    while (lindex-- > 0) {
                        xputc(8);
                        lposition--;
                    }
                    for (lindex = lcount-lposition ; lindex-- > 0; )
                        xputc(' ');
                    lposition = lcount;
                }
                for (lindex = lposition; lindex-- > 0; )
                    xputc(8);
                lcount = lposition = strlen((char *)lbuf);
                for (lindex = 0; lindex < lposition;)
                    xputc(lbuf[lindex++]);
                break;
        case C_TOPLEV:      /* control-c */
                xflush();
                xltoplevel();
        case C_CLEAN:       /* control-g */
                xflush();
                xlcleanup();
        case C_CONT:        /* control-p */
                xflush();
                xlcontinue();
        case C_EOF:     /* control-z */
                xflush();
                return (EOF);
        case C_ESC: /* ESCAPE */
                for (lindex = lposition; lindex-- > 0;)
                    xputc(8);   /*backspace to start of line */

                for (lindex = lcount; lindex-- > 0; )
                    xputc(' '); /* space to end of line */
                
                for (lindex = lcount; lindex-- > 0;)
                    xputc(8);   /*backspace to start of line */

                goto try_again;
        case C_STATUS:  /* control-t */
                xinfo();        
                /* redraw line */
                xflush();
                goto try_again;
        default:
                if (ch >= 0x20 && ch < 0x100 && lcount < LBSIZE-1) {
                    memmove(&lbuf[lposition+1], &lbuf[lposition], 
                        lcount-lposition + 1);
                    lbuf[lposition] = ch;
                    xputc(ch); 
                    lposition++;
                    lcount++;
                    for (lindex = lposition; lindex < lcount;)
                        xputc(lbuf[lindex++]);
                    lindex = lcount - lposition;
                    while (lindex-- != 0) xputc(8);
                }
                else beep();
        } /* end of character case statement and for loop */
    }
}

/* ostputc - put a character to the terminal */
VOID ostputc(ch)
  int ch;
{
    /* check for control characters */
    oscheck();

    /* output the character */
    if (ch == '\n') {
        xputc('\r'); xputc('\n');
        lposition = 0;
    }
    else if (ch == '\t')
        do { xputc(' '); } while (++lposition & 7);
    else {
        xputc(ch);
        lposition++;
   }

   /* output the character to the transcript file */
   if (tfp!=CLOSED)
#ifdef OSAPUTC
        OSAPUTC(ch,tfp);
#else
        OSPUTC(ch, tfp);
#endif
}

/* osflush - flush the terminal input buffer */
VOID osflush()
{
    lindex = lcount = 0;
}

/* oscheck - check for control characters during execution */
VOID oscheck()
{
    int ch;

#ifdef GCC
    if (!redirectin && (ch = (doscall(0x600,0,0,0xff) & 0xff)) != 0)
#else
    if (!redirectin && (ch = (bdos(6,0xFF,0) & 0xff)) != 0)
#endif
        switch (ch) {
        case C_BREAK:   /* control-b */
            xflush();
            xlbreak("**BREAK**",s_unbound);
            break;
        case C_TOPLEV:  /* control-c */
            xflush();
            xltoplevel();
            break;
        case C_PAUSE:   /* control-s */
            xgetc();    /* paused -- get character and toss */
            break;
        case C_STATUS:  /* control-t */
            xinfo();
            break;
        }
}

/* xinfo - show information on control-t */
static VOID NEAR xinfo()
{
#ifdef STSZ
    int i;
    sprintf(buf,
            "\n[ Free: %ld, Total: %ld, GC calls: %ld,\n"
            "  Edepth: %d, Adepth %d, Sdepth: %d ]",
            nfree, total, gccalls, xlstack-xlstkbase,
            xlargstktop-xlsp, STACKREPORT(i));
#else
    sprintf(buf,
            "\n[ Free: %ld, Total: %ld, GC calls: %ld,\n"
            "  Edepth: %d, Adepth %d ]",
            nfree, total, gccalls, xlstack-xlstkbase,
            xlargstktop-xlsp);
#endif
    errputstr(buf);

    flushbuf();
}

/* xflush - flush the input line buffer and start a new line */
static VOID NEAR xflush()
{
    osflush();
    ostputc('\n');
}

/* xgetc - get a character from the terminal without echo */
/* TAA MOD 12/92 to handle extended keys (return 256+code) */
#ifdef GCC
static int xgetc()
{
    int ch;
#ifdef TIMES
    unsigned long kbtime = real_tick_count();
#endif
    flushbuf();

    if (redirectin) {
        unsigned char chbuf[1];
        doscall(0x3f00,2,1,(int)chbuf);
        ch = chbuf[0];
    }
    else {
        if ((ch=doscall(0x700,0,0,0) & 0xff) == 0)
            ch = 256 + (doscall(0x700,0,0,0) & 0xff);
    }
#ifdef TIMES
    kbdtime += real_tick_count() - kbtime;
#endif
    return ch;
}

#else
static int NEAR xgetc()
{
    int ch;
#ifdef TIMES
    unsigned long kbtime = real_tick_count();
#endif
    flushbuf();

    if (!redirectin) {
        if ((ch = bdos(7,0,0) & 0xFF) == 0)
            ch = 256 + (bdos(7,0,0) & 0xFF);
    }
    else {
        unsigned char temp[1];  /* corrected to UNSIGNED 12/92 */
#ifdef __TURBOC__
        _read(2, temp, 1);
#else
#if defined(MSC) || defined(__TSC__)
        int dummy;
        _dos_read(2, temp, 1, &dummy);
#else
        read(2, temp, 1);
#endif
#endif
        ch = temp[0];   
    }
#ifdef TIMES
    kbdtime += real_tick_count() - kbtime;
#endif
    return ch;
}
#endif

/* xputc - put a character to the terminal */
#ifdef GCC
static void xputc(ch)
  int ch;
{
    *outbufp++ = ch;
    if (ch == '\n' || outbufp == &outbuf[CHBSIZE]) flushbuf();
}

#else
static void NEAR xputc(ch)
  int ch;
{
    fputc(ch,stderr);
    if (ch == '\n') flushbuf(); /* flush on each line */
}
#endif

#ifdef OVERLAY
/* Ralf Brown's SPAWNO package */
#ifdef __TSC__
int cdecl spawnvo(const char *overlay_path, const char *name, va_list args) ;
#else
#include "spawno.h"
#endif
#endif

/* xsystem - execute a system command */
#ifdef GCC
LVAL xsystem()
{
    char command[128],*s;
    int Err;
#ifdef TIMES
    unsigned long kbtime = real_tick_count();   /* subtract off as though from kbd */
#endif

    if (moreargs()) {
        s = getstring(xlgastring());
        strcpy(command,s);
        xllastarg();
    }
    else
        strcpy(command, getenv("COMSPEC"));

    unsetraw();
    Err = system(command);
    setraw();
    return ( Err == 0 ? s_true : cvfixnum((FIXTYPE)Err));
#ifdef TIMES
    kbdtime += real_tick_count() - kbtime;
#endif
}
#else
LVAL xsystem()
{
    char *cmd[4];
    int ok;
#ifdef TIMES
    unsigned long kbtime = real_tick_count();   /* subtract off as though from kbd */
#endif

    cmd[0] = getenv("COMSPEC");
    if (moreargs()) {
        cmd[1] = "/c";
#ifdef MEDMEM
        MEMCPY(buf, getstring(xlgastring()), STRMAX);
        cmd[2] = buf;
#else
        cmd[2] = getstring(xlgastring());
#endif
        cmd[3] = NULL;
        xllastarg();
    }
    else {
        cmd[1] = NULL;
    }
    unsetraw();

#ifdef OVERLAY
    ok = spawnvo("/",cmd[0], cmd);
#else
    ok = spawnv(P_WAIT,cmd[0], cmd);
#endif

    setraw();
#ifdef TIMES
    kbdtime += real_tick_count() - kbtime;
#endif
    return (ok == 0 ? s_true : cvfixnum((FIXTYPE)errno));
}
#endif

/* xgetkey - get a key from the keyboard */
LVAL xgetkey()
{
    xllastarg();
    return (cvfixnum((FIXTYPE)xgetc()));
}

/* Mode changing */
#ifdef GCC
#ifdef GRAPHICS
static VOID setgmode(int ax, int bx)
{
    calldisp(ax,bx,0,0);
}

#endif

/* setraw -- set raw mode */
static VOID setraw()
{

    savestate = doscalledx(0x4400,2,0,0) & 0xff; /* get device status */
    doscall(0x4401,2,0,savestate | 0x20);           /* set raw bit */
    savebrk = doscalledx(0x3300,0,0,0); /* get ctrl-break status */
    doscall(0x3301,0,0,0);              /* disable */
#ifdef GRAPHICS
    origmode = calldisp(0x0f00, 0, 0, 0);   /* get display mode */
    if (ourmode1 != 0)  /* mode was changed -- use it */
        setgmode(ourmode1,ourmode2);
#endif
}

/* unsetraw -- restore original mode */
static VOID unsetraw()
{
    doscall(0x4401,2,0,savestate);  /* restore raw state */
    doscall(0x3301,0,0,savebrk);    /* reset break */
#ifdef GRAPHICS
    /* restore original mode if it has changed */
    if ((ourmode1 != 0) && (ourmode2 != origmode))
        setgmode(origmode,0);
#endif
}

#else

#ifdef GRAPHICS
static VOID NEAR setgmode(int ax, int bx)
{
    union REGS regs;
    regs.x.ax = ax;
    regs.x.bx = bx;
    int86(0x10, &regs, &regs);
}
#endif

/* setraw -- set raw mode */
static VOID NEAR setraw(void)
{
    union REGS regs;

    regs.x.ax = 0x4400; /* get device status */
    regs.x.bx = 2;
    intdos(&regs,&regs);
    regs.h.dh = 0;
    savestate = regs.x.dx;
    regs.x.ax = 0x4401;
    regs.h.dl |= 0x20;
    intdos(&regs,&regs);

    regs.x.ax = 0x3300; /* get ctrl-break status */
    intdos(&regs,&regs);
    savebrk = regs.h.dl;
    regs.x.ax = 0x3301;
    regs.h.dl = 0;
    intdos(&regs,&regs);

#ifdef GRAPHICS
    regs.x.ax = 0x0f00; /* get mode */
    int86(0x10, &regs, &regs);
    origmode = regs.h.al;
    if (ourmode1 != 0)  /* mode was changed -- use it */
        setgmode(ourmode1,ourmode2);
#endif
}

/* unsetraw -- restore original mode */
static VOID NEAR unsetraw(void)
{
    union REGS regs;

    regs.x.ax = 0x4401;
    regs.x.bx = 2;
    regs.x.dx = savestate;
    intdos(&regs,&regs);
    regs.x.ax = 0x3301;
    regs.h.dl = savebrk;
    intdos(&regs,&regs);

#ifdef GRAPHICS
    if ((ourmode1 !=0) && (ourmode2 != origmode))
        setgmode(origmode,0);
#endif
}

#endif

/* ossymbols - enter os specific symbols */
VOID ossymbols()
{
}

#ifdef GRAPHICS

#ifdef GCC
extern int bytesperline;
#else
static union REGS regin, regout;
#endif
static int xpos=0, ypos=0;
static int Xmax=-1, Ymax=-1;
static unsigned char drawvalue=15;

#ifdef GCC
/* function goto-xy which set/obtains cursor position */
LVAL xgotoxy()
{
    FIXTYPE x, y;
    int pos;
    LVAL oldpos;
    
    flushbuf();

    pos = calldispdx(0x300, 0, 0, 0);   /* get cursor position */

    oldpos = cons(cvfixnum((FIXTYPE)(pos & 0xff)),
                  cons(cvfixnum((FIXTYPE)((pos >> 8) & 0xff)),NIL));
    
    if (moreargs()) {
        unsigned short xmax = _farpeekw(cmem, 0x44a);
        unsigned char ymax = _farpeekb(cmem, 0x484);
        x = getfixnum(xlgafixnum());
        y = getfixnum(xlgafixnum());
        xllastarg();
        if (x < 0) x = 0;   /* check for in bounds */
        else if (x >= xmax) x = xmax - 1;
        if (y < 0) y = 0;
        else if (ymax != 0) {
            if (y > ymax) y = ymax;
        }
        else if (y > 24) y = 24;
        
        calldisp(0x200, 0, 0, x + (y<<8));  /* set new position */

        lposition = x;
    }
        
    return oldpos;
}

LVAL xcls() /* clear the screen */
{
    int xsize, ysize, attrib;
    
    flushbuf();
    lposition = 0;

    xsize = _farpeekw(cmem, 0x44a);
    ysize = _farpeekb(cmem, 0x484);
    if (ysize == 0) ysize = 24;
    attrib = (ourmode1 > 3 ? 0 : _farpeekb(cmem,0xb8001));
    
    calldisp(0x600, attrib << 8, 0, xsize + (ysize << 8));  /* clear region */
    calldisp(0x200, 0, 0, 0);       /* home cursor */

    return NIL;
}

LVAL xcleol()   /* clear to end of line */
{
    int oldpos;
    
    flushbuf();
    
    oldpos = calldispdx(0x300, 0, 0, 0);    /* get old position */

    lposition = oldpos & 0xff;      /* just to be sure */
    calldisp(0x0600,                /* clear region */
        (ourmode1 > 3 ? 0 : _farpeekb(cmem,0xb8001) << 8), /* atrrib*/
        oldpos,
        _farpeekw(cmem, 0x44a) - 1 + (oldpos & 0xff00));
    return NIL;
}

#else  /* Not GCC */

/* function goto-xy which set/obtains cursor position */
LVAL xgotoxy()
{
    union REGS regs;
    FIXTYPE x, y;
    LVAL oldpos;
#ifdef DOS16RM  /* kludge for 80286 protected mode */
    unsigned char *basemem = D16SegAbsolute(0L);
#endif
#ifdef DOS386
    unsigned char _far *basemem;
#endif

    flushbuf();

    regs.h.ah = 0x3;    /* get old position */
    regs.h.bh = 0;
    int86(0x10, &regs, &regs);
    oldpos = cons(cvfixnum((FIXTYPE)regs.h.dl),
                  cons(cvfixnum((FIXTYPE)regs.h.dh),NIL));

    if (moreargs()) {
        x = getfixnum(xlgafixnum());
        y = getfixnum(xlgafixnum());
        xllastarg();
#ifdef DOS386
        basemem = _x386_mk_protected_ptr(0L);
#endif
        if (x < 0) x = 0;   /* check for in bounds */
#ifdef DOS16RM
        else if (x >= *(unsigned int FAR *)(basemem+0x44a))
            x = *(unsigned int FAR *)(basemem+0x44a) - 1;
#else
#ifdef DOS386
        else if (x >= *(unsigned short _far *)(basemem+0x44a))
            x = *(unsigned short _far *)(basemem+0x44a) - 1;
#else
        else if (x >= *(unsigned int FAR *) 0x44aL)
            x = *(unsigned int FAR *) 0x44aL - 1;
#endif
#endif
        if (y < 0) y = 0;
#if defined(DOS16RM) || defined(DOS386)
        else if (*(basemem+0x484) != 0) {
            if (y > *(basemem+0x484))
                y = *(basemem+0x484);
        }
#else
        else if (*(unsigned char FAR *) 0x484L != 0) {
            if (y > *(unsigned char FAR *) 0x484L)
                y = *(unsigned char FAR *) 0x484L;
        }
#endif
        else if (y > 24) y = 24;

#ifdef DOS386
        _x386_free_protected_ptr(basemem);
#endif

        regs.h.ah = 0x2;    /* set new position */
        regs.h.dl = (unsigned char)x;
        regs.h.dh = (unsigned char)y;
        regs.h.bh = 0;

        int86(0x10, &regs, &regs);
        lposition = (int)x;
    }

    return oldpos;
}

#ifdef DOS386
LVAL xcls() /* clear the screen */
{
    union REGS regs;
    int xsize, ysize, attrib;
    unsigned char _far *basemem = _x386_mk_protected_ptr(0L);

    flushbuf();
    lposition = 0;

    xsize = *(unsigned short _far *)(basemem+0x44a);
    ysize = (*(basemem+0x484) != 0 ? *(basemem+0x484) : 24);
    attrib = (ourmode1 > 3 ? 0 : *(basemem+0xb8001));

    regs.x.ax = 0x0600;
    regs.h.bh = attrib;
    regs.x.cx = 0;
    regs.h.dh = ysize;
    regs.h.dl = xsize;
    int86(0x10, &regs, &regs);
    regs.h.ah =0x2;             /* home cursor */
    regs.x.dx = 0;
    regs.h.bh = 0;
    int86(0x10, &regs, &regs);
    _x386_free_protected_ptr(basemem);
    return NIL;
}

LVAL xcleol()   /* clear to end of line */
{
    union REGS regs;
    unsigned char _far *basemem = _x386_mk_protected_ptr(0L);

    flushbuf();

    regs.h.ah = 0x3;    /* get old position */
    regs.h.bh = 0;
    int86(0x10, &regs, &regs);  /* x position in regs.h.dl, y in regs.h.dh */
    lposition = regs.h.dl;      /* just to be sure */
    regs.x.cx = regs.x.dx;
    regs.h.dl = (*(unsigned short _far *)(basemem+0x44a)) -1;/* x size */
    regs.h.bh = (ourmode1 > 3 ? 0 : *(basemem+0xb8001)); /* atrrib*/
    regs.x.ax = 0x0600;         /* scroll region */
    int86(0x10, &regs, &regs);
    _x386_free_protected_ptr(basemem);
    return NIL;
}
#else
LVAL xcls() /* clear the screen */
{
    union REGS regs;
    int xsize, ysize, attrib;
#ifdef DOS16RM  /* kludge for 80286 protected mode */
    unsigned char *basemem = D16SegAbsolute(0L);
#endif

    flushbuf();
    lposition = 0;

#ifdef DOS16RM
    xsize = *(unsigned int FAR *)(basemem+0x44a);
    ysize = (*(basemem+0x484) != 0 ? *(basemem+0x484) : 24);
    attrib = (ourmode1 > 3 ? 0 : 
        *(unsigned char FAR *)D16SegAbsolute(0xb8001L));
#else
    xsize = *(unsigned int FAR *) 0x44aL;
    ysize = (*(unsigned char FAR *) 0x484L != 0 ?
        *(unsigned char FAR *)0x484L : 24);
    attrib = (ourmode1 > 3 ? 0 : *(unsigned char FAR *)0xb8000001L);
#endif

    regs.x.ax = 0x0600;
    regs.h.bh = attrib;
    regs.x.cx = 0;
    regs.h.dh = ysize;
    regs.h.dl = xsize;
    int86(0x10, &regs, &regs);
    regs.h.ah =0x2;             /* home cursor */
    regs.x.dx = 0;
    regs.h.bh = 0;
    int86(0x10, &regs, &regs);
    return NIL;
}

LVAL xcleol()   /* clear to end of line */
{
    union REGS regs;
    flushbuf();

    regs.h.ah = 0x3;    /* get old position */
    regs.h.bh = 0;
    int86(0x10, &regs, &regs);  /* x position in regs.h.dl, y in regs.h.dh */
    lposition = regs.h.dl;      /* just to be sure */
    regs.x.cx = regs.x.dx;
#ifdef DOS16RM
    regs.h.dl = (*(unsigned int FAR *)D16SegAbsolute(0x44aL)) -1;/* x size */
    regs.h.bh = (ourmode1 > 3 ? 0 : 
        *(unsigned char FAR *)D16SegAbsolute(0xb8001L)); /* atrrib*/
#else
    regs.h.dl = *(unsigned int FAR *) 0x44aL -1;    /* x size */
    regs.h.bh = (ourmode1 > 3 ? 0 : *(unsigned char FAR *)0xb8000001L); /* atrrib*/
#endif
    regs.x.ax = 0x0600;         /* scroll region */
    int86(0x10, &regs, &regs);
    return NIL;
}
#endif
#endif

#ifdef GCC
#define xtarget x2
#define ytarget y2
#define drawpix() setpixel(x2,y2)
#else
#define xtarget regin.x.cx
#define ytarget regin.x.dx
#define drawpix() int86(0x10,&regin,&regout)
#endif

static LVAL NEAR draw(int x, int y, int x2, int y2)

{
    int xStep,yStep,xDist,yDist;
    int i, t8, t9, t10;

    flushbuf();

    if ((x < 0) | (x > Xmax) | (y < 0) | (y > Ymax) |
        (x2 < 0)| (x2 > Xmax)  | (y2 < 0) | (y2 > Ymax))
            return (NIL);

    x -= x2;     /* cvt to distance and screen coordiate (right hand) */
    y2 = Ymax - y2;
    y = (Ymax - y) - y2;

    if (x < 0) {    /* calculate motion */
        xStep = -1;
        xDist = -x;
    }
    else {
        xStep = 1;
        xDist = x;
    }
    if (y < 0) {
        yStep = -1;
        yDist = -y;
    }
    else {
        yStep = 1;
        yDist = y;
    }

#ifdef GCC
    setdrawmode(drawvalue);
#else
    regin.x.ax = drawvalue + 0x0c00;    /* write graphic pixel command */
#endif

    xtarget = x2;       /* initial coordinates */
    ytarget = y2;

    drawpix(); /* initial draw */


    if (yDist == 0) {
        i = xDist;
        while (i--) {
            xtarget += xStep;
            drawpix();
        }
    }
    else if (xDist == yDist) {
        i = xDist;
        while (i--) {
            xtarget += xStep;
            ytarget += yStep;
            drawpix();
        }
    }
    else if (xDist == 0) {
        i = yDist;
        while (i--) {
            ytarget += yStep;
            drawpix();
        }
    }
    else if (xDist > yDist) {
        t8 = 2*yDist;
        t10 = 2*yDist - xDist;
        t9 = 2*(yDist - xDist);
        i = xDist;
        while (i--) {
            xtarget += xStep;
            if (t10 < 0) {
                t10 += t8;
            }
            else {
                ytarget += yStep;
                t10 += t9;
            }
            drawpix();
        }
    }
    else {
        t8 = 2*xDist;
        t10 = 2*xDist - yDist;
        t9 = 2*(xDist - yDist);
        i = yDist;
        while (i--) {
            ytarget += yStep;
            if (t10 < 0) {
                t10 += t8;
            }
            else {
                xtarget += xStep;
                t10 += t9;
            }
            drawpix();
        }
    }
#ifdef GCC
    unsetdrawmode();
#endif
    return (s_true);
}

#undef xtarget
#undef ytarget
#undef drawpix

/* xmode -- set display mode */
/* called with either ax contents, or ax,bx,xsize,ysize */
#ifdef GCC
LVAL xmode()
{
    LVAL arg;

    arg = xlgafixnum();
    ourmode1 = (int) getfixnum(arg);
        
    if (moreargs()) {
        arg = xlgafixnum();
        ourmode2 = (int) getfixnum(arg);
        arg = xlgafixnum();
        Xmax = (int) getfixnum(arg) - 1;        /* max x coordinate */
        arg = xlgafixnum();
        Ymax = (int) getfixnum(arg) - 1;        /* max y coordinate */
        xllastarg();
    }
    else {
        ourmode2 = 0;
        switch (ourmode1) {
            case 4:
            case 5:
            case 13: Xmax = 319;
                     Ymax = 199;
                     break;
            case 6:
            case 14: Xmax = 639;
                     Ymax = 199;
                     break;
            case 16: Xmax = 639;
                     Ymax = 349;
                     break;
            case 18: Xmax = 639;    /* added VGA mode */
                     Ymax = 479;
                     break;
            default: Xmax = Ymax = -1; /* not a graphic mode */
         break;
         }
     }

        
    setgmode(ourmode1,ourmode2);        /* set mode */
    bytesperline = (Xmax + 1) / 8;

    xlsave1(arg);

    arg = consa(cvfixnum((FIXTYPE)Ymax));
    arg = cons(cvfixnum((FIXTYPE)Xmax), arg);

    arg = cons(cvfixnum((FIXTYPE)_farpeekb(cmem,0x484) + 1), arg);
    arg = cons(cvfixnum((FIXTYPE)_farpeekw(cmem,0x44a)), arg);

    xlpop();

    return arg;
}
#else
LVAL xmode()
{
    int nmode1, nmode2;
    LVAL arg;
#ifdef DOS16RM  /* kludge for 80286 protected mode */
    unsigned char *basemem = D16SegAbsolute(0L);
#endif
#ifdef DOS386
    unsigned char _far *basemem
#endif

    arg = xlgafixnum();
    nmode1 = (int) getfixnum(arg);

    if (moreargs()) {
        arg = xlgafixnum();
        nmode2 = (int) getfixnum(arg);
        arg = xlgafixnum();
        Xmax = (int) getfixnum(arg) - 1;    /* max x coordinate */
        arg = xlgafixnum();
        Ymax = (int) getfixnum(arg) - 1;    /* max y coordinate */
        xllastarg();
    }
    else {
        nmode2 = 0;
        switch (nmode1) {
        case 0: case 1: case 2: case 3:
            Xmax = Ymax = -1; /* not a graphic mode */
                 break;
        case 4:
        case 5:
        case 13:
        case 19: Xmax = 319;
                 Ymax = 199;
                 break;
        case 6:
        case 14: Xmax = 639;
                 Ymax = 199;
                 break;
        case 16: Xmax = 639;
                 Ymax = 349;
                 break;
        case 17:
        case 18: Xmax = 639;    /* added VGA mode */
                 Ymax = 479;
                 break;
        default:    return NIL; /* failed */
        }
    }

    ourmode1 = nmode1;
    ourmode2 = nmode2;
    setgmode(ourmode1,ourmode2); /* set mode */

    xlsave1(arg);

    arg = consa(cvfixnum((FIXTYPE)Ymax));
    arg = cons(cvfixnum((FIXTYPE)Xmax), arg);

#ifdef DOS386
    basemem =  = _x286_mk_protected_ptr(0L);
    arg = cons(cvfixnum((FIXTYPE)*(basemem+0x484) + 1), arg);
    arg = cons(cvfixnum((FIXTYPE)*(unsigned short _far *)(basemem+0x44a) ), arg);
    _x386_free_protected_ptr(basemem);
#else
#ifdef DOS16RM
    arg = cons(cvfixnum((FIXTYPE)*(basemem+0x484) + 1), arg);
    arg = cons(cvfixnum((FIXTYPE)*(unsigned int FAR *)(basemem+0x44a) ), arg);
#else

    arg = cons(cvfixnum((FIXTYPE)*(unsigned char FAR *)0x484L + 1), arg);
    arg = cons(cvfixnum((FIXTYPE)*(unsigned int FAR *)0x44aL), arg);
#endif
#endif

    xlpop();

    return arg;

}
#endif

/* xcolor -- set color */

LVAL xcolor()
{
    LVAL arg;

    arg = xlgafixnum();
    xllastarg();

    drawvalue = (char) getfixnum(arg);

    return (arg);
}

/* xdraw -- absolute draw */

LVAL xdraw()
{
    LVAL arg = s_true;
    int newx, newy;

    while (moreargs()) {
        arg = xlgafixnum();
        newx = (int) getfixnum(arg);

        arg = xlgafixnum();
        newy = (int) getfixnum(arg);

        arg = draw(xpos,ypos,newx,newy);

        xpos = newx;
        ypos = newy;
    }
    return (arg);
}

/* xdrawrel -- absolute draw */

LVAL xdrawrel()
{
    LVAL arg = s_true;
    int newx, newy;

    while (moreargs()) {
        arg = xlgafixnum();
        newx = xpos + (int) getfixnum(arg);

        arg = xlgafixnum();
        newy = ypos + (int) getfixnum(arg);

        arg = draw(xpos,ypos,newx,newy);

        xpos = newx;
        ypos = newy;
    }
    return (arg);
}

/* xmove -- absolute move, then draw */

LVAL xmove()
{
    LVAL arg;

    arg = xlgafixnum();
    xpos = (int) getfixnum(arg);

    arg = xlgafixnum();
    ypos = (int) getfixnum(arg);

    return (xdraw());
}

/* xmoverel -- relative move */

LVAL xmoverel()
{
    LVAL arg;

    arg = xlgafixnum();
    xpos += (int) getfixnum(arg);

    arg = xlgafixnum();
    ypos += (int) getfixnum(arg);

    return (xdrawrel());
}

#endif
#ifdef TIMES
/* For some reason, every compiler is different ... */
#if defined(MSC) || defined(__TSC__)
unsigned long ticks_per_second() { return((unsigned long) CLK_TCK); }

unsigned long run_tick_count()
{
  return((unsigned long) clock() - kbdtime);
}

unsigned long real_tick_count()
{                                  /* Real time */
  return((unsigned long) clock());
}


LVAL xtime()
{
    LVAL expr,result;
    unsigned long tm;

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    tm = run_tick_count();
    result = xleval(expr);
    tm = run_tick_count() - tm;
    sprintf(buf, "The evaluation took %.2f seconds.\n",
            ((double)tm) / ticks_per_second());
    trcputstr(buf);

    flushbuf();

    return(result);
}
#endif

#ifdef __ZTC__
unsigned long ticks_per_second() { return((unsigned long) CLK_TCK); }

unsigned long run_tick_count()
{
  return((unsigned long) clock() - kbdtime);
}

unsigned long real_tick_count()
{                                  /* Real time */
  return((unsigned long) clock());
}


LVAL xtime()
{
    LVAL expr,result;
    double tm;

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    tm = run_tick_count();
    result = xleval(expr);
    tm = (run_tick_count() - tm) / CLK_TCK ;
    sprintf(buf, "The evaluation took %.2f seconds.\n", tm);
    trcputstr(buf);

    flushbuf();
    return(result);
}

#endif

#ifdef __TURBOC__
/* We want to cheat here because ticks_per_second would have to be rounded */

#define OURTICKS 1000

unsigned long ticks_per_second() {
    return((unsigned long) OURTICKS);
}

unsigned long run_tick_count()
{                               /*Real time in MSDOS*/
  return(((unsigned long) ((OURTICKS/CLK_TCK)*clock())) - kbdtime);
}

unsigned long real_tick_count()
{                               /* Real time */
  return((unsigned long) ((OURTICKS/CLK_TCK)*clock()));
}


LVAL xtime()
{
    LVAL expr,result;
    unsigned long tm;

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    tm = run_tick_count();
    result = xleval(expr);
    tm = run_tick_count() - tm;
    sprintf(buf, "The evaluation took %.2f seconds.\n",
            ((double)tm) / ticks_per_second());
    trcputstr(buf);

    flushbuf();

    return(result);
}
#endif

#ifdef GCC
unsigned long ticks_per_second() { return((unsigned long) 1000); }

unsigned long run_tick_count()  /* actually real time */
{
    struct timeval now;
    gettimeofday(&now,0);
/* we want time in milliseconds. The function returns seconds since 1/1/70
   which is too big a number, so we will subtract off the time since the
   start of the program */
    return ((unsigned long) ((now.tv_sec-stsecs)*1000 + now.tv_usec/1000) - kbdtime);
}

unsigned long real_tick_count() /* real time */
{
    struct timeval now;
    gettimeofday(&now,0);
/* we want time in milliseconds. The function returns seconds since 1/1/70
   which is too big a number, so we will subtract off the time since the
   start of the program */
    return ((unsigned long) ((now.tv_sec-stsecs)*1000 + now.tv_usec/1000));
}

LVAL xtime()
{
    LVAL expr,result;
    unsigned long tm;
  
    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    tm = run_tick_count();
    result = xleval(expr);
    tm = run_tick_count() - tm;
    sprintf(buf, "The evaluation took %.2f seconds.\n", 
            ((double)tm) / 1000.0);
    trcputstr(buf);

    flushbuf();

    return(result);
}
#endif

LVAL xruntime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) run_tick_count()));
}

LVAL xrealtime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) real_tick_count()));
}


#endif

