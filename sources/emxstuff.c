/* emxstuff.c - OS/2 EMX specific sources */

#include "xlisp.h"
#include "osdefs.h"

#include <io.h>
#include <math.h>
#include <signal.h>
#ifdef TIMES
 #include <sys/time.h>
#endif
#include <sys/ioctl.h>
#include <sys/termio.h>
#define LBSIZE 77


char *stackbase;

/* local variables */
/* TAA mod 8/92 made unsigned for high ASCII support */
static unsigned char lbuf[LBSIZE];
static int lcount, lindex;
struct termio oldstate, newstate;

#ifdef TIMES
unsigned long kbdtime;
long stsecs;
#endif

/* Command history */
#define HISTSIZE (20)
static unsigned char *history[HISTSIZE] = {NULL, NULL, NULL, NULL, NULL};
int curhist = -1;

static int istty;

/* forward declarations */
static void NEAR xinfo(void);
static void NEAR xflush(void);
static int  NEAR xgetc(void);
static void NEAR xputc(int ch);
static void NEAR setraw(void);
static void NEAR unsetraw(void);

/* output buffering */
#define CHBSIZE 256
static char outbuf[CHBSIZE];
static char *outbufp = &outbuf[0];

void flushbuf()
{
    if (outbufp != &outbuf[0]) {
        write(2,&outbuf[0],outbufp - &outbuf[0]);
        outbufp = &outbuf[0];
    }
}

#ifdef FILETABLE
int osagetc(FILEP f)
{
    int val;
    FILE *fp = filetab[f].fp;
    
    /* read until a non \r found */
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
    
    /* read until a non \r found */
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

/* osinit - initialize */

VOID osinit(banner)
  char *banner;
{
#ifdef TIMES
    struct timeval now;
    gettimeofday(&now,0);
    stsecs = now.tv_sec;
#endif
#ifdef STSZ
    stackbase = (char *)&banner;    /* find base of stack */
#endif
    fprintf(stderr,"%s\n",banner);
    lposition = 0;
    lindex = 0;
    lcount = 0;
/*    setfpcw(); */  /* mask off fp interrupts */
    redirectout = !(_isterm(1));
    redirectin =  !(_isterm(0));
    setraw();
    signal(SIGBREAK, SIG_IGN);
}

/* osfinish - clean up before returning to the operating system */
VOID osfinish()
{
        flushbuf();
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
        drive = _getdrive();
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
        
        if (_getcwd1(curdir, drive)) return FALSE;    /* invalid drive */
        strcpy(curdir, curdir+1); /* get rid of leading / */
        for (cp = curdir; (cp = strchr(cp, '/')) != NULL; *cp = '\\') ;

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
    

VOID osclose(FILEP f)
{
    fclose(filetab[f].fp);
    free(filetab[f].tname);
    filetab[f].tname = NULL;
    filetab[f].fp = NULL;
}

#else

FILE * CDECL osopen(const char *name, const char *mode)
{
    char bmode[10];
    strcpy(bmode,mode); strcat(bmode,"b");
    return (fopen(name,bmode));
}
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
    if ((strchr(name,'/') != NULL && strchr(name, '\\') != NULL) ||
        path == NULL)
        return (ascii? OSAOPEN : OSBOPEN)(name, "r");
    do {
        if (*path == '\0')  /* no more paths to check */
            /* check current directory just in case */
            return (ascii? OSAOPEN : OSBOPEN)(name, "r");

        newnamep = newname;
        while ((ch=*path++) != '\0' && ch != ';' && ch != ' ')
            *newnamep++ = ch;

        if (ch == '\0') path--;

        if (newnamep != newname &&
            *(newnamep-1) != '/' && *(newnamep-1) != '\\')
            *newnamep++ = '/';  /* final path separator needed */
        *newnamep = '\0';

        strcat(newname, name);

        fp = (ascii? OSAOPEN : OSBOPEN)(newname, "r");
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
    /* check for a buffered character */
    if (lcount-- > 0)
        return (lbuf[lindex++]);

    /* get an input line */
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
                if (tfp!=CLOSED)
                    for (lindex=0; lindex <= lcount;) 
                        OSAPUTC(lbuf[lindex++],tfp);
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
        OSAPUTC(ch,tfp);
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

    if (!redirectin) {
        ioctl(0,FIONREAD,&ch);
        if (ch==0) return;
        read(0, &ch, 1);
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
static int xgetc()
{
    int ch;
    unsigned char chbuf[1];
#ifdef TIMES
    unsigned long kbtime = real_tick_count();
#endif
    flushbuf();

    read(0,&chbuf,1);
    ch = chbuf[0];
    if (!redirectin && ch == 0) {
        read(0,&chbuf,1);
        ch = 256 + chbuf[0];
    }
#ifdef TIMES
    kbdtime += real_tick_count() - kbtime;
#endif
    return ch;
}

/* xputc - put a character to the terminal */
static void xputc(ch)
  int ch;
{
    *outbufp++ = ch;
    if (ch == '\n' || outbufp == &outbuf[CHBSIZE]) flushbuf();
}

/* xsystem - execute a system command */
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

/* xgetkey - get a key from the keyboard */
LVAL xgetkey()
{
    xllastarg();
    return (cvfixnum((FIXTYPE)xgetc()));
}

/* setraw -- set raw mode */
static VOID setraw()
{
    ioctl(0,TCGETA,&oldstate);
    newstate = oldstate;
    newstate.c_iflag = 0;
    newstate.c_lflag = 0;
    ioctl(0,TCSETA,&newstate);
}

/* unsetraw -- restore original mode */
static VOID unsetraw()
{
    ioctl(0,TCSETA,&oldstate);
}

/* ossymbols - enter os specific symbols */
VOID ossymbols()
{
}

#ifdef TIMES
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

LVAL xruntime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) run_tick_count()));
}

LVAL xrealtime() {
    xllastarg();
    return(cvfixnum((FIXTYPE) real_tick_count()));
}


#endif
