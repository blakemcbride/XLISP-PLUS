/* IF THIS FILE DOESN'T COMPILE, MAKE WHATEVER CHANGES ARE NECESSARY AND
 * PROVIDE THE CHANGES (AND YOUR SPECIFIC SITUATION) TO ME,
 * tomalmy@aracnet.com */
/* -*-C-*-
********************************************************************************
*
* File:         linuxstuff.c
* Description:  Linux-Specific interfaces for XLISP
* Author:       David Michael Betz; Niels Mayer
*
* WINTERP 1.0 Copyright 1989 Hewlett-Packard Company (by Niels Mayer).
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Modified again by Tom Almy
* some SYSV modifications by Dave Rivers (rivers@ponds.uucp)
* some bugs fixed by Hume Smith (850347s@aucs.acadiau.ca)
* yet another fix by Tom Almy
* code for redirection added by Brian Anderson (bha@atc.boeing.com)
* Yet more revisions, fixing POSIX support, from Andrew Schwab
*   (schwab@issan.informatik.uni-dortmund.de or schwab@gnu.org) added 3/98
*   Modified for Linux in 2010 by Tom Almy
********************************************************************************
*/

/*****************************************************************************
*               edit history
*
* 92Jan29 CrT.  Edit history.  Reversed SysV gtty/stty #defs fixed.
*****************************************************************************/
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/times.h>
#include <sys/fcntl.h>
#include <time.h>
#include <errno.h>
#include <ctype.h>
#include <math.h>

#include <unistd.h>
#include <termios.h>
struct termios savetty;
struct termios newtty;
#define gtty(fd,arg)    (emacs_input ? 0 : tcgetattr(fd, arg))
#define stty(fd,arg)    (emacs_input ? 0 : tcsetattr(fd, TCSANOW, arg))

#define beep()  xputc(7)
#define flushbuf() fflush(stderr);

#include "xlisp.h"

#define LBSIZE  200
#ifdef HZ
#undef HZ
#endif
#define HZ sysconf(_SC_CLK_TCK)

/* -- external functions */
/* extern long times(); */

/* -- local variables */
static  char    lbuf[LBSIZE];
//static  int     lpos[LBSIZE];
static  int     lindex;
static  int     lcount;
static  int     emacs_input;
#define TABUFSIZE (128)
static char typeaheadbuf[TABUFSIZE];
static int nextta = 0;


static void init_tty(void);
static void xflush(void);

#define EXTENDED_INPUT
#ifdef EXTENDED_INPUT
/* Command history */
#define HISTSIZE (20)
static unsigned char *history[HISTSIZE] = {NULL};
int curhist = -1;

static int listmatches(void);
static void searchobarray(LVAL array, unsigned char *st, int l, int append,
                          int lpos, int *matchcount, LVAL *firstmatch,
                          int *endpos, int funconly);

#endif

#ifdef GRAPHICS
#include "application.h"
static int gModeOut; // True if in graphics mode rather than text mode
static int gModeInit; // True if graphics mode has been initialized
static int xpos, ypos; // Current coordiates
static int xSize, ySize; // Current window size
#define GCBUFSIZE (128)
static char gcharbuf[GCBUFSIZE+1];
static int gcindex = 0;
static void pushgcbuf(char ch);
static void flushgcbuf(void);
#endif


static int xgetc(void);

LVAL xgetkey(void) {
	xllastarg();
	return (cvfixnum((FIXTYPE)xgetc())); 
}

/******************************************************************************
 * xsystem - run a process, sending output (if any) to stdout/stderr
 *
 * syntax: (system <command line>)
 *                 <command line> is a string to be sent to the subshell (sh).
 *
 * Returns T if the command executed succesfully, otherwise returns the
 * integer shell exit status for the command.
 *
 * Added to XLISP by Niels Mayer
 * didn't spawn a shell with null string HCLS
 * didn't reset terminal for interaction HCLS
 ******************************************************************************/
LVAL
xsystem(void)
{
    char *          getenv();
    char           *comstr;
    LVAL            command;
    int             result;
//    char            temptext[1024];

    /* get shell command */
    command = xlgastring();
    xllastarg();

    comstr = (char *) getstring(command);
    if (*comstr) {
            /* restore the terminal */
        (void)stty(2, &savetty);
        /* run the process */
        result = system(comstr);
        /* restore the terminal */
        (void)stty(2, &newtty);
                
        if (result == -1) {     /* if a system error has occured */
            xlfail("error in system call");
        }
    } else {
        /*
         * We were given a null string.  We'll try to find out what
         * shell the user uses and spawn it.
         */
        if ((comstr = getenv("SHELL")) != NULL) {
            int  pid;
            /*
             * we could just system(comstr), but that would get
             * two shells running...
             */
            /* restore the terminal */
            (void)stty(2, &savetty);
            pid = fork();
            if (pid == 0) {
                    extern int errno;
                execl(comstr, comstr, NULL);
                exit(errno);
            }
            if (pid == -1) {
                xlfail("error in system call");
            }
            while (pid != wait(&result));
            (void)stty(2, &newtty);
            result >>= 8;
        } else {
            /* SHELL is expected (environ(5)) */
            xlfail("can't find SHELL variable");
        }
    }
    /*
     * return T if success (exit status 0), else return exit status
     */
    return (result ? cvfixnum(result) : s_true);
}


/******************************************************************************/
/* -- Written by dbetz for XLISP 2.0 */

char *stackbase;

/* -- osinit - initialize */
VOID osinit(char *banner)
{
        char *getenv();

#ifdef STSZ
	char foo;
	stackbase = &foo;
#endif
        redirectout = !isatty(fileno(stdout));
        redirectin = !isatty(fileno(stdin));
        emacs_input = getenv("EMACS") != NULL || getenv("EPSRUNS") != NULL;

        if(!redirectin) {
          fprintf(stderr,"%s\nLinux version\n", banner );
        }
        if (!(redirectin && batchmode)) {
            init_tty();
        }
        lindex  = 0;
        lcount  = 0;
}

/* -- osfinish - clean up before returning to the operating system */
VOID osfinish()
{
    if(!(redirectin && batchmode)) {
        stty(2, &savetty);
    }
}


/* -- xoserror - print an error message */
VOID xoserror(msg)
char         *msg;
{
        printf( "error: %s\n", msg );
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
extern VOID gc();

int truename(name, rname)
char        *name,*rname;
{
//    int i;
    char *cp;
    char pathbuf[FNAMEMAX+1];   /* copy of path part of name */
    char curdir[FNAMEMAX+1];    /* current directory */
    char *fname;        /* pointer to file name part of name */
    
    /* parse any drive specifier */

    /* check for absolute path (good news!) */
    
    if (*name == '/') {
        strcpy(rname, name);
    }
    else {
        strcpy(pathbuf, name);
        if ((cp = strrchr(pathbuf, '/')) != NULL) { /* path present */
            cp[1] = 0;
            fname = strrchr(name, '/') + 1;
        }
        else {
            pathbuf[0] = 0;
            fname = name;
        }

        /* get the current directory of the selected drive */
        
        if (getcwd(curdir, FNAMEMAX) == NULL) return FALSE;

        /* peel off "../"s */
        while (strncmp(pathbuf, "../", 3) == 0) {
            if (*curdir == 0) return FALSE;     /* already at root */
            strcpy(pathbuf, pathbuf+3);
            if ((cp=strrchr(curdir+1, '/')) != NULL)
                *cp = 0;    /* peel one depth of directories */
            else
                *curdir = 0;    /* peeled back to root */
        }
        
        /* allow for a "./" */
        if (strncmp(pathbuf, "./", 2) == 0)
            strcpy(pathbuf, pathbuf+2);
        
        /* final name is /curdir/pathbuf/fname */

        if (strlen(pathbuf)+strlen(curdir)+strlen(fname)+4 > (unsigned)FNAMEMAX) 
            return FALSE;
        
        if (*curdir)
            sprintf(rname, "%s/%s%s", curdir, pathbuf, fname);
        else
            sprintf(rname, "/%s%s", pathbuf, fname);
    }
    
    return TRUE;
}

int getslot()
{
    int i=0;
    
    for (; i < FTABSIZE; i++)   /* look for available slot */
        if (filetab[i].fp == NULL) return i;
    
    gc();   /* is this safe??????? */

    for (; i < FTABSIZE; i++) /* try again -- maybe one has been freed */
        if (filetab[i].fp == NULL) return i;

    xlfail("too many open files");
    
    return 0;   /* never returns */
}


FILEP osopen(name, mode)
  char *name, *mode;
{
    int i=getslot();
    char namebuf[FNAMEMAX+1];
    FILE *fp;
    
    if (!truename((char *)name, namebuf))
        strcpy(namebuf, name);  /* should not happen */

    if ((filetab[i].tname = (char *)malloc(strlen(namebuf)+1)) == NULL) {
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
    
void osclose(f)
  FILEP f;
{
    if (filetab[f].fp != NULL)
        fclose(filetab[f].fp);
    /* remind stdin/stdout/stderr */
    if (f>2 && filetab[f].tname != NULL)
        free(filetab[f].tname);
    filetab[f].tname = NULL;
    filetab[f].fp = NULL;
}
    
#endif

#ifdef PATHNAMES
/* ospopen - open using a search path */
FILEP ospopen(char *name, int ascii)
{
    char *getenv();
    FILEP fp;
    char *path = getenv(PATHNAMES);
    char *newnamep;
    char ch;
    char newname[256];

    /* don't do a thing if user specifies explicit path */
    if (strchr(name,'/') != NULL || path == NULL)
        return OSAOPEN(name, "r");

    do {
        if (*path == '\0')  /* no more paths to check */
            /* check current directory just in case */
            return OSAOPEN(name, "r");

        newnamep = newname;
        while ((ch = *path++) != '\0' && ch != ':' && ch != ' ')
            *newnamep++ = ch;

    if (ch == '\0') path--;

        if (*(newnamep-1) != '/')
            *newnamep++ = '/';  /* final path separator needed */
        *newnamep = '\0';

        strcat(newname, name);
        fp = OSAOPEN(newname, "r");
    } while (fp == CLOSED); /* not yet found */

    return fp;
}
#endif

/* rename argument file as backup, return success name */
/* For new systems -- if cannot do it, just return TRUE! */

int renamebackup(char *filename)
{
    char *bufp, ch=0;

    strcpy(buf, filename);  /* make copy with .bak extension */

    bufp = &buf[strlen(buf)];   /* point to terminator */
    while (bufp > buf && (ch = *--bufp) != '.' && ch != '/') ;


    if (ch == '.') strcpy(bufp, ".bak");
    else strcat(buf, ".bak");

    unlink(buf);

    return !rename(filename, buf);
}


/* -- ostputc - put a character to the terminal */
VOID ostputc(int ch )
{
        char buf[1];

        buf[0] = ch;

        if (ch == '\n') lposition = 0;
        else if (ch == '\b') {
            if (lposition > 0) --lposition;
            else return; // do nothing
        }
        else if (ch == '\t') { // recurse to handle tabs
            do { ostputc(' '); } while (lposition & 7);
            return;
        }
        else if (isprint(ch))
            lposition++;

        /* -- output the character */
#ifdef GRAPHICS
        if (gModeOut) {
            pushgcbuf(ch);
        } else {
#endif
        /*        putchar( ch ); */
        
        0 == write(2,buf,1); // It might be redirected -- always want stderr
#ifdef GRAPHICS
        }
#endif

        /* -- output the char to the transcript file */
        if ( tfp != CLOSED )
                OSPUTC( ch, tfp );
}




/* -- osflush - flush the terminal input buffer */
VOID osflush(void)
{
#ifdef GRAPHICS
    if (gModeOut) {
        flushgcbuf();
        update();
    }
#endif
//	nextta = 0;
        lindex = lcount = 0;
}

static int sigint_received;


static VOID xinfo(void);

void osx_check(char ch)
{
    switch (ch) {
        case C_BREAK:    /* control-b */
            xflush();
            xlbreak("**BREAK**",s_unbound);
            break;
        case C_TOPLEV:
            xflush();
            xltoplevel(); /* control-c */
            break;
        case C_STATUS:    /* control-t */
          xinfo();
          printf("\n ");
          break;
        default:
            if (nextta < TABUFSIZE) {
                typeaheadbuf[nextta++] = ch;
            }
            break;
     }
}

int kbhit(void) {
    struct timeval tv;
    fd_set rdfs;
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    FD_ZERO(&rdfs);
    FD_SET(STDIN_FILENO, &rdfs);

    select(STDIN_FILENO+1, &rdfs, NULL, NULL, &tv);
    return FD_ISSET(STDIN_FILENO, &rdfs);
}

/* This is a non-blocking read */
VOID oscheck(void)
{
    if (sigint_received) {
        sigint_received = 0;
        xltoplevel();
    }

    if (redirectin) return; /* No looking ahead if input redirected */
    if (kbhit())
    {
        int nrd;
        unsigned char buf[2];
        nrd = read(2, buf, 1);
        if (nrd == 1) {
            osx_check(buf[0]);
        }
    }
}

/* -- ossymbols - enter os-specific symbols */
VOID ossymbols()
{
}


/* xinfo - show information on control-t */
static VOID xinfo()
{
#ifdef STSZ
	int i;
#endif
  char tymebuf[100];
  time_t tyme;
  char buf[500];

  time(&tyme);
  strcpy(tymebuf, ctime(&tyme));
  tymebuf[19] = '\0';
#ifdef STSZ
  sprintf(buf,"\n[ %s Free: %ld, GC calls: %ld, Total: %ld \n"
	    "  Edepth: %ld, Adepth %ld, Sdepth: %ld ]",
	    tymebuf, nfree,gccalls,total,
	 (long)(xlstack-xlstkbase), (long)(xlargstktop-xlsp), (long)STACKREPORT(i));
#else
  sprintf(buf,"\n[ %s Free: %ld, GC calls: %ld, Total: %ld ]",
    tymebuf, nfree,gccalls,total);
#endif
  errputstr(buf);
}

/* xflush - flush the input line buffer and start a new line */
void xflush(void)
{
    nextta = 0; // Get rid of any typeahead
    osflush();
    ostputc('\n');
}

static void handle_interrupt(int sig)
{
#if !defined(BSD) && !defined(POSIX)
    /* SYS V requires reseting of SIGINT */
    signal(SIGINT, handle_interrupt);
#endif
    sigint_received = 1;
}

static void onsusp(int sig);

void init_tty(void)
{
        /* extern sigcatch(); */
    struct sigaction sa, old_sa;
    sa.sa_handler = handle_interrupt;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGINT, &sa, NULL);
    sa.sa_handler = SIG_IGN;
    sigaction(SIGQUIT, &sa, NULL);

#ifdef SIGTSTP
    sigaction(SIGTSTP, NULL, &old_sa);
    if (old_sa.sa_handler == SIG_DFL) {
        sa.sa_handler = onsusp;
        sigaction(SIGTSTP, &sa, NULL);
    }
#endif

    if (gtty(2, &savetty) == -1) {
        printf("ioctl failed: not a tty\n");
        exit(1);
    }

    newtty = savetty;

    
    newtty.c_lflag &= ~ICANON;  /* SYS 5 */
    newtty.c_lflag &= ~ECHO;
    newtty.c_cc[VMIN] = 1;
    newtty.c_cc[VTIME] = 1;
    /*
     * You can't request that it try to give you at least
     * 5 characters, nor set the timeout to 10 seconds,
     * as you can in the S5 example.  If characters come
     * in fast enough, though, you may get more than one.
     */
    if (stty(2, &newtty) == -1) {
        printf("cannot put tty into cbreak mode\n");
        exit(1);
    }
}

static void onsusp(int sig)
{
#ifdef SIGTSTP
    sigset_t newset, oldset;
    /* ignore SIGTTOU so we dont get stopped if csh grabs the tty */
    signal(SIGTTOU, SIG_IGN);
    stty(2, &savetty);
    xflush();
    signal(SIGTTOU,SIG_DFL);

    /* send the TSTP signal to suspend our process group */
    signal(SIGTSTP, SIG_DFL);

    sigfillset(&newset);
    sigprocmask(SIG_UNBLOCK, &newset, &oldset);
    kill(0, SIGTSTP);
    /* pause for station break */

    /* we're back */
    signal(SIGTSTP, onsusp);
    stty(2, &newtty);
#endif
}

static int xgetc()
{
    int nrd, charval;
    unsigned char buf[2];

    if (nextta > 0) { // fetch out any typeahead
        charval = typeaheadbuf[0];
        memmove(typeaheadbuf, typeaheadbuf+1, nextta);
        nextta--;
    } else { // actually read something
        do {
            nrd = read(2, buf, 1);
            if (sigint_received) {
                sigint_received = 0;
                xltoplevel();
            }
        } while (nrd < 0 && errno == EINTR);
        charval = (nrd > 0 ? buf[0] : EOF);
    }

    if (charval == 27) { // Escape sequence
        if (kbhit() == 0) { // nothing follows
            return charval;
        }
        charval = xgetc();
        if (charval == '[') {
            int numval = xgetc();
            if (numval >= '0' && numval <= '9') {
                charval = xgetc() + 256*(numval-'0' + 2);
            } else {
                charval = numval + 256;
            }
        } else if (charval == 'O') { // SS3 sequence
            int numval = xgetc();
            charval = numval + 256*12;
        } else {
            // Not an escape sequence -- save the character
            if (nextta < TABUFSIZE) typeaheadbuf[nextta++] = charval;
            charval = 27;
        } 
    }
    return charval;
}    

#ifdef EXTENDED_INPUT
static void xputc(int ch) {
#ifdef GRAPHICS
    if (gModeOut) {
        pushgcbuf(ch);
    } else {
#endif
    char chbuf = (char) ch;
    0 == write(2, &chbuf, 1);
#ifdef GRAPHICS
    }
#endif
}


static char *mystrcpy(char *dest, const char *src) {
    // Allows src and dest to overlap
    char *target = dest;
    while ((*dest++ = *src++) != '\0') {
    };
    return target;
}

/* ostgetc - get a character from the terminal */
int ostgetc()
{
    int ch;

#ifdef GRAPHICS
    if (gModeOut) {
        flushgcbuf();
        gModeOut = FALSE;
        update();
    }
#endif

    /* check for a buffered character */
    if (lcount-- > 0)
        return (lbuf[lindex++]);

    {	// Handle dosinput changes 
	    static int firsttime = TRUE;

	    if (firsttime) {
		    firsttime =FALSE;
			// Force value of dosinput if emacs input, but we only do it once 
		    if (emacs_input) setvalue(s_dosinput, s_true);
	    } else if (emacs_input ^ !null(getvalue(s_dosinput))) {
			// change in input type
		    osfinish();
		    emacs_input = !emacs_input;
		    init_tty();
	    }

    }			
    
    if (emacs_input) return getchar();

    /* get an input line */
try_again:
	for (lcount = 0, lposition=0, lbuf[0] = '\0'; ; ) {
            switch (ch = xgetc()) {
                case C_TAB:  /* TAB character -- list matches */
				{	
                    if (listmatches()) {
			    for (lindex = 0; lindex < lcount;)
				    xputc(lbuf[lindex++]);
					
			    for (lindex = lcount - lposition; lindex-- > 0; )
				    xputc(8);
		    }
                    break;
				}
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
                    xputc('\n');
                    lposition = 0;
                    if (tfp!=CLOSED) OSWRITE(lbuf,1,lcount+1,tfp);
                    lindex = 0;
                    return (lbuf[lindex++]);
                case C_BS: /* backspace (delete key) -- delete to left of cursor */
                    if (lposition != 0) {
                        lcount--;
                        lposition--;
                        mystrcpy((char *)&lbuf[lposition], (char *)&lbuf[lposition+1]);
                        xputc(8);   /* back up */
                        for (lindex = lposition; lindex < lcount; )
                            xputc(lbuf[lindex++]);
                        xputc(' '); /* "erase" at end of line */
                        lindex = lcount - lposition + 1;
                        while (lindex--) xputc(8);  /* back up */
                    }
                    else beep();
                    break;
                case C_DEL: /* FN Delete (delete forward key) -- delete at cursor */
                    if (lcount > lposition) {
                        lcount--;
                        mystrcpy((char *)&lbuf[lposition], (char *)&lbuf[lposition+1]);
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
                case C_HOME: case C_HOME2: /* Home -- goto beginning of line */
                    while (lposition != 0) {
                        lposition--;
                        xputc(8);
                    }
                    break;
                case C_END: case C_END2: /* End -- goto end of line */
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
                    if (strlen((char *)lbuf) < (unsigned)lcount) {
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
                case C_CLEAN:       /* control-g */
                    xflush();
                    xlcleanup();
                    break; // never reached
                case C_CONT:        /* control-p */
                    xflush();
                    xlcontinue();
                    break; // never reached
                case C_EOF:     /* control-D */
                    xflush();
                    return (EOF);
                case C_BREAK :                 /* CTRL-b */
                case C_TOPLEV :                 /* CTRL-c */
                case C_STATUS : osx_check(ch);   /* CTRL-t */
                              xflush();
                    goto try_again;          
                case C_ESC: /* ESCAPE */
                    for (lindex = lposition; lindex-- > 0;)
                        xputc(8);   /*backspace to start of line */

                    for (lindex = lcount; lindex-- > 0; )
                        xputc(' '); /* space to end of line */

                    for (lindex = lcount; lindex-- > 0;)
                        xputc(8);   /*backspace to start of line */

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
            } /* end of character case statement */
	} /* End of for loop */
}


#else /* Not EXTENDED_INPUT */

/* The next three functions implement the original ostgetc */
static char *xfgets(char *s, int n);
static int read_keybd(void);

/* this is a blocking read of a single character */
static int read_keybd(void)
{
    int nrd;
    unsigned char buf[2];

    if (nextta > 0) {
        buf[0] = typeaheadbuf[0];
        memmove(typeaheadbuf, typeaheadbuf+1, nextta);
        nextta--;
    } else {
#ifdef EINTR
        do {
            nrd = read(0, buf, 1);
            if (sigint_received) {
                sigint_received = 0;
                xltoplevel();
            }
        } while (nrd < 0 && errno == EINTR);
#else
        nrd = read(0, buf, 1);
#endif
        buf[nrd] = 0;
    }
    if (!emacs_input && (isprint(buf[0]) || buf[0] == '\n'))
        stdputstr((char *)buf);

    return (nrd > 0 ? buf[0] : EOF);
}

char *xfgets(char *s, int n)
{
        int c;
		char *cs;

        cs = s;
        while (--n>0 && (c = read_keybd()) != EOF) {
             switch(c) {
                 case '\007':
                     xlcleanup();  /* control-g */
                     break;
                 case '\020':
                     xlcontinue(); /* control-p */
                     break;
                 case '\002' :                 /* CTRL-b */
                  case '\003' :                 /* CTRL-c */
                  case '\024' : osx_check(c);   /* CTRL-t */
                                n++; // nothing added to buffer
                                break;

                  case 4:       c = EOF; /* control-D not caught above */
                                stdputstr("\n");
                                if (cs == s) return (NULL); // only at beginning of line
                                *cs = '\0'; // Terminate string and return
                                return (s);
                                
                  case 8      :
                  case 127    : if (cs==s) break;   /* not before beginning */

                              if (c == 127) {   /* perform erase */
                                  stdputstr("\010");
                                  stdputstr(" ");
                              }
                              stdputstr("\010"); /* BACKSPACE */

                                n+=2;           
                                cs--;
                                break;

                  default     : if (c == '\n' || isprint(c))
                                    *cs++ = c;      /* character */
                }
                if (c=='\n') break;
        }
        if (c == EOF && cs==s) return(NULL);
        *cs++ = '\0';
        return(s);
}

/* -- ostgetc - get a character from the terminal */
int     ostgetc()
{
    while(--lcount < 0 )
    {
        if ( xfgets(lbuf,LBSIZE) == NULL )
            return( EOF );

        lcount = strlen( lbuf );
        if (tfp!=CLOSED) OSWRITE(lbuf,1,lcount,tfp);

        lindex = 0;
        lposition = 0;
    }

    return( lbuf[lindex++] );
}
#endif /* EXTENDED_INPUT */

#ifdef TIMES
/***********************************************************************/
/**                                                                   **/
/**                  Time and Environment Functions                   **/
/**                                                                   **/
/***********************************************************************/

unsigned long ticks_per_second() { return((unsigned long) HZ); }

unsigned long run_tick_count()
{
  struct tms tm;

  times(&tm);
  return((unsigned long) tm.tms_utime + tm.tms_stime );
}

unsigned long real_tick_count()
{                                  /* Real time */
  return((unsigned long) (HZ * (time((time_t *) NULL))));
}


LVAL xtime()
{
  LVAL expr, result;
  unsigned long tm, rtm;
  double dtm, rdtm;

/* get the expression to evaluate */
  expr = xlgetarg();
  xllastarg();

  tm = run_tick_count();
  rtm = real_tick_count();
  result = xleval(expr);
  tm = run_tick_count() - tm;
  rtm = real_tick_count() - rtm;
  dtm = (tm > 0) ? tm : -tm;
  rdtm = (rtm > 0) ? rtm : -rtm;
  sprintf(buf, "CPU %.2f sec., Real %.2f sec.\n", dtm / ticks_per_second(),
                                            rdtm / ticks_per_second());
  trcputstr(buf);
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

#ifdef EXTENDED_INPUT
/* listmatches -- list interned symbols that match to current symbol name */

static void searchobarray(LVAL array, unsigned char *st, int l, int append,
                          int lpos, int *matchcount, LVAL *firstmatch,
                          int *endpos, int funconly)
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
		if (funconly && getfunction(car(sym))==s_unbound) continue;
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
            for (i = 0; stp[i] == targ[i]; i++) 
#else
            for (i = 0; stp[i] == target[i]; i++) 
#endif
            {
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
                            xputc('\n');
                        }
                        if (*matchcount==2 && append) {
                            xlprint(getvalue(s_debugio), *firstmatch, TRUE);
                            xputc(' ');
                            lposition++;
                        }
                        STRCPY(buf, (char FAR *)stp);
                        if (lposition + strlen(buf) > 77) {
                            lposition = 0;
                            xputc('\n');
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

static int listmatches(void)
{
        unsigned char *st = (unsigned char *)&lbuf[lposition-1];     /* string to match */
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
	int funconly = FALSE;
	
        if (lposition == 0) return FALSE; /* no string */

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

        if (l == 0) return FALSE; /* no string */

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
                    return FALSE; /* no package of that name */
            }
            if (*(++st2) == ':') st2++; /* start of target string */
            l -= (st2-st);
            st = st2;
            if (l < 0) return FALSE; /* no string */
        }
#endif
	// Check for function binding only
	if ((l < lposition-1 && *(st-1) == '(') ||
	    (l < lposition-2 && *(st-1) == '\'' && *(st-2) == '#')) {
		funconly = TRUE;
	}

        lposition = 0;
#ifdef PACKAGES
        searchobarray(getextsyms(pack), st, l, append, lpossave, &matchcount, &firstmatch, &matchend, funconly);
        if (!external_only) {
            searchobarray(getintsyms(pack), st, l, append, lpossave, &matchcount, &firstmatch, &matchend, funconly);
            pack = getuses(pack);   /* check out imports */
            for (; consp(pack); pack = cdr(pack))
                searchobarray(getextsyms(car(pack)), st, l, append, lpossave, &matchcount, &firstmatch, &matchend, funconly);
        }
#else
        searchobarray(getvalue(obarray), st, l, append, lpossave, &matchcount, &firstmatch, &matchend, funconly);
#endif
//        if (append) lposition = lcount = matchend;
//        else lposition = lpossave;
	if (matchcount == 0) { /* don't do any positioning if no match */
		lposition = lpossave;
		return FALSE;
	}
        if (matchcount > 1 || !append) {
            beep(); /* signal multiple matches */
            ostputc('\n');
        }
        if (!append) {
            lposition = lpossave;
        }
        else {
            lposition = lcount = matchend;
            for (l=lpossave; l-- >0; ) xputc(8);
            /* appending and not multiple matches -- overwrite */
        }
	return TRUE;
}

#endif

#ifdef GRAPHICS
// All graphics extension related code is here.
// We access graphics by calling C-interface functions in
// application.cpp:

// These in turn access methods of the CThread object which use the
// signal-slot interface across threads to the main thread executing
// Grapher object. Lots of work to pull it off, but it seems to work
// with as little possible upset to the existing XLISP code.
//
// The provided functions are:
// void move_to(int x, int y);
// void draw_to(int x, int y);
// void set_size(int width, int height); // Set the desired screen size
// void erase_screen(void);
// void set_foreground(int r, int g, int b); // r&256=1 to do XOR
// void set_background(int r, int g, int b);
// void write_text(char *string);            // uses default font.
//    void set_brush(int r, int g, int b, int style);
//    void draw_rectangle(int x1, int y1, int x2, int y2);
//    void draw_ellipse(int x, int y, int w, int h);
//    void set_font(int size, int type, int style);
// void update(void);                        // screen not updated
                                             // until this funcion is
                                             // called

// We will consider ourselves to be in character mode if the XLISP MODE
// is set to a character mode or we read from the keyboard as all
// terminal I/O is character mode. We go to graphics mode when any
// graphics move or draw command is given. In graphics mode text is
// written to the graphics window instead of the terminal.
// We will update the screen when we do keyboard input or call the
// XLISP MODE command. If the mode doesn't change, the mode command is
// a simple refresh of the display.

   

   // function goto-xy which set/obtains cursor position
   // If we are in a graphics mode this will end up doing a "move_to"
   // in preparation for an eventual "write_text".
   // If we are in text mode, we will set the position by sending
   // the ANSI command. This is a kludge since we won't be retrieving
   // the current position in text mode.
   
LVAL xgotoxy()
{
    LVAL res;
    int cxpos = (int)getfixnum(xlgafixnum());
    int cypos = (int)getfixnum(xlgafixnum());
    int lastx=cxpos, lasty=cypos;
    xllastarg();
    flushgcbuf();
    if (gModeOut) {
           int width, height;
           get_metrics(&width, &height);
           lastx = xpos/width + 1;
           lasty = ypos/height + 1;
           if (cxpos < 1) cxpos=1;
           else if (cxpos > xSize/width) cxpos = xSize/width;
           xpos = (cxpos-1)*width;
           if (cypos < 1) cypos = 1;
           else if (cypos > ySize/height) cypos = ySize/height;
           ypos = cypos * height;
    } else {
        char buf[32];
        sprintf(buf, "\033[%d;%dH", cypos, cxpos);
        (void)write(2, buf, strlen(buf));
    }

    xlsave1(res);
    res = consa(cvfixnum((FIXTYPE)lasty));
    res = cons(cvfixnum((FIXTYPE)lastx), res);
    xlpop();
    return res;
}

// Clear the graphics screen. In character mode we also
// clear the console screen with the ANSI sequence.

LVAL xcls()
{
    xllastarg();
    if (!gModeOut) {
        (void)write(2, "\033[2J\033[1;1H", 10);
    }

    if (gModeInit) {
        erase_screen();
        gcindex = 0;    // Flush print buffer
    }
    return NIL;
}

// Clear to end of line only works for the console and sends an ANSI
// sequence. Why do we even have this one??

LVAL xcleol()
{
    xllastarg();
    if (!gModeOut) {
        (void)write(2, "\033[K", 3);
    }
    return NIL;
}

/* xmode -- set display mode */
/* called with either ax contents, or ax,bx,xsize,ysize */
/* This obviously is bogus, but tries its best to mimic the old
 * CGA/EGA/VGA modes. If xsize and ysize are specified then this sets
 * an arbitrary screen size with 256^3 colors.
 * If mode 0-3 is selected we are left in text mode otherwise we are
 * left in graphics mode.
 * Note that doing a drawing command switches back to the last graphics
 * mode automatically. Reading from the console switches to text mode
 * automatically. */

LVAL xmode()
{
    int mode, xsize, ysize;
    int isText = FALSE;
    if (gModeInit) flushgcbuf(); // Make sure we have written out any graphics text
    mode = (int)getfixnum(xlgafixnum());
    if (moreargs()) { // ignore this
        (void)getfixnum(xlgafixnum());
    }
    if (moreargs()) { // gotta be two more
        xsize = getfixnum(xlgafixnum());
        ysize = getfixnum(xlgafixnum());
        if (xsize < 320) xsize = 320;
        if (ysize < 200) ysize = 200;
    } else {
        switch (mode) {
            case 0: case 1:
            case 2: case 3: isText = TRUE; break;
            case 4: case 5: case 13: case 19: xsize =320; ysize=200; break;
            case 6: case 14: xsize=640; ysize=200; break;
            case 16: xsize = 640; ysize = 350; break;
            case 18: xsize = 640; ysize = 480; break;
            default: xsize = 1024; ysize = 768; break;
        }
    }

    if (isText) {
        gModeOut = FALSE;
        if (gModeInit) update(); // Make sure display is updated as we leave
        LVAL res;
        xlsave1(res);
        res = consa(cvfixnum((FIXTYPE)-1));
        res = cons(cvfixnum((FIXTYPE)-1), res);
        res = cons(cvfixnum((FIXTYPE)25), res);
        res = cons(cvfixnum((FIXTYPE)(mode > 1 ? 80 : 40)), res);
        xlpop();
        return res;

    }

    if (gModeInit && !isText && xsize==xSize && ysize==ySize) {
        // no change, but refresh display
        update();
    } else {
        // we need to set the size
        xSize = xsize;
        ySize = ysize;
        set_size(xSize, ySize);
        gModeInit = TRUE;
    }
    gModeOut = TRUE; // direct output to graphics window
    {   // return current screen size
        LVAL res;
        int width, height;
        get_metrics(&width, &height);
        xlsave1(res);
        res = consa(cvfixnum((FIXTYPE)ysize));
        res = cons(cvfixnum((FIXTYPE)xsize), res);
        res = cons(cvfixnum((FIXTYPE)(ySize/height)), res);
        res = cons(cvfixnum((FIXTYPE)(xSize/width)), res);
        xlpop();
        return res;
    }
}

/* Move and draw functions. These force graphics mode as long
   as it has been initialized at some point. */
static void modeCheck() {
    if (!gModeInit) xlfail("Graphics mode not initialized");
    flushgcbuf();
    gModeOut = TRUE;
}

static unsigned char colorMap[16][3] = {
    {0,0,0},                  /* These choices work somehow */
    {0,0,170},
    {0,170,0},
    {0,170,170},
    {170,0,0},
    {170,0,170},
    {170,85,0},
    {170,170,170},
    {85,85,85},
    {85,85,255},
    {85,255,85},
    {85,255,255},
    {255,85,85},
    {255,85,255},
    {255,255,85},
    {255,255,255}};

static int inrange(int value) {
    if (value > 255) {
        return 255;
    }
    if (value < 0) {
        return 0;
    }
    return value;
}
/* Color is interpreted the same regardless of the mode.
   We allow a three argument or one argument value. The one
   argument value mimics the early PC palleted colors while
   the three argument version allows 2^8 of each R, G, and B.
   Adding 256 to R gives XOR mode. */
LVAL xcolor()
{
    LVAL arg;
    int r, g, b, r2, g2, b2;
    int setbkg = FALSE;
    int xormode;

    modeCheck();
    
    arg = xlgafixnum();
    if (moreargs()) {
        r = getfixnum(arg);
        g = getfixnum(xlgafixnum());
        b = getfixnum(xlgafixnum());
        if (moreargs()) {   /* set background */
            r2 = inrange(getfixnum(xlgafixnum()));
            g2 = inrange(getfixnum(xlgafixnum()));
            b2 = inrange(getfixnum(xlgafixnum()));
            xllastarg();
            setbkg = TRUE;
        }
        xormode = (int)(r & 256);
        arg = s_true;
        r = inrange(r & 255);
        g = inrange(g);
        b = inrange(b);
    }
    else {
        int v = (int)getfixnum(arg);
        int bgindex = ((v >> 4) & 7) + ((v >> 5) & 8);
        int fgindex = v&15;
        xormode = (v&128) != 0;
        r = colorMap[fgindex&15][0];
        g = colorMap[fgindex&15][1];
        b = colorMap[fgindex&15][2];
        r2 = colorMap[bgindex][0];
        g2 = colorMap[bgindex][1];
        b2 = colorMap[bgindex][2];
        setbkg = TRUE;
    }

    if (setbkg) set_background(r2, g2, b2);
    set_foreground(xormode ? r+256 : r, g, b);
    
    return arg;
}


/* Specify a color (integer or triplet), and style (integer) for the
 * fill brush. */
LVAL xbrush()
{
    LVAL arg, argsave;
    int r, g, b, style;

    modeCheck();
    argsave = arg = xlgetarg();
    if (consp(arg)) {
        if (ntype(car(arg)) != FIXNUM) {
            xlerror("Triplet color expected", argsave);
        }
        r = 255 & getfixnum(car(arg));
        arg = cdr(arg);
        if (!consp(arg) || ntype(car(arg)) != FIXNUM) {
            xlerror("Triplet color expected", argsave);
        }
        g = 255 & getfixnum(car(arg));
        arg = cdr(arg);
        if (!consp(arg) || ntype(car(arg)) != FIXNUM ||!null(cdr(arg)))  {
            xlerror("Triplet color expected", argsave);
        }
        b = 255 & getfixnum(car(arg));
    } else if (ntype(arg) == FIXNUM) {
        int v = getfixnum(arg);
        r = colorMap[v&15][0];
        g = colorMap[v&15][1];
        b = colorMap[v&15][2];
    } else {
        xlerror("Color value expected", arg);
    }
    style = getfixnum(xlgafixnum());
    xllastarg();
            
    set_brush(r, g, b, style);
    return (s_true);
}

LVAL xfont()
{
    int size, type, style;
    modeCheck();
    size = getfixnum(xlgafixnum());
    type = getfixnum(xlgafixnum());
    style = getfixnum(xlgafixnum());
    set_font(size, type, style);

    return s_true;
}



#define FLIPY(y) (ySize - (y) - 1)

LVAL xrect()
{
    int x1, y1, w, h;
    x1 = getfixnum(xlgafixnum());
    y1 = getfixnum(xlgafixnum());
    w = getfixnum(xlgafixnum());
    h = getfixnum(xlgafixnum());
    xllastarg();
    modeCheck();
    draw_rectangle(x1, FLIPY(y1), x1+w, FLIPY(y1+h));
    return (s_true);
}

LVAL xellipse()
{
    int x, y, w, h;
    x = getfixnum(xlgafixnum());
    y = getfixnum(xlgafixnum());
    w = getfixnum(xlgafixnum());
    if (moreargs()) {
        h = getfixnum(xlgafixnum());
    } else {
        h = w;
    }
    xllastarg();
    modeCheck();
    draw_ellipse(x, FLIPY(y), w, -h);
    return (s_true);
}    

LVAL xdraw()
{
    LVAL arg;
    int newx, newy;
    modeCheck();
    while (moreargs()) {
        arg = xlgafixnum();
        newx = (int) getfixnum(arg);

        arg = xlgafixnum();
        newy = FLIPY((int) getfixnum(arg));

        draw_to(newx,newy);

        xpos = newx;
        ypos = newy;
    }
    return s_true;
}

/* xdrawrel -- absolute draw */

LVAL xdrawrel()
{
    LVAL arg;
    int newx, newy;

    modeCheck();
    while (moreargs()) {
        arg = xlgafixnum();
        newx = xpos + (int) getfixnum(arg);

        arg = xlgafixnum();
        newy = ypos - (int) getfixnum(arg);

        draw_to(newx,newy);

        xpos = newx;
        ypos = newy;
    }
    return s_true;
}

/* xmove -- absolute move, then draw */

LVAL xmove()
{
    LVAL arg;

    modeCheck();
    arg = xlgafixnum();
    xpos = (int) getfixnum(arg);

    arg = xlgafixnum();
    ypos = FLIPY((int) getfixnum(arg));

    move_to(xpos, ypos);
    
    return (xdraw());
}

/* xmoverel -- relative move */

LVAL xmoverel()
{
    LVAL arg;

    modeCheck();
    arg = xlgafixnum();
    xpos += (int) getfixnum(arg);

    arg = xlgafixnum();
    ypos -= (int) getfixnum(arg);

    move_to(xpos, ypos);

    return (xdrawrel());
}

/* Add a character to the graphic character buffer. If it is full then
 * flush it out */
static void pushgcbuf(char ch) {
    if (ch == '\b') {
        // Backspace by flushing the buffer and then backing up the position
        int height, width;
        get_metrics(&width, &height);
        flushgcbuf();
        xpos -= width;
        if (xpos < 0) xpos = 0;
    } else if (ch == 7) {
        // We need to do the bell in text,  not graphics
        char buf[2] = {7};
        flushgcbuf();
        write(2, buf, 1);
    } else if (ch == '\n') {
        int height, width;
        get_metrics(&width, &height);
        flushgcbuf();
        xpos = 0;
        ypos += height;
        if (ypos >= ySize) ypos = ySize-1;
    } else {
        gcharbuf[gcindex++] = ch;
        if (gcindex >= GCBUFSIZE) flushgcbuf();
    }
}

/* Flush out the contents of the graphic character buffer */
static void flushgcbuf(void) {
    if (gcindex > 0) {
        int height, width;
        get_metrics(&width, &height);
        move_to(xpos, ypos);
        gcharbuf[gcindex] = '\0';
        write_text(gcharbuf);
        update();
        xpos += width*gcindex;
        gcindex = 0;
        if (xpos >= xSize) { // Do a complementary carriage return
            ypos += height;
            if (ypos >= ySize) ypos = ySize-1;
        }
    }
}

/* If we are in graphic mode, update the display and go to text mode */
void xrevertToText(void) {
    if (gModeOut) {
        if (gcindex > 0) 
            flushgcbuf();
        else
            update();
        gModeOut = FALSE;
    }
}


#endif
