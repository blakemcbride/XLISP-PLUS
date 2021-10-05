/* IF THIS FILE DOESN'T COMPILE, MAKE WHATEVER CHANGES ARE NECESSARY AND
 * PROVIDE THE CHANGES (AND YOUR SPECIFIC SITUATION) TO ME, almy@teleport.com */
/* -*-C-*-
********************************************************************************
*
* File:         unixstuff.c
* Description:  UNIX-Specific interfaces for XLISP
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
********************************************************************************
*/

/*****************************************************************************
*               edit history
*
* 92Jan29 CrT.  Edit history.  Reversed SysV gtty/stty #defs fixed.
*****************************************************************************/
#ifdef BSD
#define USE_OLD_TTY
#include <unistd.h>
#endif

#include <signal.h>
#include <sys/types.h>
#include <sys/times.h>
#include <time.h>
#include <errno.h>
#include <ctype.h>

#ifdef BSD
#include <sys/ioctl.h>
struct sgttyb savetty;
struct sgttyb newtty;
#define gtty(fd,arg)    (emacs_input ? 0 : ioctl(fd, TIOCGETP, arg))
#define stty(fd,arg)    (emacs_input ? 0 : ioctl(fd, TIOCSETP, arg))
#else
#ifdef POSIX
#include <termios.h>
struct termios savetty;
struct termios newtty;
#define gtty(fd,arg)    (emacs_input ? 0 : tcgetattr(fd, arg))
#define stty(fd,arg)    (emacs_input ? 0 : tcsetattr(fd, TCSANOW, arg))
#else
#include <termio.h>
struct termio savetty;
struct termio newtty;
#define gtty(fd,arg)    (emacs_input ? 0 : ioctl(fd, TCGETA, arg))
#define stty(fd,arg)    (emacs_input ? 0 : ioctl(fd, TCSETAF, arg))
#endif
#endif

#include "xlisp.h"

#define LBSIZE  200
#ifndef HZ
#define HZ 60
#endif

/* -- external functions */
/* extern long times(); */

/* -- local variables */
static  char    lbuf[LBSIZE];
static  int     lpos[LBSIZE];
static  int     lindex;
static  int     lcount;
static  int     emacs_input;

char *xfgets();
int read_keybd();


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
xsystem()
{
    char *          getenv();
    char           *comstr;
    LVAL            command;
    int             result;
    char            temptext[1024];

    /* get shell command */
    command = xlgastring();
    xllastarg();

    comstr = (char *) getstring(command);
    if (*comstr) {
            /* restore the terminal */
        stty(0, &savetty);
        /* run the process */
        result = system(comstr);
        /* restore the terminal */
        stty(0, &newtty);
                
        if (result == -1) {     /* if a system error has occured */
            xlfail("error in system call");
        }
    } else {
        /*
         * We were given a null string.  We'll try to find out what
         * shell the user uses and spawn it.
         */
        if (comstr = getenv("SHELL")) {
            int  pid;
            /*
             * we could just system(comstr), but that would get
             * two shells running...
             */
            /* restore the terminal */
            stty(0, &savetty);
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
            stty(0, &newtty);
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


/* -- osinit - initialize */
VOID osinit(banner)
char       *banner;
{
        char *getenv();

        redirectout = !isatty(fileno(stdout));
        redirectin = !isatty(fileno(stdin));

        if(!redirectin) {
          fprintf(stderr,"%s\nUNIX version\n", banner );
          init_tty();
        }
        emacs_input = getenv("EMACS") != NULL;
        lindex  = 0;
        lcount  = 0;
}

/* -- osfinish - clean up before returning to the operating system */
VOID osfinish()
{
  if(!redirectin)
    stty(0, &savetty);
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
    int i;
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
        
        getcwd(curdir, FNAMEMAX);

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
FILEP ospopen(name, ascii)
char *name;
int ascii;  /* value not used in UNIX */
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

int renamebackup(filename)
  char *filename;
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


/* -- ostgetc - get a character from the terminal */
int     ostgetc()
{
        while(--lcount < 0 )
                {
                if ( xfgets(lbuf,LBSIZE,stdin) == NULL )
                        return( EOF );

                lcount = strlen( lbuf );
                if (tfp!=CLOSED) OSWRITE(lbuf,1,lcount,tfp);

                lindex = 0;
                lposition = 0;
                }

        return( lbuf[lindex++] );
}


/* -- ostputc - put a character to the terminal */
VOID ostputc( ch )
int     ch;
{
        char buf[1];

        buf[0] = ch;

        if (ch == '\n') lposition = 0;
        else if (ch == '\b') {
            if (lposition > 0) --lposition;
        }
        else if (isprint(ch))
            lposition++;

        /* -- output the character */
/*        putchar( ch ); */
        write(1,buf,1);

        /* -- output the char to the transcript file */
        if ( tfp != CLOSED )
                OSPUTC( ch, tfp );
}




/* -- osflush - flush the terminal input buffer */
VOID osflush()
{
        lindex = lcount = 0;
}

static int sigint_received;

VOID oscheck()
{
    if (sigint_received) {
        sigint_received = 0;
        xltoplevel();
    }
}

static VOID xinfo();

osx_check(ch)
char ch;
{
     switch (ch) {
        case '\003':
          xltoplevel(); /* control-c */
        case '\007':
          xlcleanup();  /* control-g */
        case '\020':
          xlcontinue(); /* control-p */
        case '\024':    /* control-t */
          xinfo();
          printf("\n ");
     }
}


/* -- ossymbols - enter os-specific symbols */
VOID ossymbols()
{
}


/* xinfo - show information on control-t */
static VOID xinfo()
{
  char tymebuf[100];
  time_t tyme;
  char buf[500];

  time(&tyme);
  strcpy(tymebuf, ctime(&tyme));
  tymebuf[19] = '\0';
  sprintf(buf,"\n[ %s Free: %ld, GC calls: %ld, Total: %ld ]",
    tymebuf, nfree,gccalls,total);
  errputstr(buf);
}

/* xflush - flush the input line buffer and start a new line */
static VOID xflush()
{
  osflush();
  ostputc('\n');
}


int read_keybd()
{
   int nrd;
   unsigned char buf[2];

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
   if (!emacs_input && (isprint(buf[0]) || buf[0] == '\n'))
       stdputstr(buf);

   return (nrd > 0 ? buf[0] : EOF);
}

static void handle_interrupt(int sig)
{
#if !defined(BSD) && !defined(POSIX)
    /* SYS V requires reseting of SIGINT */
    signal(SIGINT, handle_interrupt);
#endif
    sigint_received = 1;
}

#ifdef POSIX
static void onsusp();
#else
static void onsusp();
#endif

init_tty()
{
        /* extern sigcatch(); */
#ifdef POSIX
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
#else
    signal(SIGINT, handle_interrupt);
    signal(SIGQUIT, SIG_IGN);

#ifdef SIGTSTP
    if (signal(SIGTSTP, onsusp) == SIG_DFL) {
        signal(SIGTSTP, onsusp);
    }
#endif
#endif

    if (gtty(0, &savetty) == -1) {
        printf("ioctl failed: not a tty\n");
        exit(1);
    }

    newtty = savetty;

    
#ifdef BSD
    newtty.sg_flags |= CBREAK;      /* turn off canonical mode */
                                    /* i.e., turn on cbreak mode */
    newtty.sg_flags &= ~ECHO;       /* turn off character echo */
#else
    newtty.c_lflag &= ~ICANON;  /* SYS 5 */
    newtty.c_lflag &= ~ECHO;
    newtty.c_cc[VMIN] = 1;
    newtty.c_cc[VTIME] = 1;
#endif
    /*
     * You can't request that it try to give you at least
     * 5 characters, nor set the timeout to 10 seconds,
     * as you can in the S5 example.  If characters come
     * in fast enough, though, you may get more than one.
     */
    if (stty(0, &newtty) == -1) {
        printf("cannot put tty into cbreak mode\n");
        exit(1);
    }
}

static
#ifdef BSD
   void
#endif
#ifdef POSIX
void
#endif
onsusp()
{
#ifdef SIGTSTP
#ifdef POSIX
    struct sigaction sa;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
#endif
    
    /* ignore SIGTTOU so we dont get stopped if csh grabs the tty */
#ifdef POSIX
    sa.sa_handler = SIG_IGN;
    sigaction(SIGTTOU, &sa, NULL);
#else
    signal(SIGTTOU, SIG_IGN);
#endif
    stty(0, &savetty);
    xflush();
#ifdef POSIX
    sa.sa_handler = SIG_DFL;
    sigaction(SIGTTOU, &sa, NULL);
#else
    signal(SIGTTOU,SIG_DFL);
#endif

    /* send the TSTP signal to suspend our process group */
#ifdef POSIX
    sa.sa_handler = SIG_DFL;
    sigaction(SIGTSTP, &sa, NULL);
    sigprocmask(SIG_SETMASK, &sa.sa_mask, NULL);
#else
    signal(SIGTSTP, SIG_DFL);
    sigsetmask(0);
#endif
    kill(0, SIGTSTP);
    /* pause for station break */

    /* we re back */
#ifdef POSIX
    sa.sa_handler = onsusp;
    sigaction(SIGTSTP, &sa, NULL);
#else
    signal(SIGTSTP, onsusp);
#endif
    stty(0, &newtty);
#endif
}



char *xfgets(s, n, iop)
char *s;
register FILE *iop;
{
        register c;
        register char *cs;

        cs = s;
        while (--n>0 && (c = read_keybd()) != EOF) {
             switch(c) {
                  case '\002' :                 /* CTRL-b */
                  case '\003' :                 /* CTRL-c */
                  case '\007' :                 /* CTRL-g */
                  case '\020' :                 /* CTRL-p */
                  case '\024' : osx_check(c);   /* CTRL-t */
                                n++;
                                break;

                  case 4:       c = EOF;
                                stdputstr("\n");
                                goto end_of_file;
                                
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
end_of_file:
        if (c == EOF && cs==s) return(NULL);
        *cs++ = '\0';
        return(s);
}

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


#ifndef BSD
#if 0
/*
 * substitute for BSD/SVR3 rename() system call, from
 * Janet Walz, walz@mimsy.umd.edu & Rich Salz, rsalz@pineapple.bbn.com
 */

int 
rename(oldname,newname)
char *oldname,*newname;
{
    (void)unlink(newname);
    if(link(oldname,newname))
        return(-1);
    return(unlink(oldname));
}
#endif
#endif
