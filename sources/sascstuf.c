/*I doubt that standard input and output can be redirected with this version*/
/* -*-C-*-
********************************************************************************
*
* File:     sascstuf.c
* Description:  MVS/CMS SAS/C Specific interfaces for XLISP
* Author:   Dave Rivers
*
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
* Modified again by Tom Almy
* Modified again by Dave Rivers; "cloned" from unixstuf.c, with IBM/370
*  changes for compiling with SAS/C on MVS and CMS.
********************************************************************************
*/

#include <signal.h>
#include <sys/types.h>
#include <time.h>

#include "xlisp.h"

#define LBSIZE  200
#define HZ 60

/* Be sure to use the TSO: default style for file names */
char *_style="TSO:";
char *_stdiamp=""; /* No library-supplied prompting */

/* -- external variables */
extern  FILEP   tfp;
extern long times();
extern LVAL xlenv, xlfenv, xldenv;

/* -- local variables */
static  char    lbuf[LBSIZE];
static  int lpos[LBSIZE];
int lposition;  /* export this */
static  int lindex;
static  int lcount;

char *xfgets();
char read_keybd();

/*
 *  Tables for conversions of EBCDIC to ASCII; this corresponds
 * to the IBM code page for POSIX, and the one SAS/C CSL uses
 * for X11, NFS, rpc,  etc...
 */

static unsigned int atoe_tab[256] =  {
      0x00, 0x01, 0x02, 0x03, 0x37, 0x2d, 0x2e, 0x2f,  /* 0 - 7 */
      0x16, 0x05, 0x15, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,  /* 8 - 15 */
      0x10, 0x11, 0x12, 0x13, 0x3c, 0x3d, 0x32, 0x26,  /* 16 - 23 */
      0x18, 0x19, 0x3f, 0x27, 0x1c, 0x1d, 0x1e, 0x1f,  /* 24 - 31 */
      0x40, 0x5a, 0x7f, 0x7b, 0x5b, 0x6c, 0x50, 0x7d,  /* 32 - 39 */
      0x4d, 0x5d, 0x5c, 0x4e, 0x6b, 0x60, 0x4b, 0x61,  /* 40 - 47 */
      0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,  /* 48 - 55 */
      0xf8, 0xf9, 0x7a, 0x5e, 0x4c, 0x7e, 0x6e, 0x6f,  /* 56 - 63 */
      0x7c, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,  /* 64 - 71 */
      0xc8, 0xc9, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6,  /* 72 - 79 */
      0xd7, 0xd8, 0xd9, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6,  /* 80 - 87 */
      0xe7, 0xe8, 0xe9, 0xad, 0xe0, 0xbd, 0x5f, 0x6d,  /* 88 - 95 */
      0x79, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,  /* 96 - 103 */
      0x88, 0x89, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96,  /* 104 - 111 */
      0x97, 0x98, 0x99, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6,  /* 112 - 119 */
      0xa7, 0xa8, 0xa9, 0xc0, 0x4f, 0xd0, 0xa1, 0x07,  /* 120 - 127 */
      0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x06, 0x17,  /* 128 - 135 */
      0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x09, 0x0a, 0x1b,  /* 136 - 143 */
      0x30, 0x31, 0x1a, 0x33, 0x34, 0x35, 0x36, 0x08,  /* 144 - 151 */
      0x38, 0x39, 0x3a, 0x3b, 0x04, 0x14, 0x3e, 0xff,  /* 152 - 159 */
      0x41, 0xaa, 0x4a, 0xb1, 0x9f, 0xb2, 0x6a, 0xb5,  /* 160 - 167 */
      0xbb, 0xb4, 0x9a, 0x8a, 0xb0, 0xca, 0xaf, 0xbc,  /* 168 - 175 */
      0x90, 0x8f, 0xea, 0xfa, 0xbe, 0xa0, 0xb6, 0xb3,  /* 176 - 183 */
      0x9d, 0xda, 0x9b, 0x8b, 0xb7, 0xb8, 0xb9, 0xab,  /* 184 - 191 */
      0x64, 0x65, 0x62, 0x66, 0x63, 0x67, 0x9e, 0x68,  /* 192 - 199 */
      0x74, 0x71, 0x72, 0x73, 0x78, 0x75, 0x76, 0x77,  /* 200 - 207 */
      0xac, 0x69, 0xed, 0xee, 0xeb, 0xef, 0xec, 0xbf,  /* 208 - 215 */
      0x80, 0xfd, 0xfe, 0xfb, 0xfc, 0xba, 0xae, 0x59,  /* 216 - 223 */
      0x44, 0x45, 0x42, 0x46, 0x43, 0x47, 0x9c, 0x48,  /* 224 - 231 */
      0x54, 0x51, 0x52, 0x53, 0x58, 0x55, 0x56, 0x57,  /* 232 - 239 */
      0x8c, 0x49, 0xcd, 0xce, 0xcb, 0xcf, 0xcc, 0xe1,  /* 240 - 247 */
      0x70, 0xdd, 0xde, 0xdb, 0xdc, 0x8d, 0x8e, 0xdf,  /* 248 - 255 */
 };

static unsigned int etoa_tab[256] = {
      0x00, 0x01, 0x02, 0x03, 0x9c, 0x09, 0x86, 0x7f,  /* 0 - 7 */
      0x97, 0x8d, 0x8e, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,  /* 8 - 15 */
      0x10, 0x11, 0x12, 0x13, 0x9d, 0x0a, 0x08, 0x87,  /* 16 - 23 */
      0x18, 0x19, 0x92, 0x8f, 0x1c, 0x1d, 0x1e, 0x1f,  /* 24 - 31 */
      0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x17, 0x1b,  /* 32 - 39 */
      0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x05, 0x06, 0x07,  /* 40 - 47 */
      0x90, 0x91, 0x16, 0x93, 0x94, 0x95, 0x96, 0x04,  /* 48 - 55 */
      0x98, 0x99, 0x9a, 0x9b, 0x14, 0x15, 0x9e, 0x1a,  /* 56 - 63 */
      0x20, 0xa0, 0xe2, 0xe4, 0xe0, 0xe1, 0xe3, 0xe5,  /* 64 - 71 */
      0xe7, 0xf1, 0xa2, 0x2e, 0x3c, 0x28, 0x2b, 0x7c,  /* 72 - 79 */
      0x26, 0xe9, 0xea, 0xeb, 0xe8, 0xed, 0xee, 0xef,  /* 80 - 87 */
      0xec, 0xdf, 0x21, 0x24, 0x2a, 0x29, 0x3b, 0x5e,  /* 88 - 95 */
      0x2d, 0x2f, 0xc2, 0xc4, 0xc0, 0xc1, 0xc3, 0xc5,  /* 96 - 103 */
      0xc7, 0xd1, 0xa6, 0x2c, 0x25, 0x5f, 0x3e, 0x3f,  /* 104 - 111 */
      0xf8, 0xc9, 0xca, 0xcb, 0xc8, 0xcd, 0xce, 0xcf,  /* 112 - 119 */
      0xcc, 0x60, 0x3a, 0x23, 0x40, 0x27, 0x3d, 0x22,  /* 120 - 127 */
      0xd8, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,  /* 128 - 135 */
      0x68, 0x69, 0xab, 0xbb, 0xf0, 0xfd, 0xfe, 0xb1,  /* 136 - 143 */
      0xb0, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70,  /* 144 - 151 */
      0x71, 0x72, 0xaa, 0xba, 0xe6, 0xb8, 0xc6, 0xa4,  /* 152 - 159 */
      0xb5, 0x7e, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,  /* 160 - 167 */
      0x79, 0x7a, 0xa1, 0xbf, 0xd0, 0x5b, 0xde, 0xae,  /* 168 - 175 */
      0xac, 0xa3, 0xa5, 0xb7, 0xa9, 0xa7, 0xb6, 0xbc,  /* 176 - 183 */
      0xbd, 0xbe, 0xdd, 0xa8, 0xaf, 0x5d, 0xb4, 0xd7,  /* 184 - 191 */
      0x7b, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,  /* 192 - 199 */
      0x48, 0x49, 0xad, 0xf4, 0xf6, 0xf2, 0xf3, 0xf5,  /* 200 - 207 */
      0x7d, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50,  /* 208 - 215 */
      0x51, 0x52, 0xb9, 0xfb, 0xfc, 0xf9, 0xfa, 0xff,  /* 216 - 223 */
      0x5c, 0xf7, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,  /* 224 - 231 */
      0x59, 0x5a, 0xb2, 0xd4, 0xd6, 0xd2, 0xd3, 0xd5,  /* 232 - 239 */
      0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,  /* 240 - 247 */
      0x38, 0x39, 0xb3, 0xdb, 0xdc, 0xd9, 0xda, 0x9f   /* 248 - 255 */
 };



/*
 *     etoa
 *       Convert an EBCDIC character into its ASCII representation.
 */
unsigned int
etoa(echar)
unsigned int echar;
{

        return etoa_tab[echar];
}


/*
 *      atoe.
 *        Convert an ASCII character into its EBCDIC representation.
 */
unsigned int
atoe(achar)
unsigned int achar;
{

        return atoe_tab[achar];
}

/******************************************************************************
 * xsystem - run a process, sending output (if any) to stdout/stderr
 *
 * syntax: (system <command line>)
 *         <command line> is a string to be sent to the subshell (sh).
 *
 * Returns T if the command executed succesfully, otherwise returns the
 * integer shell exit status for the command.
 *
 * Added to XLISP by Niels Mayer
 ******************************************************************************/
LVAL xsystem()
{
  extern LVAL s_true;
  extern int sys_nerr;
  extern char *sys_errlist[];
  extern int errno;
  LVAL command;
  int  result;
  char temptext[1024];

  /* get shell command */
  command = xlgastring();
  xllastarg();

  /* run the process */
  result = system((char *) getstring(command));

  if (result == -1) {       /* if a system error has occured */
    (void) sprintf(temptext, "Error in system(): %s\n", strerror(errno));
    xlfail(temptext);
  }

  /* return T if success (exit status 0), else return exit status */
  return (result ? cvfixnum(result) : s_true);
}

/******************************************************************************/
/* -- Written by dbetz for XLISP 2.0 */


/* -- osinit - initialize */
VOID osinit(banner)
char    *banner;
{
    fprintf(stderr,"%s\nSAS/C 370 version, by Dave Rivers\n", banner );
    init_tty();
    lindex  = 0;
    lcount  = 0;
    printf("\n");
}

/* -- osfinish - clean up before returning to the operating system */
VOID osfinish()
{
}


/* -- xoserror - print an error message */
VOID xoserror(msg)
char    *msg;
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
    /* On MVS/CMS; just return the name back... This could */
    /* probably be improved to handle file types, etc..    */
    strcpy(rname, name);

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
   

    quiet(1);   /* To avoid nastly little library messages */ 
    
    if ((fp = fopen(name,mode)) == NULL) {
        free(filetab[i].tname);
        quiet(0);
        return CLOSED;
    }

    filetab[i].fp = fp;

    strcpy(filetab[i].tname, namebuf);

    quiet(0);
    return i;
}
    
void osclose(f)
  FILEP f;
{
    quiet(1);

    fclose(filetab[f].fp);
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
  /* Really can't do anything on MVS/CMS; so we just return TRUE */
  return TRUE;
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
        else lposition++;

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

void oscheck()
{
}

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
static xinfo()
{
  extern int nfree, gccalls;
  extern long total;
  char tymebuf[100];
  time_t tyme;
  char buf[500];

  time(&tyme);
  strcpy(tymebuf, ctime(&tyme));
  tymebuf[19] = '\0';
  sprintf(buf,"\n[ %s Free: %d, GC calls: %d, Total: %ld ]",
    tymebuf, nfree,gccalls,total);
  errputstr(buf);
}

/* xflush - flush the input line buffer and start a new line */
static xflush()
{
  osflush();
  ostputc('\n');
}



char read_keybd()
{
   int nrd;
   char buf[1];
   char result;

   nrd = read(0, buf, 1);
   buf[nrd] = 0;

#if 0
   if (buf[0] == 127) {     /* perform the BACKSPACE */
      stdputstr("\010");
      stdputstr(" ");
      stdputstr("\010");
   }
   else
      stdputstr(buf);
#endif

   result = buf[0];
   return(result);
}


init_tty()
{
    /* extern sigcatch(); */
    extern onsusp();
    extern VOID xltoplevel();

    signal(SIGINT, xltoplevel);
}



onsusp()
{
}



char *xfgets(s, n, iop)
char *s;
register FILE *iop;
{
    register c;
    register char *cs;

    cs = s;
    while (--n>0 && (c = read_keybd()) != EOF) {
#if 0
         switch(c) {
          case '\002' :         /* CTRL-b */
          case '\003' :         /* CTRL-c */
          case '\007' :         /* CTRL-g */
          case '\020' :         /* CTRL-p */
          case '\024' : osx_check(c);   /* CTRL-t */
                n++;
                break;

          case 8      :
          case 127    : n+=2;       /* BACKSPACE */
                *cs--;
                *cs = ' ';
                break;

          default     : *cs++ = c;  /* character */
        }
#else
          *cs++ = c;
#endif
        if (c=='\n') break;
    }
    if (c == EOF && cs==s) return(NULL);
    *cs++ = '\0';
    return(s);
}

#ifdef TIMES
/***********************************************************************/
/**                                   **/
/**          Time and Environment Functions           **/
/**                                   **/
/***********************************************************************/

unsigned long ticks_per_second() { return((unsigned long) CLOCKS_PER_SEC); }

unsigned long run_tick_count()
{
  clock_t start_time;
  clock_t current_time;
  static int start_inited=0;

  if(!start_inited) {
    start_inited = 1;
    start_time = clock();
  }

  current_time = clock();
  
  return((unsigned long) (current_time - start_time));
}

unsigned long real_tick_count()
{                  /* Real time */
  return((unsigned long) (60 * (time((unsigned long *) NULL))));
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

