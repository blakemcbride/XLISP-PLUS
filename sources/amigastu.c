/* amigastuff.c
   Amiga specific routines for XLisp
   originally on Fred Fish #181
   modified for Tom Almy's 2.1c by Hume Smith 1991 Dec 31
   Lattice C 5.04
*/

#include "xlisp.h"
extern int onbreak(int (*)(void));

#ifndef MANX
#define agetc getc   /* Not sure if this will work in all cases (fnf) */
#define aputc putc   /* Not sure if this will work in all cases (fnf) */
#endif

#define LBSIZE 200

/* external routines */
extern double ran();

/* line buffer variables */
static char lbuf[LBSIZE];
static int lpos[LBSIZE];
static int lindex;
static int lcount;
int lposition; /* export to xlprin.c */

#define NEW 1006
static long xlispwindow;
extern FILE *tfp;
static void xputc(int),xflush(void);
#ifdef TIMES
static unsigned long basetime;
unsigned long real_tick_count();
#endif

/* null function for break handler HCLS */
int breakhandler(void){return 0;}

/* osinit - initialize */
void osinit(banner)
char *banner;
{
    onbreak(breakhandler); /* system will ignore ctrl-C ctrl-D HCLS */
    xlispwindow = Open("RAW:0/12/640/188/XLisp", NEW);
    while(*banner) xputc(*banner++);
    xputc('\n');
    lposition = 0;
    lindex = lcount = 0;
#ifdef TIMES
    basetime=real_tick_count();
#endif
}

void osfinish ()
{
    Close (xlispwindow);
}

/* osrand - return a random number between 0 and n-1 */
int osrand(n)
  int n;
{
    n = (int)(ran() * (double)n);
    return (n < 0 ? -n : n);
}

/* oscheck - check for control characters during execution */
void oscheck()
{
    switch (xcheck()) {

    case '\002': /* ctrl-b */
    osflush();
    xlbreak("**BREAK**",s_unbound);
    break;

    case '\004': /* ctrl-d */
    osflush();
    xltoplevel();
    break;
    }
}

/* osflush - flush the input line buffer */
void osflush()
{
    lindex = lcount = 0;
}

/* xgetc - get a character from the terminal without echo */
static int xgetc()
{
    char ch;

    Read(xlispwindow, &ch, 1);
    return ch & 0xff;
}
    
/* xputc - put a character to the terminal */
static void xputc(ch)
  int ch;
{
    char chout;

    chout = ch;
    Write (xlispwindow, &chout, 1L);
}

/* xcheck - check for a character */
static int xcheck()
{
    if (WaitForChar (xlispwindow, 0L) == 0L)
    return (0);
    return (xgetc() & 0xFF);
}

double ran ()   /* Just punt for now, not in Manx C; FIXME!!*/
{
   static long seed = 654321;
   long lval;
   double dval;

   seed *= ((8 * (123456) - 3));
   lval = seed & 0xFFFF;
   dval = ((double) lval) / ((double) (0x10000));
   return (dval);
}

/* ADDED FOR V2.0 */

/* osclose - close a file */
int osclose(fp)
  FILE *fp;
{
    return (fclose(fp));
}

/* ostputc - put a character to the terminal */
void ostputc(ch)
  int ch;
{
    /* check for control characters */
    oscheck();

    /* output the character */
    if (ch == '\n') {
   xputc('\r'); xputc('\n');
   lposition = 0;
    }
    else {
   xputc(ch);
   lposition++;
   }

   /* output the character to the transcript file */
   if (tfp)
   osaputc(ch,tfp);
}

/* ostgetc - get a character from the terminal */
int ostgetc()
{
    int ch;

    /* check for a buffered character */
    if(lcount--)
    return (int)lbuf[lindex++];

    /* get an input line */
    for(lcount = 0; ; )
    switch(ch = xgetc()){
    case 3: /* ctrl-c */
        xflush();
        xltoplevel();

    case 7: /* ctrl-g */
        xflush();
        xlcleanup();

    case 16: /* ctrl-p */
        xflush();
        xlcontinue();

    case 26: /* ctrl-z */
        xflush();
        return EOF;

    case 13: /* return */
        lbuf[lcount++] = '\n';
        xputc('\r');
        xputc('\n');
        lposition = 0;
        if(tfp)
        for(lindex = 0; lindex < lcount; ++lindex)
            osaputc(lbuf[lindex], tfp);
        lindex = 0;
        lcount--;
        return (int)lbuf[lindex++];

    case 8:
    case 127:
        if(lcount){
        lcount--;
        while(lposition > lpos[lcount]){
            xputc('\010');
            xputc(' ');
            xputc('\010');
            lposition--;
        }
        }
        break;

    default:
        if (ch == '\t' || (ch >= 0x20 && ch < 0x7F)) {
        lbuf[lcount] = ch;
        lpos[lcount] = lposition;
        if (ch == '\t')
            do {
            xputc(' ');
            } while(++lposition & 7);
        else {
            xputc(ch);
            lposition++;
        }
        lcount++;
       }
    }
}

/* xflush - flush the input line buffer */
static void xflush()
{
    ostputc('\n');
    osflush();
}

/* osaopen - open an ascii file */
FILE *osaopen(name,mode)
  char *name,*mode;
{
    return (fopen(name,mode));
}

/* xoserror - print an error message */
void xoserror(msg)
  char *msg;
{
    printf("error: %s\n",msg);
}

/* xsystem - the built-in function 'system' */
LVAL xsystem()
{
    char *str;
    int result;

    /* get the command string */
    str = getstring(xlgastring());
    xllastarg();
    result = Execute(str,0L,xlispwindow);
    return (cvfixnum((FIXTYPE)result));
}

/* osagetc - get a character from an ascii file */
int osagetc(fp)
  FILE *fp;
{
    return (getc(fp));
}

/* osaputc - put a character to an ascii file */
int osaputc(ch,fp)
  int ch; FILE *fp;
{
    return (putc(ch,fp));
}

/* ossymbols - lookup important symbols */
void ossymbols()
{
}

#ifdef PATHNAMES

#define BSIZE (252)

/* the Lattice 5.04 libraries have a bug in this funvtion */

char *getenv(char *x){
   FILE *f;
   static char val[BSIZE+4]="env:";
   char *r=val+4;

   strcpy(r,x);

  if(f=fopen(val,"r")){
      if(r==fgets(r,BSIZE,f)){
         int n=strlen(r);
         if(n<BSIZE){
            if(n && r[n-1]=='\n'){
               r[--n]=0; /* trim off any trailing newline */
            }
         }
         else{
            /* line too long for buffer */
            r=0;
         }
      }
      else{
         r=0;
      }
      fclose(f);
   }
   else{
      r=0;
   }

   return r;
}

#undef BSIZE

/* ospopen - open using a search path */
FILE *ospopen(name, ascii)
char *name;
int ascii;
{
    FILE *fp;
    char *paths = getenv(PATHNAMES);
    char *newnamep, ch;
    char newname[256];

    /* do no searching if path is explicit */
    if (strchr(name,'/') || !paths)
    return fopen(name, "r");

    do {
    if (!*paths)
        /* no more paths, check current directory */
        return fopen(name, "r");

    newnamep = newname;
    while ((ch = *paths++) && ch != ';' && ch != ' ')
        *newnamep++ = ch;

    if (ch == '\0') paths--;

    if (':' != (ch = *(newnamep-1)) && '/' != ch)
        *newnamep++ = '/';  /* final path separator needed */

    strcpy(newnamep, name);
    fp = fopen(newname, "r");
    } while (!fp);   /* not yet found */

    return fp;
}
#endif

#ifdef TIMES
/* the Amiga's clock has microsecond resolution;
   that's too much to be useful here, so we'll
   reduce it arbitrarily to seconds */

unsigned long ticks_per_second() { return 1ul; }

unsigned long real_tick_count()
{
  unsigned int i[2];

  timer(i);
  return i[0];
}

unsigned long run_tick_count()
{
  return real_tick_count()-basetime;
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

/* from TAA's unixstuff.c                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   