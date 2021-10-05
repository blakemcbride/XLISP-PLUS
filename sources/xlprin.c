/* xlprint - xlisp print routine */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

/* forward declarations */
LOCAL VOID NEAR putpacksym _((LVAL fptr, LVAL val, int flag));
LOCAL VOID NEAR putsymbol _((LVAL fptr, char FAR *str, int flag));
LOCAL VOID NEAR putstring _((LVAL fptr, LVAL str));
LOCAL VOID NEAR putqstring _((LVAL fptr, LVAL str));
LOCAL VOID NEAR putatm _((LVAL fptr, char *tag, LVAL val));
LOCAL VOID NEAR putsubr _((LVAL fptr, char *tag, LVAL val));
LOCAL VOID NEAR putclosure _((LVAL fptr, LVAL val));
LOCAL VOID NEAR putfixnum _((LVAL fptr, FIXTYPE n));
#ifdef BIGNUMS
LOCAL VOID NEAR putbignum _((LVAL fptr, LVAL n));
#endif
LOCAL VOID NEAR putflonum _((LVAL fptr, FLOTYPE n));
LOCAL VOID NEAR putchcode _((LVAL fptr, int ch, int escflag));
LOCAL VOID NEAR putoct _((LVAL fptr, int n));
#ifdef PACKAGES
LOCAL VOID putpackage _((LVAL fptr, LVAL val));
#endif /* PACKAGES */
FORWARD VOID xlprintl _((LVAL fptr, LVAL vptr, int flag));


/* $putpatch.c$: "MODULE_XLPRIN_C_GLOBALS" */

/* xlprint - print an xlisp value */
VOID xlprint(fptr,vptr,flag)
  LVAL fptr,vptr; int flag;
{
    LVAL temp;
    temp = getvalue(s_printlevel);
    if (fixp(temp) && getfixnum(temp) <= 32767 && getfixnum(temp) >= 0) {
        plevel = (int)getfixnum(temp);
    }
    else {
        plevel = 32767;     /* clamp to "reasonable" level */
    }
    temp = getvalue(s_printlength);
    if (fixp(temp) && getfixnum(temp) <= 32767 && getfixnum(temp) >= 0) {
        plength = (int)getfixnum(temp);
    }
    else
        plength = 32767;

    xlprintl(fptr,vptr,flag);
}

VOID xlprintl(fptr,vptr,flag)
  LVAL fptr,vptr; int flag;
{
    LVAL nptr,next;
    int n,i;
    int llength;

#ifdef STSZ
    /* check the stack */
    stchck();
#endif

    /* check value type */
    switch (ntype(vptr)) {
    case SUBR:
        putsubr(fptr,"Subr",vptr);
        break;
    case FSUBR:
        putsubr(fptr,"FSubr",vptr);
        break;
    case CONS:
        if (plevel-- == 0) {            /* depth limitation */
            xlputc(fptr,'#');
            plevel++;
            break;
        }
        xlputc(fptr,'(');
        llength = plength;
        for (nptr = vptr; nptr != NIL; nptr = next) {
            if (llength-- == 0) { /* length limitiation */
                xlputstr(fptr,"... ");
                break;
            }
            xlprintl(fptr,car(nptr),flag);
            if ((next = cdr(nptr)) != NIL) {
                if (consp(next)) {
                    xlputc(fptr,' ');
                }
                else {
                    xlputstr(fptr," . ");
                    xlprintl(fptr,next,flag);
                    break;
                }
            }
        }
        xlputc(fptr,')');
        plevel++;
        break;
    case SYMBOL:
        if (vptr == s_unbound) {    /* TAA MOD for new unbound 1/92 */
            xlputstr(fptr, "#<Unbound>");
            break;
        }
        putpacksym(fptr, vptr, flag);
        break;
    case FIXNUM:
        putfixnum(fptr,getfixnum(vptr));
        break;
    case FLONUM:
        putflonum(fptr,getflonum(vptr));
        break;
    case CHAR:
        putchcode(fptr,getchcode(vptr),flag);
        break;
    case STRING:
        if (flag)
            putqstring(fptr,vptr);
        else
            putstring(fptr,vptr);
        break;
    case STREAM:
#ifdef FILETABLE
        {
            char *msg;
            FILEP fp = getfile(vptr);
            if (fp == CLOSED)   xlputstr(fptr, "#<Closed-Stream>");
            else {
#ifndef BIGNUMS
                char *msg2 = (vptr->n_sflags & S_BINARY)?"Binary":"Character";
#endif
                switch (vptr->n_sflags & (S_FORREADING | S_FORWRITING)) {
                    case S_FORREADING: msg = "Input-Stream"; break;
                    case S_FORWRITING: msg = "Output-Stream"; break;
                    default: msg = "IO-Stream"; break;
                }
#ifdef BIGNUMS
                if (vptr->n_sflags & S_BINARY)
                    sprintf(buf,"#<%s-Byte-%lu-%s %d:\"%s\">",
                            (vptr->n_sflags&S_UNSIGNED)?"Unsigned":"Signed",
                            vptr->n_bsiz*8UL, msg, fp+1, filetab[fp].tname);
                else
                    sprintf(buf,"#<Character-%s %d:\"%s\">",
                            msg, fp+1, filetab[fp].tname);
#else
                sprintf(buf,"#<%s %s %d:\"%s\">", msg2, msg, fp+1, filetab[fp].tname);
#endif
                xlputstr(fptr,buf);
            }
        }
#else
        {
            char *msg;
            FILEP fp = getfile(vptr);
            if (fp == CLOSED)   msg = "Closed-Stream";
            else if (fp == STDIN) msg = "Stdin-Stream";
            else if (fp == STDOUT) msg = "Stdout-Stream";
            else if (fp == CONSOLE) msg = "Terminal-Stream";
            else switch (vptr->n_sflags & (S_FORREADING | S_FORWRITING | S_BINARY)) {
                case (S_FORREADING|S_BINARY): msg = "Binary Input-Stream"; break;
                case (S_FORWRITING|S_BINARY): msg = "Binary Output-Stream"; break;
                case S_BINARY: msg = "Binary IO-Stream"; break;
                case S_FORREADING: msg = "Input-Stream"; break;
                case S_FORWRITING: msg = "Output-Stream"; break;
                default: msg = "IO-Stream"; break;
            }
            putatm(fptr,msg,vptr);
        }
#endif
        break;
    case USTREAM:
        putatm(fptr,"Unnamed-Stream",vptr);
        break;
    case OBJECT:
        /* putobj fakes a (send obj :prin1 file) call */
        putobj(fptr,vptr);
        break;
    case VECTOR:
        if (plevel-- == 0) {            /* depth limitation */
            xlputc(fptr,'#');
            plevel++;
            break;
        }
        xlputc(fptr,'#'); xlputc(fptr,'(');
        llength = plength;
        for (i = 0, n = getsize(vptr); n-- > 0; ) {
            if (llength-- == 0) { /* length limitiation */
                xlputstr(fptr,"... ");
                break;
            }
            xlprintl(fptr,getelement(vptr,i++),flag);
            if (n) xlputc(fptr,' ');
        }
        xlputc(fptr,')');
        plevel++;
        break;
    case STRUCT:
#ifdef HASHFCNS
        if (getelement(vptr,0) == a_hashtable) {
            putatm(fptr,"Hash-table",vptr);
            break;
        }
#endif
        xlprstruct(fptr,vptr,plevel,flag);
        break;
    case CLOSURE:
        putclosure(fptr,vptr);
        break;
#ifdef BIGNUMS
    case BIGNUM:
        putbignum(fptr, vptr);
        break;
    case RATIO:
        xlprintl(fptr, getnumer(vptr), flag);
        xlputc(fptr,'/');
        xlprintl(fptr, getdenom(vptr), flag);
        break;
#endif
#ifdef COMPLX
    case COMPLEX:
        xlputstr(fptr, "#C(");
        xlprintl(fptr, getreal(vptr), flag);
        xlputc(fptr,' ');
        xlprintl(fptr, getimag(vptr), flag);
        xlputc(fptr, ')');
        break;
#endif
#ifdef PACKAGES
    case PACKAGE:
        putpackage(fptr,vptr);
        break;
#endif /* PACKAGES */
    case FREE:
        putatm(fptr,"Free",vptr);
        break;

    /* $putpatch.c$: "MODULE_XLPRIN_C_XLPRINT" */

    default:
        putatm(fptr,"Unknown",vptr);        /* was 'Foo`   TAA Mod */
        break;
    }
}

/* xlterpri - terminate the current print line */
VOID xlterpri(fptr)
  LVAL fptr;
{
    xlputc(fptr,'\n');
}

/* xlgetcolumn -- find the current file column */

int xlgetcolumn(fptr)
  LVAL fptr;
{
    if (fptr == NIL) return 0;
    else if (ntype(fptr) == USTREAM) { /* hard work ahead :-( */
        LVAL ptr = gethead(fptr);
        int count = 0;

        while (ptr != NIL) {
            if (getchcode(car(ptr)) == '\n') count = 0 ;
            else count++;
            ptr = cdr(ptr);
        }
        return count;
    }
    else if (getfile(fptr) == CONSOLE)
        return lposition;
    else
        return ((fptr->n_sflags & S_WRITING)? fptr->n_cpos : 0);
}


/* xlfreshline -- start new line if not at beginning of line */
int xlfreshline(fptr)
  LVAL fptr;
{
    if (xlgetcolumn(fptr) != 0) {
        xlterpri(fptr);
        return TRUE;
    }
    return FALSE;
}


/* xlputstr - output a string */
VOID xlputstr(fptr,str)
  LVAL fptr; char *str;
{
/* solve reentrancy problems if gc prints messages and
   xlputstr output is directed to a string stream */
    if (ustreamp(fptr)) {
        int oplevel=plevel, oplength=plength;   /* save these variables */
        char nbuf[STRMAX+1];

        if (buf == str) {   /* copy to reentrant buffer if necessary */
            str = strcpy(nbuf, buf);
        }

        while (*str)        /* print string */
            xlputc(fptr, *str++);

        plevel = oplevel;   /* restore level and length */
        plength = oplength;
    }
    else
        while (*str)
            xlputc(fptr,*str++);
}

/* print package and symbol */
#ifdef PACKAGES
LOCAL VOID NEAR putpacksym(fptr, sym, flag)
     LVAL fptr, sym;
     int flag;
{
    LVAL pack, foundsym;
    char FAR *pname;

    pack = getvalue(s_package);
    pname = getstring(getpname(sym));

    if (!goodpackagep(pack))
        putsymbol(fptr, pname, flag);
    else if (keywordp(sym)) {
        xlputc(fptr, ':');
        putsymbol(fptr, pname, flag);
    }
/* TAA MOD 10/96 -- if no home package, always print with #: */
    else if ((!null(getpackage(sym))) &&
             xlfindsymbol(pname, pack, &foundsym) && sym == foundsym)
        putsymbol(fptr, pname, flag);
    else {
        /* TAA modified to handle mixed cases when in :invert, 9/13/96 */
        LVAL olddenv = xldenv;
        pack = getpackage(sym);
        if (packagep(pack)) {
            if (getvalue(s_rtcase) == k_invert) {
                char FAR *cp, c;
                int up=0, low=0;
                cp = getstring(xlpackagename(pack));
                while ((c=*cp++)!='\0') {
                    if (ISUPPER(c)) up++;
                    else if (ISLOWER(c)) low++;
                }
                cp = getstring(getpname(sym));
                while ((c=*cp++)!='\0') {
                    if (ISUPPER(c)) up++;
                    else if (ISLOWER(c)) low++;
                }
                xldbind(s_rtcase, ((up!=0 && low!=0) ? k_preserve : k_invert));
            }
            putsymbol(fptr, getstring(xlpackagename(pack)), flag);
            if (xlfindsymbol(pname, pack, &foundsym) == SYM_EXTERNAL)
                xlputc(fptr, ':');
            else
                xlputstr(fptr, "::");
        }
        else if (flag)
            xlputstr(fptr,"#:");
        putsymbol(fptr, getstring(getpname(sym)), flag);
        xlunbind(olddenv);
    }
}
#else
LOCAL VOID NEAR putpacksym(fptr, sym, flag)
     LVAL fptr, sym;
     int flag;
{
    /* check for uninterned symbol */
    char FAR *str = getstring(getpname(sym));
    LVAL next;

    if (flag) {
        next = getelement(getvalue(obarray), hash(str, HSIZE));
        for (; !null(next); next = cdr(next))
            if (car(next) == sym) goto doprintsym;
        xlputstr(fptr,"#:");
doprintsym: ;
    }
    putsymbol(fptr, str, flag);
}
#endif /* PACKAGES */

#ifdef READTABLECASE
#define RUP  0      /* values for upcase, downcase, preserve, and invert */
#define RDWN 1
#define RPRE 2
#define RINV 3
#endif

/* putsymbol - output a symbol */
LOCAL VOID NEAR putsymbol(fptr, stri, flag)
  LVAL fptr; char FAR *stri; int flag;
{
#ifdef READTABLECASE
    LVAL rtcase = getvalue(s_rtcase);
    int rcase,up,low;
    int mixcase;
#endif
    int downcase;
    int capitalize;
    LVAL type;
    unsigned char *p;
    unsigned char c;
#ifdef MEDMEM
    char *str = buf;

    STRCPY(buf, stri);
#else
#define str stri
#endif

#ifdef READTABLECASE
    /* check value of *readtable-case* */
    if      (rtcase == k_upcase)   rcase = RUP;
    else if (rtcase == k_invert)   rcase = RINV;
    else if (rtcase == k_downcase) rcase = RDWN;
    else if (rtcase == k_preserve) rcase = RPRE;
    else rcase = RUP;                           /* default is upcase */
#endif

    /* handle escaping if flag is true */

    if (flag) {
        /* check to see if symbol needs escape characters */
        for (p = (unsigned char *)str; (c = *p) != 0 ; ++p)
#ifdef READTABLECASE
            if    ((rcase == RUP && ISLOWER(c))
                || (rcase == RDWN && ISUPPER(c))
                ||  ((type = tentry(c)) != k_const
                    && (!consp(type) || car(type) != k_nmacro)))
#else
            if (ISLOWER(c)
                ||  ((type = tentry(c)) != k_const
                    && (!consp(type) || car(type) != k_nmacro)))
#endif
            {
                xlputc(fptr,'|');
                while (*str) {
                    if (*str == '\\' || *str == '|')
                        xlputc(fptr,'\\');
                    xlputc(fptr,*str++);
                }
                xlputc(fptr,'|');
                return;
            }
        /* check for the first character being '#'
            or string looking like a number */
        if (*str == '#' || isanumber(str,NULL))
            xlputc(fptr,'\\');
    }

    /* get the case translation flag -- default upcase */
    downcase = (getvalue(s_printcase) == k_downcase);
    /* use capitalize mode if RUP or RDWN and printcase is capitalize */
    capitalize = 
#ifdef READTABLECASE
        (rcase==RUP || rcase==RDWN) &&
#endif
        (getvalue(s_printcase) == k_capitalize);

#ifdef READTABLECASE
    /* we need to know if there is a mixed case symbol if reading :INVERT */
    if (rcase == RINV)  {
        up=FALSE;
        low=FALSE;
        mixcase = FALSE;
        for (p=(unsigned char *)str ; (c = *p) != 0 && !mixcase ; ++p)  {
            if (ISLOWER(c))
                low = TRUE;
            else if (ISUPPER(c))
                up = TRUE;
            mixcase = up&low;
        }
        if (mixcase) rcase = RPRE;  /* preserve if cases mixed */
    }
    low = (rcase == RINV) || (rcase == RUP && (downcase||capitalize));
    up  = (rcase == RINV) || (rcase == RDWN && !downcase);

#endif

    /* output each character */
    mixcase = TRUE; /* set at start of a "word */
    while ((c = (unsigned char) *str++) != 0) {
#ifdef PACKAGES
        if (flag && (c == '\\' || c == '|' || c == ':'))
#else
        if (flag && (c == '\\' || c == '|'))
#endif
            xlputc(fptr,'\\');
#ifdef READTABLECASE
        if (capitalize) {
            xlputc(fptr, (mixcase ? ((ISLOWER(c)&&up) ? TOUPPER(c) : c)
                                  : ((ISUPPER(c)&&low) ? TOLOWER(c) : c)));
            mixcase = !ISLOWERA(c) && !ISUPPER(c);
        }
        else if (ISUPPER(c)) xlputc(fptr, low ? TOLOWER(c) : c);
        else if (ISLOWER(c)) xlputc(fptr, up  ? TOUPPER(c) : c);
        else xlputc(fptr,c);
#else
        if (capitalize) {
            xlputc(fptr, (mixcase ? (ISLOWER(c) ? TOUPPER(c) : c)
                                  : (ISUPPER(c) ? TOLOWER(c) : c)));
            mixcase = !ISLOWERA(c) && !ISUPPER(c);
        }
        else xlputc(fptr,(downcase && ISUPPER(c) ? TOLOWER(c) : c));
#endif
    }
}
#ifndef MEDMEM
#undef str
#endif

/* putstring - output a string */
/* rewritten to  print strings containing nulls TAA mod*/
LOCAL VOID NEAR putstring(fptr,str)
  LVAL fptr,str;
{
    char FAR *p = getstring(str);
    unsigned len = getslength(str);

    /* output each character */
    while (len-- > 0) xlputc(fptr,*p++);
}

/* putqstring - output a quoted string */
/* rewritten to  print strings containing nulls TAA mod*/
LOCAL VOID NEAR putqstring(fptr,str)
  LVAL fptr,str;
{
    char FAR *p = getstring(str);
    unsigned len = getslength(str);
    int ch;

    /* output the initial quote */
    xlputc(fptr,'"');

    /* output each character in the string */
    while (len-- > 0) {
#ifdef __SASC__
        /* For IBM mainframe, Convert EBCDIC to ASCII for the tests below */
        int testch;

        ch = *(unsigned char FAR *)p++;
        testch = etoa(ch);

        /* check for a control character */
        if (testch < 040 || testch == '\\' ||
            testch == '"' || testch > 0176) /* TAA MOD quote quote */
#else /* __SASC__ */
        ch = *(unsigned char FAR *)p++;

        /* check for a control character */
        /* TAA MOD added 10/96 -- don't quote newline, print it. */
#ifdef ASCII8   /* in this case, upper bit set characters are printable! */
                /* TAA MOD added 8/92 */
        if ((ch < 040 && ch != '\n')
            || ch == '\\' || ch == '"' || (ch&0177) == 0177)
#else
        if ((ch < 040 && ch != '\n')
            || ch == '\\' || ch == '"' || ch > 0176) /* TAA MOD quote quote */
#endif
#endif
            {
            xlputc(fptr,'\\');
#ifdef __SASC__
            switch (testch)
#else
            switch (ch) 
#endif
            {
                case '\011':
                    xlputc(fptr,'t');
                    break;
                case '\012':
                    xlputc(fptr,'n');
                    break;
                case '\014':
                    xlputc(fptr,'f');
                    break;
                case '\015':
                    xlputc(fptr,'r');
                    break;
                case 0x5c:  /* is '\\' except for EBCDIC */
                case 0x22:  /* is '"'  except for EBCDIC, so use int constant*/
                    xlputc(fptr,ch);
                    break;
                default:
                    putoct(fptr,ch);
                    break;
            }
        }

                /* output a normal character */
        else
            xlputc(fptr,ch);
    }


    /* output the terminating quote */
    xlputc(fptr,'"');
}

/* putatm - output an atom */
LOCAL VOID NEAR putatm(fptr,tag,val)
  LVAL fptr; char *tag; LVAL val;
{
    sprintf(buf, "#<%s: #", tag);
    xlputstr(fptr, buf);
    sprintf(buf, AFMT, CVPTR(val)); /* TAA Fix 2/94: was just val */
    xlputstr(fptr, buf);
    xlputc(fptr, '>');
}

/* putsubr - output a subr/fsubr */
LOCAL VOID NEAR putsubr(fptr,tag,val)
  LVAL fptr; char *tag; LVAL val;
{
/*    sprintf(buf,"#<%s-%s: #",tag,funtab[getoffset(val)].fd_name); */
    char *str;      /* TAA mod */
    if ((str = funtab[getoffset(val)].fd_name) != NULL)
        sprintf(buf,"#<%s-%s: #",tag,str);
    else
        sprintf(buf,"#<%s: #",tag);
    xlputstr(fptr, buf);
    sprintf(buf, AFMT, CVPTR(val)); /* TAA Fix 2/94: was just val */
    xlputstr(fptr, buf);
    xlputc(fptr, '>');
}

/* putclosure - output a closure */
LOCAL VOID NEAR putclosure(fptr,val)
  LVAL fptr,val;
{
    LVAL name;
    if ((name = getname(val)) != NIL)
#ifdef MEDMEM
    {
        char fmt[STRMAX];
        STRCPY(fmt, getstring(getpname(name)));
        sprintf(buf, "#<Closure-%s: #", fmt);
    }
#else
        sprintf(buf,"#<Closure-%s: #",getstring(getpname(name)));
#endif
    else
        strcpy(buf,"#<Closure: #");
    xlputstr(fptr, buf);
    sprintf(buf, AFMT, CVPTR(val)); /* TAA Fix 2/94: was just val */
    xlputstr(fptr, buf);
    xlputc(fptr, '>');
}




/* putfixnum - output a fixnum */
LOCAL VOID NEAR putfixnum(fptr,n)
  LVAL fptr; FIXTYPE n;
{
#ifdef BIGNUMS
    if (getvalue(s_printbase) != NIL) {
        /* expect non decimal radix */
        putbignum(fptr, cvtfixbignum(n));
        return;
    }
    else {
        sprintf(buf, IFMT, n);
    }
#else
    LVAL val;
#ifdef MEDMEM
    char fmt[STRMAX];
    val = getvalue(s_ifmt);
    STRCPY(fmt, stringp(val) && getslength(val) < STRMAX ?
        getstring(val) : (char FAR *)IFMT);
#else
    char *fmt;

    val = getvalue(s_ifmt);
    fmt = (stringp(val) ? getstring(val) : IFMT);
#endif
    sprintf(buf,fmt,n);
#endif
    xlputstr(fptr,buf);
}

#ifdef PACKAGES
/* putpackage - output package */
LOCAL VOID putpackage(fptr,val)
     LVAL fptr,val;
{
    LVAL names;
    names = getpacknames(val);
    if (consp(names) && stringp(car(names))) {
#ifdef MEDMEM
        strcpy(buf, "#<Package ");
        STRCAT(buf, getstring(car(names)));
        strcat(buf, ">");
#else
        sprintf(buf,"#<Package %s>",getstring(car(names)));
#endif
        xlputstr(fptr, buf);
    }
    else {
        xlputstr(fptr, "#<Package ???: #");
        sprintf(buf,AFMT, CVPTR(val)); /* TAA Fix 2/94, was (OFFTYPE)val */
        xlputstr(fptr,buf);
        xlputc(fptr,'>');
    }
}
#endif /* PACKAGES */


#ifdef BIGNUMS
/* putbignum - output a bignum */
LOCAL VOID NEAR putbignum(fptr, n)
  LVAL fptr, n;
{
    LVAL val;
    FIXTYPE radix;
    char FAR *pstring;

    if (zeropbignum(n)) {
        /* skip all of this for zero */
        xlputc(fptr, '0');
        return;
    }

    val = getvalue(s_printbase);
    if (fixp(val)) {
        radix = getfixnum(val);
        if (radix < 2 || radix > 36) radix = 10;
    }
    else
        radix = 10;

    pstring = cvtbignumstr(n, (int)radix);
#ifdef MEDMEM
    while (*pstring != '\0') 
        xlputc(fptr, *pstring++);
#else
    xlputstr(fptr, pstring);
#endif
    MFREE(pstring);
}
#endif  

/* putflonum - output a flonum */
LOCAL VOID NEAR putflonum(fptr,n)
  LVAL fptr; FLOTYPE n;
{
#ifdef MEDMEM
    char fmt[STRMAX];
#else
    char *fmt;
#endif
    LVAL val;
#ifdef IEEEFP
    union { FLOTYPE fpn; long intn[2]; } k/*ludge*/;

    k.fpn = n;
    if ((k.intn[1] & 0x7fffffffL) == 0x7ff00000L && k.intn[0] == 0) {
        xlputstr(fptr,k.intn[1]<0 ? "-INF" : "+INF");
        return;
    }
    if ((k.intn[1]&0x7ff00000L) == 0x7ff00000L &&
        ((k.intn[1]&0xfffffL) != 0 || k.intn[0] != 0)) {
        xlputstr(fptr,"NaN");
        return;
    }
#endif

#ifdef MEDMEM
    val = getvalue(s_ffmt);
    STRCPY(fmt, stringp(val) && getslength(val) < STRMAX ?
        getstring(val) : (char FAR *)"%g");
#else
    val = getvalue(s_ffmt);
    fmt = (stringp(val) ? getstring(val) : "%g");
#endif
    sprintf(buf,fmt,n);
    /* TAA MOD 3/98 -- don't allow printing FP numbers that can't be read */
    if (!stringp(val) && /* print anything if format overridden */
        strchr(buf, '.') == NULL &&
        strchr(buf, 'e') == NULL &&
        strchr(buf, 'E') == NULL) {
        /* Can't print out FP value without a decimal point and
         * trailing zero or an exponent*/
        strcat(buf, ".0");
    }
    xlputstr(fptr,buf);
}

/* putchcode - output a character */
/* modified to print control and meta characters TAA Mod */
LOCAL VOID NEAR putchcode(fptr,ch,escflag)
  LVAL fptr; int ch,escflag;
{
#ifdef __SASC__
    int testch = etoa((unsigned) ch);
#endif
    if (escflag) {
        xlputstr(fptr,"#\\");
#ifndef ASCII8  /* print graphics if not defined */
#if __SASC__
        if (testch > 127) {
            testch -= 128;
            xlputstr(fptr,"M-");
        }
#else       
        if (ch > 127) {
            ch -= 128;
            xlputstr(fptr,"M-");
        }
#endif
#endif
#ifdef __SASC__
        switch (testch)
#else
        switch (ch)
#endif
        {
            case 0x0a:  /* ASCII '\n' */
                xlputstr(fptr,"Newline");
                break;
            case 0x20:  /* ASCII ' ' */
                xlputstr(fptr,"Space");
                break;
            case 127:
                xlputstr(fptr,"Rubout");
                break;
#ifdef ASCII8
            case 255:
                xlputstr(fptr,"M-Rubout");
#endif
            default:
#ifdef __SASC__
                if (testch < 32) {
                    testch += '@';
                    xlputstr(fptr,"C-");
                }
                /* Convert ASCII testch to EBCDIC for printing... */
                xlputc(fptr,atoe((unsigned)testch));
                break;
#else
                if (ch < 32) {
                    ch += '@';
                    xlputstr(fptr,"C-");
                }
                xlputc(fptr,ch);
                break;
#endif
        }
    }
    else xlputc(fptr,ch);
}

/* putoct - output an octal byte value */
LOCAL VOID NEAR putoct(fptr,n)
  LVAL fptr; int n;
{
    sprintf(buf,"%03o",n);
    xlputstr(fptr,buf);
}
