/* xlread - xlisp expression input routine */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"
#ifndef ANSI
double atof();  /* TAA MOD 3/98 -- some compilers didn't have atof defined
                    in math.h, so changed to explicitly define it */
#endif
#ifdef AMIGA
#include <math.h>
#endif

/* symbol parser modes */
#define DONE    0
#define NORMAL  1
#define ESCAPE  2

/* string constants */
#define WSPACE "\t \f\r\n"
#define CONST1 "!$%&*+-./0123456789:<=>?@[]^_{}~"
#define CONST2 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


/* forward declarations */
LOCAL LVAL NEAR callmacro _((LVAL fptr, int ch));
LOCAL LVAL NEAR psymbol _((LVAL fptr));
LOCAL LVAL NEAR punintern _((LVAL fptr));
LOCAL LVAL NEAR pnumber _((LVAL fptr, int radix));
LOCAL LVAL NEAR pquote _((LVAL fptr, LVAL sym));
LOCAL LVAL NEAR plist _((LVAL fptr));
LOCAL LVAL NEAR pvector _((LVAL fptr));
LOCAL LVAL NEAR pstruct _((LVAL fptr));
LOCAL LVAL NEAR readlist _((LVAL fptr, int *plen));
LOCAL VOID NEAR pcomment _((LVAL fptr));
LOCAL VOID NEAR badeof _((void));/* TAA MOD to remove unnecessary arg 11/92 */
LOCAL VOID NEAR upcase _((char *str));
LOCAL VOID NEAR storech _((int *c, int ch));
LOCAL int  NEAR nextch _((LVAL fptr));
LOCAL int  NEAR checkeof _((LVAL fptr));
LOCAL int  NEAR readone _((LVAL fptr, LVAL FAR *pval));
#ifdef PACKAGES
LOCAL int  NEAR pname _((LVAL fptr, int *pescflag, int *ppackindex));
#else
LOCAL int  NEAR pname _((LVAL fptr, int *pescflag));
#endif
LOCAL VOID NEAR defmacro _((int ch, LVAL type, int offset));
#ifdef BIGNUMS
LOCAL int NEAR isadigit _((char c, int r));
LOCAL LVAL NEAR convertnumber _((char *buf, int radix));
#endif

/* xlload - load a file of xlisp expressions */
int xlload(fname,vflag,pflag)
  char *fname; int vflag,pflag;
{
    char fullname[STRMAX+1];
    LVAL fptr,expr;
    CONTXT cntxt;
    FILEP fp;
    int sts, mask;
#ifdef PACKAGES
    LVAL oldpack = getvalue(s_package);
#endif /* PACKAGES */

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fptr);
    xlsave(expr);

    /* default the extension */
    if (needsextension(fname)) {
        strcpy(fullname,fname);
        strcat(fullname,".lsp");
        fname = fullname;
    }

    /* allocate a file node */
    fptr = cvfile(CLOSED,S_FORREADING);

    /* open the file */
#ifdef PATHNAMES
    if ((fp = ospopen(fname,TRUE)) == CLOSED) 
#else
    if ((fp = OSAOPEN(fname,OPEN_RO)) == CLOSED) 
#endif
    {
        xlpopn(2);
        return (FALSE);
    }
    setfile(fptr,fp);

    /* print the information line */
    if (vflag)  /* TAA MOD -- changed from printing to stdout */
        { sprintf(buf,"; loading \"%s\"\n",fname); dbgputstr(buf); }

    /* read, evaluate and possibly print each expression in the file */
    xlbegin(&cntxt, CF_ERROR|CF_UNWIND, s_true);/*TAA mod so file gets closed*/
    if ((mask = setjmp(cntxt.c_jmpbuf)) != 0)   /* TAA mod -- save mask */
        sts = FALSE;
    else {
        while (xlread(fptr,&expr,FALSE,FALSE)) {
            expr = xleval(expr);
            if (pflag)
                stdprint(expr);
        }
        sts = TRUE;
    }
    xlend(&cntxt);

#ifdef PACKAGES
    setvalue(s_package, oldpack);
#endif /* PACKAGES */

    /* close the file */
    OSCLOSE(getfile(fptr));
    setfile(fptr,CLOSED);

    /* restore the stack */
    xlpopn(2);

    /* check for unwind protect TAA MOD */
    if ((mask & ~CF_ERROR) != 0)
        xljump(xltarget, xlmask, xlvalue);

    /* return status */
    return (sts);
}

/* TAA MOD 9/97 -- Added recursive and preservewhitespace flags */
/* xlread - read an xlisp expression */
int xlread(fptr,pval,rflag,pwflag)
  LVAL fptr,*pval;
  int rflag,pwflag;
{
    int sts;
    LVAL olddenv = xldenv;

    if (!rflag) xldbind(a_readpw, (pwflag ? s_true : NIL));

    /* read an expression */
    while ((sts = readone(fptr,pval)) == FALSE)
        ;

    /* unbind a_readpw if necessary */
    xlunbind(olddenv);

    /* return status */
    return (sts == EOF ? FALSE : TRUE);
}

/* readone - attempt to read a single expression */
LOCAL int NEAR readone(fptr,pval)
  LVAL fptr, FAR *pval;
{
    LVAL val,type;
    int ch;

#ifdef STSZ
    /* check the stack */
    stchck();
#endif

    /* get a character and check for EOF */
    if ((ch = xlgetc(fptr)) == EOF)
        return (EOF);

    /* handle white space */
    if ((type = tentry(ch)) == k_wspace)
        return (FALSE);

    /* handle symbol constituents */
    /* handle single and multiple escapes */  /* combined by TAA MOD */
    else if (type == k_const ||
             type == k_sescape || type == k_mescape) {
        xlungetc(fptr,ch);
        *pval = psymbol(fptr);
        return (TRUE);      
    }

    /* handle read macros */
    else if (consp(type)) {
        if (((val = callmacro(fptr,ch)) != NIL) && consp(val)) {
            *pval = car(val);
            return (TRUE);
        }
        else
            return (FALSE);
    }

    /* handle illegal characters */
    else {
/*      xlerror("illegal character",cvfixnum((FIXTYPE)ch)); */
        xlerror("illegal character",cvchar(ch));    /* friendlier TAA MOD*/
        return (0);  /* compiler warning */
    }
}

/* rmhash - read macro for '#' */
LVAL rmhash()
{
    LVAL fptr,val;
    char *bufp;         /* TAA fix to allow control character literals */
        int i;
    int ch;
#ifdef __SASC__
    int testch;
#endif

    /* protect some pointers */
    xlsave1(val);

    /* get the file and macro character */

    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* make the return value */
    val = consa(NIL);

    /* check the next character */
    switch (ch = xlgetc(fptr)) {
    case '\'':
        rplaca(val,pquote(fptr,s_function));
        break;

    case '(':
        xlungetc(fptr,ch);
        rplaca(val,pvector(fptr));
        break;

    case '.':
        if (readone(fptr,&car(val)) == EOF)
            badeof(); /* Check added 3/98 */
        rplaca(val,xleval(car(val)));
        break;

    case 'b':
    case 'B':
        rplaca(val,pnumber(fptr,2));
        break;

    case 'o':
    case 'O':
        rplaca(val,pnumber(fptr,8));
        break;

    case 'd':   /* added for version 2.1h */
    case 'D':
        rplaca(val,pnumber(fptr,10));
        break;

    case 'x':
    case 'X':
        rplaca(val,pnumber(fptr,16));
        break;
    case 's':
    case 'S':
        rplaca(val,pstruct(fptr));
        break;
    case '\\':
        for (i = 0; i < STRMAX-1; i++) {
            ch = xlgetc(fptr);  /* TAA fix to scan at end of file */
            if (ch == EOF || 
                     ((tentry((unsigned char)(buf[i] = (char)ch))  != k_const) &&
                (i > 0) &&      /* TAA fix for left and right paren */
                buf[i] != '\\' && buf[i] != '|')) {
                xlungetc(fptr, buf[i]);
                break;
            }
        }
        if (! null(getvalue(s_read_suppress))) {
            rplaca(val,NIL);
            break;
        }
        buf[i] = 0;
        ch = (unsigned char)buf[0];
#ifdef __SASC__
        testch = etoa(ch);
#endif
        if (strlen(buf) > (unsigned)1) {  /* TAA Fixed */
            i = buf[strlen(buf)-1]; /* Value of last character */
            upcase(buf);
            bufp = &buf[0];
#ifdef __SASC__ /* EBCDIC */
            testch = 0;
            if (strncmp(bufp,"M-",2) == 0) {
                testch = 128;
                bufp += 2;
            }
            if (strcmp(bufp,"NEWLINE") == 0)
                testch += 0x0a;
            else if (strcmp(bufp,"SPACE") == 0)
                testch += 0x20;
            else if (strcmp(bufp,"RUBOUT") == 0)
                ch += 127;
            else if (strlen(bufp) == 1) 
                ch += i;
            else if (strncmp(bufp,"C-",2) == 0 && strlen(bufp) == 3) 
                testch += etoa(bufp[2]) & 31;
            else xlerror("unknown character name",cvstring(buf));
            ch = testch;
#else   
            ch = 0;
            if (strncmp(bufp,"M-",2) == 0) {
                ch = 128;
                bufp += 2;
            }
            if (strcmp(bufp,"NEWLINE") == 0)
                ch += '\n';
            else if (strcmp(bufp,"SPACE") == 0)
                ch += ' ';
            else if (strcmp(bufp,"RUBOUT") == 0)
                ch += 127;
            else if (strlen(bufp) == 1) 
                ch += i;
            else if (strncmp(bufp,"C-",2) == 0 && strlen(bufp) == 3) 
                ch += bufp[2] & 31;
            else xlerror("unknown character name",cvstring(buf));
#endif
        }
#ifdef __SASC__
        rplaca(val, cvchar(atoe(testch)));
#else
        rplaca(val,cvchar(ch));
#endif
        break;

    case ':':
        rplaca(val,punintern(fptr));
        break;

    case '|':
        pcomment(fptr);
        val = NIL;
        break;
#ifdef COMPLX
    case 'c':
    case 'C':  /* From XLISP-STAT, Copyright (c) 1988, Luke Tierney */
    {
        LVAL list;
        if (readone(fptr, &list) == EOF)
            badeof();   /* check added 3/98 */
        if (! consp(list) || ! consp(cdr(list)) || cdr(cdr(list)) != NIL)
            xlerror("bad complex number specification", list);
        rplaca(val, newcomplex(car(list), car(cdr(list))));
        break;
    }
#endif
    case '+': /* From XLISP-STAT, Copyright (c) 1988, Luke Tierney */
    case '-':  
    {
        LVAL arg;
        int sts; /* added eof check 3/98 */
        xlsave1(arg);
        while (!(sts=readone(fptr, &arg)));
        if (sts == EOF) badeof();
        if (checkfeatures(arg, ch)) {
            while (!(sts=readone(fptr, &arg)));
            if (sts==EOF) badeof();
            rplaca(val, arg);
        }
        else {
            LVAL olddenv = xldenv;
            xldbind(s_read_suppress, s_true);
            while (!(sts=readone(fptr, &arg)));
            if (sts==EOF) badeof();
            val = NIL;
            xlunbind(olddenv);
        }
        xlpop();
        break;
    }
    case EOF:   /* added 3/98 */
        badeof();
    default:    /* added for version 2.1h */
        if (isdigit(ch)) {
            int dig2 = xlgetc(fptr);
            int radix;
            if (isdigit(dig2)) {
                radix = (ch-'0')*10+dig2-'0';
                dig2 = xlgetc(fptr);
            }
            else {
                radix = ch - '0';
            }
            if ((dig2 != 'r' && dig2 != 'R') || radix < 2 || radix > 36)
                xlfail("bad radix specifier");
            rplaca(val, pnumber(fptr, radix));
            break;
        }

/*      xlerror("illegal character after #",cvfixnum((FIXTYPE)ch)); */
        xlerror("illegal character after #",cvchar(ch)); /*TAA Mod */
    }

    /* restore the stack */
    xlpop();

    /* return the value */
    return (val);
}

/* rmquote - read macro for '\'' */
LVAL rmquote()
{
     LVAL fptr;

     /* get the file and macro character */
     fptr = xlgetarg();  /* internal -- don't bother with error checks */

     /* parse the quoted expression */
     return (consa(pquote(fptr,s_quote)));
}

/* rmdquote - read macro for '"' */
LVAL rmdquote()
{
     char buf[STRMAX+1],*p, FAR *sptr;
     LVAL fptr,str,newstr;
     int len,blen,ch,d2,d3;

     /* protect some pointers */
     xlsave1(str);

     /* get the file and macro character */
     fptr = xlgetarg();  /* internal -- don't bother with error checks */

     /* loop looking for a closing quote */
     len = blen = 0; p = buf;
     while ((ch = checkeof(fptr)) != '"') {

          /* handle escaped characters */
          switch (ch) {
          case '\\':
                     switch (ch = checkeof(fptr)) {
                     case 't':
                                ch = '\011';
                                break;
                     case 'n':
                                ch = '\012';
                                break;
                     case 'f':
                                ch = '\014';
                                break;
                     case 'r':
                                ch = '\015';
                                break;
                     default:
                                if (ch >= '0' && ch <= '7') {
                                     d2 = checkeof(fptr);
                                     d3 = checkeof(fptr);
                                     if (d2 < '0' || d2 > '7'
                                      || d3 < '0' || d3 > '7')
                                          xlfail("invalid octal digit");
                                     ch -= '0'; d2 -= '0'; d3 -= '0';
                                     ch = (ch << 6) | (d2 << 3) | d3;
                                }
                                break;
                     }
          }


          /* check for buffer overflow */

          if (blen >= STRMAX) {
                newstr = newstring(len + STRMAX);
                sptr = getstring(newstr);
                if (str != NIL)
                     MEMCPY(sptr, getstring(str), len);
                *p = '\0';
                MEMCPY(sptr+len, buf, blen+1);
                p = buf;
                blen = 0;
                len += STRMAX;
                str = newstr;
          }


          /* store the character */
          *p++ = (char)ch; ++blen;
     }

     /* append the last substring */

     if (str == NIL || blen) {
          newstr = newstring(len + blen);
          sptr = getstring(newstr);
          if (str != NIL) MEMCPY(sptr, getstring(str), len);
          *p = '\0';
          MEMCPY(sptr+len, buf, blen+1);
        str = newstr;
    }


    /* restore the stack */
    xlpop();

    /* return the new string */
    return (consa(str));
}

/* rmbquote - read macro for '`' */
LVAL rmbquote()
{
    LVAL fptr;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* parse the quoted expression */
    return (consa(pquote(fptr,s_bquote)));
}

/* rmcomma - read macro for ',' */
LVAL rmcomma()
{
    LVAL fptr,sym;
    int ch;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* check the next character */
    if ((ch = xlgetc(fptr)) == '@')
        sym = s_comat;
    else {
        xlungetc(fptr,ch);
        sym = s_comma;
    }

    /* make the return value */
    return (consa(pquote(fptr,sym)));
}

/* rmlpar - read macro for '(' */
LVAL rmlpar()
{
    LVAL fptr;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* make the return value */
    return (consa(plist(fptr)));
}

/* rmrpar - read macro for ')' */
LVAL rmrpar()
{
    xlfail("misplaced close paren");
    return (NIL);   /* never returns */
}

/* rmsemi - read macro for ';' */
LVAL rmsemi()
{
    LVAL fptr;
    int ch;

    /* get the file and macro character */
    fptr = xlgetarg();  /* internal -- don't bother with error checks */

    /* skip to end of line */
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n')
        ;

    /* return nil (nothing read) */
    return (NIL);
}

/* pcomment - parse a comment delimited by #| and |# */
LOCAL VOID NEAR pcomment(fptr)
  LVAL fptr;
{
    int lastch,ch,n;

    /* look for the matching delimiter (and handle nesting) */
    for (n = 1, lastch = -1; n > 0 && (ch = xlgetc(fptr)) != EOF; ) {
        if (lastch == '|' && ch == '#')
            { --n; ch = -1; }
        else if (lastch == '#' && ch == '|')
            { ++n; ch = -1; }
        lastch = ch;
    }
}

/* pnumber - parse a number */
#ifdef BIGNUMS
LOCAL LVAL NEAR convertnumber(buf, radix)
char *buf; int radix;
{
    if (radix==10 && strlen(buf) < 10) {
        /* take shortcut */
        return cvfixnum(ICNV(buf));
    }
    else {
        LVAL x;
        FIXTYPE temp;
        x = cvtstrbignum(buf, radix);
        return (cvtbigfixnum(x, &temp) ? cvfixnum(temp) : x);
    }
}
        
LOCAL LVAL NEAR pnumber(fptr,radix)
  LVAL fptr; int radix;
{
    int i=0;    /* index into buffer */
    int digits=0; /* number of digits so far */
    int ch;
    int sign_seen = 0; /* added 3/98 */
    LVAL resulta = NULL, resultb;
    
    while ((ch = xlgetc(fptr)) != EOF && i < STRMAX) {
        if (!sign_seen && !digits && ch == '+') { /* ignore leading + */
            sign_seen = 1;
            continue;
        }
        if (!sign_seen && !digits && ch == '-') { /* negative number */
            sign_seen = 1;
            buf[i++] = (char) ch;
            continue;
        }
        if (ch == '/' && resulta==NULL) { /* a ratio */
            buf[i] = '\0';
            if (digits==0) xlfail("unrecognized number");
            digits = i = sign_seen = 0;
            resulta = cvtstrbignum(buf, radix); /* do numerator */
            continue;
        }
        if (isadigit((char)ch, radix)) { /* number constituent */
            buf[i++] = (char) ch;
            digits++;
            continue;
        }
        break; /* invalid character terminates number */
    }
    /* TAA MOD 3/98 Fix for read-preserving-whitespace */
    if (tentry(ch) != k_wspace || !null(getvalue(a_readpw)))
        xlungetc(fptr,ch);
    if (i == STRMAX) xlfail("number too long to process");
    if (digits==0) xlfail("unrecognized number");
    buf[i] = '\0';
    if (resulta) { /* finish up a ratio */
        xlprot1(resulta);
        resultb = cvtstrbignum(buf, radix);
        xlpop();
        if (zeropbignum(resultb)) xlfail("invalid ratio");
        return cvbratio(resulta, resultb);
    }
    return convertnumber(buf, radix);
}

#else
LOCAL LVAL NEAR pnumber(fptr,radix)
  LVAL fptr; int radix;
{
    int digit,ch;
    long num;
    
    for (num = 0L; (ch = xlgetc(fptr)) != EOF; ) {
        if (ISLOWER7(ch)) ch = toupper(ch);
        if (!('0' <= ch && ch <= '9') && !('A' <= ch && ch <= 'F'))
            break;
        if ((digit = (ch <= '9' ? ch - '0' : ch - 'A' + 10)) >= radix)
            break;
        num = num * (long)radix + (long)digit;
    }
    /* TAA MOD 3/98 Fix for read-preserving-whitespace */
    if (tentry(ch) != k_wspace || !null(getvalue(a_readpw)))
        xlungetc(fptr,ch);
    return (cvfixnum((FIXTYPE)num));
}
#endif

/* plist - parse a list */
LOCAL LVAL NEAR plist(fptr)
  LVAL fptr;
{
    LVAL val,expr,lastnptr,nptr;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(val);
    xlsave(expr);

    /* keep appending nodes until a closing paren is found */
    for (lastnptr = NIL; nextch(fptr) != ')'; )

        /* get the next expression */
        switch (readone(fptr,&expr)) {
        case EOF:
            badeof();
        case TRUE:

            /* check for a dotted tail */
            if (expr == s_dot) {

                /* make sure there's a node */
                if (lastnptr == NIL)
                    xlfail("invalid dotted pair");

                /* parse the expression after the dot */
                if (!xlread(fptr,&expr,TRUE,FALSE))
                    badeof();
                rplacd(lastnptr,expr);

                /* make sure its followed by a close paren */
                if (nextch(fptr) != ')')
                    xlfail("invalid dotted pair");
            }

            /* otherwise, handle a normal list element */
            else {
                nptr = consa(expr);
                if (lastnptr == NIL)
                    val = nptr;
                else
                    rplacd(lastnptr,nptr);
                lastnptr = nptr;
            }
            break;
        }

    /* skip the closing paren */
    xlgetc(fptr);

    /* restore the stack */
    xlpopn(2);

    /* return successfully */
    return (val);
}

/* pvector - parse a vector */
LOCAL LVAL NEAR pvector(fptr)
 LVAL fptr;
{
    LVAL list,val;
    int len,i;

    /* protect some pointers */
    xlsave1(list);

    /* read the list */
    list = readlist(fptr,&len);

    /* make a vector of the appropriate length */
    val = newvector(len);

    /* copy the list into the vector */
    for (i = 0; i < len; ++i, list = cdr(list))
        setelement(val,i,car(list));

    /* restore the stack */
    xlpop();

    /* return successfully */
    return (val);
}

/* pstruct - parse a structure */
LOCAL LVAL NEAR pstruct(fptr)
 LVAL fptr;
{
    LVAL list,val;
    int len;

    /* protect some pointers */
    xlsave1(list);

    /* read the list */
    list = readlist(fptr,&len);

    /* make the structure */
    val = xlrdstruct(list);

    /* restore the stack */
    xlpop();

    /* return successfully */
    return (val);
}

/* pquote - parse a quoted expression */
LOCAL LVAL NEAR pquote(fptr,sym)
  LVAL fptr,sym;
{
    LVAL val,p;
    int sts; /* EOF checking added 3/98 */

    /* protect some pointers */
    xlsave1(val);

    /* allocate two nodes */
    val = consa(sym);
    rplacd(val,consa(NIL));

    /* initialize the second to point to the quoted expression */
/*    if (!xlread(fptr,&p,TRUE,FALSE)) */
    while ((sts = readone(fptr,&p)) == FALSE);
    if (sts == EOF)
        badeof();
    rplaca(cdr(val),p);

    /* restore the stack */
    xlpop();

    /* return the quoted expression */
    return (val);
}

/* psymbol - parse a symbol name */
#ifdef PACKAGES
LOCAL LVAL NEAR psymbol(fptr)
  LVAL fptr;
{
    int escflag, packindex;
    LVAL val, pack;
    int colons;
    char *p;

    pname(fptr,&escflag,&packindex);
    if (! null(getvalue(s_read_suppress))) return(NIL);
    if (escflag || packindex >= 0 || !isanumber(buf,&val)) {
        if (packindex >= 0) {
            /* check for zero-length name */
            if (buf[packindex+1] == 0) xlfail("zero length name after ':'");

            if (packindex == 0) {
                /* count the colons */
                for (p = buf + packindex + 1, colons = 1; *p == ':'; p++, colons++);
                if (colons > 2) xlfail("too many :'s");
                val = xlintern(p, xlkeypack);
            }
            else {
                /* find the package */
                buf[packindex] = 0;
                pack = xlfindpackage(buf);
                if (! packagep(pack))
                    xlerror("package not found", cvstring(buf));

                /* count the colons and switch */
                for (p = buf + packindex + 1, colons = 1; *p == ':'; p++, colons++);
                switch (colons) {
                    case 1:
                        if (xlfindsymbol(p, pack, &val) != SYM_EXTERNAL)
                            xlerror("external symbol not found", cvstring(p));
                        break;
                    case 2:
                        val = xlintern(p, pack);
                        break;
                    default: xlfail("too many :'s");
                }
            }
        }
        else {
            pack = getvalue(s_package);
            return(goodpackagep(pack) ? xlintern(buf, pack) : NIL);
        }
    }
    return(val);
}
#else
LOCAL LVAL NEAR psymbol(fptr)
  LVAL fptr;
{
    int escflag;
    LVAL val;
    pname(fptr,&escflag);
    if (! null(getvalue(s_read_suppress))) return(NIL);
    return (escflag || !isanumber(buf,&val) ? xlenter(buf) : val);
}
#endif /* PACKAGES */

/* punintern - parse an uninterned symbol */
#ifdef PACKAGES
LOCAL LVAL NEAR punintern(fptr)
  LVAL fptr;
{
    int escflag,packindex;
    pname(fptr,&escflag,&packindex);
    return (xlmakesym(buf));
}
#else
LOCAL LVAL NEAR punintern(fptr)
  LVAL fptr;
{
    int escflag;
    pname(fptr,&escflag);
    return (xlmakesym(buf));
}
#endif /* PACKAGES */

/* pname - parse a symbol/package name */

#ifdef PACKAGES
LOCAL int NEAR pname(fptr,pescflag,ppackindex)
     LVAL fptr; int *pescflag, *ppackindex;
#else
LOCAL int NEAR pname(fptr,pescflag)
     LVAL fptr; int *pescflag;
#endif /* PACKAGES */
{
    int mode,ch,i;
    LVAL type;
#ifdef READTABLECASE
    LVAL rtcase = getvalue(s_rtcase);
    int low=0, up=0;
#endif

    /* initialize */
    *pescflag = FALSE;
#ifdef PACKAGES
    *ppackindex = -1;
#endif /* PACKAGES */
    mode = NORMAL;
    i = 0;

    /* accumulate the symbol name */
    while (mode != DONE) {

        /* handle normal mode */
        while (mode == NORMAL)
            if ((ch = xlgetc(fptr)) == EOF)
                mode = DONE;
            else if ((type = tentry(ch)) == k_sescape) {
                storech(&i,checkeof(fptr));
                *pescflag = TRUE;
            }
            else if (type == k_mescape) {
                *pescflag = TRUE;
                mode = ESCAPE;
            }
            else if (type == k_const
                 ||  (consp(type) && car(type) == k_nmacro))
#ifdef PACKAGES
            {
                if (ch == ':') {
                    if (*ppackindex < 0) *ppackindex = i;
                    storech(&i,ch);
                }
                else
#endif /* PACKAGES */
#ifdef READTABLECASE
            {
                if (rtcase == k_preserve)
                    storech(&i,ch);
                else if (rtcase == k_downcase)
                    storech(&i,ISUPPER(ch) ? TOLOWER(ch) : ch);
                else if (rtcase == k_invert)
                    storech(&i,ISLOWER(ch) ? (low++, TOUPPER(ch)) : 
                        (ISUPPER(ch) ? (up++, TOLOWER(ch)) : ch));
                else   /*  default upcase  */
                    storech(&i,ISLOWER(ch) ? TOUPPER(ch) : ch);
            }
#else
                storech(&i,ISLOWER(ch) ? TOUPPER(ch) : ch);
#endif
#ifdef PACKAGES
            }
#endif /* PACKAGES */
            else
                mode = DONE;

        /* handle multiple escape mode */
        while (mode == ESCAPE)
            if ((ch = xlgetc(fptr)) == EOF)
                badeof();
            else if ((type = tentry(ch)) == k_sescape)
                storech(&i,checkeof(fptr));
            else if (type == k_mescape)
                mode = NORMAL;
            else
                storech(&i,ch);
    }

    buf[i] = 0;

#ifdef READTABLECASE 
    if (rtcase == k_invert && low != 0 && up != 0) {
        /* must undo inversion (ugh!). Unfortunately, we don't know if
           any characters are quoted, so we'll just label this bug as
           a feature in the manual. The problem will only occur in symbols
           with mixed case characters outside of quotes and at least one
           quoted alpha character -- not very likely, I hope. */
        int cnt, c;
        for (cnt = 0; cnt < i; cnt++ ) {
            c = buf[cnt];
            if (ISUPPER(c)) buf[cnt] = TOLOWER(c);
            else if (ISLOWER(c)) buf[cnt] = TOUPPER(c);
        }
    }
#endif

    /* check for a zero length name */
    if (i == 0)
        xlfail("zero length name");     /* TAA fix, Jeff Prothero improved*/

    /* unget the last character and return it */
    /* TAA MOD 3/15 -- fix for read-preserving-whitespace */
    if (tentry(ch) != k_wspace || !null(getvalue(a_readpw)))
        xlungetc(fptr, ch);
    return (ch);
}

/* readlist - read a list terminated by a ')' */
LOCAL LVAL NEAR readlist(fptr,plen)
 LVAL fptr; int *plen;
{
    LVAL list,expr,lastnptr,nptr;
    int ch;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(list);
    xlsave(expr);

    /* get the open paren */
    if ((ch = nextch(fptr)) != '(')
        xlfail("expecting an open paren");
    xlgetc(fptr);

    /* keep appending nodes until a closing paren is found */
    for (lastnptr = NIL, *plen = 0; (ch = nextch(fptr)) != ')'; ) {

        /* check for end of file */
        if (ch == EOF)
            badeof();

        /* get the next expression */
        switch (readone(fptr,&expr)) {
        case EOF:
            badeof();
        case TRUE:
            nptr = consa(expr);
            if (lastnptr == NIL)
                list = nptr;
            else
                rplacd(lastnptr,nptr);
            lastnptr = nptr;
            ++(*plen);
            break;
        }
    }

    /* skip the closing paren */
    xlgetc(fptr);

    /* restore the stack */
    xlpopn(2);

    /* return the list */
    return (list);
}

/* storech - store a character in the print name buffer */
/* TAA MOD -- since buffer is always global buf, it is no longer passed
   as argument. also return value is stored in i, so i is now address of
   the int rather than its value */
LOCAL VOID NEAR storech(i,ch)
  int *i,ch;
{
    if (*i < STRMAX)
          buf[(*i)++] = (char) ch;
}

/* tentry - get a readtable entry */
LVAL tentry(ch)
  int ch;
{
    LVAL rtable;
    rtable = getvalue(s_rtable);
    if (!vectorp(rtable) || ch < 0 || ch >= getsize(rtable))
        return (NIL);
    return (getelement(rtable,ch));
}

/* nextch - look at the next non-blank character */
LOCAL int NEAR nextch(fptr)
  LVAL fptr;
{
    int ch;

    /* return and save the next non-blank character */
    while ((ch = xlgetc(fptr)) != EOF && isspace(ch))
        ;
    xlungetc(fptr,ch);
    return (ch);
}

/* checkeof - get a character and check for end of file */
LOCAL int NEAR checkeof(fptr)
  LVAL fptr;
{
    int ch;

    do {
        if ((ch = xlgetc(fptr)) == EOF)
            badeof();
    } while (ch == '\r'); // skip any carriage returns -- this can happen
                        // in a UNIX-like OS when reading MSDOS
                        // formatted files. Added 2015.

    return (ch);
}

/* badeof - unexpected eof */

LOCAL VOID NEAR badeof()
{
    xlfail("EOF reached before expression end");
}

/* isanumber - check if this string is a number */
#ifdef BIGNUMS
LOCAL int NEAR isadigit(char c, int r)
{
    if (isdigit(c)) return ((c - '0') < r);
    else if (ISLOWER7(c)) return ((c + 10 - 'a') < r);
    else if (isupper(c)) return ((c + 10 - 'A') < r);
    return 0;
}


int isanumber(str,pval)
  char *str; LVAL *pval;
{
    int dl=0, dr=0;
    char *p = str;
    char *denp;         /* pointer to denominator string */
    int ratio=0;        /* flag */
    int badratio=0;     /* set if invalid ratio (upon conversion) */
    int radix = 10;
    FIXTYPE numbr;      /* converted integer */
    LVAL numerp, denomp;    /* in case it is a a bignum/ratio */

    numerp = getvalue(s_readbase);
    if (fixp(numerp)) {
        numbr = getfixnum(numerp);
        if (numbr <2 || numbr > 36) radix = 10;
        else radix = (int)numbr;
    }

    /* check for a sign */
    if (*p == '+' || *p == '-') p++;

    /* check for a string of constituent digits */
    if (radix==10) while (isdigit(*p)) p++, dl++;
    else while (isadigit(*p, radix)) p++, dl++;


    if (*p == '/') {    /* check for a ratio */
        if (dl == 0) return FALSE;
        p++;
        denp = p; /* save start of denominator */
        if (radix == 10) {
            while (isdigit(*p)) {
                if (*p++ != 0) ratio = 1;
                dr++;
            }
        }
        else {
            while (isadigit(*p, radix)) {
                if (*p++ != 0) ratio = 1;
                dr++;
            }
        }
        if (dr == 0) return FALSE;
        badratio = !ratio;
        ratio = 1;  /* providing there was no junk at the end */
    }
    else if (*p != '\0') { /* failed to complete scan */
        radix = 10; 
        if (*p == '.' && p[1] == '\0') {
            p++; /*  a forced decimal number that scanned */
        }
        else { /* force decimal and start all over */
            p = str;
            dr = dl = 0;
            /* check for a sign */
            if (*p == '+' || *p == '-') p++;

            /* check for a string of constituent digits */
            while (isdigit(*p)) p++, dl++;

            /* check for a decimal point */
            if (*p == '.') {
                p++;
                while (isdigit(*p)) p++, dr++;
            }
            /* check for an exponent */
#ifdef READTABLECASE
            if ((dl || dr) && (*p == 'E' || *p == 'e')) 
#else
                if ((dl || dr) && *p == 'E') 
#endif
                {
                    p++;

                    /* check for a sign */
                    if (*p == '+' || *p == '-') p++;

                    /* check for a string of digits */
                    while (isdigit(*p)) p++, dr++;
                }
        }
    }

    /* make sure there was at least one digit and this is the end */
    if ((dl == 0 && dr == 0) || *p) return (FALSE);

    /* convert the string to an integer and return successfully */
    if (pval != NULL) {
        if (*str == '+') ++str;
        if (str[strlen(str)-1] == '.') {
            str[strlen(str)-1] = '\0';
        }
        if (ratio) {
            if (badratio) xlerror ("invalid rational number", cvstring (str));
            *(denp-1) = '\0'; /* delimit numerator string */
            xlsave1(numerp);
            numerp = cvtstrbignum(str, radix);
            denomp = cvtstrbignum(denp, radix);
            xlpop();
            if (zeropbignum(denomp)) xlerror("invalid rational number", cvstring(str));
            *pval = cvbratio(numerp, denomp);
        }
        else if (dr) {
            *pval = cvflonum(atof(str));
        }
        else {
            *pval = convertnumber(str, radix);
        }
    }
    return (TRUE);
}
#else
int isanumber(str,pval)
  char *str; LVAL *pval;
{
    int dl=0, dr=0;
    char *p = str;

    /* check for a sign */
    if (*p == '+' || *p == '-')
        p++;

    /* check for a string of digits */
    while (isdigit(*p))
        p++, dl++;

    /* check for a decimal point */
    if (*p == '.') {
        p++;
        while (isdigit(*p))
            p++, dr++;
    }
    /* check for an exponent */
#ifdef READTABLECASE
    if ((dl || dr) && (*p == 'E' || *p == 'e')) 
#else
    if ((dl || dr) && *p == 'E') 
#endif
    {
        p++;

        /* check for a sign */
        if (*p == '+' || *p == '-')
            p++;

        /* check for a string of digits */
        while (isdigit(*p))
            p++, dr++;
    }

    /* make sure there was at least one digit and this is the end */
    if ((dl == 0 && dr == 0) || *p) return (FALSE);

    /* convert the string to an integer and return successfully */
    if (pval != NULL) {
        if (*str == '+') ++str;
        if (str[strlen(str)-1] == '.') str[strlen(str)-1] = 0;
        *pval = (dr ? cvflonum(atof(str)) : cvfixnum(ICNV(str)));
    }
    return (TRUE);
}
#endif

/* defmacro - define a read macro */
LOCAL VOID NEAR defmacro(ch,type,offset)
  int ch; LVAL type; int offset;
{
    LVAL subr;
    subr = cvsubr(funtab[offset].fd_subr,funtab[offset].fd_type,offset);
    setelement(getvalue(s_rtable),ch,cons(type,subr));
}

/* callmacro - call a read macro */
LOCAL LVAL NEAR callmacro(fptr,ch)
  LVAL fptr; int ch;
{
    FRAMEP newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(cdr(getelement(getvalue(s_rtable),ch)));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(fptr);
    pusharg(cvchar(ch));
    xlfp = newfp;
    return (xlapply(2));
}

/* upcase - translate a string to upper case */
LOCAL VOID NEAR upcase(str)
  char *str;
{
    for (; *str != '\0'; ++str)
        if (ISLOWER7(*str))
            *str = (char) toupper(*str);
}

/* xlrinit - initialize the reader */
VOID xlrinit()
{
    LVAL rtable;
    char *p;
    int ch;

    /* create the read table */
    rtable = newvector(256);
    setsvalue(s_rtable,rtable);

    /* initialize the readtable */
    for (p = WSPACE; (ch = *p++) != 0; )
        setelement(rtable,ch,k_wspace);
    for (p = CONST1; (ch = *p++) != 0; )
        setelement(rtable,ch,k_const);
    for (p = CONST2; (ch = *p++) != 0; )
        setelement(rtable,ch,k_const);

#ifdef ASCII8
/* TAA MOD (8/92) to make extended ASCII character constituent */
    for (ch=128; ch < 255; ch++)
        setelement(rtable,ch,k_const);
#endif

    /* setup the escape characters */
    setelement(rtable,'\\',k_sescape);
    setelement(rtable,'|', k_mescape);

    /* install the read macros */
    defmacro('#', k_nmacro,FT_RMHASH);
    defmacro('\'',k_tmacro,FT_RMQUOTE);
    defmacro('"', k_tmacro,FT_RMDQUOTE);
    defmacro('`', k_tmacro,FT_RMBQUOTE);
    defmacro(',', k_tmacro,FT_RMCOMMA);
    defmacro('(', k_tmacro,FT_RMLPAR);
    defmacro(')', k_tmacro,FT_RMRPAR);
    defmacro(';', k_tmacro,FT_RMSEMI);
}

