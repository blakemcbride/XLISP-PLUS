/* xlfio.c - xlisp file i/o */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use
        IBM Mainframe/SAS-C modifications - Dave Rivers (rivers@ponds.uucp)

       Revised April 2011 -- Added ~P, ~R (english text and roman
       numerals), ~^ and fixed operation of ~* Added colon modifier for
       D,H,B,R Tom Almy */
        

#include "xlisp.h"
#include <math.h>


#ifdef FILETABLE
#include <errno.h>
#endif

/* forward declarations */
LOCAL LVAL NEAR printit _((int pflag, int tflag));
LOCAL FIXTYPE NEAR flatsize _((LVAL expr, int pflag));
LOCAL VOID NEAR toomanyopt _((LVAL fmt));
LOCAL char FAR * NEAR skip_pp _((char FAR *fmt, int *colon, int *atsign));
LOCAL char FAR * NEAR decode_pp _((char FAR *fmt, FIXTYPE *pp, 
                       int *npp, int *colon, int *atsign, LVAL lfmt, int doit));
LOCAL VOID NEAR opt_print _((LVAL stream, LVAL val, int pflag, FIXTYPE *pp,
                    int colon, int atsign));
LOCAL VOID NEAR string_print _((LVAL stream, LVAL val, int colon, int atsign));

LOCAL VOID NEAR num_print _((LVAL stream,LVAL val,int pflag,FIXTYPE *pp,
                             int colon, int atsign, int fixnum));
LOCAL VOID NEAR tab_print _((LVAL stream, FIXTYPE *pp, int atsign));
LOCAL VOID NEAR indirect_print _((LVAL stream, int atsign));
LOCAL VOID NEAR case_convert_print _((char FAR *fmt, LVAL stream,
                                      int colon, int atsign));
LOCAL VOID NEAR conditional_print _((char FAR *fmt, LVAL stream, FIXTYPE count,
                                     int colon, int atsign));
LOCAL VOID NEAR iterative_print _((char FAR *fmt, LVAL stream, FIXTYPE count,
                                   int colon, int atsign));
LOCAL char FAR * NEAR skip_past_directive _((char FAR *fmt, int tch,
                                             int want_colon));

LOCAL VOID NEAR xlformat1 _((LVAL lfmt, LVAL stream));

#define MAXNPP  7 /* MAXIMUM number of arguments for format specs */
#define FMTMAX 255 /* good maximum value for a format spec, with good minimum being zero */

/* xread - read an expression */
/* eof-error-p added - L. Tierney */
LVAL xread()
{
    LVAL fptr,eof,val;
    int eof_error_p, recursive_p = FALSE;

    /* get file pointer and eof value */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    if (moreargs() && !null(xlgetarg()))  recursive_p = TRUE;
    if (moreargs()) xltoomany();    /* Trouble if still more arguments */

    /* read an expression */
    if (!xlread(fptr, &val, recursive_p, FALSE)) {
        if (eof_error_p) xlfail("end of file on read");
        else val = eof;
    }

    /* return the expression */
    return (val);
}

/* TAA MOD 9/97 -- added read-preserving-whitespace */
LVAL xreadpw()
{
    LVAL fptr,eof,val;
    int eof_error_p, recursive_p = FALSE;

    /* get file pointer and eof value */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    if (moreargs() && !null(xlgetarg()))  recursive_p = TRUE;
    if (moreargs()) xltoomany();    /* Trouble if still more arguments */

    /* read an expression */
    if (!xlread(fptr, &val, recursive_p, TRUE)) {
        if (eof_error_p) xlfail("end of file on read");
        else val = eof;
    }

    /* return the expression */
    return (val);
}

/* xprint - built-in function 'print' */
LVAL xprint()
{
    return (printit(TRUE,TRUE));
}

/* xprin1 - built-in function 'prin1' */
LVAL xprin1()
{
    return (printit(TRUE,FALSE));
}

/* xprinc - built-in function princ */
LVAL xprinc()
{
    return (printit(FALSE,FALSE));
}

/* xfreshline - start a new line if not at begining of line */
LVAL xfreshline()
{
    LVAL fptr;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* optionally terminate the print line and return action */
    return (xlfreshline(fptr)? s_true : NIL);
}


/* xterpri - terminate the current print line */
LVAL xterpri()
{
    LVAL fptr;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* terminate the print line and return nil */
    xlterpri(fptr);
    return (NIL);
}

#ifdef PRINTTOSTRING
LVAL xprin1tostring()
{
	LVAL fptr, val;
	val = xlgetarg();
	xllastarg();
	xlsave1(fptr);
	fptr = newustream();
	xlprint(fptr, val, TRUE);
	val = getstroutput(fptr);
	xlpop();
	return val;
}

LVAL xprinctostring()
{
	LVAL fptr, val;
	val = xlgetarg();
	xllastarg();
	xlsave1(fptr);
	fptr = newustream();
	xlprint(fptr, val, FALSE);
	val = getstroutput(fptr);
	xlpop();
	return val;
}
#endif

/* printit - common print function */
LOCAL LVAL NEAR printit(pflag,tflag)
  int pflag,tflag;
{
    LVAL fptr,val;

    /* get expression to print and file pointer */
    val = xlgetarg();
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* print the value */
    xlprint(fptr,val,pflag);

    /* terminate the print line if necessary */
    if (tflag)
        xlterpri(fptr);

    /* return the result */
    return (val);
}

/* xflatsize - compute the size of a printed representation using prin1 */
LVAL xflatsize()
{
    /* TAA MOD -- rewritten to use a USTREAM 1/21/97 */
    LVAL val;

    /* get the expression */
    val = xlgetarg();
    xllastarg();

    return (cvfixnum(flatsize(val, TRUE)));
}

/* xflatc - compute the size of a printed representation using princ */
LVAL xflatc()
{
    /* TAA MOD -- rewritten to use a USTREAM 1/21/97 */
    LVAL val;

    /* get the expression */
    val = xlgetarg();
    xllastarg();

    return (cvfixnum(flatsize(val, FALSE)));
}

/* flatsize - compute the size of a printed expression */
LOCAL FIXTYPE NEAR flatsize(val, pflag)
  LVAL val;
  int pflag;
{
    /* TAA MOD -- rewritten to use a USTREAM 1/21/97 */
    LVAL ustream;
    FIXTYPE size;

    /* create and protect the stream */
    ustream = newustream();
    xlprot1(ustream);

    /* print the value to compute its size */
    xlprint(ustream,val,pflag);

    /* calculate size */
    for (size = 0, ustream = gethead(ustream);
         !null(ustream);
         size++, ustream = cdr(ustream)) {} ;

    /* unprotect */
    xlpop();
    
    /* return the length of the expression */
    return (size);
}


enum ACTIONS {A_NIL, A_ERR, A_REN, A_OVER, A_APP, A_SUPER, A_CREATE};

/* xopen - open a file */
LVAL xopen()
{
#ifdef BIGNUMS
    FIXTYPE nbits = 0;
#endif
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;         /* file name strings */
#endif
    FILEP fp;           /* opened file pointer */
    LVAL fname;         /* file name string LVAL */
    LVAL temp;          /* key arguments */
    int iomode;         /* file mode, as stored in node */

/* There doesn't seem to be any consistancy here in the handling of
   "CDECL" when dealing with pointers to procedures  TAA */
#ifdef FILETABLE
    FILEP (*opencmd) _((const char *, MODETYPE));
#else
#ifdef __TURBOC__
    FILEP CDECL (*opencmd)(const char *, MODETYPE);
                            /* file type, TRUE if binary */
#else
    FILEP (* CDECL opencmd) _((const char *, MODETYPE));
#endif
#endif
    enum ACTIONS exist; /* exist action */
    enum ACTIONS nexist;/* non-exist action */

    /* get file name */
#ifdef MEDMEM
    MEMCPY(name, getstring(fname = xlgetfname()), STRMAX);
    name[STRMAX-1] = 0;
#else
    name = getstring(fname = xlgetfname());
#endif

    /* get direction */
    if (xlgetkeyarg(k_direction,&temp) && temp != k_input) {
        if (temp == k_output) iomode = S_FORWRITING;
        else if (temp == k_io) iomode = S_FORREADING|S_FORWRITING;
        else if (temp == k_probe) iomode = 0;
        else goto argerror;
    }
    else iomode = S_FORREADING;

    /* get type */

    if (xlgetkeyarg(k_elementtype,&temp) && temp != a_char ) {
#ifdef BIGNUMS
        int notsigned = TRUE;
        nbits = 8; /* default size */
        if (temp == a_sbyte) notsigned = FALSE;
        else if (consp(temp) && car(temp) == a_sbyte &&
                 consp(cdr(temp)) && fixp(car(cdr(temp))) &&
                 null(cdr(cdr(temp)))) {
            nbits = getfixnum(car(cdr(temp)));
            notsigned = FALSE;
        }
        else if (consp(temp) && car(temp) == a_unbyte &&
                 consp(cdr(temp)) && fixp(car(cdr(temp))) &&
                 null(cdr(cdr(temp)))) {
            nbits = getfixnum(car(cdr(temp)));
        }
        else if (temp != a_unbyte && temp != a_fixnum)
            goto argerror;
        if (nbits < 0 || (nbits & 7) != 0 || nbits > 32L*MAXVLEN)
            goto argerror; /* invalid value for number of bits */
        if (iomode) iomode |= (notsigned ? S_BINARY|S_UNSIGNED : S_BINARY);
#if defined(MSC) & !defined(FILETABLE)
        opencmd = (FILEP (* CDECL)(const char *, MODETYPE)) OSBOPEN;
#else
        opencmd = OSBOPEN;
#endif
        
#else
        if (temp == a_fixnum ) {
            if (iomode) iomode |= S_BINARY; /* mark as binary file type */
#if defined(MSC) & !defined(FILETABLE)
            opencmd = (FILEP (* CDECL)(const char *, MODETYPE)) OSBOPEN;
#else
            opencmd = OSBOPEN;
#endif
        }
        else goto argerror;
#endif
    }
    else
#if defined(MSC) & !defined(FILETABLE)
        opencmd = (FILEP (* CDECL)(const char *, MODETYPE)) OSAOPEN;
#else
        opencmd = OSAOPEN;
#endif

    /* get exists action */

    if (xlgetkeyarg(k_exist, &temp) &&
        (iomode & S_FORWRITING) &&  /* ignore value if :input or :probe */
        temp != k_rename && temp != k_newversion) {
        if (null(temp)) exist = A_NIL;
        else if (temp == k_error) exist = A_ERR;
        else if (temp == k_overwrite) exist = A_OVER;
        else if (temp == k_append) exist = A_APP;
        else if (temp == k_supersede || temp == k_rendel)
            exist = A_SUPER;
        else goto argerror;
    }
    else exist = A_REN;

    /* get non-exist action */

    if (xlgetkeyarg(k_nexist, &temp)) {
        if (null(temp)) nexist = A_NIL;
        else if (temp == k_error) nexist = A_ERR;
        else if (temp == k_create) nexist = A_CREATE;
        else goto argerror;
    }
    else {  /* handle confusing mess of defaults */
        if (iomode == S_FORREADING || exist == A_OVER || exist == A_APP)
            nexist = A_ERR;
        else if (iomode & S_FORWRITING) nexist = A_CREATE;
        else nexist = A_NIL;
    }

    xllastkey();

    /* attempt to open the file */

    if ((fp = (*opencmd)(name, (iomode & S_FORWRITING) ? OPEN_UPDATE : OPEN_RO))!=CLOSED) {
        /* success! */
        if (iomode & S_FORWRITING) switch (exist) { /* do exist action */
            case A_ERR: /* give error */
                OSCLOSE(fp);
                xlerror("file exists", fname);
                break;
            case A_REN: /* create new version */
                OSCLOSE(fp);
                fp = CLOSED;
                if (!renamebackup(name))
                    xlerror("couldn't create backup file", fname);
                break;
            case A_APP: /* position to end of file */
                OSSEEKEND(fp);
                break;
            case A_SUPER:   /* supersede file */
                OSCLOSE(fp);
                fp = CLOSED;
                break;
            case A_NIL:     /* return NIL */
                OSCLOSE(fp);
                return NIL;
            /*case A_OVER:*/    /* overwrite -- does nothing special */
            default: ;
        }
    }
    else {  /* file does not exist */
        switch (nexist) {
            case A_ERR: /* give error */
                xlerror("file does not exist", fname);
                break;
            case A_NIL:     /* return NIL */
                return NIL;
            /*case A_CREATE:*/  /* create a new file */
            default: ;
        }
    }

    /* we now create the file if it is not already open */
    if (fp == CLOSED)
        if ((fp = (*opencmd)(name, (iomode&S_FORREADING)? CREATE_UPDATE: CREATE_WR)) == CLOSED)
            xlerror("couldn't create file", fname);

    /* take concluding actions */
    if (iomode == 0) { /* probe */
        OSCLOSE(fp);
        fp = CLOSED;
    }

#ifdef BIGNUMS
    temp = cvfile(fp, iomode);
    temp->n_bsiz = (short)(unsigned short)(nbits/8);
    return temp;
#else
    return cvfile(fp,iomode);
#endif
    argerror: xlerror("invalid argument", temp);
    return NIL;
}


/* xfileposition - get position of file stream */
LVAL xfileposition()
{
    long j,fsize;
    double i;
    int t = 0; /* Initialized to eliminate warning 10/2015 */
    LVAL pos, fptr;
    FILEP fp;
    /* get file pointer */
    fp = getfile(fptr = xlgastream());

    /* make sure the file exists */
    if (fp == CLOSED)
        xlfail("file not open");

    /* get current position, adjusting for posible "unget" */
    j = OSTELL(fp) + (getsavech(fptr) ? -1L : 0L);

    if (moreargs()) { /* must be set position */
        pos = xlgetarg();
        xllastarg();
        if (pos == k_end) t=OSSEEKEND(fp);
        else if (pos == k_start) t=OSSEEK(fp,0L);
        else if (fixp(pos)) {   /* check for in range, then position */
            /* STDIO allows positioning beyond end of file, so we must check
                the file size (boo his!) */
            i = getfixnum(pos);
#ifdef BIGNUMS
            if (fptr->n_sflags & S_BINARY) i *= fptr->n_bsiz;
#endif
            t = OSSEEKEND(fp);
            fsize = OSTELL(fp);
            if (t == 0 && fp != CONSOLE && (i < 0 || i > fsize)) {
                OSSEEK(fp,j);
                xlerror("position outside of file", pos);
            }
            t = OSSEEK(fp, (long)i);
        }
        else xlbadtype(pos);

        setsavech(fptr,'\0');   /* toss unget character, if any */
        fptr->n_sflags &= ~(S_READING|S_WRITING);
                                /* neither reading or writing currently */
        /* t is non-zero if couldn't do seek */
        return (t != 0 || fp == CONSOLE ? NIL : s_true);
    }

#ifdef BIGNUMS
    return ((j == -1L || fp == CONSOLE) ? NIL :
            cvfixnum(fptr->n_sflags & S_BINARY ? j/fptr->n_bsiz : j));
#else
    return ((j == -1L || fp == CONSOLE) ? NIL : cvfixnum(j));
#endif
}

/* xfilelength - returns length of file */
LVAL xfilelength()
{
#ifdef BIGNUMS
    LVAL stream;
#endif
    FILEP fp;
    long i,j;

    /* get file pointer */
#ifdef BIGNUMS
    fp = getfile(stream = xlgastream());
#else
    fp = getfile(xlgastream());
#endif
    xllastarg();

    /* make sure the file exists */
    if (fp == CLOSED)
        xlfail("file not open");

    /* not all stdio packages will catch the following gaffe */
    if (fp == CONSOLE) return NIL;

    if ((i=OSTELL(fp)) == -1L ||
        OSSEEKEND(fp) ||
        (j = OSTELL(fp)) == -1L ||
        OSSEEK(fp,i)) {
        return NIL;
    }

#ifdef BIGNUMS
    return cvfixnum(stream->n_sflags & S_BINARY ? j/stream->n_bsiz : j);
#else
    return cvfixnum(j);
#endif
}


#ifdef FILETABLE
LVAL xtruename()
{
    LVAL f = xlgetfname();
    char namebuf[FNAMEMAX+1];

    xllastarg();


    STRCPY(buf, getstring(f));

    if (!truename(buf, namebuf)) xlerror("strange file name", f);

    return cvstring(namebuf);
}

LVAL xdeletefile()
{
    LVAL arg;
    FILEP fp;

    /* get the argument */

    arg = xlgetarg();
    xllastarg();

    if (streamp(arg) && getfile(arg) > CONSOLE) {
        /* close file first */
        fp = getfile(arg);
        STRCPY(buf, filetab[fp].tname);
        OSCLOSE(fp);
        setsavech(arg, '\0');
        setfile(arg,CLOSED);
    }
    else {
        if (symbolp(arg)) arg = getpname(arg);
        else if (!stringp(arg)) xlbadtype(arg);

        if (getslength(arg) >= FNAMEMAX)
            xlerror("file name too long", arg);

        STRCPY(buf,getstring(arg));
    }
    if (remove(buf) != 0 && errno == EACCES)
        xlerror("cannot delete file", arg);

    return s_true;
}

#endif

/* xclose - close a file */
LVAL xclose()
{
    LVAL fptr;
    FILEP fp;   /* TAA MOD to allow closing closed files,
                    prohibit closing the console, return the correct
                    values (true on success), and close string streams */


    /* get file pointer */
    fptr = xlgetarg();
    xllastarg();

    /* handle string stream case by converting to a closed file! */
    if (ustreamp(fptr)) {
        fptr->n_type = STREAM;
        setfile(fptr, CLOSED);
        setsavech(fptr, '\0');
        return (s_true);
    }

    /* give error of not file stream */
    if (!streamp(fptr)) xlbadtype(fptr);


    /* make sure the file exists */

    if ((fp = getfile(fptr)) == CLOSED || fp == CONSOLE)
        return (NIL);

    /* close the file */
    OSCLOSE(fp);
    setsavech(fptr, '\0');
    setfile(fptr,CLOSED);

    /* return true */
    return (s_true);
}

/* xrdchar - read a character from a file */
/* eof, eof-error-p added - L. Tierney */
LVAL xrdchar()
{
    LVAL fptr, eof;
    int ch, eof_error_p;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    xllastarg();

    /* get character and check for eof */
    ch = xlgetc(fptr);
    if (ch == EOF && eof_error_p) xlfail("end of file on read");
    return (ch == EOF ? eof : cvchar(ch));
}

#ifdef BIGENDIAN
#define bs(n) (n) /* byte select in short */
#else
#define bs(n) ((n)^1)
#endif

/* xrdbyte - read a byte from a file */
/* eof, eof-error-p added - L. Tierney */
#ifdef BIGNUMS
LVAL xrdbyte()
{
    LVAL fptr, eof, val;
    BIGNUMDATA FAR *vx;
    unsigned char FAR *v;
    int ch, eof_error_p, i, size, ofs;
    FIXTYPE temp;

    /* get file pointer */
    fptr = xlgastream();
    if ((fptr->n_sflags & S_BINARY) == 0)
        xlfail("not a binary file");
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    xllastarg();

    if (fptr->n_bsiz == 1) { /* file of bytes */
        ch = xlgetc(fptr);
        if (ch == EOF && eof_error_p) xlfail("end of file on read");
        return(ch == EOF ? eof :
               ((fptr->n_sflags&S_UNSIGNED) ? cvfixnum((FIXTYPE)ch)
                : cvfixnum((FIXTYPE)(signed char)ch)));
    }
    else { /* file of more than that */
        size = (fptr->n_bsiz+sizeof(BIGNUMDATA)-1)/sizeof(BIGNUMDATA);
                /* size of bignum needed */
        if (size < 2) size = 2;
        ofs = size*sizeof(BIGNUMDATA) - fptr->n_bsiz;   /* unused bytes */
        val = newbignum(size);
        vx = getbignumarray(val)+1; /* point to data array start */
        v = (unsigned char FAR *)vx;
#ifdef BIGENDIANFILE
        for (i = ofs; i < size*sizeof(BIGNUMDATA); i++)
#else
        for (i = size*sizeof(BIGNUMDATA)-1; i >= ofs; i--)
#endif
        {
            ch = xlgetc(fptr);
            if (ch == EOF) {
                if (eof_error_p) xlfail("end of file on read");
                else return eof;
            }
            v[bs(i)] = (unsigned char)ch;
        }
        if ((signed char)(v[bs(ofs)]) < 0 && (fptr->n_sflags&S_UNSIGNED)==0)
        {
            /* we need to handle negative number */
            unsigned long sum;
            int carry = 1;
            vx[-1] = 1;
            for (i = ofs-1; i >=0; i--) v[bs(i)] = 0xff;
            for (i = size-1; i >= 0; i--) {
                sum = (unsigned long)(BIGNUMDATA)(~vx[i]) + carry;
                carry = (int)(sum >> 16);
                vx[i] = (BIGNUMDATA)sum;
            }
        }
        val = normalBignum(val);    /* normalize in case of leading zeroes */
        return (cvtbigfixnum(val, &temp) ? cvfixnum(temp) : val);
    }
}
#else
LVAL xrdbyte()
{
    LVAL fptr, eof;
    int ch, eof_error_p;

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    xllastarg();

    /* get character and check for eof */
    ch = xlgetc(fptr);
    if (ch == EOF && eof_error_p) xlfail("end of file on read");
    return(ch == EOF ? eof : cvfixnum((FIXTYPE)ch));
}
#endif

/* xpkchar - peek at a character from a file */
/* eof, eof-error-p added */
LVAL xpkchar()
{
    LVAL flag,fptr,eof;
    int ch, eof_error_p;

    /* peek flag and get file pointer */
    flag = (moreargs() ? xlgetarg() : NIL);
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    xllastarg();

    /* skip leading white space and get a character */
    if (!null(flag))
        while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
            xlgetc(fptr);
    else
        ch = xlpeek(fptr);

    /* return the character */
    if (ch == EOF && eof_error_p) xlfail("end of file on read");
    return (ch == EOF ? eof : cvchar(ch));
}

/* xwrchar - write a character to a file */
LVAL xwrchar()
{
    LVAL fptr,chr;

    /* get the character and file pointer */
    chr = xlgachar();
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* put character to the file */
    xlputc(fptr,getchcode(chr));

    /* return the character */
    return (chr);
}

/* xwrbyte - write a byte to a file */
#ifdef BIGNUMS
/* we will continue XLISP's tradition of not checking for value
   to write being out of range. At any rate, this will save time. */
LVAL xwrbyte()
{
    LVAL fptr,chr,chr2;
    BIGNUMDATA FAR *vx;
    unsigned char FAR *v;
    int size, i, ofs;

    /* get the byte and file pointer */
    chr = xlgetarg();
    if (!(fixp(chr) || bignump(chr))) xlbadtype(chr);
    fptr = xlgastream();
    if ((fptr->n_sflags & S_BINARY) == 0)
        xlfail("not a binary file");
    xllastarg();

    /* can't really do an unsigned write of a negative number */
    if ((fptr->n_sflags&S_UNSIGNED) &&
        ((fixp(chr)&&getfixnum(chr)<0) || (bignump(chr)&&getbignumsign(chr))))
        xlerror("Can't do unsigned write-byte of", chr);
    

    if (fptr->n_bsiz == 1 && fixp(chr)) { /* handle easy case */
        /* put byte to the file */
        xlputc(fptr,(int)getfixnum(chr));
        return (chr);
    }
    /* work only with bignums from now on */
    if (fixp(chr)) chr2 = cvtfixbignum(getfixnum(chr));
    else chr2 = chr;
    vx = getbignumarray(chr2);
    size = getbignumsize(chr2) * sizeof(BIGNUMDATA); /* number size in bytes */
    ofs =  fptr->n_bsiz - size; /* number of excess bytes to write */
    if (*vx++) { /* negative value */
#ifdef BIGENDIANFILE
        int j;
        v = (unsigned FAR char *)vx;
        for (i = ofs; i > 0; i--) xlputc(fptr, 0xff); /* filler */
        for (i = size-1; i >= -ofs && i >= 0; i--) { /* find end of carries */
            if (v[bs(i)] != 0) { /* only zeroes will generate carries */
                for (j = (ofs >= 0 ? 0 : -ofs); j < i; j++) {
                    xlputc(fptr, (unsigned char) (~v[bs(j)]));
                }
                break;
            }
        }
        for (; i < size; i++) xlputc(fptr, 1 + (unsigned char)(~v[bs(i)]));
#else
        unsigned sum;
        int carry=1;
        v = (unsigned FAR char *)vx;
        for (i = size-1; i >= -ofs && i >= 0; i--) {
            sum = (unsigned)(unsigned char)~v[bs(i)] + carry;
            carry = sum >> 8;
            xlputc(fptr, (unsigned char) sum);
        }
        for (i = ofs; i > 0; i--) xlputc(fptr, 0xff); /* filler */
#endif
    }
    else { /* postive value */
        v = (unsigned FAR char *)vx;
#ifdef BIGENDIANFILE
        for (i = ofs; i > 0; i--) xlputc(fptr, 0); /* filler */
        for (i = (ofs >= 0 ? 0 : -ofs); i < size; i++) xlputc(fptr, v[bs(i)]);
#else
        for (i = size-1; i >= -ofs && i >= 0; i--) xlputc(fptr, v[bs(i)]);
        for (i = ofs; i > 0; i--) xlputc(fptr, 0); /* filler */
#endif
    }

    /* return the byte */
    return (chr);
}
#else
LVAL xwrbyte()
{
    LVAL fptr,chr;

    /* get the byte and file pointer */
    chr = xlgafixnum();
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* put byte to the file */
    xlputc(fptr,(int)getfixnum(chr));

    /* return the character */
    return (chr);
}
#endif

/* xreadline - read a line from a file */
/* eof, eof-error-p added - L. Tierney */
LVAL xreadline()
{
    char *p, FAR *sptr;
    LVAL fptr,str,newstr,eof;
    int len,blen,ch,eof_error_p;

    /* protect some pointers */
    xlsave1(str);

    /* get file pointer */
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
    eof_error_p = moreargs() ? ((xlgetarg() != NIL) ? TRUE : FALSE) : TRUE;
    eof = (moreargs() ? xlgetarg() : NIL);
    xllastarg();

    /* get character and check for eof */
    len = blen = 0; p = buf;
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n') {

        /* check for buffer overflow TAA MOD to use memcpy instead of strcat*/
        if (blen >= STRMAX) {
            newstr = newstring(len + STRMAX);
            sptr = getstring(newstr);
            if (str != NIL) MEMCPY(sptr, getstring(str), len);
            MEMCPY(sptr+len, buf, blen);
            p = buf; blen = 0;
            len += STRMAX;
            str = newstr;
        }

        /* store the character */
        *p++ = (char)ch; ++blen;
    }

    /* check for end of file */
    if (len == 0 && p == buf && ch == EOF) {
        xlpop();
        if (eof_error_p) xlfail("end of file on read");
        return (eof);
    }

    /* append the last substring */
    /* conditional removed because code always executes! */
    newstr = newstring(len + blen);
    sptr = getstring(newstr);
    if (str != NIL) MEMCPY(sptr, getstring(str), len);
    MEMCPY(sptr+len, buf, blen);
    sptr[len+blen] = '\0';
    str = newstr;

    /* restore the stack */
    xlpop();

    /* return the string */
    return (str);
}


/* xmkstrinput - make a string input stream */
/* TAA MOD - reworked for unsigned lengths */

LVAL xmkstrinput()
{
    unsigned start,end=0,len,i; /* end initialized to eliminate warning 10/2015 */
    FIXTYPE temp;
    char FAR *str;
    LVAL string,val;

    /* protect the return value */
    xlsave1(val);

    /* get the string and length */
    string = xlgastring();
    str = getstring(string);
    len = getslength(string);

    /* get the starting offset */
    if (moreargs()) {
        val = xlgafixnum();
        temp = getfixnum(val);
        if (temp < 0 || temp > (FIXTYPE)len)
            xlerror("string index out of bounds",val);
        start = (unsigned) temp;
    }
    else start = 0;

    /* get the ending offset */
    if (moreargs()) {       /* TAA mod to allow NIL for end offset */
        val = nextarg();
        if (null(val)) end = len;
        else if (fixp(val)) {
            temp = getfixnum(val);
            if (temp < (FIXTYPE)start || temp > (FIXTYPE)len)
                xlerror("string index out of bounds",val);
            end = (unsigned) temp;
        }
        else xlbadtype(val);

        xllastarg();
    }
    else end = len;

    /* make the stream */
    val = newustream();

    /* copy the substring into the stream */
    for (i = start; i < end; ++i)
        xlputc(val,str[i]);

    /* restore the stack */
    xlpop();

    /* return the new stream */
    return (val);
}

/* xmkstroutput - make a string output stream */
LVAL xmkstroutput()
{
    return (newustream());
}

/* xgetstroutput - get output stream string */
LVAL xgetstroutput()
{
    LVAL stream;
    stream = xlgaustream();
    xllastarg();
    return (getstroutput(stream));
}

/* xgetlstoutput - get output stream list */
LVAL xgetlstoutput()
{
    LVAL stream,val;

    /* get the stream */
    stream = xlgaustream();
    xllastarg();

    /* get the output character list */
    val = gethead(stream);

    /* empty the character list */
    sethead(stream,NIL);
    settail(stream,NIL);

    /* return the list */
    return (val);
}


LOCAL VOID NEAR toomanyopt(fmt)
LVAL fmt;
{
    xlerror("too many prefix parameters in format",fmt);
}

/* This function added by Luke Tierney to fix skip_past_directive. 1/97 */
LOCAL char FAR * NEAR skip_pp (fmt, colon, atsign)
char FAR *fmt;
int *colon;
int *atsign;
{
    *colon = 0;
    *atsign= 0;
    do {
        if (*fmt == '\'') fmt += 2; /* character code */
        else if (*fmt == '#') fmt++; /* xlargc is value */
        else if (*fmt == 'v' || *fmt == 'V') fmt++; /* lisp arg is value */
        else if (isdigit(*fmt)) /* integer literal */
            do { fmt++; } while (isdigit(*fmt));
        else if (*fmt == ',') {     /* empty field */
        }
        else  break;                /* nothing to process */
        
        if (*fmt != ',') break;         /* no comma -- done */
        fmt++;                          /* toss comma */
    } while (TRUE);

    do {    /* pick up any colon or atsign modifier */
        if (*fmt == ':') *colon = 1;
        else if (*fmt == '@') *atsign = 1;
        else break;
        fmt++;
    } while (TRUE);
    return fmt;
}

    


/* decode prefix parameters and modifiers for a format directive */
/* TAA MOD Entirely rewritten -- return value -1 for unassigned since
   negative numbers are inappropriate for all arguments we are concerned
   with. Also clips args to reasonable values, allows both : and @ modifiers
   at once. */
LOCAL char FAR * NEAR decode_pp( fmt, pp, npp, colon, atsign, lfmt, doit)
char FAR *fmt;
FIXTYPE *pp;            /* prefix parameters */
int     *npp;           /* actual number of them */
int     *colon;         /* colon modifier given? */
int     *atsign;        /* atsign modifier given? */
LVAL    lfmt;           /* format string in case of failure */
int	doit;		/* Actually process (important for "V" argument) */
{
    int i;
    int gotone = 0;
    
    FIXTYPE accum;

    char defined[MAXNPP] = {0}; /* TAA addition April 2011 so we can have negative arguements */
    
    *npp = 0;
    *colon = 0;
    *atsign = 0;
    do {
        if (*fmt == '\'') { /* character code */
            pp[*npp] = *(++fmt);
            defined[*npp] = TRUE;
            gotone = 1;
            fmt++;
        }
        else if (*fmt == 'v' || *fmt == 'V') { /* lisp arg is value */
		if (doit) {
			accum = getfixnum(xlgafixnum());
		} else {
			accum = 0;
		}
//            if (accum < 0) accum = 0;   /* clip at reasonable values */
//            else if (accum>FMTMAX) accum = FMTMAX;
            pp[*npp] = accum;
            defined[*npp] = TRUE;
            gotone = 1;
            fmt++;
        }
        else if (*fmt == '-') { /* negative integer literal */
            accum = 0;
            fmt++;
            do {
                if (accum > MAXFIX/10) xlerror("prefix value out of range", lfmt);
                accum = accum*10 + (int)(*fmt++ - '0');
            } while (isdigit(*fmt));
            gotone = 1;
            pp[*npp] = -accum;
            defined[*npp] = TRUE;
        }
        else if (isdigit(*fmt)) { /* integer literal */
            accum = 0;
            do {
                if (accum > MAXFIX/10) xlerror("prefix value out of range", lfmt);
                accum = accum*10 + (int)(*fmt++ - '0');
            } while (isdigit(*fmt));
            gotone = 1;
            pp[*npp] = accum;
            defined[*npp] = TRUE;
        }
        else if (*fmt == '#') {     /* use number of remaining arguments */
            pp[*npp] = xlargc;
            defined[*npp] = TRUE;
            gotone = 1;
            fmt++;
        }
        else if (*fmt == ',') {     /* empty field */
            gotone = 1;
        }
        else  break;                /* nothing to process */

        if (*fmt != ',') break;         /* no comma -- done */
        *npp += 1;                  /* got an argument */
        fmt++;                          /* toss comma */
        if( *npp >= MAXNPP ) toomanyopt(lfmt);
    } while (TRUE);
    *npp += gotone;

    do {    /* pick up any colon or atsign modifier */
        if (*fmt == ':') *colon = 1;
        else if (*fmt == '@') *atsign = 1;
        else break;
        fmt++;
    } while (TRUE);
    if (!doit) return fmt;  /* Arguments will not be used */
    if (*fmt == '^' ) { /* Only ~^ allows negative arguments */
        for (i = 0; i < *npp; i++) {
            if (!defined[i]) xlerror("no undefined prefix parameters allowed for ~^", lfmt);
        }
    } else { /* values must be in range 0 through FMTMAX */
        for (i = 0; i < MAXNPP; i++) {
            if (defined[i]) {
                if (pp[i] > FMTMAX || pp[i] < 0) xlerror("prefix parameters must be in range 0 through 255", lfmt);
            } else {
                pp[i] = -1;
            }
        }
    }
                
    return fmt;
}

#define mincol  pp[0]
#define colinc  pp[1]
#define minpad  pp[2]
#define padchar pp[3]


/* opt_print - print a value using prefix parameter options */
LOCAL VOID NEAR opt_print(stream,val,pflag,pp,colon,atsign)
LVAL    stream;
LVAL    val;
int     pflag;          /* quoting or not */
FIXTYPE *pp;            /* prefix parameters */
int     colon;          /* colon modifier given? */
int     atsign;         /* at-sign modifier given? */
{
    int flatsiz;
    int i;

    if (mincol < 0) mincol = 0; /* handle default values */
    if (colinc < 1) colinc = 1;    /* also arg of 0 for colinc */
    if (minpad < 0) minpad = 0;
    if (padchar < 0) padchar = ' ';

    if( mincol < minpad )
            mincol = minpad;

    if (mincol > 0) {
        /* we will need to pad, so must calculate flat size */
        if (colon && null(val))         /* flat size is 2 */
            flatsiz = 2;
        else 
            flatsiz = (int)flatsize(val, pflag);
        if (atsign) {       /* padding may be required on left */
            for( i = 0; i < minpad; flatsiz++, i++ )
                xlputc(stream,(int)padchar);
            while( flatsiz < mincol ) {
                for( i = 0; i < colinc; i++ )
                    xlputc(stream,(int)padchar);
                flatsiz += (int)colinc;
            }
        }
    }

    /* print the value */
    if( colon && null(val)) 
        xlputstr(stream,"()");
    else 
        xlprint(stream,val,pflag);

    if( mincol > 0 && !atsign ) {       /* padding required on right */
        for( i = 0; i < minpad; flatsiz++, i++ )
            xlputc(stream,(int)padchar);
        while( flatsiz < mincol ) {
            for( i = 0; i < colinc; i++ )
                xlputc(stream,(int)padchar);
            flatsiz += (int)colinc;
        }
    }
}

LOCAL VOID NEAR roman_print (stream, val, chars, oldstyle)
LVAL stream;
int val;
char *chars;
int oldstyle;
{
    switch (val) {
        case 0: break;
        case 9:
            if (!oldstyle) {
                xlputc(stream, chars[0]);
                xlputc(stream, chars[2]);
                break;
            }
        case 5:
        case 6:
        case 7:
        case 8:
            xlputc(stream, chars[1]);
            val -= 5;
        case 1:
        case 2:
        case 3:
        case 4:
            if (!oldstyle && val == 4) {
                xlputc(stream, chars[0]);
                xlputc(stream, chars[1]);
            } else {
                while (val-- > 0) xlputc(stream, chars[0]);
            }
            break;
    }
}

LOCAL char *SINGLES[] = {"", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
LOCAL char *TEENS[] = {"ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"};
LOCAL char *ORD[] = {"", "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth"};
LOCAL char *TENS[] = {"", "", "twent", "thirt", "fort", "fift", "sixt", "sevent", "eight", "ninet"};
LOCAL VOID NEAR english_print(stream, val, units, ordinal, comma)
LVAL    stream;         /* Output stream */
int     val;            /* value 1-999 */
char*   units;          /* units string */
int     ordinal;        /* treat value as ordinal */
int     comma;          /* comma at end */
{
    if (val > 99) { /* we need to print hundreds */
        int digit = val/100;
        int remainder = (val % 100) != 0;
        xlputstr(stream, SINGLES[digit]);
        if (ordinal && !remainder) xlputstr(stream, " hundredth");
        else {
            xlputstr(stream, " hundred");
            if (remainder) xlputstr(stream, " and "); /* OPTIONAL GRAMMAR VARIATION */
        }
        val -= digit*100;
    }

    if (val > 0) {
        if (val < 10) {
            xlputstr(stream, ordinal ? ORD[val] : SINGLES[val]);
        } else if (val <= 19) {
            xlputstr(stream, TEENS[val-10]);
            if (ordinal) xlputstr(stream, "th");
        } else {
            xlputstr(stream, TENS[val/10]);
            if (val%10 != 0) { /* units as well */
                xlputstr(stream, "y-");
                xlputstr(stream, ordinal ? ORD[val%10] : SINGLES[val%10]);
            } else {
                xlputstr(stream, ordinal ? "ieth" : "y");
            }
        }

    }

    if (units != NULL) xlputstr(stream, units);
    if (comma) xlputstr(stream, ", ");
            

}


LOCAL VOID NEAR string_print (stream, val, colon, atsign)
LVAL    stream;
LVAL    val;
int     colon;
int     atsign;
{
    FIXTYPE value = getfixnum(val);
    if (atsign) { /* roman numeral */
        if (colon && (value < 1 || value > 4999))
            xlfail("~@:R format value must be within 1 through 4999");
        else if (!colon && (value < 1 || value > 3999))
            xlfail("~@R format value must be within 1 through 3999");
        roman_print(stream, (value/1000) % 10, "M  ", colon);
        roman_print(stream, (value/100) % 10, "CDM", colon);
        roman_print(stream, (value/10) % 10, "XLC", colon);
        roman_print(stream, value % 10, "IVX", colon);
    } else { /* Cardinals and ordinals */
        if (value == 0) {
            xlputstr(stream, (colon ? "zeroth" : "zero"));
        } else {
            unsigned FIXTYPE v;
            unsigned digit;
            if (value < 0) {
                xlputstr(stream, "minus ");
                v = (unsigned FIXTYPE)-value;
            } else {
                v = (unsigned FIXTYPE)value;
            }
#ifdef __x86_64__
            digit = v/1000000000000000000L;
            v = v - digit*1000000000000000000L;
            if (digit > 0) {
                english_print(stream, digit, colon && v == 0 ? " quintillionth" : " quintillion", FALSE, v>0);
            }
            digit = v/1000000000000000L;
            v = v - digit*1000000000000000L;
            if (digit > 0) {
                english_print(stream, digit, colon && v == 0 ? " quadrillionth" : " quadrillion", FALSE, v>0);
            }
            digit = v/1000000000000L;
            v = v - digit*1000000000000L;
            if (digit > 0) {
                english_print(stream, digit, colon && v == 0 ? " trillionth" : " trillion", FALSE, v>0);
            }
#endif
            digit = v/1000000000L;
            v = v - digit*1000000000L;
            if (digit > 0) {
                english_print(stream, digit, colon && v == 0 ? " billionth" : " billion", FALSE, v>0);
            }
            digit = v/1000000L;
            v = v - digit*1000000L;
            if (digit > 0) {
                english_print(stream, digit, colon && v == 0 ? " millionth" : " million", FALSE, v>0);
            }
            digit = v/1000L;
            v = v - digit*1000L;
            if (digit > 0) {
                english_print(stream, digit, colon && v == 0 ? " thosandth" : " thousand", FALSE, v > 0);
            }
            digit = v;
            english_print(stream, digit, NULL, colon, FALSE);
        }
    }
        
}
#define round pp[1]
LOCAL VOID NEAR num_print(stream,val,pflag,pp,colon,atsign,fixnum)
LVAL    stream;
LVAL    val;
int     pflag;          /* Style */
FIXTYPE *pp;            /* prefix parameters */
int     colon;		/* colon modifier given? */
int     atsign;         /* at-sign modifier given? */
int     fixnum;         /* type D, O or X */
{
#ifdef BIGNUMS
    char FAR *bufr;
    FIXTYPE radix;
#endif
    char cmd[50];
    int fillchar, i, commachar, commacount, startcount;

#ifdef BIGNUMS
    if (pflag == 'R') {
        radix = pp[0];
        if (radix < 2 || radix > 36)
            xlerror("bad radix specified", cvfixnum(radix));
        pp++;
    }
#endif

    fillchar = (int)pp[fixnum? 1 : 2];

    if (fillchar < 0) fillchar = ' ';

    if (colon && fixnum) {
	    commachar = pp[2];
	    if (commachar < 0) commachar = ',';
	    commacount = pp[3];
	    if (commacount < 0) commacount = 3;
    }
    

#ifdef BIGNUMS
/* can't print in binary or arbitrary radix with printf*/
    if (fixp(val) && (pflag == 'B' || pflag == 'R')) {
        if (getfixnum(val) == 0) pflag = 'D'; /* print zero in "decimal" */
        else val = cvtfixbignum(getfixnum(val));
    }
#endif

    if (fixnum && fixp(val)) { /* D O or X and fixnum */
        FIXTYPE v = getfixnum(val); /* TAA mod 3/95 to handle @X and @O
                                       and negative values with X and O */
        switch (pflag) {
            case 'D':
                sprintf(buf, (atsign?"%+ld":"%ld"), v);
                break;
            case 'O':
                if (v<0) sprintf(buf, "-%lo", -v);
                else sprintf(buf, (atsign? "+%lo" : "%lo"), v);
                break;
            case 'X':
                if (v<0) sprintf(buf, "-%lx", -v);
                else sprintf(buf, (atsign? "+%lx" : "%lx"), v);
                break;
        }
    }
#ifdef BIGNUMS
    else if (fixnum && bignump(val)) { /* D O or X and bignum */
        switch (pflag) {
            case 'D': radix = 10; break;
            case 'X': radix = 16; break;
            case 'B': radix = 2; break;
            case 'O': radix = 8; break;
        }
        bufr = cvtbignumstr(val, (int)radix);
        if (atsign && getbignumsign(val)) atsign = 0; /* add leading "+"? */
	if (mincol > 0) {   /* need to fill */
		i = (int)mincol - atsign - STRLEN(bufr);
		if (colon) i -= (STRLEN(bufr)-1)/commacount;
            while (i-- > 0)
		    xlputc(stream,fillchar);
        }
	if (atsign) xlputc(stream, '+');
	if (colon) {
		char FAR *bufp = bufr;
		if (*bufp == '-') xlputc(stream, *bufp++);
		startcount = (STRLEN(bufp)-1)%commacount + 1;;
		while (*bufp != '\0') {
			if (startcount-- == 0) {
				xlputc(stream, commachar);
				startcount = commacount-1;
			}
			xlputc(stream, *bufp++);
		}
			
	} else {
#ifdef MEDMEM
		while (*bufr != '\0') xlputc(stream,*bufr++);
#else
		xlputstr(stream,bufr);
#endif
	}
        MFREE(bufr);
        return;
    }
#endif
    else if (fixnum || !realp(val)) {   /* not a number */
        padchar = colinc = minpad = -1; /* zap arg if provided */
        opt_print(stream,val,FALSE,pp,0,0);
        return;
    }
    else {  /* one of the floating point formats, and a number */
#ifdef BIGNUMS
        FLOTYPE num = makefloat(val);
#else
        FLOTYPE num = fixp(val) ? (FLOTYPE)getfixnum(val) : getflonum(val);
#endif
	colon = 0;	/* No colon modifier */
	if (pflag == 'F' &&                            
#ifdef __SASC__                            
            /* IBM 370 floating pt format; the largest number isn't */ 
            /* quite so large... - Dave Rivers (rivers@ponds.uucp) */  
            fabs(num) > 1e75                       
#else                                      
            fabs(num) > 1e100                      
#endif                                 
            )
            pflag = 'E';    /* don't generate extra big number */
        strcpy(cmd,"%");
        if (atsign) strcat(cmd,"+");
        if (round >= 0) {
            sprintf(buf, ".%d", (int) round);
            strcat(cmd, buf);
        }
        buf[0] = (char) tolower(pflag);
        buf[1] = '\0';
        strcat(cmd,buf);
        sprintf(buf, cmd, (double)num);
        /* TAA MOD 3/98 --
         * don't allow printing FP numbers that can't be read */
        if (strchr(buf, '.') == NULL &&
            strchr(buf, 'e') == NULL) {
        /* Can't print out FP value without a decimal point and
         * trailing zero or an exponent*/
            strcat(buf, ".0");
        }
    }
    if (mincol > 0) {   /* need to fill */
	    i = (int)mincol - strlen(buf);
	    if (colon) i -= (strlen(buf)-1)/commacount;
	    while ( i-- > 0)
		    xlputc(stream,fillchar);
    }
    if (colon) {
        char FAR *bufp = buf;
	    if (*bufp == '-' || *bufp == '+') xlputc(stream, *bufp++);
	    startcount = (strlen(bufp)-1)%commacount + 1;
	    while (*bufp != '\0') {
		    if (startcount-- == 0) {
			    xlputc(stream, commachar);
			    startcount = commacount-1;
		    }
		    xlputc(stream, *bufp++);
	    }
    } else {
	    xlputstr(stream,buf);
    }
}

#undef colinc

/* tabulate */
LOCAL VOID NEAR tab_print(stream, pp, atsign)
LVAL stream;
FIXTYPE *pp;
int atsign;
{
    int pos = xlgetcolumn(stream);  /* where are we now??? */
    int count;                      /* number of spaces to insert */
    int column = (int)pp[0];        /* desired column */
    int colinc = (int)pp[1];        /* desired column increment */

    if (column < 0) column = 1; /* handle defaults */
    if (colinc < 0) colinc = 1;

    if (atsign) { /* relative */
        if (colinc == 0) colinc = 1;
        count = column + (colinc - (pos + column) % colinc) % colinc;
    }
    else { /* absolute */
        if (pos >= column) {
            if (colinc > 0) {
                int k = (pos+ (colinc-1) - column)/colinc;
                count = column-pos + k*colinc;
                if (count==0) count = colinc;
            }
            else count = 0;
        }
        else count = column - pos;
    }
    while (count-- > 0)
        xlputc(stream, ' ');
}

LOCAL VOID NEAR indirect_print(stream, atsign)
LVAL stream;
int atsign;
{
    LVAL *oldargv, lfmt, args;
    int oldargc;

    lfmt = xlgastring();

    if (atsign) xlformat1(lfmt, stream); /* Use internal recursion */
    else {
        args = xlgalist();
        oldargv = xlargv;
        oldargc = xlargc;
        xlargv = xlsp;
        for (xlargc = 0; consp(args); args = cdr(args), xlargc++)
            pusharg(car(args));
        xlformat(lfmt, stream); /* start all over again since new argument list */
        xlargv = oldargv;
        xlargc = oldargc;
    }
}

/* adapted from changecase in xlstr.c */
LOCAL VOID NEAR case_convert_print(fmt, stream, colon, atsign)
char FAR *fmt;
LVAL stream;
int colon, atsign;
{
    LVAL tmp;
    LVAL lfmt;
    int ch, fcn;
    int lastspace = TRUE;

    xlstkcheck(2);
    xlsave(lfmt);
    xlsave(tmp);

    lfmt = cvstring(fmt);
    tmp = newustream();

    xlformat1(lfmt, tmp); /* Internal recursion */

    if (colon && atsign) fcn = 'U';
    else if (colon) fcn = 'C';
    else if (atsign) fcn = 'S';
    else fcn = 'D';

    while ((ch = xlgetc(tmp)) != EOF) {
        switch (fcn) {
            case 'U':   if (ISLOWER(ch)) ch = TOUPPER(ch); break;
            case 'D':   if (ISUPPER(ch)) ch = TOLOWER(ch); break;
            case 'C':   if (lastspace && ISLOWER(ch)) ch = TOUPPER(ch);
                        if (!lastspace && ISUPPER(ch)) ch = TOLOWER(ch);
                        lastspace = !ISLOWERA(ch) && !ISUPPER(ch);
                        break;
            case 'S':   if (lastspace && ISLOWER(ch)) ch = TOUPPER(ch);
                        if (!lastspace && ISUPPER(ch)) ch = TOLOWER(ch);
                        if (ISUPPER(ch)) lastspace = FALSE;
                        break;      
        }
        xlputc(stream, ch);
    }

    xlpopn(2);
}

LOCAL VOID NEAR conditional_print(fmt, stream, count, colon, atsign)
char FAR *fmt;
LVAL stream;
FIXTYPE count;
int colon, atsign;
{
    LVAL lfmt;
    char FAR *oldfmt;

    xlsave1(lfmt);

    lfmt = cvstring(fmt);

    if (atsign) {
        if (! null(xlgetarg())) {
            xlargv--;
            xlargc++;
            xlformat1(lfmt, stream); /* TAA 04/2011 Internal recursion */
        }
    }
    else if (colon) {
        if (! null(xlgetarg())) {
            fmt = skip_past_directive(fmt, ';', FALSE);
            if (fmt == NULL) xlerror("missing 'true' clause", lfmt);
            lfmt = cvstring(fmt);
        }
        xlformat1(lfmt, stream);  /* TAA 04/2011 Internal recursion */
    }
    else {
        if (count < 0) count = getfixnum(xlgafixnum());
        oldfmt = fmt;
        while (count-- > 0) {
            fmt = skip_past_directive(fmt, ';', FALSE);
            if (fmt == NULL) break;
        }
        if (fmt == NULL)
            fmt = skip_past_directive(oldfmt, ';', TRUE);
        if (fmt != NULL) {
            lfmt = cvstring(fmt);
            xlformat1(lfmt, stream); /* TAA 04/2011 Internal recursion */
        }
    }

    xlpop();
}
  
#define MAXNPP  7

LOCAL int abort_iteration = FALSE; /* This is kludgy */

LOCAL VOID NEAR iterative_print(fmt, stream, count, colon, atsign)
char FAR *fmt;
LVAL stream;
FIXTYPE count;
int colon, atsign;
{
    LVAL lfmt, args, alist;
    LVAL *oldargv, *oldsp;
    int oldargc, once;
    int npp;              /* number of prefix parameters */
    FIXTYPE pp[MAXNPP];   /* list of prefix parameters */
    int tcolon, tatsign;

    xlsave1(lfmt);

    abort_iteration = FALSE; /* TAA added April 2011 */
    lfmt = cvstring(fmt);
    once = (skip_past_directive(fmt, '}', TRUE) == NULL) ? FALSE : TRUE;
    if (*fmt == '~' &&
        *decode_pp(fmt + 1, pp, &npp, &tcolon, &tatsign, lfmt, FALSE) == '}')
        lfmt = xlgastring();
    if (! atsign) args = xlgetarg();

    if (! atsign || colon) {
        oldargv = xlargv;
        oldargc = xlargc;
        oldsp = xlsp;
        xlargv = xlsp;
        xlargc = 0;
    }

    if (colon) {
        if (atsign) {
            for (; (oldargc > 0 || once) && count != 0; oldargc--, count--) {
                once = FALSE;
                alist = *oldargv++;
                xlargc = 0;
                xlargv = oldsp;
                xlsp = oldsp;
                for (; consp(alist); alist = cdr(alist)) {
                    pusharg(car(alist));
                    xlargc++;
                }
                abort_iteration = oldargc > 1 ? -1 : 1;
                xlformat(lfmt, stream);  /* Full recursion */
            }
        }
        else {
            for (; (consp(args) || once) && count != 0; args = cdr(args), count--) {
                once = FALSE;
                alist = car(args);
                xlargc = 0;
                xlargv = oldsp;
                xlsp = oldsp;
                for (; consp(alist); alist = cdr(alist)) {
                    pusharg(car(alist));
                    xlargc++;
                }
                abort_iteration = consp(cdr(args)) ? -1 : 1;
                xlformat(lfmt, stream); /* Full recursion */
            }
        }
    }
    else {
        if (! atsign) {
            for (; consp(args); args = cdr(args)) {
                pusharg(car(args));
                xlargc++;
            }
        }
        while ((xlargc > 0 || once) && count-- != 0) {
            once = FALSE;
            if (--xlsample <= 0) {
                xlsample = SAMPLE;
                oscheck();
            }
	    /*    xlformat1(lfmt, stream);  *//* TAA 04/2011 internal  recursion */
            xlformat(lfmt, stream);  /* TAA 05/2011 full  recursion */
        }
    }

    if (! atsign || colon) {
        xlargv = oldargv;
        xlargc = oldargc;
        xlsp = oldsp;
    }
    xlpop();
}

LOCAL char FAR * NEAR skip_past_directive(fmt, tch, want_colon)
char FAR *fmt;
int tch;
int want_colon;
{
    int ch;
    int colon, atsign;  /* : and @ modifiers given? */
    int nesting = 0;

    /* process the format string */
    while ((ch = *fmt++) != 0)
        if (ch == '~') {
/* Fix from Luke Tierney, 1/97 */
            fmt = skip_pp( fmt, &colon, &atsign); 
            ch = *fmt++;
            if (! nesting && (! want_colon || colon) && ch == tch)
                return(fmt);
            switch (ch) {
                case '[':
                case '(':
                case '{':
                    nesting++;
                    break;
                case ']':
                case ')':
                case '}':
                    nesting--;
                    break;
            }
            if (nesting < 0) break;
        }
    return (NULL);
}

/* xlformat - formatted output function */
/* TAA MOD 6/22/93 -- split out from xformat so routine can
   be called internally by xerror() and xcerror() */
/* TAA MOD 4/1/2011 -- added xlformat1 for internal recursion
 * so that oldargc is retained */
LOCAL int oldargc;

VOID xlformat(lfmt, stream)
LVAL lfmt;
LVAL stream;
{
    int saved_oldargc = oldargc;
    oldargc = xlargc;
    xlformat1(lfmt, stream);
    oldargc = saved_oldargc;
}


VOID xlformat1(lfmt, stream)
  LVAL lfmt;
  LVAL stream;
{
    int ch;
    int npp;            /* number of prefix parameters */
    FIXTYPE pp[MAXNPP];     /* list of prefix parameters */
    int colon, atsign;  /* : and @ modifiers given? */
    char FAR *fmt = getstring(lfmt);
//  LVAL *oldargv;
//  int oldargc;

//  oldargv = xlargv;
//  oldargc = xlargc;

    /* process the format string */
    while ((ch = *fmt++) != 0)
        if (ch == '~') {
            fmt = decode_pp( fmt, pp, &npp, &colon, &atsign, lfmt, TRUE);
            ch = *fmt++;
            if (ISLOWER7(ch)) ch = toupper(ch);
            switch (ch) {
            case '\0':
                xlerror("expecting a format directive",cvstring(fmt-1));
            case 'A':
                opt_print(stream,xlgetarg(),FALSE,pp,colon,atsign);
                break;
            case 'S':
                opt_print(stream,xlgetarg(),TRUE,pp,colon,atsign);
                break;
#ifdef BIGNUMS
            case 'R':
                if (npp == 0) { /* Added 2011, TAA */
                    if (moreargs() && fixp(*xlargv)) {
                        string_print(stream, xlgetarg(), colon, atsign);
                    } else {
                            xlerror("~R requires a FIXNUM value", xlgetarg());
                    }
                    break;
                }
                if (npp > 5) toomanyopt(lfmt);
                num_print(stream,xlgetarg(),ch,pp,colon, atsign, TRUE);
                break;
            case 'B':
#endif
            case 'D': case 'O': case 'X':
                if (npp > 4) toomanyopt(lfmt);
                num_print(stream,xlgetarg(),ch,pp, colon, atsign, TRUE);
                break;
            case 'E': case 'F': case 'G':
                if (npp > 3) toomanyopt(lfmt);
                num_print(stream,xlgetarg(),ch,pp, colon, atsign, FALSE);
                break;
            case '&':
                if ( pp[0] < 0 ) pp[0] = 1;
                if ((pp[0])-- > 0)
                    xlfreshline(stream);
                while( (pp[0])-- > 0 )
                    xlterpri(stream);
                break;
            case '*':
                if (npp > 1) toomanyopt(lfmt);
                if (atsign) {
                    if (pp[0] < 0) pp[0] = 0;
                    if (pp[0] > oldargc) xltoofew();
                    xlargv = xlargv + (int)pp[0] - (oldargc-xlargc);
                    xlargc = oldargc - (int)pp[0];
//                    xlargv = oldargv + (int)pp[0];
                }
                else if (colon) {
                    if (pp[0] < 0) pp[0] = 1;
                    if (pp[0] > oldargc - xlargc) pp[0] = oldargc - xlargc;
                    xlargc += (int)pp[0];
                    xlargv -= (int)pp[0];
                }
                else {
                    if (pp[0] < 0) pp[0] = 1;
                    if (pp[0] > xlargc) xltoofew();
                    xlargc -= (int)pp[0];
                    xlargv += (int)pp[0];
                }
                break;
            case 'T':
                tab_print(stream,pp,atsign);
                break;
            case '%':
                if( pp[0] < 0 ) pp[0] = 1;
                while( (pp[0])-- > 0 )
                    xlterpri(stream);
                break;
            case '~':
                if( pp[0] <= 0 ) pp[0] = 1;
                while( (pp[0])-- > 0 )
                    xlputc(stream,'~');
                break;
            case '\n':
                if( colon )
                    break;
                if( atsign )
                     xlterpri(stream);
                while (*fmt && *fmt != '\n' && isspace(*fmt))
                    ++fmt;
                break;
            case '?':
                indirect_print(stream, atsign);
                break;
            case '|':
                if (pp[0] < 0) pp[0] = 1;
                while ((pp[0])-- > 0)
                    xlputc(stream, '\f');
                break;
            case '(':
                case_convert_print(fmt, stream, colon, atsign);
                fmt = skip_past_directive(fmt, ')', FALSE);
                if (fmt == NULL) xlerror("incomplete ~( directive", lfmt);
                break;
            case '[':
                conditional_print(fmt, stream, pp[0], colon, atsign);
                fmt = skip_past_directive(fmt, ']', FALSE);
                if (fmt == NULL) xlerror("incomplete ~[ directive", lfmt);
                break;
            case '{':
                iterative_print(fmt, stream, pp[0], colon, atsign);
                fmt = skip_past_directive(fmt, '}', FALSE);
                if (fmt == NULL) xlerror("incomplete ~{ directive", lfmt);
                break;
            case '^': /* Added TAA, April 2011 */
                if (npp == 0) { /* No argument version */
                    if (colon && abort_iteration) { /* special case if active */
                        if (abort_iteration > 0) {
                            return;
                        } else {
                            break;
                        }
                    }
                    if (xlargc > 0) break; /* more items remain, so continue */
                    return; /* return if nothing left */
                }
                else if (npp > 3) toomanyopt(lfmt);
                else { /* 1, 2, or 3 argument versions */
                    if ((npp==1 && pp[0]==0) ||
                          (npp == 2 && pp[0] == pp[1]) ||
                          (npp == 3 && pp[0] <= pp[1] && pp[1] <= pp[2]))
                        return;
                    break;
                }
                
            case ';':
            case ')':
            case ']':
            case '}':
                return;
            case 'P':   /* Added 2011 TAA */
            {
                if (colon) { /* back up one for colon argument */
                    if (oldargc == xlargc) xltoofew();
                    xlargc++;
                    xlargv--;
                }
                {
                    LVAL arg = xlgetarg(); // get argument
                    if (fixp(arg) && getfixnum(arg) == 1) { // 1 -- singular
                        if (atsign) {
                            xlputc(stream, 'y');
                        }
                    } else { // plural
                        if (atsign) {
                            xlputstr(stream, "ies");
                        } else {
                            xlputc(stream, 's');
                        }
                    }
                }
                break;
            }
            default:
                xlerror("unknown format directive",cvstring(fmt-1));
            }
        }
        else
            xlputc(stream,ch);
}


/* xformat - formatted output function */
LVAL xformat()
{
    LVAL stream,val;
    LVAL lfmt;

    xlsave1(val);                       /* TAA fix */

    /* get the stream and format string */
    stream = xlgetarg();
    if (null(stream)) {
        val = stream = newustream();
    }
    else {
        if (stream == s_true)
            stream = getvalue(s_stdout);
                                        /* fix from xlispbug.417 */
        else if (streamp(stream)) {     /* copied from xlgetfile() */
                if (getfile(stream) == CLOSED)
                        xlfail("file not open");
        }
        else if (!ustreamp(stream))
                xlbadtype(stream);
        val = NIL;
    }

    lfmt=xlgastring();

    abort_iteration = FALSE;

    /* go do it! */
    xlformat(lfmt, stream);

    /* get string if output to string */
    if (!null(val)) val = getstroutput(val);

    /* unprotect */
    xlpop();

    /* return the value */
    return val;
}


/* getstroutput - get the output stream string (internal) */
LVAL getstroutput(stream)
  LVAL stream;
{
    char FAR *str;
    LVAL next,val;
    unsigned len;           /* TAA MOD */
    int ch;

    /* compute the length of the stream */
    for (len = 0, next = gethead(stream); !null(next); next = cdr(next)) {
        if (++len > MAXSLEN) xltoolong();   /* TAA MOD addition for overflow detect */
    }

    /* create a new string */
    val = newstring(len);

    /* copy the characters into the new string */
    str = getstring(val);
    while ((ch = xlgetc(stream)) != EOF)
        *str++ = (char)ch;
    *str = '\0';

    /* return the string */
    return (val);
}

