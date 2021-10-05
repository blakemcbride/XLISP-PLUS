/* xlstr - xlisp string and character built-in functions */
/*      Copyright (c) 1985, by David Michael Betz
        All Rights Reserved
        Permission is granted for unrestricted non-commercial use       */

#include "xlisp.h"

/* local definitions */
#define fix(n)  cvfixnum((FIXTYPE)(n))
#define TLEFT   1
#define TRIGHT  2

/* Function prototypes */
LOCAL VOID NEAR getbounds _((LVAL str, LVAL skey, LVAL ekey, unsigned *pstart, unsigned *pend));
LOCAL LVAL NEAR strcompare _((int fcn, int icase));
LOCAL LVAL NEAR changecase _((int fcn, int destructive));
LOCAL int NEAR inbag _((int ch, LVAL bag));
LOCAL LVAL NEAR trim _((int fcn));
LOCAL LVAL NEAR chrcompare _((int fcn, int icase));


/* getbounds - get the start and end bounds of a string */
LOCAL VOID NEAR getbounds(str,skey,ekey,pstart,pend)
  LVAL str,skey,ekey; unsigned *pstart,*pend;
{
    LVAL arg;
    unsigned len;
    FIXTYPE n;

    /* get the length of the string */
    len = getslength(str);

    /* get the starting index */
    if (xlgkfixnum(skey,&arg)) {
        *pstart = (unsigned) (n = getfixnum(arg));
        if (n < 0 || n > (FIXTYPE)len)
            xlerror("string index out of bounds",arg);
    }
    else
        *pstart = 0;

    /* get the ending index */
    if (xlgetkeyarg(ekey, &arg) && arg != NIL) {
        if (!fixp(arg)) xlbadtype(arg);
        *pend = (unsigned)(n = getfixnum(arg));
        if (n < 0 || n > (FIXTYPE)len)
            xlerror("string index out of bounds",arg);
    }
    else
        *pend = len;

    /* make sure the start is less than or equal to the end */
    if (*pstart > *pend)
        xlerror("starting index error",cvfixnum((FIXTYPE)*pstart));
}

/* strcompare - compare strings */
LOCAL LVAL NEAR strcompare(fcn,icase)
  int fcn,icase;
{
    unsigned start1,end1,start2,end2;
    int ch1,ch2;
    unsigned char FAR *p1, FAR *p2;
    LVAL str1,str2;

    /* get the strings */
    str1 = xlgastrorsym();
    str2 = xlgastrorsym();

    /* get the substring specifiers */
    getbounds(str1,k_1start,k_1end,&start1,&end1);
    getbounds(str2,k_2start,k_2end,&start2,&end2);

    xllastkey();

    /* setup the string pointers */
    p1 = (unsigned char FAR *) &getstring(str1)[start1];
    p2 = (unsigned char FAR *) &getstring(str2)[start2];

    /* compare the strings */
    for (; start1 < end1 && start2 < end2; ++start1,++start2) {
        ch1 = *p1++;
        ch2 = *p2++;
        if (icase) {
            if (ISUPPER(ch1)) ch1 = TOLOWER(ch1);
            if (ISUPPER(ch2)) ch2 = TOLOWER(ch2);
        }
        if (ch1 != ch2)
            switch (fcn) {
            case '<':   return (ch1 < ch2 ? fix(start1) : NIL);
            case 'L':   return (ch1 <= ch2 ? fix(start1) : NIL);
            case '=':   return (NIL);
            case '#':   return (fix(start1));
            case 'G':   return (ch1 >= ch2 ? fix(start1) : NIL);
            case '>':   return (ch1 > ch2 ? fix(start1) : NIL);
            }
    }

    /* check the termination condition */
    switch (fcn) {
    case '<':   return (start1 >= end1 && start2 < end2 ? fix(start1) : NIL);
    case 'L':   return (start1 >= end1 ? fix(start1) : NIL);
    case '=':   return (start1 >= end1 && start2 >= end2 ? s_true : NIL);
    case '#':   return (start1 >= end1 && start2 >= end2 ? NIL : fix(start1));
    case 'G':   return (start2 >= end2 ? fix(start1) : NIL);
    case '>':   return (start2 >= end2 && start1 < end1 ? fix(start1) : NIL);
    }
    return (NIL);   /* avoid compiler warning */
}

/* string comparision functions */
LVAL xstrlss() { return (strcompare('<',FALSE)); } /* string< */
LVAL xstrleq() { return (strcompare('L',FALSE)); } /* string<= */
LVAL xstreql() { return (strcompare('=',FALSE)); } /* string= */
LVAL xstrneq() { return (strcompare('#',FALSE)); } /* string/= */
LVAL xstrgeq() { return (strcompare('G',FALSE)); } /* string>= */
LVAL xstrgtr() { return (strcompare('>',FALSE)); } /* string> */

/* string comparison functions (not case sensitive) */
LVAL xstrilss() { return (strcompare('<',TRUE)); } /* string-lessp */
LVAL xstrileq() { return (strcompare('L',TRUE)); } /* string-not-greaterp */
LVAL xstrieql() { return (strcompare('=',TRUE)); } /* string-equal */
LVAL xstrineq() { return (strcompare('#',TRUE)); } /* string-not-equal */
LVAL xstrigeq() { return (strcompare('G',TRUE)); } /* string-not-lessp */
LVAL xstrigtr() { return (strcompare('>',TRUE)); } /* string-greaterp */

/* changecase - change case */
LOCAL LVAL NEAR changecase(fcn,destructive)
  int fcn,destructive;
{
    char FAR *srcp, FAR *dstp;
    unsigned start,end,len,i;
    int ch;
    int lastspace = TRUE;
    LVAL src,dst;

    /* get the string */
    src = (destructive? xlgastring() : xlgastrorsym());

    /* get the substring specifiers */
    getbounds(src,k_start,k_end,&start,&end);
    len = getslength(src);

    xllastkey();

    /* make a destination string */
    dst = (destructive ? src : newstring(len));

    /* setup the string pointers */
    srcp = getstring(src);
    dstp = getstring(dst);

    /* copy the source to the destination */
    for (i = 0; i < len; ++i) {
        ch = *srcp++;
        if (i >= start && i < end) 
            switch (fcn) {
            case 'U':   if (ISLOWER(ch)) ch = TOUPPER(ch); break;
            case 'D':   if (ISUPPER(ch)) ch = TOLOWER(ch); break;
            case 'C':   if (lastspace && ISLOWER(ch)) ch = TOUPPER(ch);
                        if (!lastspace && ISUPPER(ch)) ch = TOLOWER(ch);
                        lastspace = !ISLOWERA(ch) && !ISUPPER(ch);
                        break;
            }
        *dstp++ = (char) ch;
    }
    *dstp = '\0';

    /* return the new string */
    return (dst);
}

/* case conversion functions */
LVAL xupcase()   { return (changecase('U',FALSE)); }
LVAL xdowncase() { return (changecase('D',FALSE)); }
LVAL xcapcase()  { return (changecase('C',FALSE)); }

/* destructive case conversion functions */
LVAL xnupcase()   { return (changecase('U',TRUE)); }
LVAL xndowncase() { return (changecase('D',TRUE)); }
LVAL xncapcase()  { return (changecase('C',TRUE)); }

/* inbag - test if a character is in a bag */
LOCAL int NEAR inbag(ch,bag)
  int ch; LVAL bag;
{
                                    /* TAA MOD -- rewritten for \0 */
                                    /*            and chars >= 128 */
    char FAR *p = getstring(bag);
    unsigned len =getslength(bag);

    while (len--)
        if (*p++ == ch)
            return (TRUE);
    return (FALSE);
}

/* trim - trim character from a string */
LOCAL LVAL NEAR trim(fcn)
  int fcn;
{
    char FAR *leftp, FAR *rightp, FAR *dstp;
    LVAL bag,src,dst;

    /* get the bag and the string */
    bag = xlgastrorsym();
    src = xlgastrorsym();
    xllastarg();

    /* setup the string pointers */
    leftp = getstring(src);
    rightp = leftp + getslength(src) - 1;

    /* trim leading characters */
    if (fcn & TLEFT)
        while (leftp <= rightp && inbag(*leftp,bag))
            ++leftp;

    /* trim character from the right */
    if (fcn & TRIGHT)
        while (rightp >= leftp && inbag(*rightp,bag))
            --rightp;

    /* make a destination string and setup the pointer */
    dst = newstring((unsigned)(rightp-leftp)+1);
    dstp = getstring(dst);

    /* copy the source to the destination */
    while (leftp <= rightp)
        *dstp++ = *leftp++;
    *dstp = '\0';

    /* return the new string */
    return (dst);
}

/* trim functions */
LVAL xtrim()      { return (trim(TLEFT|TRIGHT)); }
LVAL xlefttrim()  { return (trim(TLEFT)); }
LVAL xrighttrim() { return (trim(TRIGHT)); }


/* xstring - return a string consisting of a single character */
LVAL xstring()
{
    LVAL arg,val;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

    /* check the argument type */
    switch (ntype(arg)) {
    case STRING:
        return (arg);
    case SYMBOL:
        return (getpname(arg));
    case CHAR:
        /* Changed 10/94 to allow string '\000' */
        val = newstring(1);
        val->n_string[0] = (char)getchcode(arg);
        val->n_string[1] = '\0';
        return (val);
    case FIXNUM:
        /* Changed 10/94 to allow string 0 */
        val = newstring(1);
        val->n_string[0] = (char)getfixnum(arg);
        val->n_string[1] = '\0';
        return (val);
    default:
        xlbadtype(arg);
        return (NIL);   /* avoid compiler warning */
    }
}

/* xchar - extract a character from a string */
LVAL xchar()
{
    LVAL str,num;
    FIXTYPE n;

    /* get the string and the index */
    str = xlgastring();
    num = xlgafixnum();
    xllastarg();

    /* range check the index */
    if ((n = getfixnum(num)) < 0 || n >= (FIXTYPE)getslength(str))
        xlerror("index out of range",num);

    /* return the character */
    return (cvchar(getstringch(str,(unsigned int)n)));
}

/* xcharint - convert a character to an integer */
LVAL xcharint()
{
    LVAL arg;
    arg = xlgachar();
    xllastarg();
    return (cvfixnum((FIXTYPE)getchcode(arg)));
}

/* xintchar - convert an integer to a character */
LVAL xintchar()
{
    LVAL arg;
    arg = xlgafixnum();
    xllastarg();
    return (cvchar((int)getfixnum(arg)));
}

/* xcharcode - built-in function 'char-code' */
/* TAA mod so that result is 7 bit ascii code */
LVAL xcharcode()
{
    int ch;
    ch = 0x7f  & getchcode(xlgachar());
    xllastarg();
    return (cvfixnum((FIXTYPE)ch));
}

/* xcodechar - built-in function 'code-char' */
/* like int-char except range must be 0-127 */
LVAL xcodechar()
{
    LVAL arg;
    FIXTYPE ch;
#ifdef __SASC__
    FIXTYPE testch;
#endif

    arg = xlgafixnum(); ch = getfixnum(arg);
    xllastarg();
    
#ifdef __SASC__
    /* On MVS/CMS, convert EBCDIC character to ASCII for subsequent */
    /* test - Dave Rivers (rivers@ponds.uucp) */
    testch = etoa((unsigned char)ch);
    return (testch >= 0 && testch <= 127 ? cvchar((int)ch) : NIL);
#else   
    return (ch >= 0 && ch <= 127 ? cvchar((int)ch) : NIL);
#endif
}

/* xuppercasep - built-in function 'upper-case-p' */
LVAL xuppercasep()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISUPPER(ch) ? s_true : NIL);
}

/* xlowercasep - built-in function 'lower-case-p' */
LVAL xlowercasep()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISLOWERA(ch) ? s_true : NIL);
}

/* xbothcasep - built-in function 'both-case-p' */
LVAL xbothcasep()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISUPPER(ch) || ISLOWER(ch) ? s_true : NIL);
}

/* xdigitp - built-in function 'digit-char-p' */
LVAL xdigitp()
{
    int ch;
    FIXTYPE radix=10;
    ch = getchcode(xlgachar());
    if (moreargs()) {
        radix = getfixnum(xlgafixnum());
        if (radix < 1 || radix > 36) xlfail("radix out of range");
    }
    xllastarg();

    if (isdigit(ch)) ch = ch - '0';
    else if (ISUPPER(ch)) ch = ch - 'A' + 10;
    else if (ISLOWER(ch)) ch = ch - 'a' + 10;
    else return NIL;

    return (ch < radix ? cvfixnum((FIXTYPE)ch) : NIL);
}

/* xchupcase - built-in function 'char-upcase' */
LVAL xchupcase()
{
    LVAL arg;
    int ch;
    arg = xlgachar(); ch = getchcode(arg);
    xllastarg();
    return (ISLOWER(ch) ? cvchar(TOUPPER(ch)) : arg);
}

/* xchdowncase - built-in function 'char-downcase' */
LVAL xchdowncase()
{
    LVAL arg;
    int ch;
    arg = xlgachar(); ch = getchcode(arg);
    xllastarg();
    return (ISUPPER(ch) ? cvchar(TOLOWER(ch)) : arg);
}

/* xdigitchar - built-in function 'digit-char' */
LVAL xdigitchar()
{
    FIXTYPE n, radix=10;
    n = getfixnum(xlgafixnum());
    if (moreargs()) {
        radix = getfixnum(xlgafixnum());
        if (radix < 1 || radix > 36) xlfail("radix out of range");
    }
    xllastarg();
    return (n >= 0 && n < radix ? cvchar((int)n + (n < 10 ? '0' : 'A'-10 ) )
                                : NIL);
}

/* xalphanumericp - built-in function 'alphanumericp' */
LVAL xalphanumericp()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISUPPER(ch) || ISLOWERA(ch) || isdigit(ch) ? s_true : NIL);
}

/* xalphacharp - built-in function 'alpha-char-p' */
LVAL xalphacharp()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (ISUPPER(ch) || ISLOWERA(ch) ? s_true : NIL);
}

/* chrcompare - compare characters */
LOCAL LVAL NEAR chrcompare(fcn,icase)
  int fcn,icase;
{
    int ch1,ch2,icmp;
    LVAL arg;

    /* get the characters */
    arg = xlgachar(); ch1 = getchcode(arg);

    /* convert to lowercase if case insensitive */
    if (icase && ISUPPER(ch1))
        ch1 = TOLOWER(ch1);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && moreargs(); ch1 = ch2) {

        /* get the next argument */
        arg = xlgachar(); ch2 = getchcode(arg);

        /* convert to lowercase if case insensitive */
        if (icase && ISUPPER(ch2))
            ch2 = TOLOWER(ch2);

        /* compare the characters */
        switch (fcn) {
        case '<':       icmp = (ch1 < ch2); break;
        case 'L':       icmp = (ch1 <= ch2); break;
        case '=':       icmp = (ch1 == ch2); break;
        case '#':       icmp = (ch1 != ch2); break;
        case 'G':       icmp = (ch1 >= ch2); break;
        case '>':       icmp = (ch1 > ch2); break;
        }
    }

    /* return the result */
    return (icmp ? s_true : NIL);
}

/* character comparision functions */
LVAL xchrlss() { return (chrcompare('<',FALSE)); } /* char< */
LVAL xchrleq() { return (chrcompare('L',FALSE)); } /* char<= */
LVAL xchreql() { return (chrcompare('=',FALSE)); } /* char= */
LVAL xchrneq() { return (chrcompare('#',FALSE)); } /* char/= */
LVAL xchrgeq() { return (chrcompare('G',FALSE)); } /* char>= */
LVAL xchrgtr() { return (chrcompare('>',FALSE)); } /* char> */

/* character comparision functions (case insensitive) */
LVAL xchrilss() { return (chrcompare('<',TRUE)); } /* char-lessp */
LVAL xchrileq() { return (chrcompare('L',TRUE)); } /* char-not-greaterp */
LVAL xchrieql() { return (chrcompare('=',TRUE)); } /* char-equalp */
LVAL xchrineq() { return (chrcompare('#',TRUE)); } /* char-not-equalp */
LVAL xchrigeq() { return (chrcompare('G',TRUE)); } /* char-not-lessp */
LVAL xchrigtr() { return (chrcompare('>',TRUE)); } /* char-greaterp */

