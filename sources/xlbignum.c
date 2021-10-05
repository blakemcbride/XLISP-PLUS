/* Multiple precision integer arithmetic for implementing BIGNUMS
   in XLISP-PLUS.  Writen by Tom Almy and placed in public domain.
   Math algorithms for multiply and divide are based on those in Donald
   Knuth's "Art of Computer Programming Volume 2: Seminumerical Algorithms". */

/* Numbers stored in arrays of unsigned shorts, in a S/M "big endian fashion"--
   array[0] = sign (0 positive, 1 negative)
   array[1] = most significant word
   array[2] = next word...
   array[N] = least significant word

   The code insures that all generated values have no more than one
   significant word =0, except that there is always at least two words.
   Even with the signed magnitude organization, negative zero cannot be
   generated.

   The code assumes that a short is 16 bits and a long is 32 bits. IEEE
   floating point and little-endian orgainzation assumed for the conversions
   to/from floating point -- these would have to be recoded for other
   environments.
*/

/* TAA MOD -- replaced 3 occurances of log(radix) with log((double)radix)
 * for non-ANSI compilers 3/98 */

#include "xlisp.h"
#include <string.h>
#include <math.h>
#include <ctype.h>
#ifdef BIGNUMS

/* local declarations */
typedef BIGNUMDATA (*LOGFUNCTION) _((BIGNUMDATA, BIGNUMDATA));
LOCAL VOID memallocfail _((void));
LOCAL double setmant _((LVAL x, int *expo));
LOCAL LVAL logbignum _((LVAL ux, LVAL vx, LOGFUNCTION logfcn));
LOCAL BIGNUMDATA andfcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA iorfcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA xorfcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA eqvfcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA nandfcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA andc1fcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA andc2fcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA norfcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA orc1fcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL BIGNUMDATA orc2fcn _((BIGNUMDATA x, BIGNUMDATA y));
LOCAL LVAL NEAR logbinary _((int which));
LOCAL LVAL NEAR logbinary2 _((int which));
LOCAL LVAL ashbignum _((LVAL ux, FIXTYPE count));


LOCAL VOID memallocfail()
{
    xlfail("memory allocation failure in bignum calculations");
}

LVAL copybignum(x,s)
LVAL x;
int s;
{
    /* returns a copy of bignum x with sign s */
    LVAL b = newbignum(getbignumsize(x));
    MEMCPY(getbignumarray(b), getbignumarray(x),
           (getbignumsize(x)+1)*sizeof(BIGNUMDATA));
    *getbignumarray(b) = (BIGNUMDATA) s;
    return b;
}

int zeropbignum(x)
LVAL x;
{
    /* is the bignum zero? */
    BIGNUMDATA FAR *xd = getbignumarray(x);
    return (getbignumsize(x)==2 && xd[1] == 0 && xd[2] == 0);
}


LVAL normalBignum(x)
LVAL x;
{
    /* fixes bignums with too many leading 0's */
    LVAL newobj;
    BIGNUMDATA FAR *nd;
    BIGNUMDATA FAR *xd = getbignumarray(x);
    int signx = *xd++;
    int sizex = (int) getbignumsize(x);

    while (sizex > 2 && *xd == 0) {
        sizex--;
        xd++;
    }
    if (sizex == 2 && xd[0] == 0 && xd[1] == 0 && n_bigzero != 0)
        return n_bigzero;   /* actual value is zero */
    xlprot1(x);
    newobj = newbignum(sizex);
    nd = getbignumarray(newobj);
    *nd++ = (BIGNUMDATA) signx;
    MEMCPY(nd, xd, sizex*sizeof(short));
    xlpop();
    return newobj;
}

int comparebignum(x, y)
LVAL x,y;
{
    /* return -1 if x<y, 1 if x>y, else return 0 */
    BIGNUMDATA FAR *xd = getbignumarray(x);
    BIGNUMDATA FAR *yd = getbignumarray(y);
    int signx = *xd++;
    int signy = *yd++;

    if (signx != signy) return (signx? -1 : 1); /* quick exit */
    {
        int sizex = (int) getbignumsize(x);
        int sizey = (int) getbignumsize(y);
        int i;
        if (xd[0] == 0) { xd++; sizex--; };
        if (yd[0] == 0) { yd++; sizey--; };

        if (sizex != sizey) return ((sizex<sizey)^signx) ? -1 : 1;
        for (i=0; i < sizex; i++) {
            if (xd[i]!=yd[i]) return ((xd[i]<yd[i])^signx ? -1 : 1);
        }
        return 0;
    }
}

/* Code from Luke Tierney */
#ifndef FIXBIGDIGS
#define FIXBIGDIGS (sizeof(long)/sizeof(short)) /* = 2 for 32-bit long's */
#endif
#ifndef FLOATBIGDIGS
#define FLOATBIGDIGS 4
#endif

LVAL cvtfixbignum(n)
FIXTYPE n;
{
    /* TAA mod -- fixed so that abs(n) is first argument to convert */
    return (n<0 ? cvtulongbignum(-n, 1) : cvtulongbignum(n, 0));
}

LVAL cvtulongbignum(n, sign)
unsigned long n; int sign;
{
  LVAL x = newbignum(FIXBIGDIGS);
  BIGNUMDATA FAR *v = getbignumarray(x);
  int i;

  *v++ = (BIGNUMDATA) sign;
  v += FIXBIGDIGS - 1;  /* advance to point to least significant digit */
  for (i = 0; i < (int)FIXBIGDIGS; i++, n >>= 16)
    *v-- = (BIGNUMDATA)(n & 0xffff);
  return (FIXBIGDIGS > 2) ? normalBignum(x) : x;
}

LOCAL double setmant(x,expo)
LVAL x; int *expo;
{
  /* returns proper mantissa value for x, but with exponent in expo */
  BIGNUMDATA FAR *v = getbignumarray(x);
  int sign = *v++, i;
  double temp;
  int size = (int) getbignumsize(x);
    
  if (v[0] == 0) { v++; size--; };

  if (size == 1 && v[0] == 0) { *expo = 0; return 0.0; }

  for (i = 0, temp = 0.0; size > 0 && i < FLOATBIGDIGS; i++, v++, size--)
    temp = LDEXP(temp, 16) + v[0];
  temp = frexp(temp, expo);
  if (size > 0) *expo += size * 16;
  return (sign?-(double)temp : (double)temp);
}


FLOTYPE cvtbigratioflonum(num, denom)
LVAL num, denom;
{
    int expo1, expo2;
    double f;

    f = setmant(num, &expo1)/setmant(denom, &expo2);

    return LDEXP(f, expo1-expo2);
}

FLOTYPE cvtratioflonum(ratio)
LVAL ratio;
{
    if (fixp(getnumer(ratio)) && fixp(getdenom(ratio)))
            return getfixnum(getnumer(ratio))/
                    (FLOTYPE)getfixnum(getdenom(ratio));
    else {
        LVAL t1, t2;
        xlstkcheck(2);
        xlprotect(ratio);
        xlprotect(t1);
        if (fixp(t1 = getnumer(ratio))) t1 = cvtfixbignum(getfixnum(t1));
        if (fixp(t2 = getdenom(ratio))) t2 = cvtfixbignum(getfixnum(t2));
        xlpopn(2);
        return cvtbigratioflonum(t1, t2);
    }
}

FLOTYPE cvtbigflonum(x)
LVAL x;
{
    int expo;
    double f;

    f = setmant(x, &expo);

    if (f == 0.0) return f;

    return LDEXP(f, expo);
}


LVAL cvtflobignum(n)
FLOTYPE n;
{
  LVAL x;
  BIGNUMDATA FAR *v;
  int expo, size, shift;
  double mantissa;

#ifdef IEEEFP
  if (! is_finite(n))
    xlfail("Can't convert NaN or infinity to integer");
#endif

  mantissa = frexp(n, &expo);
  if (expo <= 31) return cvtfixbignum((long)n);
  if (mantissa < 0) mantissa = -mantissa;
  size = (expo + 15) / 16;      /* must be at least 2 */
  shift = 16 - (expo & 15);
  x = newbignum(size);
  v = getbignumarray(x);

  /* set sign */
  *v++ = (BIGNUMDATA) (n<0);

  /* adjust the exponent */
  if (shift < 16)
    mantissa = ldexp(mantissa, -shift);

  /* compute bignum digits */
  for (; size > 0 && mantissa != 0.0; size--, v++) {
    mantissa = ldexp(mantissa, 16);
    *v = (BIGNUMDATA) mantissa;
    mantissa -= *v;
  }
  return x;
}

#ifdef GENERIC  
int cvtbigfixnum(x, n)
LVAL x; FIXTYPE *n;
{
  /* returns a success flag */
  BIGNUMDATA FAR *v = getbignumarray(x);
  int sign = *v++;
  int size = (int) getbignumsize(x);
  FIXTYPE value;
  unsigned long uvalue;

  /* toss leading zero */
  if (*v == 0 && size > 2) {
    v++;
    size--;
  }

  /* is it too big?? */
  if (size > FIXBIGDIGS) return FALSE;

  for (uvalue = 0; size > 0; size--, v++)
    uvalue = (uvalue << 16) + v[0];
  value = (long) uvalue;
  if (sign) {
    value = -value;
    if (value > 0) return FALSE; /* negative value too big */
  }
  else if (value < 0) return FALSE; /* positive value too big */
  *n = value;
  return TRUE;
}
#else
int cvtbigfixnum(x, n)
LVAL x; FIXTYPE *n;
{
    /* returns a success flag */
    BIGNUMDATA FAR *v = getbignumarray(x);
    int sign = *v++;
    int size = (int) getbignumsize(x);
    FIXTYPE value;
    /* toss leading zeroes */
    if (*v == 0 && size > 2) {
        v++;
        size--;
    }
    /* is it too big?? */
    if (size > (int)FIXBIGDIGS) return FALSE;

	for (value = 0; size > 0; size--, v++)
		value = (value << 16) + v[0];
//    value = ((long)(v[0]) << 16) + v[1];
    if (sign) {
        value = -value;
        if (value > 0) return FALSE; /* negative value too big */
    }
    else if (value < 0) return FALSE; /* positive value too big */
    *n = value;
    return TRUE;
}
#endif

LVAL cvtstrbignum(s, radix)
char FAR *s; int radix;
{
    LVAL x;
    BIGNUMDATA FAR *v;
    int sign=0, size, i;
    unsigned int carry;
    unsigned long temp;
    
    if (*s == '\0') return n_bigzero;
    if (*s == '-') {
        sign = 1;
        s++;
    }
    else if (*s == '+') s++;

    while (*s == '0') s++;  /* delete leading zeroes */

    size = (int)(STRLEN(s)*log((double)radix)/log(65536.0)) + 1;
    if (size==1) size = 2;
    x = newbignum(size);
    v = getbignumarray(x);
    *v++ = (BIGNUMDATA) sign;
    size--;
    while((i=*s++)!=0) {
        if (ISLOWER7(i)) i = toupper(i);
        temp = (unsigned long)v[size] * radix +
               (i > '9'? i - 'A' + 10 : i - '0');
        carry = (unsigned int)(temp >> 16);
        v[size] = (BIGNUMDATA) (temp & 0xffff);
        for (i = size-1; i >= 0; i--) {
            temp = (unsigned long)v[i]*radix + carry;
            carry = (unsigned int)(temp >> 16);
            v[i] = (BIGNUMDATA) (temp & 0xffff);
        }
    }
    return x;
}
        

char FAR *cvtbignumstr(x, radix)
LVAL x; int radix;
{
    /* returns MALLOCed string which must be freed by calling program */
    /* radix must be in range 2-36 */
    BIGNUMDATA FAR *vx = getbignumarray(x);
    BIGNUMDATA FAR *v, FAR *vv;
    int zcheck; /* check for zero */
    int i;
    int sign = *vx++;
    int size = (int) getbignumsize(x);
    long strsize = (long)(size*log(65536.0)/log((double)radix)) + 3;
    int digits = (int)(log(65536.0)/log((double)radix)); /* digits per BIGNUMDATA */
    BIGNUMDATA bradix = (BIGNUMDATA)(pow(radix, digits)+0.5); /* radix to use */
    char FAR *str, FAR *cp;

    if (strsize > MAXSLEN) xlfail("bignum too large to convert to string");
    str = (char FAR *) MALLOC(strsize);
    
    if (str == NULL) memallocfail();
    cp = str+(int)strsize;  /* point to string end */

    if ((vv = v = (BIGNUMDATA FAR *)MALLOC(size*sizeof(short))) == NULL) {
        /* out of memory */
        MFREE(str);
        memallocfail();
    }
    MEMCPY(v, vx, size*sizeof(short));

    *--cp = '\0';

    do {
        unsigned long d;
        BIGNUMDATA d2,q,r=0;

        zcheck = -1;
        if (bradix == 0) { /* base 2, 4 or 16, bradix axtually 0x10000 */
            for (i = 0; i < size; i++) {
                q = r;
                r = v[i];
                v[i] = q;
                /* find first nonzero value */
                if (zcheck==-1 && q != 0) zcheck = i;
            }
        }
        else {
            for (i = 0; i < size; i++) {
                d = v[i] + ((unsigned long)r << 16); /* dividend */
                q = (BIGNUMDATA) (d/bradix);
                r = (BIGNUMDATA) (d%bradix);
                /* set quotient and find first nonzero value */
                v[i] = q;
                if (zcheck==-1 && q != 0) zcheck = i;
            }
        }
        d2 = r;
        for (i = digits; i>0; i--) {
            if (d2==0 && zcheck < 0) break; /* no leading zeroes */
            r = (BIGNUMDATA) (d2 % radix);
            d2 = (BIGNUMDATA) (d2 / radix);
            *--cp = (char)(r<10 ? r + '0' : r - 10 + 'A');
        }
        if (zcheck>0) { /* reduce dividend size if possible */
            v += zcheck;
            size -= zcheck;
        }
    } while (zcheck >= 0);

    if (sign) *--cp = '-';

    if (cp > str) STRCPY(str, cp);  /* Delete extra space */

    MFREE(vv);
    return str;
}


LVAL ashbignum(ux,count)
LVAL ux; FIXTYPE count;
{
    BIGNUMDATA FAR *u = getbignumarray(ux);
    int signu = *u++;
    int usize = (int) getbignumsize(ux);
    LVAL wx;
    BIGNUMDATA FAR *w;
    BIGNUMDATA lowdata;
    int wsize;
    int fullshift, bitshift;
    int i,adj=0;
    unsigned int carry;
    unsigned long k;

    if (count==0) return ux;    /* no shift */

    if (u[0] == 0) {u++; usize--;}

    if (count>0) {
        fullshift = (int)(count/16);
        bitshift = (int)(count % 16);
        k = usize + count/16 + (bitshift?1:0);
        if (k > MAXVLEN * 2) xlfail("shift too large");
        wsize = (int)k;
    }
    else {
        fullshift = (int)((-count)/16);
        bitshift = (int)((-count) % 16);
        k = usize + count/16 + (signu ? 1 : 0);
        if (k<=0) { /* shift so big that it won't fit */
            return (signu ? n_bigmone : n_bigzero);
        }
        wsize = (int)k;
        if (wsize<2) {wsize=2; adj=1;}
    }
    wx = newbignum(wsize);
    w = getbignumarray(wx);
    *w++ = (BIGNUMDATA) signu;

    if (count>0 && bitshift) {
        w[0] = (BIGNUMDATA) (u[0] >> (16-bitshift));
        for (i=1; i<usize; i++)
            w[i] = (BIGNUMDATA)((u[i-1] << bitshift) + (u[i] >> (16-bitshift)));
        w[usize] = (BIGNUMDATA) (u[usize-1] << bitshift);
    }
    else if (count > 0) { /* no bitshift */
        MEMCPY(w, u, usize*sizeof(BIGNUMDATA));
    }
    else if (bitshift) { /* right shift with bitshift */
        if (signu) { /* negative value being shifted */
            lowdata = 0; /* if we "shift out" nonzero, need to subtract one
                                 from the result */
            for (i=usize-fullshift; i < usize; i++) lowdata |= u[i];
            lowdata |= (BIGNUMDATA) (u[usize-fullshift-1] << (16-bitshift));
            w[1] = (BIGNUMDATA) (u[0] >> bitshift);
            for (i=2; i<wsize; i++)
                w[i]=(BIGNUMDATA)((u[i-2] << (16-bitshift)) + (u[i-1] >> bitshift));
            if (lowdata) {
                carry = 1;
                for (i=wsize-1; i>=0; i--) {
                    k = w[i] + carry;
                    carry = k > 0xffff;
                    w[i] = (BIGNUMDATA) k;
                }
            }
            /* we might need to normalize the bignum */
            if (wsize > 2 && w[0] == 0 && w[1] == 0)
                wx = normalBignum(wx);
        }
        else {
            w[adj] = (BIGNUMDATA) (u[0] >> bitshift);
            for (i=1; i<wsize; i++)
                w[i+adj] = (BIGNUMDATA)((u[i-1]<<(16-bitshift)) + (u[i]>>bitshift));
        }
    }
    else { /* right shift without bitshift */
        if (signu) { /* negative value being shifted */
            lowdata = 0; /* if we shift out nonzero, then we need to subtract
                            one from the result */
            for (i=usize-fullshift; i < usize; i++) lowdata |= u[i];
            MEMCPY(w+1,u,(wsize-1)*sizeof(BIGNUMDATA));
            if (lowdata) {
                carry = 1;
                for (i=wsize-1; i>=0; i--) {
                    k = w[i] + carry;
                    carry = k > 0xffff;
                    w[i] = (BIGNUMDATA) k;
                }
            }
            /* we might need to normalize the bignum */
            if (wsize > 2 && w[0] == 0 && w[1] == 0)
                wx = normalBignum(wx);
        }
        else MEMCPY(w+adj,u,wsize*sizeof(BIGNUMDATA));
    }
    return wx;
}
    
/* for this operation we must fake two's complement representation */
LOCAL LVAL logbignum(ux, vx, logfcn)
LVAL ux, vx;
LOGFUNCTION logfcn;
{
    BIGNUMDATA FAR *u = getbignumarray(ux);
    BIGNUMDATA FAR *v = getbignumarray(vx);
    BIGNUMDATA signu = (BIGNUMDATA)(*u++ ? 0xffff : 0);
    BIGNUMDATA signv = (BIGNUMDATA)(*v++ ? 0xffff : 0);
    int usize = (int) getbignumsize(ux);
    int vsize = (int) getbignumsize(vx);
    int wsize;
    LVAL wx;
    BIGNUMDATA FAR *w;
    /* sign (leading 1's) of result depend on logical function on
       signs of operands */
    BIGNUMDATA signw = logfcn(signu, signv);
    int i, j, k, c1=1, c2=1, c3=1;
    BIGNUMDATA n1, n2, n3;
    unsigned long sum;

    if (u[0] == 0) { u++; usize--; };
    if (v[0] == 0) { v++; vsize--; };

    wsize = (usize>vsize) ? usize : vsize;
    if (signw) wsize++; /* may need 1 more for 2's complement */
    if (wsize<2) wsize=2;
    wx = newbignum(wsize);
    w = getbignumarray(wx);
    *w++ = (BIGNUMDATA) (signw != 0);

    for (i=wsize-1, j=usize-1, k=vsize-1; i>=0; i--, j--, k--) {
        if (j<0) n1 = signu;
        else if (signu) { /* use 2's complement value */
            sum = (unsigned long)(BIGNUMDATA)(~u[j]) + c1;
            c1 = (int)(sum>>16);
            n1 = (BIGNUMDATA)sum;
        }
        else n1 = u[j];
        if (k<0) n2 = signv;
        else if (signv) { /* use 2's complement value */
            sum = (unsigned long)(BIGNUMDATA)(~v[k]) + c2;
            c2 = (int)(sum>>16);
            n2 = (BIGNUMDATA)sum;
        }
        else n2 = v[k];
        n3 = logfcn(n1, n2);
        if (signw) { /* use 2's complement value */
            sum = (unsigned long)(BIGNUMDATA)(~n3) + c3;
            c3 = (int)(sum>>16);
            w[i] = (BIGNUMDATA) sum;
        }
        else w[i] = n3;
    }

    if (w[0]==0 && w[1]==0) {
        if (wsize==2) getbignumsign(wx) = 0; /* force positive */
        else wx = normalBignum(wx);
    }
                      
    return wx;
}

LOCAL BIGNUMDATA andfcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return x & y;
}

LOCAL BIGNUMDATA iorfcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return x | y;
}

LOCAL BIGNUMDATA xorfcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return x ^ y;
}

LOCAL BIGNUMDATA eqvfcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return (BIGNUMDATA) ~(x ^ y);
}

LOCAL BIGNUMDATA nandfcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return (BIGNUMDATA) ~(x & y);
}

LOCAL BIGNUMDATA norfcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return (BIGNUMDATA) ~(x | y);
}

LOCAL BIGNUMDATA andc1fcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return (BIGNUMDATA)((~x) & y);
}

LOCAL BIGNUMDATA andc2fcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return (BIGNUMDATA)(x & (~y));
}

LOCAL BIGNUMDATA orc1fcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return (BIGNUMDATA)((~x) | y);
}

LOCAL BIGNUMDATA orc2fcn(BIGNUMDATA x, BIGNUMDATA y)
{
    return (BIGNUMDATA)(x | (~y));
}


LVAL addsubbignum(ux, vx, subvflag)
LVAL ux, vx; int subvflag;
{
    BIGNUMDATA FAR *u = getbignumarray(ux);
    BIGNUMDATA FAR *v = getbignumarray(vx);
    int signu = *u++;
    int signv = *v++ ^ subvflag;
    int usize = (int) getbignumsize(ux);
    int vsize = (int) getbignumsize(vx);
    int wsize;
    LVAL wx;
    BIGNUMDATA FAR *w;
    int j,k;
    BIGNUMDATA carry;
    unsigned long temp;
    long temp2;

    if (u[0] == 0) { u++; usize--; };
    if (v[0] == 0) { v++; vsize--; };

    if (signu == signv) {
        /* Addition algorithm */
        if (vsize > usize) {    /* u is biggest */
            BIGNUMDATA FAR *temp1 = u;
            int temp2 = usize;
            u = v;
            usize = vsize;
            v = temp1;
            vsize = temp2;
        }
        if ((u[0] > 32767) ||
            (vsize==usize && v[0] > 32767)) {
            wx = newbignum(usize+1);
            w = getbignumarray(wx);
            *w++ = (BIGNUMDATA) signu;
            *w++ = 0;   /* skip carry location */
        }
        else {
            wx = newbignum(usize);
            w = getbignumarray(wx);
            *w++ = (BIGNUMDATA) signu;
        }
        carry = 0;
        for (j = usize-1, k=vsize-1; j >=0; j--, k--) {
            if (k>=0) temp = (unsigned long)u[j] + v[k] + carry;
            else temp = (unsigned long)u[j] + carry;
            w[j] = (BIGNUMDATA) (temp & 0xffff);
            carry = (BIGNUMDATA) (temp >> 16);
        }
        if (carry != 0) *--w = carry;
        return wx;
    }
    else { /* do a subtraction */
        if (vsize > usize) { /* make u the biggest */
            BIGNUMDATA FAR *temp1 = u;
            int temp2 = usize;
            u = v;
            usize = vsize;
            v = temp1;
            vsize = temp2;
            signu ^= 1; /* complement sign of bigger */
        }
        else if (vsize == usize) { /* must find the biggest */
            for (j = vsize; j > 0; j--) {
                if (*u > *v) break;
                if (*u < *v) {  /* v is bigger so swap */
                    BIGNUMDATA FAR *temp1 = u;
                    u = v;
                    v = temp1;
                    signu ^= 1;
                    break;
                }
                u++; v++; usize--; vsize--;
            }
            /* if values are the same, then we are finished */
            if (j == 0) return n_bigzero;
        }
        wsize = usize;
        if (usize < 2) wsize = 2;
        wx = newbignum(wsize);
        w = getbignumarray(wx);
        *w++ = (BIGNUMDATA) signu;
        if (usize==1) w++; /* skip leading cell */
        
        carry = 0;
        for (j = usize-1, k = vsize-1; j >=0; j--, k--) {
            if (k>=0) temp2 = (long)u[j] - v[k] - carry;
            else temp2 = (long)u[j] - carry;
            w[j] = (BIGNUMDATA) (temp2 & 0xffff);
            carry = (BIGNUMDATA) (temp2 < 0);
        }
        /* we might need to normalize the bignum */
        if (wsize > 2 && w[0] == 0 && w[1] == 0)
                wx = normalBignum(wx);
        return wx;
    }
}
                        
LVAL multbignum(ux, vx)
LVAL ux, vx;
{
    BIGNUMDATA FAR *u = getbignumarray(ux);
    BIGNUMDATA FAR *v = getbignumarray(vx);
    int sign = *u++ ^ *v++; /* sign calculation */
    int usize = (int) getbignumsize(ux);
    int vsize = (int) getbignumsize(vx);
    int j,i;
    BIGNUMDATA k;
    unsigned long t;
    LVAL wx;
    BIGNUMDATA FAR *w;

    if (zeropbignum(ux)) return ux; /* don't multiply if by zero */
    if (zeropbignum(vx)) return vx; /* because result won't be normalized */

    /* multiply by 1 will work, but much slower than if we special cased
       it as well */

    /* toss a leading 0, if any. system size must be at least 2, so
       size of u and v cannot drop to less than 1. Product size cannot
       drop to less than 2. Since routine can generate only one leading
       zero in product, next multbig will delete it */
    if (u[0] == 0) { u++; usize--; };
    if (v[0] == 0) { v++; vsize--; };

    wx = newbignum(usize+vsize);
    w = getbignumarray(wx);
    *w++ = (BIGNUMDATA) sign;   /* sign is now taken care of */

    for (j=vsize-1; j>=0; j--) {
        for (k=0, i=usize-1; i>=0; i--) {
            t = (unsigned long)(u[i])*v[j]+w[i+j+1]+k;
            w[i+j+1] = (BIGNUMDATA) (t & 0xffff);
            k = (BIGNUMDATA) (t >> 16);
        }
        w[j] = k;
    }
    return wx;
}
    
LVAL divbignum(ux, vx, rem)
LVAL ux, vx, *rem;
{
    /* Doesn't check for division by zero */
    BIGNUMDATA FAR *u = getbignumarray(ux);
    BIGNUMDATA FAR *v = getbignumarray(vx);
    BIGNUMDATA FAR *temp;
    int rsign = *u;         /* sign of remainder */
    int sign = *u++ ^ *v++; /* sign calculation */
    int usize = (int) getbignumsize(ux);
    int vsize = (int) getbignumsize(vx);
    int rsize, qsize;
    int j,i;
    BIGNUMDATA k,k2,d,qest;
    unsigned long t;
    signed long t1;
    LVAL qx, rx;
    BIGNUMDATA FAR *q, FAR *r;

    /* toss a leading 0, if any. system size must be at least 2, so
        size of u and v cannot drop to less than 1. */
    if (u[0] == 0) { u++; usize--; };
    if (v[0] == 0) { v++; vsize--; };

    if (usize < vsize) {
        /* result is zero, with remainder being dividend */
        *rem = rx = newbignum(usize = getbignumsize(ux));
        MEMCPY(getbignumarray(rx), getbignumarray(ux),
                sizeof(short)*(usize+1));
        return n_bigzero;
    }
    /* Allocate space for results */
    qsize = usize-vsize+1;
    if (qsize < 2) qsize = 2;
    xlsave1(qx);
    qx = newbignum(qsize);
    q = getbignumarray(qx);
    *q++ = (BIGNUMDATA) sign;
    if (qsize > usize-vsize+1) q++; /* unused entry */
    rsize = vsize;
    if (rsize < 2) rsize = 2;
    *rem = rx = newbignum(rsize);
    r = getbignumarray(rx);
    *r++ = (BIGNUMDATA) rsign;
    if (rsize > vsize) r++; /* unused entry */

    if (vsize == 1) {
        /* shortcut algorithm vsize=1, qsize=usize */
        for (k=0, i=0; i<usize; i++) {
            t = ((unsigned long)k<<16) + u[i]; /* dividend */
            q[i] = (BIGNUMDATA) (t / v[0]);
            k = (BIGNUMDATA) (t % v[0]);
        }
        r[0] = k;
        if (k == 0) getbignumsign(*rem) = 0; /* force positive zero */
        if (q[0]==0 && q[1]==0) {
            if (qsize > 2)  qx = normalBignum(qx);
            else getbignumsign(qx) = 0;
        }
        xlpop();
        return qx;
    }

    /* In long version, dividend and divisor are modified, so we need to
        copy their data arrays and delete when done */
    temp = (BIGNUMDATA FAR*) MALLOC((usize+1)*sizeof(short));
    if (temp == NULL)  memallocfail();
    MEMCPY(temp, u-1, (usize+1)*sizeof(short));
    u = temp;
    *u = 0;
    temp = (BIGNUMDATA FAR*) MALLOC(vsize*sizeof(short));
    if (temp == NULL) {
        MFREE(u);
        memallocfail();
    }
    MEMCPY(temp, v, vsize*sizeof(short));
    v = temp;

    /* Normalize step */
    d = (BIGNUMDATA) (65536UL/((unsigned long)(v[0]) + 1));
    if (d>1) {
        k = 0;
        for (i=vsize-1; i>=0; i--) {
            t = v[i]*(unsigned long)d + k;
            v[i] = (BIGNUMDATA) (t & 0xffff);
            k = (BIGNUMDATA) (t >> 16);
        }
        k = 0;
        for (i=usize; i>=0; i--) {
            t = u[i]*(unsigned long)d + k;
            u[i] = (BIGNUMDATA) (t & 0xffff);
            k = (BIGNUMDATA) (t >> 16);
        }
    }
    for (j=0; j<usize-vsize+1; j++) {
        /* loop on quotient cells */
        /* estimate quotient cell */
        if (u[j] == v[0]) qest = 65535U;
        else {
            qest = (BIGNUMDATA)(((((unsigned long)(u[j]))<<16) + u[j+1])/v[0]);
        }
        while ( (t = ((unsigned long)(u[j])<<16) + u[j+1] -
                 qest*(unsigned long)(v[0])) < 0x10000L &&
                v[1]*(unsigned long)qest > (t <<16) + u[j+2]) {
            qest--;
        }
        /* multiply and subtract */
        k = k2 = 0;
        for (i=vsize-1; i>=0; i--) {
            t = qest*(unsigned long)v[i] + k; /* t is partial product */
            k = (BIGNUMDATA)(t >> 16);  /* upper part of product */
            t1 = (long)u[i+j+1] - (long)(t & 0xffff)  - (long)k2;
            u[i+j+1] = (BIGNUMDATA)(t1 & 0xffff);
            k2 = (BIGNUMDATA) (t1 < 0);
        }
        t1 = (long)u[j] - (long)(k&0xffff) - (long)k2;
        u[j] = (BIGNUMDATA)(t1 & 0xffff);
        if (t1 & 0xffff0000L) {
            /* overshot */
            qest--;
            k=0;
            for (i =vsize-1; i>=0; i--) {
                t = u[i+j+1] + (unsigned long)k + (unsigned long)v[i];
                u[i+j+1] = (BIGNUMDATA)(t & 0xffff);
                k = (BIGNUMDATA) (t >> 16);
            }
            u[j] = (BIGNUMDATA) (u[j] + k);
        }
        q[j] = qest;
    }
    k = 0;
    for (j=0; j<vsize; j++) { /* unnormalize remainder */
        t = ((unsigned long)k << 16) + u[1+j+usize-vsize];
        r[j] = (BIGNUMDATA) (t / d);
        k = (BIGNUMDATA) (t % d);
    }

    if (r[0]==0 && r[1]==0) {
        if (rsize > 2) *rem = normalBignum(rx);
        else getbignumsign(*rem) = 0;   /* force positive zero */
    }
    if (q[0]==0 && q[1]==0) {
        if (qsize > 2)  qx = normalBignum(qx);
        else getbignumsign(qx) = 0;
    }

    MFREE(u);
    MFREE(v);
    xlpop();
    return qx;
}


/**************************/
/* SUBRs for XLISP follow */
/**************************/

LVAL xintlen() /* integer length */
{
    LVAL ux;
    BIGNUMDATA FAR *u;
    BIGNUMDATA num;
    int size, i;
    FIXTYPE count=0;

    ux = nextarg();
    if (fixp(ux)) ux = cvtfixbignum(getfixnum(ux));
    else if (!bignump(ux)) xlbadtype(ux);
    xllastarg();

    u = getbignumarray(ux);
    size = getbignumsize(ux);
    if (*u++) { /* negative -- we must work more */
        long sum;
        int borrow = 1;

        if (*u == 0) { u++; size--;}

        for (i = size-1; i >=0; i--) {
            sum = ((unsigned long)u[i]) - borrow;
            borrow = sum < 0;
        }
        num = (BIGNUMDATA) sum;
        if (num != 0) {
            i = 16;
            while ((num & 0x8000) == 0) {
                i--;
                num <<= 1;
            }
            count = i;
        }
        
    }
    else {
        if (*u == 0) { u++; size--;}
        if (*u != 0) {
            num = *u;
            i = 16;
            while ((num & 0x8000) == 0) {
                i--;
                num <<= 1;
            }
            count = i;
        }
    }
    count += (size-1)*16L;
    return cvfixnum(count);
}

LVAL xlogcount() /* count bits */
{
    LVAL ux;
    BIGNUMDATA FAR *u;
    BIGNUMDATA num;
    int size, i;
    FIXTYPE count=0;

    ux = nextarg();
    if (fixp(ux)) ux = cvtfixbignum(getfixnum(ux));
    else if (!bignump(ux)) xlbadtype(ux);
    xllastarg();

    u = getbignumarray(ux);
    size = getbignumsize(ux);
    if (*u++) { /* negative -- we must work more */
        long sum;
        int borrow = 1, j;

        for (j = size-1; j >=0; j--) {
            sum = ((unsigned long)u[j]) - borrow;
            num = (BIGNUMDATA) sum;
            borrow = sum < 0;
            for (i=16; i-- > 0; num >>=1)
                count += num&1;
        }
    }
    else {
        while (size-- != 0)
            for (num = *u++, i=16; i-- > 0; num >>= 1)
                count += num&1;
            
    }
    return cvfixnum(count);
}

LVAL xlogbitp() /* bit position */
{
    LVAL ux;
    FIXTYPE pos, numpos;
    BIGNUMDATA FAR *u;
    int bitpos;

    pos = getfixnum(ux=xlgafixnum());
    if (pos < 0) xlerror("must be non-negative", ux);
    ux = nextarg();
    xllastarg();

    if (fixp(ux)) {
        FIXTYPE val = getfixnum(ux);
        if (pos > 31) return (val < 0 ? s_true : NIL);
        return (((getfixnum(ux) >> (int)pos)&1) != 0 ? s_true : NIL);
    }
    if (!bignump(ux)) xlbadtype(ux);

    u = getbignumarray(ux);
    numpos = getbignumsize(ux) - (pos/16) - 1;
    bitpos =(int) (pos % 16);

    if (*u++) { /* negative -- in for more work! */
        unsigned long sum;
        int carry = 1, i;
        if (numpos < 0) return s_true;
        for (i = getbignumsize(ux)-1; i >= numpos; i--) {
            sum = (unsigned long)(BIGNUMDATA)(~u[i]) + carry;
            carry = (int)(sum>>16);
        }
        return ((((BIGNUMDATA)sum) >> bitpos) & 1 ? s_true : NIL);
    }
    else {
        if (numpos < 0) return NIL;
        return ((u[(int)numpos]>>bitpos) & 1 ? s_true : NIL);
    }
}


LVAL xash() /* arithmetic shift left */
{
    LVAL arg;
    FIXTYPE val;

    xlprot1(arg); /* protect against garbage collection */
    arg = xlgetarg();
    /* we will only shift bignums */
    if (fixp(arg)) arg = cvtfixbignum(getfixnum(arg));
    else if (!bignump(arg)) xlbadtype(arg);

    /* get the shift count */
    val = getfixnum(xlgafixnum());
    xllastarg();

    /* do the work */
    arg = ashbignum(arg,val);
    xlpop();
    return (cvtbigfixnum(arg, &val) ? cvfixnum(val) : arg);
}


LOCAL LVAL NEAR logbinary(which)
        int which;
{
    /* logical operations will be only performed using bignums */
    FIXTYPE temp;
    LVAL val, arg;

    /* protect our pointers */
    xlstkcheck(2);
    xlsave(val);
    xlsave(arg);

    if (!moreargs()) { /* return identity */
        switch (which) {
            case '=':
            case '&': val = n_bigmone; break;
            case '|': 
            case '^': val = n_bigzero; break;
        }
    }
    else {
        val = xlgetarg();   /* first argument */
        /* arguments must be integers */
        if (fixp(val)) val = cvtfixbignum(getfixnum(val));
        else if (!bignump(val)) xlbadtype(val);

        while (moreargs()) { /* process additional arguments */
            arg = xlgetarg();
            if (fixp(arg)) arg = cvtfixbignum(getfixnum(arg));
            else if (!bignump(arg)) xlbadtype(arg);
            switch (which) {
                case '&': val = logbignum(val, arg, andfcn); break;
                case '|': val = logbignum(val, arg, iorfcn); break;
                case '^': val = logbignum(val, arg, xorfcn); break;
                case '=': val = logbignum(val, arg, eqvfcn); break;
            }
        }
    }
    xlpopn(2);
    return (cvtbigfixnum(val, &temp) ? cvfixnum(temp) : val);
}
LOCAL LVAL NEAR logbinary2(which)
        int which;
{
    /* logical operations will be only performed using bignums */
    FIXTYPE temp;
    LVAL arg1, arg2;

    /* protect our pointers */
    xlstkcheck(2);
    xlprotect(arg1);
    xlsave(arg2);

    arg1 = xlgetarg();  /* first argument */
    /* arguments must be integers */
    if (fixp(arg1)) arg1 = cvtfixbignum(getfixnum(arg1));
    else if (!bignump(arg1)) xlbadtype(arg1);

    arg2 = xlgetarg();  /* second argument */
    if (fixp(arg2)) arg2 = cvtfixbignum(getfixnum(arg2));
    else if (!bignump(arg2)) xlbadtype(arg2);

    xllastarg();

    switch (which) {
        case 'a': arg1 = logbignum(arg1, arg2, nandfcn); break;
        case 'b': arg1 = logbignum(arg1, arg2, andc1fcn); break;
        case 'c': arg1 = logbignum(arg1, arg2, andc2fcn); break;
        case 'd': arg1 = logbignum(arg1, arg2, norfcn); break;
        case 'e': arg1 = logbignum(arg1, arg2, orc1fcn); break;
        case 'f': arg1 = logbignum(arg1, arg2, orc2fcn); break;
        case 'g': arg1 = logbignum(arg1, arg2, andfcn);
                  xlpopn(2);
                  return ((zeropbignum(arg1) ? NIL : s_true));
    }
    
    xlpopn(2);
    return (cvtbigfixnum(arg1, &temp) ? cvfixnum(temp) : arg1);
}

LVAL xlogand()   { return (logbinary('&')); } /* logand */
LVAL xlogior()   { return (logbinary('|')); } /* logior */
LVAL xlogxor()   { return (logbinary('^')); } /* logxor */
LVAL xlogeqv()   { return (logbinary('=')); } /* logeqv */
LVAL xlognand()  {return (logbinary2('a')); } /* lognand */
LVAL xlogandc1() {return (logbinary2('b')); } /* logandc1 */
LVAL xlogandc2() {return (logbinary2('c')); } /* logandc2 */
LVAL xlognor()   {return (logbinary2('d')); } /* lognor */
LVAL xlogorc1()  {return (logbinary2('e')); } /* logorc1 */
LVAL xlogorc2()  {return (logbinary2('f')); } /* logorc2 */
LVAL xlogtest()  {return (logbinary2('g')); } /* logtest */

LVAL xlognot()
{
    LVAL arg;
    arg = xlgetarg();   /* the argument */
    xllastarg();
    if (fixp(arg)) return cvfixnum(~getfixnum(arg));
    if (!bignump(arg)) xlbadtype(arg);

    arg = logbignum(arg, n_bigmone, xorfcn);
    return arg;
}

#endif /* ifdef BIGNUMS */


