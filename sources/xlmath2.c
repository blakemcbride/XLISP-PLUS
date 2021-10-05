/* commonmath - xlisp math functions modified and augmented to         */
/* correspond more closely to Common Lisp standard                     */
/*      Modifications by Luke Tierney for                              */
/*      XLISP-STAT 2.0 Copyright (c) 1988 by Luke Tierney              */
/* xlmath - xlisp built-in arithmetic functions                        */
/*      Copyright (c) 1985, by David Michael Betz                      */
/*      All Rights Reserved                                            */
/*      Permission is granted for unrestricted non-commercial use      */

/* 7/92 TAA -- 
   Added code so that integers that overflow become flonums when
   NOOVFIXNUM is defined. Complex integers fixed as well */

#include "xlisp.h"
#include <math.h>
#ifndef M_SQRT2     /* this is not always defined */
#define M_SQRT2     1.41421356237309504880
#endif

#if defined(COMPLX) && !defined(BIGNUMS)
#ifdef applec
/* a gross hack */
VOID MODF(double x, double *y)
{
    extended value;
    modf(x, &value);
    *y = value;
}
#else
#define MODF modf
#endif /* applec */

/* These definititions used instead of those in XLMATH.C */
/* Somewhat cleaned by by Tom Almy */

#define IN 0
#define FL 1
#define CI 2
#define CF 3

typedef struct {
  int mode;
  FIXTYPE val, crval, cival;
  FLOTYPE fval, cfrval, cfival;
} Number;


typedef struct {
  double real, imag;
} Complex;


/* Function prototypes */
LOCAL VOID NEAR checkizero _((FIXTYPE iarg));
LOCAL VOID NEAR checkfzero _((FLOTYPE farg));
LOCAL VOID NEAR badiop _((void));
LOCAL VOID NEAR badfop _((void));
LOCAL VOID NEAR badcop _((void));
LOCAL Complex NEAR makecomplex _((LVAL x));
LOCAL double NEAR modulus _((Complex c));
LOCAL Complex NEAR cart2complex _((double real, double imag));
LOCAL LVAL NEAR cvcomplex _((Complex c));
LOCAL Complex NEAR polar2complex _((double mod, double phi));
LOCAL Complex NEAR csqrt _((Complex c));
LOCAL Complex NEAR cexp _((Complex c));
LOCAL Complex NEAR clog _((Complex c));
LOCAL Complex NEAR cmul _((Complex c1, Complex c2));
LOCAL Complex NEAR cexpt _((Complex c1, Complex c2));
LOCAL Complex NEAR cadd _((Complex c1, Complex c2));
LOCAL Complex NEAR csub _((Complex c1, Complex c2));
LOCAL Complex NEAR cdiv _((Complex c1, Complex c2));
LOCAL Complex NEAR csin _((Complex c));
LOCAL Complex NEAR ccos _((Complex c));
LOCAL Complex NEAR ctan _((Complex c));
LOCAL Complex NEAR casin _((Complex c));
LOCAL Complex NEAR cacos _((Complex c));
LOCAL Complex NEAR catan _((Complex c));
LOCAL Complex NEAR catan2 _((Complex n, Complex d));
LOCAL LVAL NEAR readnumber _((Number *num));
LOCAL VOID NEAR setmode _((Number *x, int mode));
LOCAL VOID NEAR matchmodes _((Number *x, Number *y));
LOCAL LVAL NEAR lispnumber _((Number *x));
LOCAL LVAL NEAR binary _((int which));
LOCAL LVAL NEAR logbinary _((int which));
LOCAL FIXTYPE NEAR xget_gcd _((FIXTYPE n, FIXTYPE m));
LOCAL LVAL NEAR unary _((int which));
LOCAL LVAL NEAR unary2 _((int which));
LOCAL LVAL NEAR predicate _((int fcn));
LOCAL LVAL NEAR compare _((int fcn));
LOCAL LVAL NEAR ccompare _((int which));
LOCAL double NEAR logarithm _((FLOTYPE x, FLOTYPE base, int base_supplied));


/* Error checking and messages */

/* checkizero - check for integer division by zero */
LOCAL VOID NEAR checkizero(iarg)
  FIXTYPE iarg;
{
  if (iarg == 0)
  xlfail("illegal zero argument");
}

/* checkfzero - check for floating point division by zero or log of zero */
LOCAL VOID NEAR checkfzero(farg)
  FLOTYPE farg;
{
  if (farg == 0.0)
  xlfail("illegal zero argument");
}

/* badiop - bad integer operation */
LOCAL VOID NEAR badiop()
{
  xlfail("bad integer operation");
}

/* badfop - bad floating point operation */
LOCAL VOID NEAR badfop()
{
  xlfail("bad floating point operation");
}

/* badcop - bad complex number operation */
LOCAL VOID NEAR badcop()
{
  xlfail("bad complex number operation");
}

/* TAA MOD badarg() removed, using preexisting xlbadtype which
   gives same message */

/* complex - Complex number functions  */

/*TAA MOD--do inline with libm call*/
#define phase(c) ((c).imag==0.0 && (c).real == 0.0 ? 0 : atan2((c).imag,(c).real))

/* TAA Mod--do inline, old test for complexp inline as appropriate */
/*  badcomplex() eliminated since it didn't make sense (other numereric
    types were ok), so returned error is now from xlbadtype */

LOCAL Complex NEAR makecomplex(x)
        LVAL x;
{
  Complex c;

  if (realp(x)) {
    c.real = makefloat(x);
    c.imag = 0.0;
  }
  else if (complexp(x)) {
    c.real = makefloat(getreal(x));
    c.imag = makefloat(getimag(x));
  }
  else xlerror("not a number", x);
  return(c);
}

LOCAL double NEAR modulus(c)
        Complex c;
{
/*  return(sqrt(c.real * c.real + c.imag * c.imag));  Schlocky code */
    /* Better code by Hume Smith (11/92) */
    double m, re, im;
   
    re = fabs(c.real);
    im = fabs(c.imag);
    if((m = re>im ? re : im) > 0.0){
        re = c.real / m;
        im = c.imag / m;
        return m * sqrt(re * re + im * im);
    }
    return 0.0;
}

LOCAL Complex NEAR cart2complex(real, imag)
        double real, imag;
{
  Complex val;
  val.real = real;
  val.imag = imag;
  return(val);
}

LOCAL LVAL NEAR cvcomplex(c)
        Complex c;
{
  return(newdcomplex(c.real, c.imag));
}

LOCAL Complex NEAR polar2complex(mod, phi)
        double mod, phi;
{
  Complex val;
  double cs, sn;

  if (phi == 0) {
    cs = 1.0;
    sn = 0.0;
  }
  else if (phi == PI / 2) {
    cs = 0.0;
    sn = 1.0;
  }
  else if (phi == PI) {
    cs = -1.0;
    sn = 0.0;
  }
  else if (phi == -PI / 2) {
    cs = 0.0;
    sn = -1.0;
  }
  else {
    cs = cos(phi);
    sn = sin(phi);
  }
  val.real = mod * cs;
  val.imag = mod * sn;
  return(val);
}

LOCAL Complex NEAR csqrt(c)
        Complex c;
{
/*  return(polar2complex(sqrt(modulus(c)), phase(c) / 2)); */
    /* A better version by Hume Smith 11/92 */

    double ar = c.real, ai = c.imag, m;
    Complex r;
    int f;
   
    if((f = (ar < 0.0)) != 0)
        ar = -ar;

    if((m = ai>ar ? ai : -ai>ar ? -ai : ar) > 0.0){
        double sr = ar/m,
            si = ai/m,
            t = sqrt(m*sqrt(sr*sr + si*si) + ar);
        if(f){
            if(ai < 0.0)
                t = -t;
            r.real = ai / t / M_SQRT2;
            r.imag = t / M_SQRT2;
        }
        else {
            r.real = t / M_SQRT2;
            r.imag = ai / t / M_SQRT2;
        }
    }
    else
        r.real = r.imag = 0.0;
  
    return r;
}

LOCAL Complex NEAR cexp(c)
        Complex c;
{
  return(polar2complex(exp(c.real), c.imag));
}

LOCAL Complex NEAR clog(c)
        Complex c;
{
  double mod;

  mod = modulus(c);
  checkfzero(mod);
  return(cart2complex(log(mod), phase(c)));
}

LOCAL Complex NEAR cmul(c1, c2)
        Complex c1, c2;
{
#if 0   /* This is rediculous, says TAA */
        /* why pay the cost for the two conversions? */
  double m1, m2, p1, p2;

  m1 = modulus(c1);
  p1 = phase(c1);
  m2 = modulus(c2);
  p2 = phase(c2);
  return(polar2complex(m1 * m2, p1 + p2));
#else
    Complex val;

    val.real = c1.real * c2.real - c1.imag * c2.imag;
    val.imag = c1.imag * c2.real + c1.real * c2.imag;
    return val;
#endif
}

LOCAL Complex NEAR cexpt(cb, cp)
        Complex cb, cp;
{
  if (modulus(cp) == 0.0) return(cart2complex(1.0, 0.0));
  else  return(cexp(cmul(clog(cb), cp)));
}

LOCAL Complex NEAR cadd(c1, c2)
        Complex c1, c2;
{
  return(cart2complex(c1.real + c2.real, c1.imag + c2.imag));
}

LOCAL Complex NEAR csub(c1, c2)
        Complex c1, c2;
{
  return(cart2complex(c1.real - c2.real, c1.imag - c2.imag));
}

LOCAL Complex NEAR cdiv(c1, c2)
        Complex c1, c2;
{
#if 0
  double m1, m2, p1, p2;

  m1 = modulus(c1);
  p1 = phase(c1);
  m2 = modulus(c2);
  p2 = phase(c2);
  checkfzero(m2);
  return(polar2complex(m1 / m2, p1 - p2));
#else /* replace with code that is faster */
    double ratio, temp;

    if (fabs(c2.real) > fabs(c2.imag)) {
        ratio = c2.imag / c2.real;
        temp = c2.real + ratio*c2.imag;
        return  cart2complex((c1.real + c1.imag*ratio)/temp,
                             (c1.imag - c1.real*ratio)/temp);
    }
    else {
        checkfzero(c2.imag);
        ratio = c2.real / c2.imag;
        temp = c2.imag + ratio*c2.real;
        return  cart2complex ((c1.real*ratio + c1.imag)/temp,
                              (c1.imag*ratio - c1.real)/temp);
    }
#endif
}

LOCAL Complex NEAR csin(c)
        Complex c;
{
#if 0
  Complex x1, x2, val;

  x1 = cart2complex(-c.imag, c.real);
  x2 = cart2complex(c.imag, -c.real);
  val = csub(cexp(x1), cexp(x2));
  return(cart2complex(val.imag / 2.0, -val.real / 2.0));
#endif
   /* Better code by Hume Smith, 11/92 */
  Complex val;

  val.real = sin(c.real) * cosh(c.imag);
  val.imag = cos(c.real) * sinh(c.imag);
  return val;
}

LOCAL Complex NEAR ccos(c)
        Complex c;
{
#if 0
  Complex x1, x2, val;

  x1 = cart2complex(-c.imag, c.real);
  x2 = cart2complex(c.imag, -c.real);
  val = cadd(cexp(x1), cexp(x2));
  return(cart2complex(val.real / 2.0, val.imag / 2.0));
#endif
  /* Better code by Hume Smith, 11/92 */
  Complex val;

  val.real =  cos(c.real) * cosh(c.imag);
  val.imag = -sin(c.real) * sinh(c.imag);
  return val;

}

LOCAL Complex NEAR ctan(c)
        Complex c;
{
  Complex e1, e2, val;

  e1 = cexp(cart2complex(-c.imag, c.real));
  e2 = cexp(cart2complex(c.imag, -c.real));
  val = cdiv(csub(e1, e2), cadd(e1, e2));
  return(cart2complex(val.imag, -val.real));
}

LOCAL Complex NEAR casin(c)
        Complex c;
{
  /* Improved code by Hume Smith 11/92 */
  Complex sx, val;

  /* saves some function calls */

  sx.real = 1.0 - c.real*c.real + c.imag*c.imag;
  sx.imag = -2.0*c.real*c.imag;

  /* sx = 1 - c*c */

  sx = csqrt(sx);

  sx.real -= c.imag;
  sx.imag += c.real;

  /* sx = i*c + sqrt(1 - c*c) */

  sx = clog(sx);

  val.real =  sx.imag;
  val.imag = -sx.real;

  /* asin(c) = -i ln(i*c + sqrt(1 - c*c)) */
  return val;

}

LOCAL Complex NEAR cacos(c)
        Complex c;
{
  /* Improved code by Hume Smith 11/92 */
  Complex sx, val;

  /* saves some function calls */

  sx.real = 1.0 - c.real*c.real + c.imag*c.imag;
  sx.imag = -2.0*c.real*c.imag;

  /* sx = 1 - c*c */

  val = csqrt(sx);

  sx.real = c.real - val.imag;
  sx.imag = c.imag + val.real;

  /* sx = c + i sqrt(1 - c*c) */

  sx = clog(sx);

  val.real =  sx.imag;
  val.imag = -sx.real;
  
  /* acos(c) = -i ln(c + i*sqrt(1 - c*c)) */
  return val;

}

LOCAL Complex NEAR catan(c)
        Complex c;
{
#if 0   /* This has been redefined in Jan 1989 */
  Complex sx, ix, val, one;

  sx = cmul(c, c);
  sx = cart2complex(1.0 + sx.real, sx.imag);
  one = cart2complex(1.0, 0.0);
  sx = csqrt(cdiv(one, sx));
  ix = cadd(one, cart2complex(-c.imag, c.real));
  val = clog(cmul(ix, sx));
  return(cart2complex(val.imag, -val.real));
#else
  Complex sx, ix;

  sx = clog(cart2complex(1.0-c.imag, c.real));
  ix = clog(cart2complex(1.0+c.imag, -c.real));
  sx.real -= ix.real;   /* complex addition w.o. subroutine call */
  sx.imag -= ix.imag;
  return cdiv(sx,cart2complex(0.0, 2.0));
#endif
}

LOCAL Complex NEAR catan2(n,d)  /* by Tom Almy, and a kludge at that */

Complex n, d;
{
    double tmp;

    tmp = modulus(d);
    if (tmp == 0 || modulus(n)/tmp > 1e50 ) { /* worst case */
        tmp = phase(n) - phase(d);
        if (tmp < -PI) tmp += 2.0*PI;
        else if (tmp > PI) tmp -= 2.0*PI;
        return cart2complex(fabs(tmp) > PI/2.0 ? -PI/2.0 : PI/2.0 ,0.0);
    }
    n = cdiv(n,d);  /* best case */
    return (catan(n));
}

/* Helper functions */

LOCAL LVAL NEAR readnumber(number)
        Number *number;
{
  LVAL arg = xlgetarg(), real, imag;

  if (fixp(arg)) {
      number->mode = IN;
      number->val = getfixnum(arg);
  }
  else if (floatp(arg)) {
      number->mode = FL;
      number->fval = getflonum(arg);
  }
  else if (complexp(arg)) {
      real = getreal(arg);
      imag = getimag(arg);
      if (fixp(real)) {
          number->mode = CI;
          number->crval = getfixnum(real);
          number->cival = getfixnum(imag);
      }
      else {
          number->mode = CF;
          number->cfrval = makefloat(real);
          number->cfival = makefloat(imag);
      }
  }
  else xlerror("not a number", arg);
  return(arg);
}

LOCAL VOID NEAR setmode(x, mode)
        Number *x;
        int mode;
{
  switch (mode) {
  case FL:
    if (x->mode == IN) {
        x->mode = mode;
        x->fval = x->val;
    }
    else return;
    break;
  case CI:
    if (x->mode != IN) return;
    x->mode = mode;
    x->crval = x->val;
    x->cival = 0;
    break;
  case CF:
    switch (x->mode) {
    case IN:
      x->mode = mode;
      x->cfrval = x->val;
      x->cfival = 0.0;
      break;
    case FL:
      x->mode = mode;
      x->cfrval = x->fval;
      x->cfival = 0.0;
      break;
    case CI:
      x->mode = mode;
      x->cfrval = x->crval;
      x->cfival = x->cival;
      break;
    }
    break;
  }
}

LOCAL VOID NEAR matchmodes(x, y)
        Number *x, *y;
{
  int mode = x->mode;
  switch (mode) {
  case IN: mode = y->mode; break;
  case FL: if (y->mode == CI || y->mode == CF) mode = CF; break;
  case CI: if (y->mode == FL || y->mode == CF) mode = CF; break;
  case CF: break;
  }
  if (x->mode != mode) setmode(x, mode);
  if (y->mode != mode) setmode(y, mode);
}

LOCAL LVAL NEAR lispnumber(x)
        Number *x;
{
  switch (x->mode) {
  case IN: return(cvfixnum(x->val));
  case FL: return(cvflonum(x->fval));
  case CI: return(newicomplex(x->crval, x->cival));
  case CF: return(newdcomplex(x->cfrval, x->cfival));
  }
  return NIL; /* avoid warning messages */
}

/* TAA Mod to return FIXTYPE */
LOCAL FIXTYPE NEAR xget_gcd(n,m)                 /* euclid's algorith */
FIXTYPE n, m;
{
   FIXTYPE r;

   for (;;) {
      r = m % n;
      if (r == (FIXTYPE) 0)
        break;
      m = n;
      n = r;
   }

   return (n);
}


LOCAL LVAL NEAR binary(which)
        int which;
{
  LVAL larg;
  Number val, arg;
#ifndef NOOVFIXNUM
  FIXTYPE rtemp;
#endif
  FIXTYPE itemp;
  FLOTYPE frtemp, fitemp;

  if (xlargc == 1 && (which == '-' || which == '/')) {
    val.mode = IN;
    switch (which) {
    case '-': val.val = 0; break;
    case '/': val.val = 1; break;
    }
  }
  else larg = readnumber(&val);
  while (moreargs()) {
    larg = readnumber(&arg);
    matchmodes(&val, &arg);
    switch (which) {
    case '+':
      switch (val.mode) {
      case IN: 
#ifdef NOOVFIXNUM
        if ((val.val < 0) ^ (arg.val < 0)) 
            val.val += arg.val;
        else {
            itemp = val.val + arg.val;
            if ((itemp < 0) ^ (arg.val < 0)) {
                val.fval = val.val + (double) arg.val;
                val.mode = FL;
            }
            else
                val.val = itemp;
        }
#else
      val.val   += arg.val;  
#endif
      break;
      case FL: val.fval  += arg.fval; break;
      case CI: 
#ifdef NOOVFIXNUM
        frtemp = val.crval+(double)arg.crval;
        fitemp = val.cival+(double)arg.cival;
        if (fabs(frtemp) > (double)MAXFIX || fabs(fitemp) > (double)MAXFIX) {
            val.cfrval = frtemp;
            val.cfival = fitemp;
            val.mode = CF;  /* TAA fixed, was CI, 2/94 */
        }
        else {
            val.crval += arg.crval;
            val.cival += arg.cival;
        }
#else
          val.crval += arg.crval;   val.cival += arg.cival;
#endif
    break;
    case CF: val.cfrval += arg.cfrval; val.cfival += arg.cfival; break;
      }
      break;
    case '-':
      switch (val.mode) {
      case IN: 
#ifdef NOOVFIXNUM
        if ((val.val < 0) ^ (arg.val < 0)) {
            itemp = val.val - arg.val;
            if ((itemp < 0) ^ (val.val < 0)) {
                val.fval = val.val - (double) arg.val;
                val.mode = FL;
            }
            else
                val.val = itemp;
        }
        else
            val.val -= arg.val;
#else
          val.val   -= arg.val;  
#endif
          break;
      case FL: val.fval  -= arg.fval; break;
      case CI: 
#ifdef NOOVFIXNUM
        frtemp = val.crval-(double)arg.crval;
        fitemp = val.cival-(double)arg.cival;
        if (fabs(frtemp) > (double)MAXFIX || fabs(fitemp) > (double)MAXFIX) {
            val.cfrval = frtemp;
            val.cfival = fitemp;
            val.mode = CF;  /* TAA fixed, was CI, 2/94 */
        }
        else {
            val.crval -= arg.crval;
            val.cival -= arg.cival;
        }
#else
        val.crval -= arg.crval;   val.cival -= arg.cival;   
#endif
      break;
      case CF:  val.cfrval -= arg.cfrval; val.cfival -= arg.cfival;  break;
      }
      break;
    case '*':
      switch (val.mode) {
      case IN: 
#ifdef NOOVFIXNUM
          frtemp = val.val*(double)arg.val; /* do math in floating point! */
          if (fabs(frtemp) > (double)MAXFIX) {
              val.fval = frtemp;
              val.mode = FL;
          }
          else
              val.val = (FIXTYPE)frtemp;
#else
          val.val   *= arg.val;
#endif
          break;
      case FL: val.fval  *= arg.fval; break;
      case CI:
#ifdef NOOVFIXNUM
        frtemp = val.crval*(double)arg.crval - val.cival*(double)arg.cival;
        fitemp = val.cival*(double)arg.crval + val.crval*(double)arg.cival;
        if (fabs(frtemp) > (double)MAXFIX || fabs(fitemp) > (double)MAXFIX) {
            val.cfrval = frtemp;
            val.cfival = fitemp;
            val.mode = CF;
        }
        else {
            val.crval = (FIXTYPE)frtemp;
            val.cival = (FIXTYPE)fitemp;
        }
#else
        rtemp = val.crval * arg.crval - val.cival * arg.cival;
        itemp = val.cival * arg.crval + val.crval * arg.cival;
        val.crval = rtemp; val.cival = itemp;
#endif
        break;
      case CF:
        frtemp = val.cfrval * arg.cfrval - val.cfival * arg.cfival;
        fitemp = val.cfival * arg.cfrval + val.cfrval * arg.cfival;
        val.cfrval = frtemp; val.cfival = fitemp;
        break;
      }
      break;
    case '/':
      switch (val.mode) {
      case IN:
        checkizero(arg.val);
        if (val.val % arg.val == 0) {
          val.val /= arg.val;
          break;
        }
        else {
          setmode(&val, FL);
          setmode(&arg, FL);
        }
        /* drop through */
      case FL:
        checkfzero(arg.fval);
        val.fval /= arg.fval;
        break;
      case CI:
        setmode(&val, CF);
        setmode(&arg, CF);
        /* drop through */
      case CF:
#if 0   /* we can do better */
    { double magn;
        magn = arg.cfrval * arg.cfrval + arg.cfival * arg.cfival;
        checkfzero(magn);
        frtemp = (val.cfrval * arg.cfrval + val.cfival * arg.cfival) / magn;
        fitemp = (val.cfival * arg.cfrval - val.cfrval * arg.cfival) / magn;
        val.cfrval = frtemp; val.cfival = fitemp;
        break;
    }
#else
    { double ratio,temp;
        if (fabs(arg.cfrval) > fabs(arg.cfival)) {
            ratio = arg.cfival / arg.cfrval;
            temp = arg.cfrval + ratio*arg.cfival;
            frtemp = (val.cfrval + val.cfival*ratio)/temp;
            fitemp = (val.cfival - val.cfrval*ratio)/temp;
        }
        else {
            checkfzero(arg.cfival);
            ratio = arg.cfrval / arg.cfival;
            temp = arg.cfival + ratio*arg.cfrval;
            frtemp = (val.cfrval*ratio + val.cfival)/temp;
            fitemp = (val.cfival*ratio - val.cfrval)/temp;
        }
        val.cfrval = frtemp; val.cfival = fitemp;
        break;
    }
#endif
      }
      break;
    case 'M':
      switch (val.mode) {
      case IN: val.val  = (val.val > arg.val)   ? val.val  : arg.val;  break;
      case FL: val.fval = (val.fval > arg.fval) ? val.fval : arg.fval; break;
      default: xlbadtype(larg);
      }
      break;
    case 'm':
      switch (val.mode) {
      case IN: val.val  = (val.val < arg.val)   ? val.val  : arg.val;  break;
      case FL: val.fval = (val.fval < arg.fval) ? val.fval : arg.fval; break;
      default: xlbadtype(larg);
      }
      break;
    }
  }
  return(lispnumber(&val));
}


/* This has been completely rewritten by Tom Almy to handle floating point
   arguments */

LVAL xrem()
{
    Number numer, div;
    double ftemp;

    readnumber(&numer);
    readnumber(&div);
    xllastarg();

    matchmodes(&numer, &div);

    switch (numer.mode) {
    case IN:    checkizero(div.val);
                return (cvfixnum((FIXTYPE) numer.val % div.val));
    case FL:    checkfzero(div.fval);
                MODF(numer.fval / div.fval, &ftemp);
                return (cvflonum((FLOTYPE)(numer.fval - ftemp*div.fval)));
    }
    badcop();
    return NIL; /* fool compiler into not giving warning */
}


LVAL xash() /* arithmetic shift left */
{
    FIXTYPE arg, val;

    arg = getfixnum(xlgafixnum());
    val = getfixnum(xlgafixnum());
    xllastarg();

    return cvfixnum((val < 0 ? arg >> -val : arg << val));
}


LOCAL LVAL NEAR logbinary(which)
        int which;
{
  FIXTYPE val, arg; /* TAA Mod -- was int */

  switch (which) {
  case '&': val = -1; break;
  case '|': val =  0; break;
  case '^': val =  0; break;
  }
  while (moreargs()) {
    arg = getfixnum(xlgafixnum());
    switch (which) {
    case '&': val &= arg; break;
    case '|': val |= arg; break;
    case '^': val ^= arg; break;
    }
  }
  return(cvfixnum((FIXTYPE) val));
}

/* binary functions */
/* TAA fix allowing (+) */
LVAL xadd()    { return (moreargs()?binary('+'):cvfixnum((FIXTYPE)0)); }
LVAL xsub()    { return (binary('-')); } /* - */
/* TAA fix allowing (*) */
LVAL xmul()    { return (moreargs()?binary('*'):cvfixnum((FIXTYPE)1)); }
LVAL xdiv()    { return (binary('/')); } /* / */
LVAL xmin()    { return (binary('m')); } /* min */
LVAL xmax()    { return (binary('M')); } /* max */
LVAL xlogand() { return (logbinary('&')); } /* logand */
LVAL xlogior() { return (logbinary('|')); } /* logior */
LVAL xlogxor() { return (logbinary('^')); } /* logxor */


/* New by Tom Almy */

LVAL xmod()
{
    Number numer, div;

    readnumber(&numer);
    readnumber(&div);
    xllastarg();

    matchmodes(&numer, &div);

    switch (numer.mode) {
    case IN:    checkizero(div.val);
                return(cvfixnum(numer.val -
                    div.val*(FIXTYPE)floor(numer.val/(double)div.val)));
    case FL:    checkfzero(div.fval);
                return(cvflonum((FLOTYPE)(numer.fval -
                    div.fval*floor((double)(numer.fval/div.fval)))));
    }
    badcop();
    return NIL; /* fool compiler into not giving warning */
}

LVAL xexpt()    /* modified for ratios by TAA */
{
  LVAL base, power;
  int bsign, psign;
  FIXTYPE b, p, val;
  FLOTYPE fb, fp, fval;

  base = xlgetarg();
  power = xlgetarg();
  xllastarg();

  if (fixp(base) && fixp(power)) {
    b = getfixnum(base);
    p = getfixnum(power);
    if (p == 0) return(cvfixnum((FIXTYPE) 1));
    if (b == 0 && p > 0) return(cvfixnum((FIXTYPE) 0));
    checkizero(b);
    if ((bsign = (b < 0)) != 0) b = -b;
    if ((psign = (p < 0)) != 0) p = -p;

    fval = floor(pow((double) b, (double) p) + 0.1); /* to get integer right */
    if (bsign && (p & 1)) fval = -fval;
    if (!psign) {
      val = fval;
      if (val == fval) return(cvfixnum(val));
      else return(cvflonum((FLOTYPE) fval));    /* to handle precision for large results */
    }
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
  else if (floatp(base) && fixp(power)) {
    fb = getflonum(base);
    p = getfixnum(power);
    if (p == 0) return(cvflonum((FLOTYPE) 1.0)); /* TAA MOD - used to return
                                    fixnum 1, but CL says should be flonum */
    if (fb == 0.0 && p > 0) return(cvflonum((FLOTYPE) 0.0));
    checkfzero(fb);
    if ((bsign = (fb < 0.0)) != 0) fb = -fb;
    if ((psign = (p < 0)) != 0) p = -p;
    fval = pow((double) fb, (double) p);
    if (bsign && (p & 1)) fval = -fval;
    if (!psign) return(cvflonum((FLOTYPE) fval));
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
  else if (realp(base) && floatp(power))
  {
    fb = makefloat(base);
    fp = getflonum(power);
    if (fp == 0.0) return(cvflonum((FLOTYPE) 1.0));
    if (fb == 0.0 && fp > 0.0) return(cvflonum((FLOTYPE) 0.0));
    if (fb < 0.0)
      return(cvcomplex(cexpt(makecomplex(base), makecomplex(power))));
    if ((psign = (fp < 0.0)) != 0) fp = -fp;
    fval = pow((double) fb, (double) fp);
    if (!psign) return(cvflonum((FLOTYPE) fval));
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
  else if (complexp(base) || complexp(power))
    return(cvcomplex(cexpt(makecomplex(base), makecomplex(power))));
  else xlfail("bad argument type(s)");
  return NIL; /* avoid compiler warnings */
}

/* arc tangent -- Tom Almy */
LVAL xatan()
{
    Number numer, denom;
    LVAL lnum, ldenom;
    Complex cnum, cdenom;

    lnum = readnumber(&numer);

    if (moreargs()) {
        ldenom = readnumber(&denom);
        xllastarg();
        matchmodes(&numer, &denom);

        switch (numer.mode) {
            case IN:
                numer.fval = numer.val; denom.fval = denom.val;
            case FL:
                return (cvflonum((FLOTYPE)atan2(numer.fval, denom.fval)));
            default: /* complex */
                cnum = makecomplex(lnum);
                cdenom = makecomplex(ldenom);
                return (cvcomplex(catan2(cnum,cdenom)));
        }
    }
    else {
        switch (numer.mode) {
            case IN:
                numer.fval = numer.val;
            case FL:
                return (cvflonum((FLOTYPE)atan(numer.fval)));
            default: /* complex */
                cnum = makecomplex(lnum);
                return (cvcomplex(catan(cnum)));
        }
    }
}



/* two argument logarithm */
LOCAL double NEAR logarithm(x, base, base_supplied)
     FLOTYPE x, base;
     int base_supplied;
{
  double lbase;
  if (x <= 0.0) xlfail("logarithm of a nonpositive number");
  if (base_supplied) {
    if (base <= 0.0) xlfail("logarithm to a nonpositive base");
    else {
      lbase = log(base);
      if (lbase == 0.0) xlfail("logarith to a unit base");
      else return((log(x)/lbase));
    }
  }
  return (log(x));
}

LVAL xlog()
{
  LVAL arg, base;
  int base_supplied = FALSE;
  double fx, fb;

  arg = xlgetarg();
  if (moreargs()) {
    base_supplied = TRUE;
    base = xlgetarg();
  }
  if (base_supplied) {
    if (realp(arg) && realp(base)) {
      fx = makefloat(arg);
      fb = makefloat(base);
      if (fx <= 0.0 || fb <= 0.0)
        return(cvcomplex(cdiv(clog(makecomplex(arg)), clog(makecomplex(base)))));
      else return(cvflonum((FLOTYPE) logarithm(fx, fb, TRUE)));
    }
    else if ((realp(arg) && complexp(base))
             || (complexp(arg) && realp(base))
             || (complexp(arg) && complexp(base)))
      return(cvcomplex(cdiv(clog(makecomplex(arg)), clog(makecomplex(base)))));
    else xlfail("bad argument type(s)");
  }
  else {
    if (realp(arg)) {
      fx = makefloat(arg);
      if (fx <= 0.0) return(cvcomplex(clog(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) logarithm(fx, 0.0, FALSE)));
    }
    else if (complexp(arg))
      return(cvcomplex(clog(makecomplex(arg))));
    else xlfail("bad argument type(s)");
  }
  return NIL; /* avoid compiler warnings */
}

/* xgcd - greatest common divisor */
LVAL xgcd()
{
  FIXTYPE m,n;
  LVAL arg;

  if (!moreargs())                  /* check for identity case */
    return (cvfixnum((FIXTYPE)0));
  arg = xlgafixnum();
  n = getfixnum(arg);
  if (n < (FIXTYPE)0) n = -n;           /* absolute value */
  while (moreargs()) {
    arg = xlgafixnum();
    m = getfixnum(arg);
    if (m == 0 || n == 0) xlfail("zero argument");
    if (m < (FIXTYPE)0) m = -m;     /* absolute value */

    n = xget_gcd(n,m);
  }
  return (cvfixnum(n));
}

LVAL xlcm()                         /* added by kcw */
{
  LVAL arg;
  FIXTYPE n, m, g;
  double t; /* TAA mod, 7/72, to reduce chance of overflow */

  arg = xlgafixnum();
  n = getfixnum(arg);
  if (!moreargs())  {
     if (n < (FIXTYPE) 0) n = -n;
     return (cvfixnum(n));
  }

  while (moreargs())  {
     arg = xlgafixnum();
     m = getfixnum(arg);
     if ((n == (FIXTYPE) 0) || (m == (FIXTYPE) 0))
        return(cvfixnum(0));

     t = n * (double)m;
     g = xget_gcd(n,m);
     t = t/g;
     if (fabs(t) > (double)MAXFIX) xlfail("lcm larger than maximum fixnum");
     n = (FIXTYPE)t;

  }

  if (n < (FIXTYPE) 0) n = -n;

  return (cvfixnum(n));
}

#ifndef RANDOM
LOCAL long rseed=1L;
#endif

/* unary - handle unary operations */
LOCAL LVAL NEAR unary(which)
        int which;
{
  FLOTYPE fval;
  FIXTYPE ival;
  Complex cval;
  LVAL arg, real, imag;
  int mode;

  /* get the argument */
  arg = xlgetarg();
  if (which == 'F' && moreargs()) { /*TAA MOD to eliminate compiler warnings*/
      if (floatp(*xlargv)) xlargc--;
      else xlbadtype(*xlargv);
  }
  xllastarg();

  /* check its type */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    mode = IN;
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    mode = FL;
  }
  else if (complexp(arg)) {
    cval = makecomplex(arg);
    real = getreal(arg);
    imag = getimag(arg);
    if (fixp(getreal(arg))) mode = CI;
    else mode = CF;
  }
  else xlerror("not a number", arg);

  switch (which) {
  case '~':
    if (mode == IN) return(cvfixnum((FIXTYPE) ~ival));
    else badiop();
    break;
  case 'A':
    switch (mode) {
    case IN: return(cvfixnum((FIXTYPE) (ival < 0   ? -ival : ival)));
    case FL: return(cvflonum((FLOTYPE) (fval < 0.0 ? -fval : fval)));
    case CI:
    case CF: return(cvflonum((FLOTYPE) modulus(cval)));
    }
    break;
  case '+':
    switch (mode) {
    case IN: 
#ifdef NOOVFIXNUM
    if (ival == MAXFIX) return (cvflonum(1+(double)MAXFIX));
    else
#endif
    return(cvfixnum((FIXTYPE) ival + 1));
    case FL: return(cvflonum((FLOTYPE) fval + 1.0));
    case CI:
#ifdef NOOVFIXNUM
    if (getfixnum(real) == MAXFIX)
        return (newdcomplex((FLOTYPE)(1+(double)getfixnum(real)), 
            (FLOTYPE)getfixnum(imag)));
    else
#endif      
        return(newicomplex(getfixnum(real) + 1, getfixnum(imag)));
    case CF: return(newdcomplex(getflonum(real) + 1.0, getflonum(imag)));
    }
    break;
  case '-':
    switch (mode) {
    case IN: 
#ifdef NOOVFIXNUM
        /* TAA MOD -- fixed 1/94 */
    if (ival < -(MAXFIX)) return (cvflonum(-2.0 - (double)MAXFIX));
    else
#endif
        return(cvfixnum((FIXTYPE) ival - 1));
    case FL: return(cvflonum((FLOTYPE) fval - 1.0));
    case CI: 
#ifdef NOOVFIXNUM
    if (getfixnum(real) < -(MAXFIX))
        return (newdcomplex((FLOTYPE)(-1 + (double)getfixnum(real)),
            (FLOTYPE)getfixnum(imag)));
     else
#endif
        return(newicomplex(getfixnum(real) - 1, getfixnum(imag)));
    case CF: return(newdcomplex(getflonum(real) - 1.0, getflonum(imag)));
    }
    break;
  case 'S':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) sin((double) ival)));
    case FL: return(cvflonum((FLOTYPE) sin((double) fval)));
    case CI:
    case CF: return(cvcomplex(csin(cval)));
    }
  case 'C':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) cos((double) ival)));
    case FL: return(cvflonum((FLOTYPE) cos((double) fval)));
    case CI:
    case CF: return(cvcomplex(ccos(cval)));
    }
  case 'T':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) tan((double) ival)));
    case FL: return(cvflonum((FLOTYPE) tan((double) fval)));
    case CI:
    case CF: return(cvcomplex(ctan(cval)));
    }
  case 'E':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) exp((double) ival)));
    case FL: return(cvflonum((FLOTYPE) exp((double) fval)));
    case CI:
    case CF: return(cvcomplex(cexp(cval)));
    }
    break;
  case 'R':
    switch (mode) {
    case IN:
      if (ival < 0) return(cvcomplex(csqrt(makecomplex(arg))));
          else return(cvflonum((FLOTYPE) sqrt((double) ival)));
    case FL:
      if (fval < 0) return(cvcomplex(csqrt(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) sqrt(fval)));
    case CI:
    case CF: return(cvcomplex(csqrt(cval)));
    }
    break;
  case 'F':
    switch (mode) {
    case IN: return (cvflonum((FLOTYPE) ival));
    case FL: return (cvflonum((FLOTYPE) fval));
        default: badcop();
        }
        break;
#ifndef RANDOM
  case '?':
    switch (mode) {
    case IN: return (cvfixnum((FIXTYPE)(rseed=osrand(rseed)) % ival));
    case FL: badfop();
        default: badcop();
        }
        break;
#endif
  case 's':
    switch (mode) {
    case IN:
      fval = ival;
      /* drop through */
    case FL:
      if (fval > 1.0 || fval < -1.0)
        return(cvcomplex(casin(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) asin(fval)));
        case CI:
        case CF: return(cvcomplex(casin(cval)));
        }
        break;
  case 'c':
    switch (mode) {
    case IN:
      fval = ival;
      /* drop through */
    case FL:
      if (fval > 1.0 || fval < -1.0)
        return(cvcomplex(cacos(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) acos(fval)));
        case CI:
        case CF: return(cvcomplex(cacos(cval)));
        }
        break;
  case 'P':
    switch (mode) {
        case IN: return(cvflonum((FLOTYPE) (ival >= 0) ? 0.0 : PI));
        case FL: return(cvflonum((FLOTYPE) (fval >= 0.0) ? 0.0 : PI));
        case CI:
        case CF: return(cvflonum((FLOTYPE) phase(cval)));
        }
        break;
  default: xlfail("unsupported operation");
  }
        return NIL; /* avoid compiler warning */
}

LOCAL LVAL NEAR unary2(which)   /* handle truncate, floor, ceiling, and round */
                            /* 1 or two arguments */
                            /* By Tom Almy */
int which;
{
#ifdef MULVALS  /* Yep, it all has to be redone for MULVALS! TAA 10/93 */
    Number numer, denom;
    double quotient;
    LVAL lval;

    xlnumresults = 2;   /* Always the case */

    lval = readnumber(&numer);

    if (moreargs()) {   /* two argument version */
        readnumber(&denom);
        xllastarg();
        matchmodes(&numer, &denom);

        switch (numer.mode) {
            case IN:
                checkizero(denom.val);
                quotient = numer.val / (double)denom.val;
                break;
            case FL:
                checkfzero(denom.fval);
                quotient = numer.fval/denom.fval;
                break;
            default: badcop();
        }
    }
    else { /* single argument version, denominator is one */
        switch (numer.mode) {
            case IN:
                xlresults[1] = cvfixnum((FIXTYPE) 0);
                return (xlresults[0]=lval); /* done early! */
            case FL: 
                denom.fval = 1.0;
                quotient = numer.fval;
                break;
            default: badcop();
        }
    }

    switch (which)  { /* now do it! */
        case 'I': MODF(quotient,&quotient); break;
        case '_': quotient = floor(quotient); break;
        case '^': quotient = ceil(quotient); break;
        case 'r':
        {   /* TAA MOD 3/98 for improved operation */
            double absquo = fabs(quotient);
            double dummy;
            absquo = (MODF(absquo/2,&dummy) < 0.5)?
                                ceil(absquo - 0.5) :
                                floor(absquo + 0.5);
            quotient = (quotient < 0) ? -absquo : absquo;
            break;
        }
    }

    /* calculate remainder in same type as arguments */
    switch (numer.mode) {
        case IN:
            xlresults[1] = cvfixnum(numer.val - (FIXTYPE)quotient*denom.val);
            break;
        case FL:
            xlresults[1] = cvflonum((FLOTYPE)(numer.fval-quotient*denom.fval));
            break;
    }

    /* quotient is (almost) always an integer, but allow for overflow */
    return (xlresults[0] = (numer.mode==IN || fabs(quotient)<(double)MAXFIX)?
                           cvfixnum((FIXTYPE)quotient):
                           cvflonum((FLOTYPE)quotient));
#else
    Number numer, denom;
    LVAL lval;

    lval = readnumber(&numer);

    if (moreargs()) {   /* two argument version */
        readnumber(&denom);
        xllastarg();
        matchmodes(&numer, &denom);

        switch (numer.mode) {
            case IN:
                checkizero(denom.val);
                numer.fval = numer.val / (double)denom.val;
                break;
            case FL:
                checkfzero(denom.fval);
                numer.fval /= denom.fval;
                break;
            default: badcop();
        }
    }
    else { /* single argument version */
        switch (numer.mode) {
            case IN: return lval;   /* no-operation */
            case FL: break;         /* continue */
            default: badcop();
        }
    }
    switch (which)  { /* now do it! */
        case 'I': MODF(numer.fval,&numer.fval); break;
        case '_': numer.fval = floor(numer.fval); break;
        case '^': numer.fval = ceil(numer.fval); break;
        case 'r':
        {   /* TAA MOD 3/98 for improved operation */
            double absquo = fabs(numer.fval);
            double dummy;
            absquo = (MODF(absquo/2,&dummy) < 0.5)?
                     ceil(absquo - 0.5) :
                     floor(absquo + 0.5);
            numer.fval = (numer.fval < 0) ? -absquo : absquo;
            break;
        }
    }
    if (fabs(numer.fval) > (double)MAXFIX)
        return cvflonum((FLOTYPE)numer.fval);
    return cvfixnum((FIXTYPE)numer.fval);
#endif
}

/* unary functions */
LVAL xlognot() { return (unary('~')); } /* lognot */
LVAL xabs()    { return (unary('A')); } /* abs */
LVAL xadd1()   { return (unary('+')); } /* 1+ */
LVAL xsub1()   { return (unary('-')); } /* 1- */
LVAL xsin()    { return (unary('S')); } /* sin */
LVAL xcos()    { return (unary('C')); } /* cos */
LVAL xtan()    { return (unary('T')); } /* tan */
LVAL xexp()    { return (unary('E')); } /* exp */
LVAL xsqrt()   { return (unary('R')); } /* sqrt */
LVAL xfloat()  { return (unary('F')); } /* float */
#ifndef RANDOM
LVAL xrand()   { return (unary('?')); } /* random */
#endif
LVAL xasin()   { return (unary('s')); } /* asin */
LVAL xacos()   { return (unary('c')); } /* acos */
LVAL xphase()  { return (unary('P')); } /* phase */
LVAL xfix()    { return (unary2('I')); } /* truncate */
LVAL xfloor()  { return (unary2('_')); } /* floor */
LVAL xceil()   { return (unary2('^')); } /* ceiling */
LVAL xround()  { return (unary2('r')); } /* round */

/* predicate - handle a predicate function */
LOCAL LVAL NEAR predicate(fcn)
  int fcn;
{
  FLOTYPE fval;
  FIXTYPE ival;
  LVAL arg;

  /* get the argument */
  arg = xlgetarg();
  xllastarg();

  /* check the argument type */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    switch (fcn) {
    case '-': ival = (ival < 0); break;
    case 'Z': ival = (ival == 0); break;
    case '+': ival = (ival > 0); break;
    case 'E': ival = ((ival & 1) == 0); break;
    case 'O': ival = ((ival & 1) != 0); break;
    default:  badiop();
    }
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    switch (fcn) {
    case '-': ival = (fval < 0); break;
    case 'Z': ival = (fval == 0); break;
    case '+': ival = (fval > 0); break;
    default:  badfop();
    }
  }
  else
    xlerror("bad argument type",arg);

  /* return the result value */
  return (ival ? s_true : NIL);
}

/* unary predicates */
LVAL xminusp() { return (predicate('-')); } /* minusp */
LVAL xzerop()  { return (predicate('Z')); } /* zerop */
LVAL xplusp()  { return (predicate('+')); } /* plusp */
LVAL xevenp()  { return (predicate('E')); } /* evenp */
LVAL xoddp()   { return (predicate('O')); } /* oddp */


/* compare - common compare function */
LOCAL LVAL NEAR compare(fcn)
  int fcn;
{
    FIXTYPE icmp,ival,iarg;
    FLOTYPE fcmp,fval,farg;
    LVAL arg;
    int mode;

    /* get the first argument */
    arg = xlgetarg();

    /* set the type of the first argument */
    if (fixp(arg)) {
        ival = getfixnum(arg);
        mode = 'I';
    }
    else if (floatp(arg)) {
        fval = getflonum(arg);
        mode = 'F';
    }
    else
        xlerror("bad argument type",arg);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && moreargs(); ival = iarg, fval = farg) {

        /* get the next argument */
        arg = xlgetarg();

        /* check its type */
        if (fixp(arg)) {
            switch (mode) {
            case 'I':
                iarg = getfixnum(arg);
                break;
            case 'F':
                farg = (FLOTYPE)getfixnum(arg);
                break;
            }
        }
        else if (floatp(arg)) {
            switch (mode) {
            case 'I':
                fval = (FLOTYPE)ival;
                farg = getflonum(arg);
                mode = 'F';
                break;
            case 'F':
                farg = getflonum(arg);
                break;
            }
        }
        else
            xlerror("bad argument type",arg);

        /* compute result of the compare */
        switch (mode) {
        case 'I':
            icmp = ival - iarg;
            switch (fcn) {
            case '<':   icmp = (icmp < 0); break;
            case 'L':   icmp = (icmp <= 0); break;
            case '=':   icmp = (icmp == 0); break;
            case '#':   icmp = (icmp != 0); break;
            case 'G':   icmp = (icmp >= 0); break;
            case '>':   icmp = (icmp > 0); break;
            }
            break;
        case 'F':
            fcmp = fval - farg;
            switch (fcn) {
            case '<':   icmp = (fcmp < 0.0); break;
            case 'L':   icmp = (fcmp <= 0.0); break;
            case '=':   icmp = (fcmp == 0.0); break;
            case '#':   icmp = (fcmp != 0.0); break;
            case 'G':   icmp = (fcmp >= 0.0); break;
            case '>':   icmp = (fcmp > 0.0); break;
            }
            break;
        }
    }

    /* return the result */
    return (icmp ? s_true : NIL);
}

LOCAL LVAL NEAR ccompare(which)
        int which;
{
  Number val, arg;
  int icmp;

  switch (which) {
  case '=': icmp = TRUE;  break;
  case '#': icmp = FALSE; break;
  }
  /*larg =*/ readnumber(&val);
  while (moreargs()) {
    /*larg =*/ readnumber(&arg);
    matchmodes(&val, &arg);
    switch (which) {
    case '=':
      switch (val.mode) {
      case IN: icmp = icmp && val.val  == arg.val;  break;
      case FL: icmp = icmp && val.fval == arg.fval; break;
      case CI: icmp = icmp && val.crval==arg.crval && val.cival==arg.cival;
          break;
      case CF: icmp = icmp && val.cfrval==arg.cfrval && val.cfival==arg.cfival;
          break;
      }
      break;
    case '#':
      switch (val.mode) {
      case IN: icmp = icmp || val.val  != arg.val;  break;
      case FL: icmp = icmp || val.fval != arg.fval; break;
      case CI: icmp = icmp || val.crval!=arg.crval || val.cival!=arg.cival;
          break;
      case CF: icmp = icmp || val.cfrval!=arg.cfrval || val.cfival!=arg.cfival;
          break;
      }
      break;
    }
  }
  return((icmp) ? s_true : NIL);
}

/* comparison functions */
LVAL xlss() { return (compare('<'));  } /* < */
LVAL xleq() { return (compare('L'));  } /* <= */
LVAL xequ() { return (ccompare('=')); } /* = */
LVAL xneq() { return (ccompare('#')); } /* /= */
LVAL xgeq() { return (compare('G'));  } /* >= */
LVAL xgtr() { return (compare('>'));  } /* > */


/***********************************************************************/
/**                                                                   **/
/**                     Complex Number Functions                      **/
/**                                                                   **/
/***********************************************************************/

LVAL xcomplex() /* TAA rewrite so (complex 12.0) => #c(12.0 0.0) as required
                    by CL. */
{
    LVAL real, imag;

    real = xlgetarg();

    if (moreargs()) {
        imag = xlgetarg();
        xllastarg();
        return (newcomplex(real, imag));
    }
    if (fixp(real)) return (real);
    return (newcomplex(real,cvflonum((FLOTYPE)0.0)));
}

LVAL xconjugate()
{
  LVAL arg = xlgetarg();

  xllastarg();
  if (realp(arg)) return(arg);
  if (!complexp(arg)) xlbadtype(arg);
  if (fixp(getreal(arg)) && fixp(getimag(arg)))
    return(newicomplex(getfixnum(getreal(arg)), -getfixnum(getimag(arg))));
  else
    return(newdcomplex(makefloat(getreal(arg)), -makefloat(getimag(arg))));
}

LVAL xrealpart()
{
  LVAL arg = xlgetarg();

  xllastarg();
  if (fixp(arg) || floatp(arg)) return(arg);
  if (!complexp(arg)) xlbadtype(arg);
  return(getreal(arg));
}

LVAL ximagpart()
{
  LVAL arg = xlgetarg();

  xllastarg();
  if (fixp(arg)) return(cvfixnum((FIXTYPE) 0));
  if (floatp(arg)) return(cvflonum((FLOTYPE) 0.0));
  if (!complexp(arg)) xlbadtype(arg);
  return(getimag(arg));
}

#endif
