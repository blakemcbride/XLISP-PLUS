/* xlpp.c - xlisp pretty printer */
/*      Copyright (c) 1985, by David Betz
        All Rights Reserved                     */

#include "xlisp.h"

/* local variables */
static int pplevel,ppmargin,ppmaxlen;
static LVAL ppfile;

/* forward declarations */
LOCAL VOID NEAR pp _((LVAL expr));
LOCAL VOID NEAR pplist _((LVAL expr));
LOCAL VOID NEAR ppexpr _((LVAL expr));
LOCAL VOID NEAR ppputc _((int ch));
LOCAL VOID NEAR ppterpri _((void));
LOCAL int  NEAR ppflatsize _((LVAL expr));

/* xpp - pretty-print an expression */
LVAL xpp()
{
    LVAL expr;

    /* get printlevel and depth values */
    expr = getvalue(s_printlevel);
    if (fixp(expr) && getfixnum(expr) <= 32767 && getfixnum(expr) >= 0) {
        plevel = (int)getfixnum(expr);
    }
    else {
        plevel = 32767;
    }
    expr = getvalue(s_printlength);
    if (fixp(expr) && getfixnum(expr) <= 32767 && getfixnum(expr) >= 0) {
        plength = (int)getfixnum(expr);
    }
    else
        plength = 32767;

    /* get expression to print and file pointer */
    expr = xlgetarg();
    ppfile = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
    xllastarg();

    /* pretty print the expression */
    pplevel = ppmargin = 0; ppmaxlen = 40;
    pp(expr); ppterpri();

    /* return nil */
#ifdef MULVALS
    xlnumresults = 0;   /* no returned results if Multiple values */
#endif /* MULVALS */
    return (NIL);
}

/* pp - pretty print an expression */
LOCAL VOID NEAR pp(expr)
  LVAL expr;
{
    if (consp(expr))
        pplist(expr);
    else
        ppexpr(expr);
}

/* pplist - pretty print a list */
LOCAL VOID NEAR pplist(expr)
  LVAL expr;
{
    int n;

    /* if the expression will fit on one line, print it on one */
    if ((n = ppflatsize(expr)) < ppmaxlen) {
        xlprintl(ppfile,expr,TRUE);
        pplevel += n;
    }

    /* otherwise print it on several lines */
    else {
        int llength = plength;

        if (plevel-- == 0) {
            ppputc('#');
            plevel++;
            return;
        }

        n = ppmargin;
        ppputc('(');
        if (atom(car(expr))) {
            ppexpr(car(expr));
            ppputc(' ');
            ppmargin = pplevel;
            expr = cdr(expr);
        }
        else
            ppmargin = pplevel;
        for (; consp(expr); expr = cdr(expr)) {
            if (llength-- == 0) {
                xlputstr(ppfile,"... )");
                pplevel += 5;
                ppmargin =n;
                plevel++;
                return;
            }
            pp(car(expr));
            if (consp(cdr(expr)))
                ppterpri();
        }
        if (expr != NIL) {
            ppputc(' '); ppputc('.'); ppputc(' ');
            ppexpr(expr);
        }
        ppputc(')');
        ppmargin = n;
        plevel++;
    }
}

/* ppexpr - print an expression and update the indent level */
LOCAL VOID NEAR ppexpr(expr)
  LVAL expr;
{
    xlprintl(ppfile,expr,TRUE);
    pplevel += ppflatsize(expr);
}

/* ppputc - output a character and update the indent level */
LOCAL VOID NEAR ppputc(ch)
  int ch;
{
    xlputc(ppfile,ch);
    pplevel++;
}

/* ppterpri - terminate the print line and indent */
LOCAL VOID NEAR ppterpri()
{
    xlterpri(ppfile);
    for (pplevel = 0; pplevel < ppmargin; pplevel++)
        xlputc(ppfile,' ');
}

/* ppflatsize - compute the flat size of an expression */
LOCAL int NEAR ppflatsize(expr)
  LVAL expr;
{
    LVAL ustream = newustream();
    int size;

    xlprot1(ustream);
    
    xlprint(ustream,expr,TRUE);

    /* calculate size */
    for (size = 0, ustream = gethead(ustream);
         !null(ustream);
         size++, ustream = cdr(ustream)) {};

    xlpop();
    
    return (size);
}
