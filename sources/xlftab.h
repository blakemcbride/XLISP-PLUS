/* xlftab.h - xlisp function table */
/*New file which holds the external declarations for all the xlisp functions*/
#ifdef ANSI
#define V void
#else
#define V /* */
#endif
/* external functions */
extern LVAL
    rmhash(V),rmquote(V),rmdquote(V),rmbquote(V),rmcomma(V),
    clnew(V),clisnew(V),clanswer(V),
    obisnew(V),obclass(V),obshow(V),
    rmlpar(V),rmrpar(V),rmsemi(V),xcomplement(V),
    xeval(V),xapply(V),xfuncall(V),xquote(V),xfunction(V),xbquote(V),
    xlambda(V),xset(V),xsetq(V),xsetf(V),xdefun(V),xdefmacro(V),
    xgensym(V),xmakesymbol(V),xintern(V),
    xsymname(V),xsymvalue(V),xsymplist(V),
    xget(V),xgetf(V),xputprop(V),xremprop(V),
    xhash(V),xmkarray(V),xaref(V),
    xcar(V),xcdr(V),
    xcaar(V),xcadr(V),xcdar(V),xcddr(V),
    xcaaar(V),xcaadr(V),xcadar(V),xcaddr(V),
    xcdaar(V),xcdadr(V),xcddar(V),xcdddr(V),
    xcaaaar(V),xcaaadr(V),xcaadar(V),xcaaddr(V),
    xcadaar(V),xcadadr(V),xcaddar(V),xcadddr(V),
    xcdaaar(V),xcdaadr(V),xcdadar(V),xcdaddr(V),
    xcddaar(V),xcddadr(V),xcdddar(V),xcddddr(V),
    xcons(V),xlist(V),xappend(V),xreverse(V),xlast(V),xnth(V),xnthcdr(V),
    xnsublis(V),xnsubst(V),xnsubstif(V),xnsubstifnot(V),
    xmember(V),xassoc(V),xsubst(V),xsublis(V),xlength(V),xlistlength(V),
    xsort(V),xremove(V),xremif(V),xremifnot(V),
    xmapc(V),xmapcar(V),xmapl(V),xmaplist(V),xmapcan(V),xmapcon(V),
    xrplca(V),xrplcd(V),xnconc(V),
    xdelete(V),xdelif(V),xdelifnot(V),
    xatom(V),xsymbolp(V),xnumberp(V),xboundp(V),xnull(V),xlistp(V),xendp(V),
    xconsp(V),xeq(V),xeql(V),xequal(V),
    xcond(V),xcase(V),xand(V),xor(V),xlet(V),xletstar(V),xif(V),
    xprog(V),xprogstar(V),xprog1(V),xprog2(V),xprogn(V),xgo(V),xreturn(V),
    xcatch(V),xthrow(V),
    xerror(V),xcerror(V),xbreak(V),
    xcleanup(V),xtoplevel(V),xcontinue(V),xerrset(V),
    xbaktrace(V),xevalhook(V),
    xdo(V),xdostar(V),xdolist(V),xdotimes(V),
    xminusp(V),xzerop(V),xplusp(V),xevenp(V),xoddp(V),
    xfix(V),xfloat(V),
    xgcd(V),xadd(V),xsub(V),xmul(V),xdiv(V),xrem(V),xmin(V),xmax(V),xabs(V),
    xadd1(V),xsub1(V),xlogand(V),xlogior(V),xlogxor(V),xlognot(V),
    xsin(V),xcos(V),xtan(V),xexpt(V),xexp(V),xsqrt(V),xrand(V),
    xlss(V),xleq(V),xequ(V),xneq(V),xgeq(V),xgtr(V),
    xsubseq(V),xstring(V),xchar(V),
    xread(V),xreadpw(V),xprint(V),xprin1(V),xprinc(V),xterpri(V),
    xflatsize(V),xflatc(V),
    xopen(V),xclose(V),xrdchar(V),xpkchar(V),xwrchar(V),xreadline(V),
    xload(V),xtranscript(V),
    xtype(V),xexit(V),xpeek(V),xpoke(V),xaddrs(V),
    xvector(V),xblock(V),xrtnfrom(V),xtagbody(V),
    xpsetq(V),xpsetf(V),xflet(V),xlabels(V),xmacrolet(V),
    xunwindprotect(V),xpp(V),
    xstrlss(V),xstrleq(V),xstreql(V),xstrneq(V),xstrgeq(V),xstrgtr(V),
    xstrilss(V),xstrileq(V),xstrieql(V),xstrineq(V),xstrigeq(V),xstrigtr(V),
    xupcase(V),xdowncase(V),xnupcase(V),xndowncase(V),
    xcapcase(V),xncapcase(V),
    xtrim(V),xlefttrim(V),xrighttrim(V),
    xuppercasep(V),xlowercasep(V),xbothcasep(V),xdigitp(V),xalphanumericp(V),
    xalphacharp(V),
    xcharcode(V),xcodechar(V),xchupcase(V),xchdowncase(V),xdigitchar(V),
    xchrlss(V),xchrleq(V),xchreql(V),xchrneq(V),xchrgeq(V),xchrgtr(V),
    xchrilss(V),xchrileq(V),xchrieql(V),xchrineq(V),xchrigeq(V),xchrigtr(V),
    xintegerp(V),xfloatp(V),xstringp(V),xarrayp(V),xstreamp(V),xobjectp(V),
    xwhen(V),xunless(V),xloop(V),
    xsymfunction(V),xfboundp(V),xsend(V),xsendsuper(V),
    xprogv(V),xrdbyte(V),xwrbyte(V),xformat(V),
    xcharp(V),xcharint(V),xintchar(V),
    xmkstrinput(V),xmkstroutput(V),xgetstroutput(V),xgetlstoutput(V),
    xgetlambda(V),xmacroexpand(V),x1macroexpand(V),
    xtrace(V),xuntrace(V),obprin1(V),
    xdefconstant(V), xconstantp(V), xdefparameter(V),
    xdefvar(V), xmakunbound(V), xfmakunbound(V),
    xspecialp(V), xmarkspecial(V),
    xdefstruct(V),xmkstruct(V),xcpystruct(V),xstrref(V),xstrset(V),
    xstrtypep(V), xasin(V),xacos(V),xatan(V),
    xnreverse(V),xbutlast(V),xcoerce(V),xconcatenate(V),xelt(V),xtypep(V),
    xliststar(V),
    xfileposition(V), xfilelength(V), xfreshline(V),
    xtoplevelloop(V), xresetsystem(V),
    xopenstreamp(V), xinputstreamp(V), xoutputstreamp(V);

#ifdef SUBSTITUTE
extern LVAL
    xsubstitute(V),xsubstituteif(V),xsubstituteifnot(V),
    xnsubstitute(V),xnsubstituteif(V),xnsubstituteifnot(V);
#endif
#ifdef ADDEDTAA
extern LVAL
    xgeneric(V);
#endif
#ifdef SRCHFCN
  extern LVAL xsearch(V);
#endif   
#ifdef POSFCNS
  extern LVAL xcount(V), xfind(V), xposition(V);
  extern LVAL xcountif(V), xfindif(V), xpositionif(V);
  extern LVAL xcountifnot(V), xfindifnot(V), xpositionifnot(V);
#endif
#ifdef REMDUPS
  extern LVAL xremove_duplicates(V);
#endif
#ifdef MAPFCNS
  extern LVAL xsome(V), xevery(V), xnotany(V), xnotevery(V),
     xmap(V), xmapinto(V);
#endif
#ifdef REDUCE
  extern LVAL xreduce(V);
#endif
#ifdef FILETABLE
extern LVAL
    xtruename(V), xdeletefile(V);
#endif

/* functions specific to xldmem.c */
extern LVAL 
    xgc(V),xexpand(V),xalloc(V),xmem(V);

#ifdef SAVERESTORE
extern LVAL 
    xsave(V),xrestore(V);
#endif

#ifdef APPLYHOOK
extern LVAL 
    xapplyhook(V);
#endif

#ifdef SETS
extern LVAL 
    xadjoin(V), xunion(V), xintersection(V), xsubsetp(V),
    xset_difference(V);
#endif

#ifdef TIMES
extern LVAL
    xtime(V), xruntime(V), xrealtime(V);
#endif

#ifdef HASHFCNS
extern LVAL
    xgethash(V),xremhash(V),xmakehash(V),xclrhash(V),xmaphash(V),
    xhashcount(V);
#endif

#ifdef COMPLX
extern LVAL
    xcomplexp(V), xcomplex(V), xconjugate(V), xrealpart(V), ximagpart(V),
    xlog(V), xfloor(V), xceil(V), xround(V), xphase(V), xlcm(V), xash(V),
    xmod(V);
#endif

#ifdef BIGNUMS
extern LVAL xrationalp(V), xnumerator(V), xdenominator(V), xrational(V);
extern LVAL xlogeqv(V), xlognand(V), xlognand(V), xlognor(V), xlogandc1(V);
extern LVAL xlogandc2(V), xlogorc1(V), xlogorc2(V);
extern LVAL xlogtest(V), xlogbitp(V), xlogcount(V), xintlen(V);
#endif

#ifdef RANDOM
extern LVAL xmakerandom(V);
#endif

#ifdef STSZ
extern LVAL xsetmark(V);
#endif

#ifdef PACKAGES
extern LVAL xmakepackage(V);
extern LVAL xinpackage(V);
extern LVAL xexport(V);
extern LVAL xunexport(V);
extern LVAL ximport(V);
extern LVAL xsympackage(V);
extern LVAL xusepackage(V);
extern LVAL xunusepackage(V);
extern LVAL xpackageuselist(V);
extern LVAL xpackageusedbylist(V);
extern LVAL xpackageshadows(V);
extern LVAL xpackagename(V);
extern LVAL xpackagenicknames(V);
extern LVAL xpackageobarray(V);
extern LVAL xpackagevalidp(V);
extern LVAL xlistallpackages(V);
extern LVAL xunintern(V);
extern LVAL xshadow(V);
extern LVAL xshadowingimport(V);
extern LVAL xfindpackage(V);
extern LVAL xfindsymbol(V);
extern LVAL xdeletepackage(V);
extern LVAL xrenamepackage(V);
extern LVAL xfindallsymbols(V);
#endif /* PACKAGES */

#ifdef MULVALS
extern LVAL xvalues(V);
extern LVAL xmulvalcall(V);
extern LVAL xmulvalprog1(V);
extern LVAL xnthvalue(V);
#endif /* MULVALS */

/* $putpatch.c$: "MODULE_XLFTAB_H_GLOBALS" */
