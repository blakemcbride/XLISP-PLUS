
# XLISP-PLUS Almy

XLISP-PLUS is an enhanced version of David Michael Betz's XLISP 2.1 to
have additional features of Common Lisp. XLISP-PLUS runs on Microsoft
Windows, Apple macOS, Linux and UNIX, but can be easily ported to
other platforms. Complete source code is provided to allow easy
modification and extension.

Since XLISP-PLUS is based on XLISP 2.1, most XLISP programs will run on
XLISP-PLUS. Since XLISP-PLUS incorporates many more features of
Common Lisp, many small Common Lisp applications will run on
XLISP-PLUS with little modification. 

Many Common Lisp functions are built into XLISP-PLUS. In addition,
XLISP defines the objects Object and Class as primitives. Object is
the only class that has no superclass and hence is the root of the
class hierarchy tree. Class is the class of which all classes are
instances (it is the only object that is an instance of itself).

## GitHub release

After interacting with Tom Almy over many years and making a few bug
fixes and enhancements to XLISP-PLUS, Tom gave me permission to put
this system on GitHub.  I have also obtained permission from David Betz.

Version 2.x of Betz's XLISP followed Common Lisp.  Version 3.x of Betz's
XLISP changed to Scheme rather than Common Lisp.  Thus, this version is, in
a sense, the continuation of XLISP 2.x.

The version numbers between Betz's XLISP and Almy's XLISP are also
confusing.  When Almy forked XLISP he made it version 3.x in order to
distinguish it from Betz's XLISP.  However, when Betz moved from
Common Lisp to Scheme he also called it version 3.x.  Thus they're
both version 3.x.  The best way to differentiate them is that Almy
also renamed the package XLISP-PLUS.

The home for this GitHub release is []()

Blake McBride
