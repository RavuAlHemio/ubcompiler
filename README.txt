UBCompiler
==========

I wrote this compiler for TU Wien's *Übersetzerbau* (*Compiler Construction*)
course for the summer semester of 2011.

It is written using [Flex](http://flex.sourceforge.net/),
[Bison](http://www.gnu.org/software/bison/) and [LLVM](http://llvm.org).

(Originally, it used Ox as well, but I decided to excise it because I never used
any Ox feature that Bison didn't provide natively. Also, I have updated the code
to run with LLVM 3.4 -- the current release of LLVM at the time of writing.)
