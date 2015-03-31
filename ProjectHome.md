# IMPORTANT #

google code no longer provides Downloads, so
the latest release of casacore - casacore-1.7.0 can be found here:

[ftp://ftp.atnf.csiro.au/pub/software/casacore/casacore-1.7.0.tar.bz2](ftp://ftp.atnf.csiro.au/pub/software/casacore/casacore-1.7.0.tar.bz2)

There will be a new major release (2.0.0) soon containing significant changes.


# Description #

This project contains c++ libraries which were the code of the discontinued **aips++** package. The build system has been fully rewritten. Links to the documentation of the build system (using scons or cmake) is given under the 'featured wiki pages' on this page.

**casacore** provides the following packages:

  * casa
  * components
  * coordinates
  * fits
  * images
  * lattices
  * measures
  * mirlib
  * ms
  * derivedmscal
  * msfits
  * scimath
  * tables

The API of the packages is documented in the external links on this page (Australian site at ATNF, European site at ASTRON). They also contain a link to notes that describe some parts in more detail.

Thread-safety can be compiled in (see under CmakeInstructions). It means that access to all static variables in casacore is thread-safe. Objects are not thread-safe, thus if a programmer accesses the same casacore object from multiple threads, the programmer is responsible for correct (un)locking. The casacore Mutex and ScopedLock classes can be used for that purpose.
<br>So far, only the statics in the modules casa, tables, and scimath are thread-safe.<br>
<br>
