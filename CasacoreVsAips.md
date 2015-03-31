# Introduction #

**casacore** is the set of core libraries taken from **aips++**. However, a few things have been modified.

  * **casacore** uses auto-templating and is not using the `__ReposFiller`

  * **casacore** uses .tcc files for fully templated source files. These are also installed into the include location.

  * The libraries are named `libcasa_<pkg>` instead of `lib<pkg>` to avoid possible clashes with other software packages.

  * _aipsinit/AIPSPATH_ are no longer used. This needs to be tidied up for things which require it (e.g. measures). For now the _AIPSPATH_ has to be set by hand, but only the path (first argument) is relevant to find the data directory.

  * **casacore** uses an extended way of finding aipsrc vaiables (see CasaAipsRC). The old **aips++** way is still supported

  * **aips++** `VersionInfo` is no longer used. In the few places where it was used, _casacore trunk_ is hardcoded. Probably just use $Revision there.

  * **wcslib** is no longer part of the **casa** package.

  * **mirlib** is now a package itself (maybe remove completely)