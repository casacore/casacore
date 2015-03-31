## Q: What are the differences between casacore and aips++? ##

see CasacoreVsAips++

## Q: Where are the measures data gone? ##

A snapshot is provided at [ftp://ftp.atnf.csiro.au/pub/software/asap/data/asap_data.tar.bz2](ftp://ftp.atnf.csiro.au/pub/software/asap/data/asap_data.tar.bz2) until the measures data scripts have been revised.

## Q: How do I build a 32bit version on a 64bit machine ##

Provided you are running a 32bit version of python,
use the command-line options
```
extracppflags=-DAIPS_64b,-m32
```

## Q: How do I build my app against casacore? ##

Just set the include and library path, e.g. `-I/usr/local/include/casacore, -L/usr/local/lib`

## Q: Where can I find a source code documentation? ##
Follow the links in the Links section on the project frontpage.