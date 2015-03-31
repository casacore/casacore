# Introduction #

This is a quick overview of the **casacore** build system. **casacore** uses [scons](http://www.scons.org), a build system written in [python](http://www.python.org) which provides auto-configure capabilities.

# Installing scons #
**scons** works with very old versions of python. However, some of the extensions used in this project have only been tested with python2.4 or higher. Version 1.0 or greater is required.

Download and install **scons** as per instructions on their website. It can be installed
in a custom location in parallel to older versions.

**Note**: For OS X users we recommend using the gfortran compiler available [here](http://r.research.att.com/tools/) as it is universal build.


# Building the whole system #

The current version can be retrieved via subversion:
```
svn co http://casacore.googlecode.com/svn/tags/casacore-0.5.0
```

or get the latest stable source tarball from the Downloads section.

The top-level directory contains a script _batchbuild.py_ which is there to forward scons build requests to the individual **casacore** packages. It passes on all command-line option.
This is not a **scons** build script, it only facilitates building of the whole project.
Individual packages can be build from the source directories (remember that **casa** needs to be built first).

You should also download and unpack  the measures data files in the `prefix/share/casacore` directory (default is `stage/share/casacore`).
A copy updated weekly is available on [ftp://ftp.atnf.csiro.au/pub/software/asap/data/asap_data.tar.bz2](ftp://ftp.atnf.csiro.au/pub/software/asap/data/asap_data.tar.bz2).

These are not need for building casacore, but are needed for running casacore tests.
see also [CasaAipsRC](CasaAipsRC.md) for how to point to measures data explicity.

## Requirements ##

**casacore** needs the following external libraries:

  * cfitsio
  * wcslib ([download](http://www.atnf.csiro.au/pub/software/wcslib/wcslib.tar.gz), **Note** you might need to remove the .c files corresponding to the .l files in the C subdirectory)
  * lapack/blas
  * flex/bison
  * fortran compiler and fortran-to-c library, e.g. gfortran and libgfortran

The HDF5 package is optional (see http://www.hdfgroup.org/HDF5/index.html). If enabled (enable\_hdf5=1), the HDF5 classes in the casacore package will be activated. For example, HDF5Image can be used to support images in HDF5 format.

### Fortran ###
The build system looks for **gfortran** first, then g77 and then f77.

## Build and install to '/usr/local' ##
The following example will build and install the libraries into '/usr/local/lib', the headers into '/usr/local/include/casacore', **scons** helper scripts into '/usr/local/share/casacore' and any binaries into '/usr/local/bin'.
This will only build the static libraries

```
./batchbuild.py prefix=/usr/local install
```


## Build Targets ##

  * **static** - the default target to build static libraries
  * **shared** -  build shared libraries
  * **test** - run assay tests
  * **install** - install casacore to _prefix_

## Build options ##

Options (_scons -h_) are:

### General Options ###

  * build=<dbg,**opt**> (or both) - the build type
  * prefix= - the install location
  * universal=<'ppc', 'i386', 'ppc64', 'x86\_64', **'none'**> - platforms to cross-compile and make universal build Mac OS X **only**
  * -j< N > - N the number of processes to use for the build
  * -Q - suppress scons status messages
  * --quiet - suppress all build messages (useful when running **test** target)

Once **scons** has been invoked once, these options are stored in the individual packages' _options.cfg_ and have to be overwritten explicitly

### Compiler Options ###

The compiler auto-detection can be overwritten:

**Note:** multiple options can be specified using ',' (commas) e.g.
```
scons extracppflags=-DDO_THIS,-DDO_THAT
```
Be careful not to use spaces in the options

  * CC=<custom c compiler>
  * CXX=<custom c++ compiler>
  * extracppflags=<extra pre-processor options>
  * extracxxflags=<extra c++ compiler options>
  * extracflags=<extra c compiler options>
  * extralinkflags=<extra linker options>

  * FORTRAN=<custom fortran compiler>
  * f2clib=<library name> (e.g. g2c)
  * extrafflags=<extra fortran compiler options>

There are two special preprocessor options:
  * -DCASACORE\_NO\_AUTO\_TEMPLATES prohibits the inclusion of .tcc files in .h files. The effect is that no automatic instantiation will be done for templated functions and classes in casacore. This should only be used by experts.
  * -DCASACORE\_UNLOCK\_TABLE\_ON\_DESTRUCT tells that a table should always be unlocked in the Table destructor. Normally it is only done when the reference counts gets zero. This flag is meant to be used by the casapy group.

### Library options ###

  * cfitsioroot=<path to cfitsioroot> (disables the following)
  * cfitsiolibdir
  * cfitsioincdir

  * wcsroot  (disables the following)
  * wcslibdir
  * wcsincdir

  * lapacklib - custom name for the lapack library, e.g. AMD specialized libraries (can also be a list e.g. lapacklib=a,b,c)
  * lapackroot  (disables the following)
  * lapacklibdir
  * blaslib - custom name for the blas library, e.g. AMD specialized libraries (can also be a list e.g. blaslib=a,b,c)
  * blasroot  (disables the following)
  * blaslibdir


  * enable\_hdf5 - enable configure for HDF5, default is not enabled (enable\_hdf5=0)
  * hdf5root  - for HDF5 support(disables the following)
  * hdf5libdir
  * hdf5incdir

  * disable\_dl - disable the use of dlopen

The default root directories searched for the libraries and include files are /usr and /usr/local.

### Configuration Options ###
  * datadir=<path to measures data directory> - This location will be added to the default search locations  of measures data directories, i.e. this directory doesn't need to be specified in e.g ~/.casarc.