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
svn co http://casacore.googlecode.com/svn/tags/casacore-1.0.0
```

or get the latest stable source tarball from the Downloads section.

After installing casacore to run the tests you should also download and unpack the measures data files in the `prefix/share/casacore` directory.
A copy updated weekly is available on [ftp://ftp.atnf.csiro.au/pub/software/asap/data/asap_data.tar.bz2](ftp://ftp.atnf.csiro.au/pub/software/asap/data/asap_data.tar.bz2).

These are not need for building casacore, but are needed for running casacore tests.
see also [CasaAipsRC](CasaAipsRC.md) for how to point to measures data explicitly.

## Requirements ##

**casacore** needs the following external libraries:

  * cfitsio
  * wcslib ([download](http://www.atnf.csiro.au/pub/software/wcslib/wcslib.tar.gz), **Note** you might need to remove the .c files corresponding to the .l files in the C subdirectory)
  * lapack/blas
  * flex/bison
  * fortran compiler and fortran-to-c library, e.g. gfortran and libgfortran

The HDF5 package is optional (see http://www.hdfgroup.org/HDF5/index.html). If enabled (--enable-hdf5), the HDF5 classes in the casacore package will be activated. For example, HDF5Image can be used to support images in HDF5 format.

### Fortran ###
The build system looks for **gfortran** first, then g77 and then f77.

## Build and install to '/usr/local' ##
The following example will build and install the libraries into '/usr/local/lib', the headers into '/usr/local/include/casacore', **scons** helper scripts into '/usr/local/share/casacore' and any binaries into '/usr/local/bin'.
This will only build the static libraries

```
scons --prefix=/usr/local
scons install
```

Once **scons** has been invoked once, command-line options are stored in  _options.cache_ and have to be overwritten explicitly

## Build Targets ##

  * **test** - run assay tests
  * **install** - install casacore to _prefix_
  * **xyz** - build xyz where xyz is a package e.g. _measures_
  * **test\_xyz** - test xyz where xyz is a package e.g. _measures_
  * **txyz** - run the test for an explict test program txyz

## Build options ##

Options (_scons --help) are ( Under local options):_

### Scons Options ###

  * -j< N > - N the number of processes to use for the build
  * -Q - suppress scons status messages
  * --quiet - suppress all build messages (useful when running **test** target)

### Build options ###

```
  --enable-shared             Enable building shared (dynamic) libraries
  --disable-static            Disable building static libraries
  --enable-hdf5               Enable the HDF5 library
  --disable-dl                Disable the use of dlopen
  --enable-readline           Enable the readline library
  --data-dir=DATA_DIR         The location of the measures data directory to
                                compile in as the default search location
  --build-type=BUILD_TYPE     Build optimized 'opt' (default) or debug 'dbg'
  --hdf5-lib=HDF5_LIB         hdf5 library name (default: hdf5)
  --hdf5-root=HDF5_ROOT       hdf5 package root
  --hdf5-incdir=HDF5_INCDIR   hdf5 package 'include' directory (overwrites
                                '-root')
  --hdf5-libdir=HDF5_LIBDIR   hdf5 package 'lib' directory (overwrites
                                '-root')
  --dl-lib=DL_LIB             dl library name (default: dl)
  --dl-root=DL_ROOT           dl package root
  --dl-incdir=DL_INCDIR       dl package 'include' directory (overwrites
                                '-root')
  --dl-libdir=DL_LIBDIR       dl package 'lib' directory (overwrites '-root')
  --readline-lib=READLINE_LIB
                              readline library name (default: readline)
  --readline-root=READLINE_ROOT
                              readline package root
  --readline-incdir=READLINE_INCDIR
                              readline package 'include' directory (overwrites
                                '-root')
  --readline-libdir=READLINE_LIBDIR
                              readline package 'lib' directory (overwrites
                                '-root')
  --blas-lib=BLAS_LIB         blas library name (default: blas)
  --blas-root=BLAS_ROOT       blas package root
  --blas-incdir=BLAS_INCDIR   blas package 'include' directory (overwrites
                                '-root')
  --blas-libdir=BLAS_LIBDIR   blas package 'lib' directory (overwrites
                                '-root')
  --lapack-lib=LAPACK_LIB     lapack library name (default: lapack)
  --lapack-root=LAPACK_ROOT   lapack package root
  --lapack-incdir=LAPACK_INCDIR
                              lapack package 'include' directory (overwrites
                                '-root')
  --lapack-libdir=LAPACK_LIBDIR
                              lapack package 'lib' directory (overwrites
                                '-root')
  --f2c-lib=F2C_LIB           gfortran library name (default: gfortran)
  --f2c-root=F2C_ROOT         f2c package root
  --f2c-incdir=F2C_INCDIR     f2c package 'include' directory (overwrites
                                '-root')
  --f2c-libdir=F2C_LIBDIR     f2c package 'lib' directory (overwrites '-root')
  --cfitsio-lib=CFITSIO_LIB   cfitsio library name (default: cfitsio)
  --cfitsio-root=CFITSIO_ROOT
                              cfitsio package root
  --cfitsio-incdir=CFITSIO_INCDIR
                              cfitsio package 'include' directory (overwrites
                                '-root')
  --cfitsio-libdir=CFITSIO_LIBDIR
                              cfitsio package 'lib' directory (overwrites
                                '-root')
  --wcs-lib=WCS_LIB           wcs library name (default: wcs)
  --wcs-root=WCS_ROOT         wcs package root
  --wcs-incdir=WCS_INCDIR     wcs package 'include' directory (overwrites
                                '-root')
  --wcs-libdir=WCS_LIBDIR     wcs package 'lib' directory (overwrites '-root')
  --extra-cppflags=EXTRA_CPPFLAGS
                              Extra pre-processor flags
  --extra-cxxflags=EXTRA_CXXFLAGS
                              Extra c++ compiler falgs
  --extra-cflags=EXTRA_CFLAGS
                              Extra c compiler flags
  --extra-linkflags=EXTRA_LINKFLAGS
                              Extra linker flags
  --extra-fflags=EXTRA_FFLAGS
                              Extra fortran compiler flags
  --extra-includedir=EXTRA_INCLUDEDIR
                              Extra 'include' dir(s)
  --extra-librarydir=EXTRA_LIBRARYDIR
                              Extra 'lib' dir(s)
  --extra-ldlibrarypath=EXTRA_LDLIBRARYPATH
                              Extra (DY)LD_LIBRARY_PATH
  --extra-libs=EXTRA_LIBS     Extra libraries for linker
  --extra-path=EXTRA_PATH     Extra PATH (bin) to search
  --extra-root=EXTRA_ROOT     Extra hierachy root to search
  --with-cc=CC                The c compiler
  --with-cxx=CXX              The c++ compiler
  --with-fortran=FORTRAN      The fortran compiler
  --prefix=PREFIX             The installation prefix (default: /usr/local)
  --eprefix=EPREFIX           
  --bindir=BINDIR             The installation bin directory (default:
                                /usr/local/bin)
  --libdir=LIBDIR             The installation lib directory (default:
                                /usr/local/lib)
  --includedir=INCLUDEDIR     The installation include directory (default:
                                /usr/local/include)
  --sharedir=SHAREDIR         The installation share directory (default:
                                /usr/local/share)
```