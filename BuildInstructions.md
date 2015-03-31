# Introduction #

CMake is now used to build casacore. The CmakeInstructions page contains more details.

# Installing CMake #

CMake >= 2.6 is required. Check your operating system's package manager if it is available. If not get it CMake at [www.cmake.org](http://www.cmake.org/cmake/resources/software.html)

# Third party requirements #

**casacore** needs the following external libraries:

  * cfitsio
  * wcslib ([download](ftp://ftp.atnf.csiro.au/pub/software/wcslib/wcslib.tar.bz2))
  * lapack/blas
  * flex/bison
  * fortran compiler and fortran-to-c library, e.g. gfortran and libgfortran

**Note**: For OS X users we recommend using the gfortran compiler available [here](http://r.research.att.com/tools/).

The HDF5 package is optional (see http://www.hdfgroup.org/HDF5/index.html). If enabled (-DUSE\_HDF5), the HDF5 classes in the casacore package will be activated. For example, HDF5Image can be used to support images in HDF5 format.

The fftw3 package is also optional (see http://www.fftw.org). If enabled (-DUSE\_FFTW3), casacore's FFT classes will use fftw3 instead of fftpack from lapack. Both the single precision and double precision versions are needed. It is possible to use the single or multi-threaded version of fftw3 (see CmakeInstructions for details)

_readline_ support is optional through -DUSE\_READLINE.

# Building the casacore #

The current version can be retrieved via subversion:
```
svn co http://casacore.googlecode.com/svn/tags/casacore-1.7.0
```

or get the latest stable source from the main project page.

## For the impatient ##

If every dependency is in its default location (/usr/local) and no special options are required you can go ahead an build.

```
cd casacore-1.7.0
mkdir build; cd build
cmake ..
make
make install
```

After building casacore it is recommended to also run the tests.

For running casacore tests and using other programs depending on the casacore libraries you will need the measures' modules meta-data (e.g. leap second tables, observatory locations)
See also [CasaAipsRC](CasaAipsRC.md) for how to point to measures data explicitly.

A copy of this data is updated weekly made available at [ftp://ftp.atnf.csiro.au/pub/software/measures_data/measures_data.tar.bz2](ftp://ftp.atnf.csiro.au/pub/software/measures_data/measures_data.tar.bz2).

```
make test
```

For more detailed instructions go to CmakeInstructions.