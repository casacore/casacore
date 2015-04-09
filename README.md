
# Casacore

A Suite of c++ libraries for radio astronomy data processing.


# Installation

## Obtaining the source

The casacore source code is maintained on github.

You can obtain it using:

```
$ git clone https://github.com/casacore/casacore
```

## Requirements

To compile casacore you need to meet the following requirements:

* cmake
* gfortran
* ncurses
* flex 
* bison
* blas
* lapack
* cfitsio (3.181 or later)
* wcslib (4.20 or later)
* fftw3 (optional)
* hdf5 (optional)
* numpy (optional)
* boost-python (optional)

On Debian / Ubuntu you can install these with:
 ``` 
$ sudo apt-get install cmake gfortran libncurses5-dev libreadline-dev flex \
    bison libblas-dev liblapacke-dev libcfitsio3-dev wcslib-dev
```

and the optional libraries:
```
$ sudo apt-get install libhdf5-serial-dev libfftw3-dev python-numpy \
    libboost-python-dev
```


## Obtaining measures data

Various parts of casacore require measures data, which requires regular
updating. You can obtain the WSRT measures archive from the ASTRON FTP server:

ftp://ftp.astron.nl/outgoing/Measures/

Extract this somewhere on a permanent location on your filesystem.


## Compilation

In the casacore source folder run:
```
mkdir build
cd build
cmake ..
make 
make install
```

there are various flags available to cmake to enable and disable options:
```
$ cmake -DUSE_FFTW3=ON -DDATA_DIR=/usr/share/casacore/data -DUSE_OPENMP=ON \
    -DUSE_HDF5=ON -DBUILD_PYTHON=ON -DUSE_THREADS=ON
```

The `DATA_DIR` should point to the location where you extracted the measures
data.


## Ubuntu 14.04 packages

If you run Ubuntu 14.04 you can use precompiled binary packages

https://launchpad.net/~radio-astro/+archive/ubuntu/main

installation commands:
```
sudo apt-get install software-properties-common
sudo add-apt-repository ppa:radio-astro/main
sudo apt-get update
sudo apt-get install casacore2 casacore-data
```



# Documentation

http://casacore.github.io/casacore


# Problems & bugs

If you have any issues compiling or using casacore, please open an issue on
the issue tracker on github. If you have patches please open a pull request.


