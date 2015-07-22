#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = linux ]; then
    sudo apt-get update -q
    sudo apt-get install -qy cmake flex bison libblas-dev liblapack-dev \
            libcfitsio3-dev wcslib-dev libfftw3-dev gfortran libncurses5-dev \
            libreadline6-dev libhdf5-serial-dev python-all-dev libboost-dev \
            libboost-python-dev python-numpy valgrind

elif [ "$TRAVIS_OS_NAME" = osx ]; then
    brew tap homebrew/science
    brew install cmake cfitsio wcslib fftw hdf5 boost-python readline 
fi
