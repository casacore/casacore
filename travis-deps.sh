#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = linux ]; then

    sudo apt-get update -q
    sudo apt-get install -qy python-software-properties
    sudo add-apt-repository -y ppa:radio-astro/main
    sudo apt-get install -qy cmake flex bison libblas-dev liblapack-dev \
            libcfitsio3-dev wcslib-dev libfftw3-dev gfortran libncurses5-dev \
            libreadline6-dev libhdf5-serial-dev python-all-dev libboost-dev \
            libboost-python-dev python-numpy

elif [ "$TRAVIS_OS_NAME" = osx ]; then
    brew tap homebrew/science
    brew install cmake bison blas lapack cfitsio wcslib libfftw3 libhdf5 boost-python numpy
fi
