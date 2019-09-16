#!/bin/bash

alias ccache=true

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
    SOFA_ARCHIVE=sofa.tgz
    MEASURES_ARCHIVE=WSRT_Measures.ztar

    if [ ! -f "$SOFA_ARCHIVE" ]; then
      wget http://www.iausofa.org/2015_0209_F/sofa_f-20150209_a.tar.gz -O $SOFA_ARCHIVE
    fi
    
    if [ ! -f "$MEASURES_ARCHIVE" ]; then
        wget ftp://ftp.astron.nl/outgoing/Measures/WSRT_Measures.ztar -O $MEASURES_ARCHIVE
    fi

    cd sofa/20150209_a/f77/src/ && make && make test && cd ../../../../

    mkdir -p build
    cd build

   tar zxvf ../$SOFA_ARCHIVE
   tar zxvf ../$MEASURES_ARCHIVE

   ccache -M 80M

   pip3 install numpy

   CXX="ccache $CXX" cmake .. \
        -DUSE_FFTW3=ON \
        -DBUILD_TESTING=ON \
        -DUSE_OPENMP=OFF \
        -DUSE_HDF5=ON \
        -DBUILD_PYTHON=OFF \
        -DBUILD_PYTHON3=ON \
        -DPYTHON3_EXECUTABLE=/usr/local/bin/python3 \
        -DBOOST_PYTHON3_LIBRARY_NAME=python37 \
        -DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH} \
        -DDATA_DIR=$PWD \
        -DSOFA_ROOT_DIR=$PWD \
        -DCMAKE_INSTALL_PREFIX=${TRAVIS_BUILD_DIR}/installed
else
    docker build . -f .travis/${DIST}_${CC}.docker -t casacore/${DIST}_${CC}
fi
