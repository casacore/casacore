#!/bin/bash

set -e
set -x

wget http://www.iausofa.org/2015_0209_F/sofa_f-20150209_a.tar.gz -O /tmp/sofa.tgz
tar -xzf /tmp/sofa.tgz
cd sofa/20150209_a/f77/src/ && make && make test && cd ../../../../

mkdir build
cd build

wget ftp://ftp.astron.nl/outgoing/Measures/WSRT_Measures.ztar
tar zxvf WSRT_Measures.ztar

# no extern templates makes the build a lot larger
if [ "$CXX11" = "False" ]; then
  ccache -M 160M
else
  ccache -M 80M
fi

if [ "$TRAVIS_OS_NAME" = osx ]; then
    BUILD_PYTHON3=OFF
else
    BUILD_PYTHON3=ON
fi

CXX="ccache $CXX" cmake .. \
    -DUSE_FFTW3=ON \
    -DBUILD_TESTING=ON \
    -DUSE_OPENMP=OFF \
    -DUSE_HDF5=ON \
    -DBUILD_PYTHON=ON \
    -DBUILD_PYTHON3=$BUILD_PYTHON3 \
    -DDATA_DIR=$PWD \
    -DSOFA_ROOT_DIR=$HOME \
    -DCXX11=$CXX11 \
    -DCMAKE_INSTALL_PREFIX=${TRAVIS_BUILD_DIR}/installed

