#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
    brew update >/dev/null
    brew tap homebrew/science
    brew install cfitsio wcslib fftw hdf5
    curl -L -O https://bintray.com/artifact/download/casacore/homebrew-bottles/boost-python-1.60.0.el_capitan.bottle.1.tar.gz
    brew install ./boost-python-1.60.0.el_capitan.bottle.1.tar.gz
fi
