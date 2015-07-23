#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
    brew update >/dev/null
    brew tap homebrew/science
    brew install cfitsio wcslib fftw hdf5 boost-python
fi
