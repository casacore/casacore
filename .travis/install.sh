#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
  brew install fftw hdf5 ccache boost-python boost-python3 python2 python3
fi
