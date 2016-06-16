#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
    cachefile="$HOME/.ccache/homebrew-cache.tar.gz"
    if [ -e "$cachefile" ]; then
        tar xf "$cachefile" --directory /usr/local/Cellar
        brew link szip cfitsio wcslib fftw hdf5 ccache boost-python
    else
        brew update >/dev/null
        brew tap homebrew/science
        brew install cfitsio wcslib fftw hdf5 ccache
        curl -L -O https://bintray.com/artifact/download/casacore/homebrew-bottles/boost-python-1.60.0.el_capitan.bottle.1.tar.gz
        brew install ./boost-python-1.60.0.el_capitan.bottle.1.tar.gz
        ls /usr/local/Cellar
        tar cfz "$cachefile" --directory /usr/local/Cellar szip cfitsio wcslib fftw hdf5 ccache boost-python
    fi
    # 93M with gz, 85M with xz -9
    ls -lh "$cachefile"
fi
