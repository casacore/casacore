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
        # custom binary bottle with py3, not provided by brew and compile is too slow
        # https://github.com/casacore/casacore/wiki/How-to-create-a-python-boost-bottle-with-python3-enabled
        curl -L -O https://bintray.com/artifact/download/casacore/homebrew-bottles/boost-python-1.61.0.el_capitan.bottle.1.tar.gz
        brew install boost-python-1.61.0.el_capitan.bottle.1.tar.gz
        ls /usr/local/Cellar
        tar cfz "$cachefile" --directory /usr/local/Cellar szip cfitsio wcslib fftw hdf5 ccache boost-python
    fi
    brew install gcc
    # 93M with gz, 85M with xz -9
    ls -lh "$cachefile"
fi
