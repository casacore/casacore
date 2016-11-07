#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
    cachefile="$HOME/.ccache/homebrew-cache.tar.gz"
    if [ -e "$cachefile" ]; then
        tar xf "$cachefile" --directory /usr/local/Cellar
        brew link szip cfitsio wcslib fftw hdf5 ccache boost-python gcc
    else
        brew update >/dev/null
        brew tap homebrew/science
        brew install cfitsio wcslib fftw hdf5 ccache boost-python gcc
        ls /usr/local/Cellar
        tar cfz "$cachefile" --directory /usr/local/Cellar szip cfitsio wcslib fftw hdf5 ccache boost-python gcc
    fi
    # 93M with gz, 85M with xz -9
    ls -lh "$cachefile"
fi
