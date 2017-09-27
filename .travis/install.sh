#!/bin/bash

set -e
set -x

if [ "$TRAVIS_OS_NAME" = osx ]; then
  wget https://repo.continuum.io/miniconda/Miniconda2-latest-MacOSX-x86_64.sh -O miniconda2.sh;

  brew update >/dev/null
  brew tap homebrew/science
  brew install cfitsio wcslib fftw hdf5 ccache

  bash miniconda2.sh -b -p $HOME/miniconda2
  export PATH="$HOME/miniconda2/bin:$PATH"
  hash -r
  conda config --set always_yes yes --set changeps1 no
  conda update -q conda
  conda config --add channels conda-forge
  conda info -a
  conda create -q -n testenv2 python=2.7 numpy
  source activate testenv2
  conda install -q -y -c meznom boost-python
fi
